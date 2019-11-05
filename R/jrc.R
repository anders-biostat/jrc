#' @import stringr
#' @import mime
#' @importFrom jsonlite fromJSON

#global variable with current page information
pageobj <- new.env()

handle_http_request <- function( req ) {
  
  reqPage <- req$PATH_INFO
  if(grepl("^/http_root", reqPage)) {
    pack <- substring(strsplit(reqPage, "/")[[1]][2], 11)
    reqPage <- sub(str_c("_", pack), "", reqPage)
    reqPage <- system.file( reqPage, package = pack )
  } else {
    if(reqPage == "/index.html" & !is.null(pageobj$startPagePath)) {
      reqPage <- pageobj$startPagePath
    } else {
      reqPage <- str_c(pageobj$rootDirectory, reqPage)
    }
  }
    
  if( !file.exists(reqPage) ) {
    reqPage <- str_remove(reqPage, pageobj$rootDirectory)
    if(!file.exists(reqPage)) {
      warning(str_interp("File '${reqPage}' is not found"))
      return( list( 
        status = 404L,
        headers = list( "Content-Type" = "text/html" ),
        body = "404: Resource not found" ) )
    }
  }
  
  content_type <- mime::guess_type(reqPage)
  content <- readLines(reqPage, warn = F)
  
  if(content_type == "text/html") {
    jsfile <- str_c("<script src='http_root_jrc/jrc.js'></script>")
    stop <- F
    for(i in 1:length(content))
      if(str_detect(content[i], regex("<head", ignore_case = T))) {
        stop <- T
        content[i] <- str_replace(content[i], regex("(<head[^>]*>)", ignore_case = T), str_c("\\1", jsfile))
      }
    #the document has no <head> tag
    if(!stop) {
      jsfile <- str_c("<head>", jsfile, "</head>")
      for(i in 1:length(content))
        if(str_detect(content[i], regex("<html", ignore_case = T))) {
          stop <- T
          content[i] <- str_replace(content[i], regex("(<html[^>]*>)", ignore_case = T), str_c("\\1", jsfile))
        }
    }
    if(!stop)
      content <- c(jsfile, content)
  }
  
  list(
    status = 200L,
    headers = list( 'Content-Type' = content_type ),
    body = str_c( content, collapse="\n" )
  )
}

execute <- function(msg) {

  if(msg[1] == "COM") {
    eval(parse(text = msg[2]), envir = pageobj$envir)
  } else if(msg[1] == "DATA") {
    assign(msg[[2]], msg[[3]], envir = pageobj$envir)
  } else if(msg[1] == "FUN") {
    # 1 = "FUN"
    # 2 - function name
    # 3 - list of arguments
    # 4 - assignTo
    # 5 - package
    chain <- strsplit(msg[[2]], "[$]")[[1]]
    if(is.na(msg[[5]])) {
      f <- get(chain[1], envir = pageobj$envir)
      chain <- chain[-1]
    } else {
      f <- getNamespace(msg[[5]])
    }
    for(el in chain) f <- f[[el]]
    
    tmp <- do.call(f, msg[[3]], envir = pageobj$envir)  

    if(!is.na(msg[[4]]))
      assign(msg[[4]], tmp, envir = pageobj$envir)
  }
}

#' @importFrom stringi stri_rand_strings
#' @importFrom utils object.size
store <- function(msg) {
  if(pageobj$maxN == 0 | pageobj$maxSize == 0) {
    message(str_c("Message can't be stored, sincse message storage is set to zero. ",
                  "Please, use 'limitStorage' function to change the limits."))
    return()
  }
  id <- stri_rand_strings(1, 6)
  pageobj$storedMessages[[length(pageobj$storedMessages) + 1]] <- list(msg = msg, id = id, size = object.size(msg))

  if(msg[1] == "COM") {
    message(str_c("Command '", msg[2], "' is stored."))
  } else if(msg[1] == "DATA") {
    message(str_c("Assignment to the variable '", msg[2], "' is stored."))
  } else if(msg[1] == "FUN") {
    message(str_c("Call to the function '", msg[2], "' is stored."))
  }
  message(str_c("To authorize execution, please, type 'authorize(id = \"", id, "\")'"))
  callFunction("jrc.notifyStorage", list(id))
  
  cleanStorage()
}

cleanStorage <- function() {
  if(length(pageobj$storedMessages) > pageobj$maxN){
    message(str_c("Too many messages! Message with id '", pageobj$storedMessages[[1]]$id, "' removed"))
    pageobj$storedMessages[1] <- NULL
  }
  
  while(sum(sapply(pageobj$storedMessages, `[[`, "size")) > pageobj$maxSize & 
        length(pageobj$storedMessages) > 1){
    message(str_c("Messages size is too big! Message with id '", pageobj$storedMessages[[1]]$id, "' removed"))
    pageobj$storedMessages[1] <- NULL
  }
}

handle_websocket_open <- function( ws ) {
  ws$onMessage( function( isBinary, msg ) {
    if( isBinary )
      stop( "Unexpected binary message received via WebSocket" )
    msg <- fromJSON(msg)
    if(!(msg[1] %in% c("COM", "FUN", "DATA")))
      stop(str_interp("Unknown message type: ${msg[1]}"))
    
    if(msg[1] == "COM") {
      store(msg) #vector of characters
    } 
    if(msg[1] == "DATA") {
      if(!is.character(msg[2]))
        stop("Invalid message structure. Variable name is not character.")
      
      msg <- as.list(msg)
      msg[[3]] <- fromJSON(msg[[3]])
      
      if(msg[[2]] %in% pageobj$allowedVars) {
        execute(msg)
      } else {
        store(msg)
      }
    }
    
    if(msg[1] == "FUN") {
      if(!is.character(msg[2]))
        stop("Invalid message structure. Function name is not character.")
      #make sure that function arguments is a list
      
      msg <- as.list(msg)
      msg[[3]] <- fromJSON(msg[[3]])
      
      msg[[3]] <- as.list(msg[[3]])
      if(!is.list(msg[[3]]))
        stop("Invalid message structure. List of arguments is not a list.")
      #go through all arguments and turn to numeric
      
      if(msg[[2]] %in% pageobj$allowedFuns & (is.na(msg[[4]]) | msg[[4]] %in% pageobj$allowedVars)) {
        execute(msg)
      } else {
        store(msg)
      }
    }
  
  } );
  if(is.null(pageobj$websocket)) {
    message("WebSocket opened")
    pageobj$websocket <- ws
  } else {
    stop("WebSocket for this page is already opened. If you want to open page in your 
         browser instead of R Viewer, use 'openPage(useViewer = FALSE)'")
  }
}

#' Create a server
#' 
#' \code{openPage} creates a server and establishes a websocket connection between it and the current
#' R session. This allows commands exchange. In R use \code{\link{sendCommand}} function to send and 
#' execute JavaScript code on the server. On the server use \code{jrc.sendCommand} function to send and
#' execute R code in the current R session. 
#' 
#' @param startPage A path to the HTML file that should be opened, when the server is initialised.
#' This can be an absolute path to a local file, or it can be relative to the \code{rootDirectory}
#' or to the current R working directory. If \code{startPage} is not defined, this function opens an 
#' empty HTML page. The file must have \emph{.html} extension.
#' @param rootDirectory A path to the root directory of the server. If \code{rootDirectory} is not 
#' defined, the \code{http_root} in the package directory will be used as a root directory.
#' @param useViewer If \code{TRUE}, the start page will be opened in the RStudio Viewer. If \code{FALSE}
#' a default web browser will be used.
#' @param port Defines which TCP port to use for websocket connection. If not defined, random available port
#' is used.
#' @param browser A browser in which the web page will be opened. Is used only if \code{useViewer = FALSE}.
#' If not defined, default browser will be used. For more information check \code{\link[utils]{browseURL}}.
#' @param allowedFunctions List of functions that can be called from the web page without any additional actions 
#' from the user. All other functions will require authorization in the current R session to be executed. 
#' This should be a vector of function names. Check \code{\link{authorize}} and \code{\link{allowFunctions}}
#' for more information. 
#' @param allowedVariables List of variables that can be reassigned from the web page without any additional actions 
#' from the user. All other reassignments will require authorization in the current R session to be executed. 
#' This should be a vector of variable names. Check \code{\link{authorize}} and \code{\link{allowVariables}}
#' for more information.
#' 
#' @seealso \code{\link{closePage}}, \code{\link{setEnvironment}}, \code{\link{limitStorage}}, \code{\link{allowVariables}},
#' \code{\link{allowFunctions}}.
#' 
#' @export
#' @import httpuv
#' @importFrom utils browseURL
#' @importFrom utils compareVersion
#' @importFrom utils packageVersion
openPage <- function(useViewer = T, rootDirectory = NULL, startPage = NULL, port = NULL, browser = getOption("browser"),
                     allowedFunctions = NULL, allowedVariables = NULL) {
  closePage()
  
  if(is.null(rootDirectory))
    rootDirectory = system.file("http_root", package = "jrc")
  if(is.null(startPage)) 
    startPage <- system.file( "http_root/index.html", package="jrc" )
  
  if(!dir.exists(rootDirectory))
    stop(str_interp("There is no such directory: '${rootDirectory}'"))
  pageobj$rootDirectory <- normalizePath(rootDirectory)
  
  if(file.exists(file.path(pageobj$rootDirectory, startPage))){
    pageobj$startPage <- startPage
  } else {
    if(!file.exists(startPage))
      stop(str_interp("There is no such file: '${startPage}'"))
    startPage <- normalizePath(startPage)
    if(grepl(startPage, pageobj$rootDirectory, fixed = T)) {
      pageobj$startPage <- str_remove(startPage, str_c(pageobj$rootDirectory, "/"))
    } else {
      pageobj$startPage <- "index.html"
      pageobj$startPagePath <- startPage
    }
  }
  
  pageobj$maxN <- Inf
  pageobj$maxSize <- Inf
  
  if(!is.null(allowedFunctions) & !is.vector(allowedFunctions))
    stop("'allowedFunctions' must be a vector of function names.")
  if(!is.null(allowedVariables) & !is.vector(allowedVariables))
    stop("'allowedVariables' must be a vector of variable names.")
  
  pageobj$allowedFuns <- allowedFunctions
  pageobj$allowedVars <- allowedVariables
  pageobj$storedMessages <- list()
  pageobj$app <- list( 
    call = handle_http_request,
    onWSOpen = handle_websocket_open )
  
  if(is.null(port)) {
    if(compareVersion(as.character(packageVersion("httpuv")), "1.5.4") >= 0){
      port <- randomPort(n = 50)
    } else {
      #if there is no randomPort function in the httpuv package
      #in later versions of jrc this will be removed and httpuv >= 1.5.2 will be required
      #code adopted from httpuv::randomPort
      for (port in sample(seq(1024L, 49151L), 50)) {
        s <- NULL
        
        # Check if port is open
        tryCatch(
          s <- startServer("0.0.0.0", port, list(), quiet = TRUE),
          error = function(e) { }
        )
        if (!is.null(s)) {
          s$stop()
          break
        }
      }
    }
  }
  port <- as.integer(port)
  if(is.na(port))
    stop("Port number must be an integer number.")
    
  if(!(compareVersion(as.character(packageVersion("httpuv")), "1.3.5") > 0)) {
    pageobj$httpuv_handle <- startDaemonizedServer( "0.0.0.0", port, pageobj$app )
  } else {
    pageobj$httpuv_handle <- startServer( "0.0.0.0", port, pageobj$app )
  }

  if( useViewer & !is.null( getOption("viewer") ) )
    getOption("viewer")( str_c("http://localhost:", port, "/", pageobj$startPage) )
  else
    browseURL( str_c("http://localhost:", port, "/", pageobj$startPage), browser = browser )
  
  pageobj$envir <- globalenv()
  
  # Wait up to 5 seconds for the a websocket connection
  # incoming from the client
  for( i in 1:(5/0.05) ) {
    service(100)
    if( !is.null(pageobj$websocket) ){
      break
    } 
    Sys.sleep( .05 )
  }
  if( is.null(pageobj$websocket) ) {
    closePage()
    stop( "Timeout waiting for websocket." )
  }

  invisible(TRUE)  
}

#' Send a command to the server
#' 
#' \code{sendCommand} sends JavaScript code to the server and executes it on the currently
#' opened page. Use JavaScript function \code{jrc.sendCommand} to send R code from the server
#' and execute it in the current R session. All commands send to R from the server will be executed
#' only after authorization in the currently running R session.
#' @details Note, that in both cases commands are executed inside a function. Therefore use for R code use \code{<<-} instead
#' of \code{<-} to change global variables and in JavaScript use \code{windows.varibleName = "SomeValue"} or
#' \code{varibleName = "SomeValue"}. Variables declared like \code{var variableName = "SomeValue"} or 
#' \code{variableName <- "SomeValue"} will be accessible only within the current \code{sendCommand} call.
#' 
#' @param command A line (or several lines separated by \code{\\n}) of JavaScript code. This code
#' will be immediately executed on the opened page. No R-side syntax check is performed.
#' 
#' @examples  
#' \donttest{k <- 0
#' openPage()
#' sendCommand(paste0("button = document.createElement('input');",
#'               "button.type = 'button';",
#'               "button.addEventListener('click', function() {jrc.sendCommand('k <<- k + 1')});", 
#'               "button.value = '+1';",
#'               "document.body.appendChild(button);", collapse = "\n"))
#' closePage()}
#' 
#' @seealso \code{\link{authorize}}, \code{\link{sendData}}, \code{\link{sendHTML}}, \code{\link{callFunction}},
#' \code{\link{openPage}}.
#' 
#' @export
#' @importFrom jsonlite toJSON
sendCommand <- function(command) {
  if(is.null(pageobj$websocket))
    stop("There is no open page. Use 'openPage()' to create a new one.")
  
  pageobj$websocket$send( toJSON(c("COM", command)) )  
}


#' Stop server
#' 
#' Stop the server and close currently opened page (if any).
#' 
#' @seealso \code{\link{openPage}}
#' 
#' @export
closePage <- function() {
  if( !is.null(pageobj$httpuv_handle) ) {
    if( !is.null(pageobj$websocket) ) {
      pageobj$websocket$close()
    }
    if(compareVersion(as.character(packageVersion("httpuv")), "1.3.5") > 0) {
      stopServer(pageobj$httpuv_handle )
    } else {
      stopDaemonizedServer(pageobj$httpuv_handle)
    }
  }
  
  rm( list=ls(pageobj), envir=pageobj )
}

#' Send data to the server
#' 
#' Sends a variable to the server, where it is assigned to the variable with a specified name. A JavaScript function
#' \code{jrc.sendData(variableName, variable)} can send data back from the server to the current R session. If variable
#' name hasn't been previously added to the list of the allowed variables, attempt to assign it from the server will
#' require manual authorization in the R session. 
#' 
#' @param variableName Name that the variable will have on the server.
#' @param variable Variable to send.
#' @param keepAsVector If \code{TRUE}, variables with length 1 will be saved as arrays on the server, otherwise they 
#' will be converted to atomic types.
#' @param rowwise If \code{TRUE}, matrices and data.frames will be transformed into JavaScript Objects or Arrays
#' rowwise (e.g. a matrix will become an Array of its rows). 
#' 
#' @examples 
#' \donttest{openPage()
#' x <- 1:100
#' sendData("x", x)
#' sendCommand("console.log(x);")
#' sendCommand("jrc.sendData('x', x.filter(function(e) {return e % 2 == 0}))")
#' closePage()}
#' 
#' @seealso \code{\link{authorize}}, \code{\link{allowVariables}}, \code{\link{sendCommand}},
#' \code{\link{callFunction}}, \code{\link{sendHTML}}, \code{\link{openPage}}.
#'  
#' @export
#' @importFrom jsonlite toJSON
sendData <- function(variableName, variable, keepAsVector = FALSE, rowwise = TRUE) {
  if(is.null(pageobj$websocket))
    stop("There is no open page. Use 'openPage()' to create a new one.")
  
  if(rowwise) {
    dataframe <- "rows"
    matrix <- "rowmajor"
  } else  {
    dataframe <- "columns"
    matrix <- "columnmajor"
  }
  pageobj$websocket$send( toJSON(c("DATA", variableName, 
                                   toJSON(variable, digits = NA, dataframe = dataframe, matrix = matrix), 
                                   keepAsVector)))
}

#' Set Environment
#' 
#' Defines the environment, where the commands, received from the server, will be evaluated. By default,
#' \code{globalenv()} is used.
#' 
#' @param envir Environment where to evaluate the commands.
#' 
#' @examples
#' setEnvironment(environment())
#' 
#' @export
setEnvironment <- function(envir) {
  pageobj$envir <- envir
}

#' Send HTML to the server
#' 
#' Sends a piece of HTML code to the server and adds it at the end
#' or the \code{body} element.
#' 
#' @param html HTML code that will be added to the web page.
#' 
#' @examples 
#' \donttest{sendHTML("Test...")
#' sendHTML("This is <b>bold</b>")
#' sendHTML("<table><tr><td>1</td><td>2</td></tr><tr><td>3</td><td>4</td></tr></table>")}
#' 
#' @seealso \code{\link{sendData}}, \code{\link{sendCommand}}, \code{\link{callFunction}},
#' \code{\link{openPage}}.
#' 
#' @export
sendHTML <- function(html = "") {
  if(!is.character(html))
    stop("html must be a character string")
  #html <- str_replace_all(html, "(\\W)", "\\\\\\1")

  if(is.null(pageobj$websocket))
    stop("There is no open page. Use 'openPage()' to create a new one.")
  
  pageobj$websocket$send( toJSON(c("HTML", html)) )
}

#' Trigger a function call
#' 
#' Calls a function on the opened web page given its name and arguments.
#' JavaScript counterpart is \code{jrc.callFunction(name, arguments, assignTo, package)}, 
#' where the \code{package} argument allow to call function from some other
#' package. The result, however, will be anyway assigned to a variable in the
#' environment set by \code{\link{setEnvironment}}.
#' For security reasons, if function or variable to which its returned value
#' should be assigned are not in the lists of allowed functions and variables,
#' manual authorization of the call form JavaScript in the R session will be 
#' required. For more details check \code{\link{authorize}}.
#' 
#' @param name Name of the function. If the function is a method of some object
#' its name must contain the full chain of calls (e.g. \code{myArray.sort} or 
#' \code{Math.rand}).
#' @param arguments List of arguments for the function. Note that in JavaScript 
#' arguments must be given in a fixed order, naming is not necessary and will 
#' be ignored.
#' @param assignTo Name of a variable to which will be assigned the returned value
#' of the called function. If variable with this name doesn't exist, it will be added
#' to the currently active environment.
#' @param thisArg JavaScript functions (methods) can belong to some object, which 
#' is referred as \code{this} inside the function (e.g. in
#' \code{someObject.myFunction()} function \code{myFunction} is a method of \code{someObject}).
#' \code{thisArg} specified object that will be passed as \code{this} to the function. If \code{NULL}
#' then the function will be applied to the global object.
#' @param ... further arguments passed to \code{\link{sendData}} that is used to send
#' \code{arguments} to the web server.
#' 
#' @examples 
#' \donttest{
#' openPage()
#' callFunction("alert", "Some alertText")
#' callFunction("Math.random", assignTo = "randomNumber")
#' }
#' 
#' @seealso \code{\link{authorize}}, \code{\link{allowFunctions}}, \code{\link{allowVariables}},
#' \code{\link{setEnvironment}}.
#' 
#' @export
callFunction <- function(name, arguments = NULL, assignTo = NULL, thisArg = NULL, ...) {
  if(is.null(pageobj$websocket))
    stop("There is no open page. Use 'openPage()' to create a new one.")
  if(!is.character(name))
    stop("Function name must be a character")
  if(!is.null(assignTo) & !is.character(assignTo))
    stop("Variable name in 'assignTo' must be a character")
  
  if(!is.null(arguments)) {
    if(!is.list(arguments))
      stop("Arguments must be a list")
    names(arguments) <- NULL
    sendData("___args___", arguments, ...)
  }
  
  pageobj$websocket$send(toJSON(c("FUN", name, assignTo)))
}

#' Authorize further message processing
#' 
#' `jrc` library allows one to get a full control over the currently running R session from 
#' a web page. Therefore for security reasons one should manually authorize function calls,
#' variable assignments or expression evaluations. All the received messages that are not
#' processed automatically are given an ID and stored. This function executes a command
#' from a message with a given ID
#' 
#' Expressions has to be always authorized before evaluation. One can specify a list of
#' variables that can be reassigned automatically and functions that can be called without
#' authorization.
#' 
#' @param id ID of the message to be processed. In `jrc` library all stored messages are
#' give a random ID that consists of 6 letters or numbers. The ID is reported when the
#' message is stored. To get a list of IDs of all currently stored messages, set this argument
#' to \code{NULL}.
#' @param show If \code{TRUE} information of the message with a given ID will be show before executing
#' it with a choice to go on with execution, ignore the message (meaning it will be removed from memory) or
#' do nothing.
#' 
#' @return A vector of IDs of all currently stored messages.
#' 
#' @seealso \code{\link{allowFunctions}}, \code{\link{allowVariables}}, \code{\link{sendCommand}},
#' \code{\link{sendData}}, \code{\link{callFunction}}, \code{\link{limitStorage}}.
#' 
#' @export
#' @importFrom utils menu
authorize <- function(id = NULL, show = FALSE) {
  if(is.null(id)) return(sapply(pageobj$storedMessages, `[[`, "id"))
  
  if(!is.logical(show))
    stop("show must be a logical variable")

  k <- which(sapply(pageobj$storedMessages, `[[`, "id") == id)
  if(length(k) == 0)
    stop(str_c("There is no message with id '", id, "'."))
  if(length(k) > 1) #well... Just in case))
    k <- k[1]
  
  if(!show) {
    tryCatch(execute(pageobj$storedMessages[[k]]$msg), finally = {pageobj$storedMessages[k] <- NULL})
  } else {
    type <- pageobj$storedMessages[[k]]$msg[1]
    if(type == "COM") {
      text <- str_c("Command '", pageobj$storedMessages[[k]]$msg[2], "'.")
    } else if(type == "DATA") {
      text <- str_c("Assignment of varible '", pageobj$storedMessages[[k]]$msg[[2]], 
                    "'. New type is '", typeof(pageobj$storedMessages[[k]]$msg[[3]]), "'. ",
                    "New size is ", object.size(pageobj$storedMessages[[k]]$msg[[3]]), " bytes.")
    } else if(type == "FUN") {
      text <- str_c("Call of function '", pageobj$storedMessages[[k]]$msg[[2]], "'.")
      if(!is.na(pageobj$storedMessages[[k]]$msg[[4]]))
        text <- str_c(text, " Results will be assigned to variable '", pageobj$storedMessages[[k]]$msg[[4]], "'.")
    }
    text <- str_c(text, " To cancel enter '0'.")
    
    choice <- menu(c("Execute", "Ignore"), 
         title = text)
    if(choice == 0) return()
    if(choice == 1) tryCatch(execute(pageobj$storedMessages[[k]]$msg))
    
    pageobj$storedMessages[k] <- NULL
  }
  
  invisible(sapply(pageobj$storedMessages, `[[`, "id"))
}

#' Allow function calls without authorization
#' 
#' This function adds function names to the list of functions, which
#' can be called from the web page without manual confirmation in the R
#' session.
#' 
#' @param funs Vector of function names to be added to the list. If is \code{NULL},
#' returns names of all currently allowed functions.
#' 
#' @return Names of all currently allowed functions if \code{funs = NULL}.
#' 
#' @examples
#' allowFunctions(c("myFunction1", "print", "someObject$method"))
#' funs <- allowFunctions()
#' 
#' @seealso \code{\link{allowVariables}}, \code{\link{authorize}}, \code{\link{openPage}} (check argument
#' \code{allowedFunctions}), \code{\link{callFunction}}.
#' 
#' @export
allowFunctions <- function(funs = NULL) {
  if(is.null(funs)) return(pageobj$allowedFuns)
  if(!is.vector(funs) | !is.character(funs))
    stop("'funs' must be a vector of function names")
   
  pageobj$allowedFuns <- unique(c(pageobj$allowedFuns, funs))
  invisible(pageobj$allowedFuns)
}

#' Allow variable assignment without authorization
#' 
#' This function adds variable names to the list of variables, which
#' can be reassigned from the web page without manual confirmation in the R
#' session.
#' 
#' @param vars Vector of variable names to be added to the list. If is \code{NULL},
#' returns names of all currently allowed variables.
#' 
#' @examples
#' allowVariables(c("myVariable", "anotherOne"))
#' vars <- allowVariables()
#' 
#' @return Names of all currently allowed variables if \code{vars = NULL}.
#' 
#' @seealso \code{\link{allowFunctions}}, \code{\link{authorize}}, \code{\link{openPage}} (check argument
#' \code{allowedVariables}), \code{\link{sendData}}.
#' 
#' @export
allowVariables <- function(vars = NULL) {
  if(is.null(vars)) return(pageobj$allowedVars)
  if(!is.vector(vars) | !is.character(vars))
    stop("'funs' must be a vector of function names")
  
  pageobj$allowedVars <- unique(c(pageobj$allowedVars, vars))
  invisible(pageobj$allowedVars)
}

#' Change size of the message storage
#' 
#' This function allows to change number of total size of the messages
#' that are received via the websocket and are stored in the memory.
#' 
#' For security reasons, the control of the currently running R session is limited
#' to calling only some user specified functions and reassigning some user specified
#' variables. All other messages are stored in the memory and can be later processed
#' by calling \code{\link{authorize}} function. To prevent overuse of memory, one can 
#' limit the size of the storage by number of messages or by their total size estimated
#' by \code{\link[utils]{object.size}}. If the storage grows above the set limits, older
#' messages are removed. The last received message will not be removed even if its 
#' takes more memory than is allowed by this function.
#' 
#' @param n Number of messages that can be stored simultaneously.
#' @param size Maxim total size of all stored messages in bytes.
#' 
#' @return Current maximum size of the storage and maximum allowed number of stored messages.
#' 
#' @examples 
#' limitStorage(n = 10)
#' limitStorage(size = 10 * 1024^2)
#' lim <- limitStorage()
#' 
#' @seealso \code{\link{authorize}}, \code{\link{allowFunctions}}, \code{\link{allowVariables}}.
#' 
#' @export
limitStorage <- function(n = NULL, size = NULL) {
  if(!is.null(n)) {
    if(!is.numeric(n))
      stop("Maximum number of stored messages 'n' must be numeric")
    if(n < 0)
      stop("Maximum number of stored messages 'n' must be non-negative")
    pageobj$maxN <- n
  }
  if(!is.null(size)) {
    if(!is.numeric(size))
      stop("Maximum size of stored messages 'size' must be numeric")
    if(size < 0)
      stop("Maximum size of stored messages 'size' must be non-negative")
    pageobj$maxSize <- size
  }
  c(n = pageobj$maxN, size = pageobj$maxSize)
}

#' Get opened page
#' 
#' Checks if there is a currently opened page. If so, returns an object with all
#' the information about the current session.
#' 
#' TO DO: Do we really need this function?
#' 
#' @return page-handling object if there is a currently opened jrc page, \code{NULL} otherwise.
#' 
#' @export
getPage <- function() {
  if(!is.null(pageobj$websocket))
    return(pageobj)
}