#' @import stringr
#' @import mime
#' @import R6
#' @importFrom jsonlite fromJSON
#' @importFrom utils object.size

#Should it be exported????
#' @importFrom stringi stri_rand_strings
Session <- R6Class("Session", public = list(
  id = "",
  lastActive = NULL,
  maxN = Inf,
  maxSize = Inf,
  startDate = NULL,
  
  storeMessage = function(msg) {
    
    if(self$maxN == 0 | self$maxSize == 0) {
      message(str_c("Message can't be stored, sincse message storage is set to zero. ",
                    "Please, use 'limitStorage' function to change the limits."))
      return()
    }
    if(!is.vector(msg))
      stop("Unknown message format")

    if(msg[1] == "COM") {
      message(str_c("Command '", msg[2], "' is stored."))
    } else if(msg[1] == "DATA") {
      message(str_c("Assignment to the variable '", msg[2], "' is stored."))
    } else if(msg[1] == "FUN") {
      message(str_c("Call to the function '", msg[2], "' is stored."))
    } else {
      stop("Unknown message type. Must be one of 'COM', 'DATA' or 'FUN'")
    }
    
    messageId <- stri_rand_strings(1, 6)
    private$storage[[id]] <- list(msg = msg, size = object.size(msg), id = messageId)
    
    message(str_c("To authorize execution, please, type 'authorize(id = \"", messageId, "\")'"))
    self$callFunction("jrc.notifyStorage", list(messageId))
    self$lastActive <- Sys.time()
    
    private$cleanStorage()
  },
  execute = function(messageId) {
    msg <- self$getMessage(messageId)
    if(is.null(msg))
      stop(str_c("There is no message with ID ", messageID))
    
    tryCatch({
      if(msg[1] == "COM") {
        eval(parse(text = msg[2]), envir = private$envir)
      } else if(msg[1] == "DATA") {
        assign(msg[[2]], msg[[3]], envir = private$envir)
      } else if(msg[1] == "FUN") {
        # 1 = "FUN"
        # 2 - function name
        # 3 - list of arguments
        # 4 - assignTo
        # 5 - package
        chain <- strsplit(msg[[2]], "[$]")[[1]]
        if(is.na(msg[[5]])) {
          f <- get(chain[1], envir = private$envir)
          chain <- chain[-1]
        } else {
          f <- getNamespace(msg[[5]])
        }
        for(el in chain) f <- f[[el]]
        
        environment(f) <- private$envir
        tmp <- do.call(f, msg[[3]], envir = private$envir)  
        
        if(!is.na(msg[[4]]))
          assign(msg[[4]], tmp, envir = private$envir)
      }
    }, finally = {self$removeMessage(messageId); self$lastActive <- Sys.time()})
  },
  
  getMessage = function(messageId) {
    if(!is.character(messageId))
      stop("Message ID must be a string")
    if(length(messageId) > 1) {
      warning("An attepmt to supply several message IDs. Only the first one will be used")
      messageId <- messageId[1]
    }
    
    private$storage[[messageId]]
  },
  removeMessage = function(messageId) {
    if(!is.character(messageId))
      stop("Message ID must be a string")
    if(length(messageId) > 1) {
      warning("An attepmt to supply several message IDs. Only the first one will be used")
      messageId <- messageId[1]
    }
  
    private$storage[[messageId]] <- NULL
    
    invisible(self)
  },
  
  sendCommand = function(command) {
    if(is.null(private$ws))
      stop("Websocket is already closed.")
    
    stopifnot(is.character(command))
      
    private$ws$send( toJSON(c("COM", command)) )      
  },
  
  callFunction = function(name, arguments = NULL, assignTo = NULL, thisArg = NULL,  ...) {
    if(!is.null(private$ws))
      stop("Websocket is already closed.")
    
    if(!is.character(name))
      stop("Function name must be a character")
    if(!is.null(assignTo) & !is.character(assignTo))
      stop("Variable name in 'assignTo' must be a character")
    
    if(!is.null(arguments)) {
      if(!is.list(arguments))
        stop("Arguments must be a list")
      names(arguments) <- NULL
      self$sendData("___args___", arguments, ...)
    }
    
    private$ws$send(toJSON(c("FUN", name, assignTo)))
    
  },
  
  sendData = function(variableName, variable, keepAsVector = FALSE, rowwise = TRUE) {
    if(!is.null(private$ws))
      stop("Websocket is already closed.")
    
    stopifnot(is.character(variableName))
    if(length(variableName) > 1) {
      warning("An attempt to supply multiple variable names. Only the first one will be used.")
      variableName <- variableName[1]
    }
    
    if(rowwise) {
      dataframe <- "rows"
      matrix <- "rowmajor"
    } else  {
      dataframe <- "columns"
      matrix <- "columnmajor"
    }
    private$ws$send( toJSON(c("DATA", variableName, 
                                      toJSON(variable, digits = NA, dataframe = dataframe, matrix = matrix), 
                                      keepAsVector)))
  },
  
  sendHTML = function(html) {
    if(!is.null(private$ws))
      stop("Websocket is already closed.")
    
    stopifnot(is.character(html))

    private$ws$send( toJSON(c("HTML", html)) )    
  },
  
  authorize = function(messageId = NULL, show = FALSE) {
    
    if(is.null(messageId)) return(sapply(private$storage, `[[`, "id"))
    
    if(!is.logical(show))
      stop("show must be a logical variable")
    
    k <- which(sapply(pageobj$storedMessages, `[[`, "id") == id)
    if(length(k) == 0)
      stop(str_c("There is no message with id '", id, "'."))
    if(length(k) > 1) #well... Just in case))
      k <- k[1]
  
    if(!show) {
      self$execute(messageId)
    } else {
      msg <- self$getMessage(messageId)
      if(is.null(msg))
        stop(str_c("There is no message with ID ", messageId))
  
      if(msg[1] == "COM") {
        text <- str_c("Command '", msg[2], "'.")
      } else if(type == "DATA") {
        text <- str_c("Assignment of varible '", msg[[2]], 
                      "'. New type is '", msg[[3]], "'. ",
                      "New size is ", msg[[3]], " bytes.")
      } else if(type == "FUN") {
        text <- str_c("Call of function '", msg[[2]], "'.")
        if(!is.na(msg[[4]]))
          text <- str_c(text, " Results will be assigned to variable '", msg[[4]], "'.")
      }
      text <- str_c(text, " To cancel enter '0'.")
      
      choice <- menu(c("Execute", "Ignore"), 
                     title = text)
      if(choice == 0) return()
      if(choice == 1) self$execute(messageId)
      
      self$removeMessage(messageId)
    }
    
    invisible(self)
  },
  
  setSessionVariables = function(vars) {
    list2env(vars, private$envir)
  },
  
  close = function(message = NULL) {
    if(!is.null(message)) {
      if(!is.character(message))
        stop("Closing message must be a string.")
  
      self$sendCommand(str_c("alert('", mesage, "');"))
    }
    if(!is.null(private$ws))
      private$ws$close()
  },
  
  initialize = function(ws, id = NULL, envir = NULL) {
    if(is.null(id))
      id <- stri_rand_strings(1, 6)
    if(!is.character(id))
      stop("Session ID must be a string")
    if(length(id) > 1) {
      warning("An attempt to supply multiple IDs for a new session. Only one will be used.")
      id <- id[1]
    }
    
    self$id <- id
    if(is.null(envir))
      envir <- new.env()
    stopifnot(is.environment(envir))
    
    private$envir <- envir
    
    self$lastActive <- Sys.time()
    self$startDate <- Sys.time()
    
    private$ws <- ws
    
    self$setSessionVariables(list(.id = id), id)      
  }
  
), private = list(
  ws = NULL,
  envir = NULL,
  storage = list(),
  
  cleanStorage = function() {
    if(length(self$storage) > self$maxN){
      message(str_c("Too many messages! Message with id '", self$storage[[1]]$id, "' removed"))
      self$storage[1] <- NULL
    }
    
    while(sum(sapply(self$storage, `[[`, "size")) > self$maxSize & 
          length(self$storage) > 1){
      message(str_c("Messages size is too big! Message with id '", self$storage[[1]]$id, "' removed"))
      self$storage[1] <- NULL
    }
  }
))

App <- R6Class("App", public = list(
  rootDirectory = "",
  startPage = "",
  
  addSession = function(session) {
    stopifnot(class(session) == "Session")
    if(length(private$sessions) >= self$maxCon) {
      session$close("Maximum number of active connections has been reached.")
      stop("Maximum number of connections has been reached. Please, close some of 
           the existing sessions, before adding a new one.")
    }
    oldSession <- self$getSession(session$id)
    if(!is.null(oldSession)) {
      warning(str_c("Session with id ", session$id, " already exists. Existing session will be closed."))
      self$closeSession(oldSession)
    }
    private$sessions[[session$id]] <- session
  },
  
  getSession = function(sessionId) {
    if(!is.character(sessionId))
      stop("Session ID must be a string")
    
    private$sessions[[sessionId]]
  },
  
  closeSession = function(session) {
    if(is.character(session))
      session <- self$getSession(session)
    stopifnot(class(session) == "Session")
    session$close()
    private$sessions[[session$id]] <- NULL
    
    invisible(self)
  },
  
  getSessionIds = function() {
    data.frame(id = names(private$sessions), startDate = sapply(private$sessions, `[[`, startDate), 
               lastActive = sapply(private$sessions, `[[`, lastActive))
  },
  
  stopServer = function() {
    lapply(private$sessions, self$closeSession)
    
    if(!is.null(private$serverHandle)) {
      if(compareVersion(as.character(packageVersion("httpuv")), "1.3.5") > 0) {
        stopServer(private$serverHandle)
      } else {
        stopDaemonizedServer(private$serverHandle)
      }
      message("Server has been stopped.")      
    }
  },
  
  startSever = function(port = NULL) {
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
      private$serverHandle <- startDaemonizedServer( "0.0.0.0", port, private$getApp() )
    } else {
      private$serverHandle <- startServer( "0.0.0.0", port, private$getApp() )
    }
  },
  
  openPage = function(useViewer = TRUE, browser = getOption("browser")) {
    if(is.null(private$serverHandle))
      stop("No server is running. Please, start a server before opening a page.")
    if( useViewer & !is.null( getOption("viewer") ) )
      getOption("viewer")( str_c("http://localhost:", port, "/", self$startPage) )
    else
      browseURL( str_c("http://localhost:", port, "/", self$startPage), browser = browser )
    
    
    # Wait up to 5 seconds for the a websocket connection
    # incoming from the client
    for( i in 1:(5/0.05) ) {
      service(100)
      if( length(private$sessions) > 0 ){
        break
      } 
      Sys.sleep( .05 )
    }
    if( length(private$sessions) == 0 ) {
      self$stopServer()
      stop( "Timeout waiting for websocket." )
    }    
  },
  
  setEnvironment = function(envir) {
    stopifnot(is.environment(evir))
    private$envir <- envir
  },
  
  allowFunctions = function(funs = NULL) {
    if(is.null(funs)) return(private$allowedFuns)
    if(!is.vector(funs) | !is.character(funs))
      stop("'funs' must be a vector of function names")
    
    private$allowedFuns <- unique(c(private$allowedFuns, funs))
    invisible(private$allowedFuns)
  },
  
  allowVariables = function(vars = NULL) {
    if(is.null(vars)) return(private$allowedVars)
    if(!is.vector(vars) | !is.character(vars))
      stop("'funs' must be a vector of function names")
    
    private$allowedVars <- unique(c(private$allowedVars, vars))
    invisible(private$allowedVars)
    
  },
  
  limitStorage = function(n = NULL, size = NULL) {
    if(!is.null(n)) {
      if(!is.numeric(n))
        stop("Maximum number of stored messages 'n' must be numeric")
      if(n < 0)
        stop("Maximum number of stored messages 'n' must be non-negative")
      private$maxCon <- n
    }
    if(!is.null(size)) {
      if(!is.numeric(size))
        stop("Maximum size of stored messages 'size' must be numeric")
      if(size < 0)
        stop("Maximum size of stored messages 'size' must be non-negative")
      private$maxSize <- size
    }
    
    c(n = private$maxN, size = private$maxSize)
  },
  
  setSessionVariables = function(vars, sessionId = NULL) {
    if(is.null(sessionId))
      sessionId = names(private$sessions)
    
    if(!is.list(vars))
      stop("Variables must be set as a list")
    if(length(vars) > 0 & is.null(names(vars)))
      stop("List of variables must be named")
    
    for(id in sessionId)
      self$getSession(id)$setSessionVariables(vars)
  },
  
  setRootDirectory = function(dir) {
    stopifnot(is.character(dir))
    
    if(!dir.exists(dir))
      stop(str_interp("There is no such directory: '${dir}'"))
    
    self$rootDirectory <- normalizePath(dir)
    
    invisible(self)
  },
  
  setStartPage = function(page) {
    stopifnot(is.character(page))
    
    if(file.exists(file.path(self$rootDirectory, page))){
      self$startPage <- page
    } else {
      if(!file.exists(page))
        stop(str_interp("There is no such file: '${page}'"))
      page <- normalizePath(page)
      if(grepl(page, self$rootDirectory, fixed = T)) {
        self$startPage <- str_remove(page, str_c(self$rootDirectory, "/"))
      } else {
        self$startPage <- "index.html"
        self$startPagePath <- startPage
      }
    }
    
  },
  
  limitConnectionNumbers = function(maxCon = NULL) {
    if(is.null(maxCon))
      return(self$maxCon)
    
    stopifnot(is.numeric(maxCon))
    
    self$maxCon <- maxCon
    
    invisible(self)
  },
  
  initialize = function(rootDirectory = NULL, startPage = NULL, onStart = NULL, 
                        connectionNumber = Inf, allowedFunctions = c(), 
                        allowedVariables = c(), sessionVars = list()) {
    if(is.null(rootDirectory)) 
      rootDirectory <- system.file("http_root", package = "jrc")
    self$setRootDirectory(rootDirectory)
    
    if(is.null(startPage))
      startPage <- system.file("http_root/index.html", package = "jrc")
    self$setStartPage(startPage)
    
    private$envir <- globalenv()
    
    if(!is.null(startPage)) {
      stopifnot(is.function(onStart))
      private$onStart <- onStart
    }
    
    self$allowFunctions(allowedFunctions)
    self$allowVarables(allowedVariables)
    self$setSessionVarables(sessionVars)
    sefl$limitConnectionNumbers(connectionNumber)
    
    invisible(self)
  }
  
), private = list(
  sessions = list(),
  serverHandle = NULL,
  envir = NULL,
  allowedFuns = c(),
  allowedVars = c(),
  maxCon = Inf,
  maxSize = Inf,
  maxN = Inf,
  
  getApp = function() {
    handle_http_request <- function( req ) {
      
      reqPage <- req$PATH_INFO
      if(grepl("^/http_root", reqPage)) {
        pack <- substring(strsplit(reqPage, "/")[[1]][2], 11)
        reqPage <- sub(str_c("_", pack), "", reqPage)
        reqPage <- system.file( reqPage, package = pack )
      } else {
        if(reqPage == "/index.html" & !is.null(self$startPagePath)) {
          reqPage <- self$startPagePath
        } else {
          reqPage <- str_c(self$rootDirectory, reqPage)
        }
      }
      
      if( !file.exists(reqPage) ) {
        reqPage <- str_remove(reqPage, self$rootDirectory)
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
    handle_websocket_open <- function( ws ) {
      session <- Session$new(ws, envir = new.env(parent = private$envir))  
      
      ws$onMessage( function( isBinary, msg ) {
        if( isBinary )
          stop( "Unexpected binary message received via WebSocket" )
        msg <- fromJSON(msg)
        if(!(msg[1] %in% c("COM", "FUN", "DATA")))
          stop(str_interp("Unknown message type: ${msg[1]}"))
        
        if(msg[1] == "COM") {
          session$storeMessage(msg) #vector of characters
        } 
        if(msg[1] == "DATA") {
          if(!is.character(msg[2]))
            stop("Invalid message structure. Variable name is not character.")
          
          msg <- as.list(msg)
          msg[[3]] <- fromJSON(msg[[3]])
          
          if(msg[[2]] %in% private$allowedVars) {
            session$execute(msg)
          } else {
            session$storeMessage(msg)
          }
        }
        
        if(msg[1] == "FUN") {
          if(!is.character(msg[2]))
            stop("Invalid message structure. Function name is not character.")
          #make sure that function arguments is a list
          
          msg <- as.list(msg)
          if(!is.na(msg[[3]]))
            msg[[3]] <- fromJSON(msg[[3]])
          if(is.na(msg[[3]]))
            msg[[3]] <- list()
          
          msg[[3]] <- as.list(msg[[3]])
          if(!is.list(msg[[3]]))
            stop("Invalid message structure. List of arguments is not a list.")
          #go through all arguments and turn to numeric
          
          if(msg[[2]] %in% private$allowedFuns & (is.na(msg[[4]]) | msg[[4]] %in% private$allowedVars)) {
            session$execute(msg)
          } else {
            session$store(msg)
          }
        }
      } );
      
      ws$onClose(function() {
        self$closeSession(session$id)
      })      
      
      session$setSessionVariables(private$sessionVars)
      self$addSession(session)
    
      self$onStart(session)
    }
    
    list(call = handle_http_request,
         onWSOpen = handle_websocket_open)
  }, 
  
  onStart = function(session) {}
))

pcg.env <- new.env()


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
#' @param connectionNumber Maximum number of connections that is allowed simultaneously.
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
                     allowedFunctions = NULL, allowedVariables = NULL, connectionNumber = Inf, sessionVars = list(),
                     onStart = NULL) {
  closePage()
  
  app <- App$new(rootDirectory, startPage, onStart, connectionNumber, allowedFunctions, allowedVariables, sessionVars)
  app$startServer(port)
  app$openPage(useViewer, browser)
  
  app
}

sendMessage <- function(type, id, ...) {
  if(is.null(app))
    stop("There is no opened page. Please, use 'openPage()' function to create one.")
  
  if(!is.null(id))
    id <- app$getSessionIds()$id
  for(i in id){
    session <- app$getSession(i)
    if(is.null(session)) {
      warning(str_c("There is no session with ID ", i))
    } else {
      tryCatch(session[[type]](...), 
               error = function(e) {
                 if(e$message == "Websocket is already closed.") {
                   app$closeSession(session)
                   stop(str_c("Websocket is already closed.", 
                              "Session ", session$id, " has been terminated."))
                 } else {
                   stop(e)
                 }
               })
    }
  }
  
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
#' @param id Session id (randomly generated for each established web socket connection)
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
sendCommand <- function(command, id = NULL) {
  sendMessage("sendCommand", id, command = command)
}


#' Stop server
#' 
#' Stop the server and close currently opened page (if any).
#' 
#' @seealso \code{\link{openPage}}
#' 
#' @export
closePage <- function() {
  if(!is.null(app)) {
    app$stopServer()
  } else {
    message("There is no opened page.")
  }
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
sendData <- function(variableName, variable, id = NULL, keepAsVector = FALSE, rowwise = TRUE) {
  sendMessage("sendData", id, variableName = variableName, variable = variable, keepAsVector = keepAsVector,
              rowwise = rowwise)
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
  app$setEnvironment(envir)
}

#' Send HTML to the server
#' 
#' Sends a piece of HTML code to the server and adds it at the end
#' or the \code{body} element.
#' 
#' @param html HTML code that will be added to the web page.
#' 
#' @examples 
#' \donttest{
#' openPage(FALSE)
#' 
#' sendHTML("Test...")
#' sendHTML("This is <b>bold</b>")
#' sendHTML("<table><tr><td>1</td><td>2</td></tr><tr><td>3</td><td>4</td></tr></table>")}
#' 
#' @seealso \code{\link{sendData}}, \code{\link{sendCommand}}, \code{\link{callFunction}},
#' \code{\link{openPage}}.
#' 
#' @export
sendHTML <- function(html = "", id = NULL) {
  sendMessage("sendHTML", id, html = html)
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
#' callFunction("alert", list("Some alertText"))
#' callFunction("Math.random", assignTo = "randomNumber")
#' }
#' 
#' @seealso \code{\link{authorize}}, \code{\link{allowFunctions}}, \code{\link{allowVariables}},
#' \code{\link{setEnvironment}}.
#' 
#' @export
callFunction <- function(name, arguments = NULL, assignTo = NULL, thisArg = NULL, id = NULL, ...) {
  sendMessage("callFunction", id, name = name, arguments = arguments, assignTo = assignTo, thisArg = thisArg,
                ...)
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
authorize <- function(sessionId, messageId = NULL, show = FALSE) {
  session <- app$getSession(sessionId)
  if(is.null(session))
    stop(str_c("There is no session with ID ", sessionId))
  
  session$authorize(messageId, show)
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
  app$allowFunctions(funs)
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
  app$allowVariables(vars)
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
  app$limitStorage(n, size)
}

#' Get opened page
#' 
#' Checks if there is a currently opened page. If so, returns an object with all
#' the information about the current session.
#' 
#' @return page-handling object if there is a currently opened jrc page, \code{NULL} otherwise.
#' 
#' @export
getPage <- function() {
  app
}

#' Set session-specific variables
#' 
#' Specifies variables that will be available (can be read or rewritten) only within a given session.
#' This is useful to safe state of the app for each client or for other personal settings. You can
#' also use it to limit user's access to data loaded in the current R session.
#' 
#' @export
setSessionVariables <- function(vars, sessionId = NULL) {
  app$setSessionVariables(vars, sessionId)
}

#' Get IDs of all active sessions
#' 
#' Returns IDs of all currently active sessions with date and time of their initialization
#' and the last received message.
#' 
#' @return a \code{data.frame} with three columns: \code{id} - session ID, \code{startDate} - time and date
#' of the initialization of this session, \code{lastActive} - time and date of the last received message
#' from this web socket.
#' 
#' @export
getSessionIds <- function() {
  app$getSessionIds()
}