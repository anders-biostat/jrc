#' @import stringr
#' @importFrom jsonlite fromJSON

#global variable with current page information
pageobj <- new.env()

handle_http_request <- function( req ) {
  
  reqPage <- req$PATH_INFO
  #print(reqPage)
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
  
  file_extension = str_extract( reqPage, "(?<=\\.)[^\\.]*$" )
  
  if( file_extension == "html" )
    content_type <- "text/html"
  else if( file_extension == "js" )
    content_type <- "text/javascript"
  else if( file_extension == "css" )
    content_type <- "text/css"
  else {
    content_type <- "text";
    #print( reqPage )
    warning( "Serving file of unknown content type (neither .html nor .js nor .css)." )
  }
  
  content <- readLines(reqPage, warn = F)
  #print(str_c("Reading ", reqPage))
  if(file_extension == "html") {
    #jsfile <- system.file( "http_root/JsRCom.js", package="JsRCom" )
    jsfile <- str_c("<script src='http_root_JsRCom/JsRCom.js'></script>")
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
  
  ws$onMessage( function( isBinary, msg ) {
    if( isBinary )
      stop( "Unexpected binary message received via WebSocket" )
    msg <- fromJSON(msg)
    if(msg[1] == "COM"){
      eval(parse(text = msg[2]), envir = pageobj$envir)
    } else if(msg[1] == "DATA") {
      assign(msg[2], fromJSON(msg[3]), envir = pageobj$envir)
    } else {
      stop(str_interp("Unknown message type : ${msg[2]}"))
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
#' exectute R code in the current R session. 
#' 
#' @param startPage A path to the HTML file that should be opened, when the server is initialised.
#' This can be an absolute path to a local file, or it can be relative to the \code{rootDirectory}
#' or to the current R working directory. If \code{startPage} is not defined, this function opens an 
#' empty HTML page. The file must have \emph{.html} extension.
#' @param rootDirectory A path to the root directory of the server. If \code{rootDirectory} is not 
#' defined, the \code{http_root} in the package directory will be used as a root directory.
#' @param useViewer If \code{TRUE}, the start page will be opened in the RStudio Viewer. If \code{FALSE}
#' a default web browser will be used.
#' 
#' @export
#' @importFrom httpuv startServer
#' @importFrom later run_now
#' @importFrom utils browseURL
openPage <- function(useViewer = T, rootDirectory = NULL, startPage = NULL) {
  closePage()
  
  if(is.null(rootDirectory))
    rootDirectory = system.file("http_root", package = "JsRCom")
  if(is.null(startPage)) 
    startPage <- system.file( "http_root/index.html", package="JsRCom" )
  
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
  
  pageobj$app <- list( 
    call = handle_http_request,
    onWSOpen = handle_websocket_open )
  
  port <- 20001
  stop <- F
  while(!stop) {
   tryCatch({
     stop <- T
     pageobj$httpuv_handle <- startServer( "0.0.0.0", port, pageobj$app )
   }, error = function(e) {
     port <<- port + 1
     stop <<- F
     if(port > 20100){
       stop <<- T
       stop(e$message)
     }
   }) 
  }

  if( useViewer & !is.null( getOption("viewer") ) )
    getOption("viewer")( str_c("http://localhost:", port, "/", pageobj$startPage) )
  else
    browseURL( str_c("http://localhost:", port, "/", pageobj$startPage) )
  
  pageobj$envir <- globalenv()
  
  # Wait up to 5 seconds for the a websocket connection
  # incoming from the client
  for( i in 1:(5/0.05) ) {
    run_now()
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
#' and execute it in the current R sesion.
#' @details Note, that in both cases commands are executed inside a function. Therefore use for R code use \code{<<-} instead
#' of \code{<-} to change global variables and in JavaScript use \code{windows.varibleName = "SomeValue"} or
#' \code{varibleName = "SomeValue"}. Variables declared like \code{var variableName = "SomeValue"} or 
#' \code{variableName <- "SomeValue"} will be accessable only within the current \code{sendCommand} call.
#' 
#' @param command A line (or several lines separated by \code{\%n}) of JavaScript code. This code
#' will be immediately executed on the opened page. No R-side syntax check is performed.
#' 
#' @examples  
#' k <- 0
#' openPage()
#' sendCommand(str_c("button = document.createElement('input');",
#'               "button.type = 'button';",
#'               "button.addEventListener('click', function() {jrc.sendCommand('k <<- k + 1')});", 
#'               "button.value = '+1';",
#'               "document.body.appendChild(button);", collapse = "\%n"))
#' closePage()
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
#' Stop the server and close currently opened page (if any)
#' 
#' @export
#' @importFrom httpuv stopServer
closePage <- function() {
  if( !is.null(pageobj$httpuv_handle) ) {
    if( !is.null(pageobj$websocket) ) {
      pageobj$websocket$close()
    }
    stopServer(pageobj$httpuv_handle )
  }
  
  rm( list=ls(pageobj), envir=pageobj )
}

#' Send data to the server
#' 
#' Sends a variable to the server, where it is assigned to the variable with a specified name. A JavaScript function
#' \code{jrc.sendData(variableName, variable)} can send data back from the server to the current R session.
#' 
#' @param variableName Name that the variable will have on the server.
#' @param data Variable to send
#' @param keepAsVector If TRUE, variables with length 1 will be saved as arrays on the server, otherwise they 
#' will be converted to atomic types
#' 
#' @examples 
#' openPage()
#' x <- 1:100
#' sendData("x", x)
#' sendCommand("console.log(x);")
#' sendCommand("jrc.sendData('x', x.filter(function(e) {return e % 2 == 0}))")
#'  
#' @export
#' @importFrom jsonlite toJSON
sendData <- function(variableName, variable, keepAsVector = F) {
  if(is.null(pageobj$websocket))
    stop("There is no open page. Use 'openPage()' to create a new one.")
  
  pageobj$websocket$send( toJSON(c("DATA", variableName, toJSON(variable, digits = NA), keepAsVector)))
}

#' Set Environment
#' 
#' Defines the environment, where the commands, recieved from the server, will be evaluated. By default,
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
