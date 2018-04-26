#global variable with current page information
pageobj <- new.env()


handle_http_request <- function( req ) {
  
  if(!is.null(pageobj$file)) {
    if(!file.exists(pageobj$file)) {
      warning(str_c("File ", file, " does not exist. An empty page is created."))
      pageobj$file <- NULL
    } else {
      filename <- pageobj$file
    }
  } else {
    filename <- req$PATH_INFO
    filename = system.file( str_replace( req$PATH_INFO, "^/", "http_root/" ), package="JsRCom" )
  }
  
  if( !file.exists(filename) ) {
    warning(str_interp("File '$filename' is not found"))
    return( list( 
      status = 404L,
      headers = list( "Content-Type" = "text/html" ),
      body = "404: Resource not found" ) )
  }
  
  file_extension = str_extract( filename, "(?<=\\.)[^\\.]*$" )
  
  if( file_extension == "html" )
    content_type <- "text/html"
  else if( file_extension == "js" )
    content_type <- "text/javascript"
  else if( file_extension == "css" )
    content_type <- "text/css"
  else {
    content_type <- "text";
    print( filename )
    warning( "Serving file of unknown content type (neither .html nor .js nor .css)." )
  }
  
  content <- readLines(filename, warn = F)
  if(!is.null(pageobj$file)) {
    jsfile <- system.file( "http_root/JsRCom.js", package="JsRCom" )
    jsfile <- str_c('<script src="', jsfile, '"></script>')
    stop <- F
    for(i in 1:length(content))
      if(str_detect(content[i], regex("<head", ignore_case = T))) {
        stop <- T
        content[i] <- str_replace(content[i], regex("(<head[^>]+>)", ignore_case = T), str_c("\\1", jsfile))
      }
    #the document has no <head> tag
    if(!stop) {
      jsfile <- str_c("<head>", jsfile, "</head>")
      for(i in 1:length(content))
        if(str_detect(content[i], regex("<html", ignore_case = T))) {
          stop <- T
          content[i] <- str_replace(content[i], regex("(<html[^>]+>)", ignore_case = T), str_c("\\1", jsfile))
        }
    }
    if(!stop)
      content <- c(jsfile, content)
  }
  
  list(
    status = 200L,
    headers = list( 'Content-Type' = content_type ),
    body = str_c( readLines( filename, warn=FALSE ), collapse="\n" )
  )
}


handle_websocket_open <- function( ws ) {
  
  ws$onMessage( function( isBinary, msg ) {
    if( isBinary )
      stop( "Unexpected binary message received via WebSocket" )
    eval(parse(text = msg))
  } );
  print("WebSocket opened")
  pageobj$websocket <- ws
}

#opens a new html page
#file - a path to html file to open
#useViewer - if TRUE, opens the page in RStudio Viewer. If FALSE, opens it in th default browser
openPage <- function(file = NULL, useViewer = T) {
  closePage()
  
  pageobj$file <- file
  pageobj$app <- list( 
    call = handle_http_request,
    onWSOpen = handle_websocket_open )
  
  port <- 20001
  stop <- F
  while(!stop) {
   tryCatch({
     stop <- T
     pageobj$httpuv_handle <- startDaemonizedServer( "0.0.0.0", port, pageobj$app )
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
    getOption("viewer")( str_c("http://localhost:", port, "/init.html") )
  else
    browseURL( str_c("http://localhost:", port, "/init.html") )
  
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

#executes a JavaScript command in the opened page
execute <- function(command) {
  if(is.null(pageobj$websocket))
    stop("There is no open page. Use 'openPage()' to create a new one.")
  
  pageobj$websocket$send( command )  
}

#closes a preiously opened page (if any)
closePage <- function() {
  if( !is.null(pageobj$httpuv_handle) ) {
    if( !is.null(pageobj$websocket) ) {
      pageobj$websocket$close()
    }
    stopDaemonizedServer(pageobj$httpuv_handle )
  }
  
  rm( list=ls(pageobj), envir=pageobj )
}

