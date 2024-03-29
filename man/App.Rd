% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jrc.R
\name{App}
\alias{App}
\title{App class}
\description{
Object of this class represents the entire jrc-based app. It stores all the active connections,
client-specific variables and all the global app settings.

You can create interactive apps by initializing
new instances of this class and manage the apps with the methods that are described below. There are no limitations
on the number of apps that can run simultaneously in one R session.

A wrapper function is also exported for almost each method (see links in the Methods section). This functions allow
you to gain full control over the app without ever dealing with this class. However, in this case only a single app
can run per R session. Attempt to create a new app (with \code{\link{openPage}} function) will force the existing one (if any)
to stop. You can always get the \code{App} object for the currently running app with \code{\link{getPage}} function.
}
\section{Methods}{

\describe{
   \item{\code{new(rootDirectory = NULL, startPage = NULL, onStart = NULL, onClose = NULL,  
   connectionNumber = Inf, allowedFunctions = c(), allowedVariables = c(), sessionVars = NULL)}}{
      Creates a new instance of class \code{App}. Check \code{\link{openPage}} man page for information about
      arguments.
   }
   \item{\code{startServer(port = NULL)}}{
       Starts a local server that listens to a given port. If \code{port = NULL}, picks a random available port.
       See also \code{\link{openPage}}.
   }
   \item{\code{stopServer()}}{
       Closes all active sessions and stops a running server. See also \code{\link{closePage}}.
   }
   \item{\code{openPage(useViewer = TRUE, browser = NULL)}}{
      Opens a new web page either in a browser, or in the R Studio viewer. If \code{useViewer = FALSE} and browser is not selected,
      a default installed browser is used. If browser is specified, \code{useViewer} is ignored. This method returns
      a new \code{\link{Session}} object, which should correspond to the page that has been just opened. However, if someone would start
      a new connection at the moment when \code{openPage} method is called, it may return a wrong session. See also \code{\link{openPage}}.
   }
   \item{\code{getSession(sessionId = NULL)}}{
      Returns a session with the given ID or \code{NULL} if session with this ID doesn't exist. If \code{sessionId = NULL}
      and there is only one active session, returns it. See also \code{\link{getSession}}.
   }
   \item{\code{closeSession(sessionId = NULL, inactive = NULL, old = NULL)}}{
      Closes WebSocket connection of one or multiple sessions and removes all the related data from the app. For more information on 
      the arguments, please, check \code{\link{closeSession}} man page.
   }
   \item{\code{getSessionIds()}}{
       Returns IDs of all currently active sessions. See also \code{\link{getSessionIds}}.
   }
   \item{\code{setEnvironment(envir)}}{
      Specifies the outer environment of the app, in which all the messages from the web pages will be evaluated. For more information,
      please, check \code{\link{setEnvironment}}.
   }
   \item{\code{allowFunctions(funs = NULL)}}{
      Adds function names to a list of allowed R functions. These functions can be called from a web page without authorization
      on the R side. If \code{funs = NULL}, returns a list of all currently allowed functions. For more information,
      please, check \code{\link{allowFunctions}}.
   }
   \item{\code{allowVariables(vars = NULL)}}{
      Adds variable names to  the list of allowed variables. These variables can be changed from a web page without 
      authorization on the R side. If \code{vars = NULL}, then returns a vector of names of all currently allowed variables.
      For more information, please, check \code{\link{allowVariables}}.
   }
   \item{\code{allowDirectories(dir = NULL)}}{
      Allows app to serve files from an existing directory. Files from the \code{rootDirectory} can always be accessed
      by the app. By default, the current working directory is
      added to the list of the allowed directories, when the app is initialized. All the subdirectories of the allowed 
      directories can also be accessed. Attempt to request file from outside allowed directory will produce 
      \code{403 Forbidden} error. If \code{dirs = NULL}, then returns a vector of names of all currently allowed directories.
      Also see \code{\link{allowDirectories}}.
   }
   \item{\code{startPage(path = NULL)}}{
      Sets path to a starting web page of the app. Path can be full, relative to the app's root directory or relative
      to the current R working directory. If 
      \code{path = NULL}, returns current path to the starting page.
   }
   \item{\code{rootDirectory(path = NULL)}}{
      Sets path to the root directory for the server. Any file, requested by the server, will be looked for in this directory.
      Can be a full path or a path relative to the current R working directory. If \code{path = NULL}, returns path to the
      current root directory.
   }
   \item{\code{setLimits(...)}}{
      Sets limits for memory usage, number of simultaneously active connections and amount of messages processed per second. 
      These settings will apply for each new connection. To change memory usage for an existing session use method \code{setLimits}
      of class \code{\link{Session}}. For information about possible arguments, please, check \code{\link{setLimits}}.
   }
   \item{\code{getPort()}}{
      Returns number of the port which the running server listens to. After the app has been initialized, the port number cannot be changed.
   }
}
}

