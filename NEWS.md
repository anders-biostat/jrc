# jrc 0.5.1

* `onlyServer` argument is added to the `openPage` function to prevent opening a browser tab.

* Bug in the `thisArg` argument of the `callFunction` function is fixed.


# jrc 0.5.0

* `setLimits` now used to set any kind of restriction to memory usage. It replaces `numberOfConnections` and `limitStorage`. Now, also number of processed messages per second or the amount of processed information can be limited, so that the app will not get stuck after receiving too many messages.

* `onClose` callback added to the `openPage` function. It allows to specify actions to be taken when a WebSocket connection is closed (user closes the page).

* `getPort` added as a function and as a method of class `App` to query for a port number of the running app.

* `sendData` now handles the NAs properly.

* Issue with failing to establish a WebSocket connection to a complex address fixed.

# jrc 0.4.0

* `allowedDirectories` argument is added to the `openPage` function (and also the corresponding function and method `allowDirectories`).
Now user can specify the list of directories that can be accessed by server. By default, it's the `rootDirectory`, the current working 
directory and a temporary directory for the current R session.

* Issue with incorrect serving of the file types that are supposed to be send as bytes fixed.

* `jrc` now imports `R.utils` for more robust paths handling.

# jrc 0.3.1

* Issue with file separator in Windows fixed.

* Now app is stored in packages namespace immediately after initialization. This fixes a problem with wrapper functions inside `onStart`.

# jrc 0.3.0

* `jrc` now supports multiple connections to a single server and thus can be used to create server apps that are intended
to be used by multiple clients simultaneously. This change requires some additional arguments in some of the functions as well
as several new ones. However, backwards compatibility is maintained.

* `jrc` now depends on `R6` and each app is represented with a single object. One can manage the app with methods of this object,
which allows to run several apps inside one R session. For more information, check man pages of classes `App` and `Session`.

# jrc 0.2.1

* `jrc` now works properly on RStudio Server

* `sendData` no longer crashes when `keepAsVector = FALSE` and some NAs are present.

* `jrc` now works with `httpuv < 1.5.2` (it no longer depends on `httpuv::randomPort()`).

* `jrc` now imports `mime` for defining content type when serving a page.


# jrc 0.2.0

* Now most of the request from the server must be manually authorized in the R session to prevent misuse of publicly available
apps based on jrc. Functions `authorize`, `allowVariables`, `allowFunctions`, `limitStorage` have been added. Check their man 
pages for more information.

* `openPage` now have `browser` argument, which allows to specify a browser to open a page (previously the default browser was
used with no alternatives).

* Function `getPage` is added. This function returns the main page-handling object with all the information about current session.

* Function `callFunction` added on both R and JavaScript sides. It allows to call a function by name, list of arguments and name
of variable to which assign the result.

* Now port for the local server is selected by `httpuv::randomPort()` function. User can also provide a port number as an 
argument to the `openPage` function.

* `sendData` now has argument `rowwise` which allows to send matrices and data.frames to JavaScript not only rowwise (default),
but also columnwise.

* If in `sendData` `keepAsVector = FALSE` `jrc` now checks recursively for any arrays of length 1 to replace them with scalars 
(important for lists).

* Some bugs with changing variable types in `jrc.sendData` fixed.