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