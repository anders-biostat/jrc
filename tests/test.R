library( jrc )
library( stringr )

app <- App$new()
app$startServer()
app$openPage()
app$getSessionIds()
app$openPage(useViewer = F, browser = "google-chrome")
app$getSessionIds()
session <- app$getSession("A4Iqa2")
app$closeSession("Ld0mWS")
app$closeSession(session)
app$stopServer()

port <- httpuv::randomPort()
openPage(port = port)

appTest <- App$new()
env <- new.env()
env$someVariable <- 1
appTest$setEnvironment(env)
appTest$startServer()
appTest$openPage()
id <- appTest$getSessionIds()$id
sessionTest <- appTest$getSession(id)
#sessionTest$sendCommand("jrc.sendCommand('k_test <<- 15')", wait = 3)
#mesId <- sessionTest$getMessageIds()
#msg <- sessionTest$getMessage(mesId)
#sessionTest$authorize(mesId, show = T)
sessionTest$sendCommand("jrc.sendCommand('env1 <<- environment()')", wait = 1)
mesIds <- sessionTest$getMessageIds()
sessionTest$authorize(mesIds[1])
appTest$stopServer()

appTest <- App$new()
appTest$startServer()
appTest$openPage()
id <- appTest$getSessionIds()$id
sessionTest <- appTest$getSession(id)
sessionTest$sendCommand("jrc.sendCommand('env1 <<- environment()')", wait = 1)
mesIds <- sessionTest$getMessageIds()
sessionTest$authorize(mesIds[1])
appTest$stopServer()



openPage(useViewer = F, port = 12345, browser = "firefox")
closePage()

#test basic functions
openPage(useViewer = F)
allowVariables("type")

appTest <- getPage()
sessionTest <- appTest$getSession(appTest$getSessionIds()$id)
sessionTest$getMessage("1ivSCK")

sendCommand("alert('Bla-bla-bla')")

sendData("x", 10) # number -> number
sendData("y", "abc") # character -> string
sendData("z", 1:10) # vector (with multiple elements) -> Array
sendData("k", c("a", "b", "c")) # vector (with several characters) -> Array
sendData("f", as.factor(1:10)) # factor -> Array of strings

sendData("m", matrix(1:12, nrow = 4)) # matrix -> Array of Arrays
sendData("df", data.frame(a = 1:10, b = 11:20)) # data.frame -> Array of Objects (rowwise)
sendData("df2", data.frame(a = 1:10,            # data.frame -> Array of Objects (rowwise) (factor -> character)
                           b = paste0("a", 1:10), 
                           c = as.factor(rep(c("a", "b"), times = 5)),
                           stringsAsFactors = F))
####columnwise#####
sendData("m_r", matrix(1:12, nrow = 4), rowwise = F) # matrix -> Array of Arrays
sendData("df_r", data.frame(a = 1:10, b = 11:20), rowwise = F) # data.frame -> Array of Objects
sendData("df2_r", data.frame(a = 1:10,            # data.frame -> Array of Objects (factor -> character)
                           b = paste0("a", 1:10), 
                           c = as.factor(rep(c("a", "b"), times = 5)),
                           stringsAsFactors = F), rowwise = F)

#rownames and colnames
#row and column names of matrices are ignored
#data.frames are turned into objects if rowwise = T or get additional _row column.
#let's leave it like that for now
a <- matrix(1:12, nrow = 4)
rownames(a) <- paste0("row", 1:4)
colnames(a) <- paste0("col", 1:3)
sendData("m_names", a)
sendData("df_names", as.data.frame(a))
sendData("df_names_r", as.data.frame(a), rowwise = F)

#TO DO: make sure that keepAsVector also works on elements of a list (recursively?)
sendData("l", list(a = 1, b = 1:10, c = c("a", "b", "c"), d = as.factor(1:10))) # list of vectors -> Object of Arrays
sendData("l2", list(data.frame(a = 1:10,                                        # unnamed list -> Array of Arrays and Objects
                               b = paste0("a", 1:10), 
                               c = as.factor(rep(c("a", "b"), times = 5)),
                               stringsAsFactors = F),
                    matrix(1:12, nrow = 4),
                    list(a = 1, b = 1:10, c = c("a", "b", "c"), d = as.factor(1:10))))

# to run from the browser
# jrc.sendCommand("a <<- 10")
#
# jrc.sendData("x", 10) number -> number
allowVariables(c("y", "z", "k", "m"))
# jrc.sendData("y", "abc") string -> character
# jrc.sendData("z", [1, 2, 3, 4, 5, 6]) Array -> vector of numerics
# jrc.sendData("k", ["a", "b", "c"])

# jrc.sendData("m", [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]) Array of numeric Arrays -> matrix (rowwise)
# jrc.sendData("df", {a: [1, 2, 3, 4, 5, 6], b: [7, 8, 9, 10, 11, 12]}) TO DO: Make this turn df into data.frame

callFunction("alert", list("AAAAAAAA!"))
callFunction("Math.random", assignTo = "x")

# jrc.callFunction("print", 10)
# jrc.callFunction("rnorm", 10, "x")
allowFunctions("rnorm")
allowVariables("x")

#basic button example
k <- 0
openPage()
sendCommand(str_c("button = document.createElement('input');",
              "button.type = 'button';",
              "button.addEventListener('click', function() {jrc.sendCommand('k <<- k + 1')});", 
              "button.value = '+1';",
              "document.body.appendChild(button);", collapse = "\n"))
closePage()



#load linked charts from a local file
openPage(rootDirectory = "~/Work/Git/linked-charts/", useViewer = F)
x <- seq(0, 5, length.out = 100)
y <- sin(x) + rnorm(n = 100, sd = 0.2)

sendData("x", x)
sendData("y", y)

selPoint <- -1

sendCommand(str_c("script = document.createElement('script');", 
              "script.src = 'build/linked-charts.js';", 
              "document.head.appendChild(script);", collapse = "\n"))

sendCommand(str_c("script = document.createElement('script');", 
              "script.src = 'build/adds.js';", 
              "document.head.appendChild(script);", collapse = "\n"))

sendCommand(str_c("link = document.createElement('link');", 
              "link.rel = 'stylesheet';", 
              "link.href = 'lib/linked-charts.css';", 
              "document.head.appendChild(link);", collapse = "\n"))

sendCommand("selPoint = -1")
sendCommand(str_c("sc = lc.scatter()", 
              ".x(function(k) {return x[k]})",
              ".y(function(k) {return y[k]})",
              ".colour(function(k) {return k == selPoint ? 'red' : 'black';})",
              ".on_click(function(k) {selPoint = k; jrc.sendCommand('selPoint <<- ' + k); jrc.callFunction('print', selPoint); sc.update()})", 
              ".place()", collapse = "\n"))

closePage()

k <- 0
a <- 0
#try multiple sessions
openPage(useViewer = F, onStart = function(id) {
  sendHTML(str_c("<p>Session ID: <b>", id, "</b></p>"), id = id)
  sendHTML("<p><b>a = </b><span id = 'a'></span>; <b>k = </b><span id = 'k'></span></p>", id = id)
  
  sendCommand(str_c("buttonK = document.createElement('input');",
                    "buttonK.type = 'button';",
                    "buttonK.addEventListener('click', function() {jrc.sendCommand('k <- k + 1'); jrc.callFunction('updateText')});", 
                    "buttonK.value = 'k + 1';",
                    "document.body.appendChild(buttonK);", collapse = "\n"), id = id)
  
  sendCommand(str_c("buttonA = document.createElement('input');",
                    "buttonA.type = 'button';",
                    "buttonA.addEventListener('click', function() {jrc.sendCommand('a <<- a + 1'); jrc.callFunction('updateText')});", 
                    "buttonA.value = 'a + 1';",
                    "document.body.appendChild(buttonA);", collapse = "\n"), id = id)  
  
  }, allowedFunctions = "updateText")


updateText <- function() {
  sendCommand(str_c("document.getElementById('a').innerHTML = ", a, ";",
                    "document.getElementById('k').innerHTML = ", k, ";"), id = .id)
}

Accum <- R6::R6Class("Accum", public = list(
  addone = function(){
    private$count <- private$count + 1
  },
  show = function() {
    private$count
  }
),
private = list(
  count = 0
))
obj <- Accum$new()

openPage(allowedFunctions = c("obj$addone"))
sendCommand("jrc.callFunction('obj$addone')", wait = 3)
