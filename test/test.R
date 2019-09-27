library( jrc )
library( stringr )

#test basic functions
openPage(useViewer = F)

sendCommand("alert('Bla-bla-bla')")

sendData("x", 10) # number -> number
sendData("y", "abc") # character -> string
sendData("z", 1:10) # vector (with multiple elements) -> Array
sendData("k", c("a", "b", "c")) # vector (with several characters) -> Array
sendData("m", matrix(1:12, nrow = 4)) # matrix -> Array of Arrays
sendData("df", data.frame(a = 1:10, b = 11:20)) # data.frame -> Array of Objects (rowwise)
sendData("df2", data.frame(a = 1:10,            # data.frame -> Array of Objects (rowwise) (factor -> character)
                           b = paste0("a", 1:10), 
                           c = as.factor(rep(c("a", "b"), times = 5)),
                           stringsAsFactors = F))
sendData("f", as.factor(1:10)) # factor -> Array of strings
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
# jrc.sendData("x", 10) number -> character TO DO: Fix that
allowVariables(c("y", "z", "k", "m"))
# jrc.sendData("y", "abc")

callFunction(...) #to be implemented

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
openPage(rootDirectory = "~/Work/Git/linked-charts/")
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
