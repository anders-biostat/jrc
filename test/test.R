library( JsRCom )
library( stringr )

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
              "script.src = 'build/additional.js';", 
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
              ".on_click(function(k) {selPoint = k; jrc.sendCommand('selPoint <<- ' + k); jrc.sendCommand('print(selPoint)'); sc.update()})", 
              ".place()", collapse = "\n"))

closePage()
