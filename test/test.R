library( JsRCom )
library( stringr )

#basic button example
k <- 0
openPage()
execute(str_c("button = document.createElement('input');",
              "button.type = 'button';",
              "button.addEventListener('click', function() {ws.send('k <<- k + 1')});", 
              "button.value = '+1';",
              "document.body.appendChild(button);", collapse = "\n"))
closePage()



openPage(useViewer = F)
execute("script = document.createElement('script');")
execute("script.type = 'text/javascript';")
execute("script.src = 'https://d3js.org/d3.v5.min.js';")
execute("document.head.appendChild(script);")

execute("d3.select('body').append('svg')")
closePage()


