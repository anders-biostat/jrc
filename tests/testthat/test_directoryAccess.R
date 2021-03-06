context("Access only allowed directories")

test_that("Files inside default working directories can be accessed", {
  app <- openPage(allowedVariables = "loaded", useViewer = F)
  
  loaded <- -1
  #root directory
  sendCommand(str_c("var myScript = document.createElement('script');",
                        "myScript.src = 'test.js';",
                        "myScript.onload = function(){",
                        "jrc.sendData('loaded', 1);",
                        "};",
                        "document.head.appendChild(myScript);"), wait = 3)
  expect_equal(loaded, 1)
  
  sendCommand(str_c("var client = new XMLHttpRequest();",
                        "client.open('GET', 'test_directoryAccess.R');",
                        "client.onreadystatechange = function() {",
                        "if(client.responseText.substr(0, 7) == 'context')",
                        "jrc.sendData('loaded', 2);",
                        "};",
                        "client.send();"), wait = 3)
  expect_equal(loaded, 2)
  closePage()
})

test_that("Files outside working directories can't be accessed", {
  openPage(allowedVariables = "status")
  
  status <- -1
  sendCommand(str_c("var client = new XMLHttpRequest();",
                    "client.open('GET', '../test.R');",
                    "client.onreadystatechange = function() {",
                    "if(client.response)",
                    "jrc.sendData('status', client.status);",
                    "};",
                    "client.send();"), wait = 3)
  expect_equal(status, 404)
  
  closePage()
  
  openPage(allowedDirectories = NULL, allowedVariables = "status")
  sendCommand(str_c("var client = new XMLHttpRequest();",
                    "client.open('GET', 'test_directoryAccess.R');",
                    "client.onreadystatechange = function() {",
                    "if(client.response)",
                    "jrc.sendData('status', client.status)",
                    "};",
                    "client.send();"), wait = 3)
  expect_equal(status, 403)
  closePage()
})

test_that("Directories can be added to the list of allowed directories", {
  openPage(allowedVariables = "loaded", allowedDirectories = NULL)
  
  allowDirectories(getwd())
  
  loaded <- -1
  sendCommand(str_c("var client = new XMLHttpRequest();",
                    "client.open('GET', 'test_directoryAccess.R');",
                    "client.onreadystatechange = function() {",
                    "if(client.responseText.substr(0, 7) == 'context')",
                    "jrc.sendData('loaded', 1);",
                    "};",
                    "client.send();"), wait = 3)
  expect_equal(loaded, 1)
  
  closePage()
  
  
  openPage(allowedVariables = "loaded")
  
  png(str_c(tempdir(), .Platform$file.sep, "testImage.png"))
  plot(1:10, 1:10)
  dev.off()
  
  allowDirectories(tempdir())
  
  sendCommand(str_c("var myImage = document.createElement('img');",
                        "myImage.src = 'testImage.png';",
                        "myImage.onload = function() {",
                        "jrc.sendData('loaded', 2);",
                        "};",
                        "document.body.appendChild(myImage);"), wait = 3)
  expect_equal(loaded, 2)
  
  closePage()
  
})