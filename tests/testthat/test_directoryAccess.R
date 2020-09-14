context("Access only allowed directories")

test_that("Files inside default working directories can be accessed", {
  app <- openPage(allowedVariables = "loaded", useViewer = F)
  ses <- app$getSession()
  
  loaded <- -1
  #root directory
  ses$sendCommand(str_c("var myScript = document.createElement('script');",
                        "myScript.src = 'test.js';",
                        "myScript.onload = function(){",
                        "jrc.sendData('loaded', 1);",
                        "};",
                        "document.head.appendChild(myScript);"), wait = 3)
  expect_equal(loaded, 1)
  
  ses$sendCommand(str_c("var client = new XMLHttpRequest();",
                        "client.open('GET', 'test_directoryAccess.R');",
                        "client.onreadystatechange = function() {",
                        "if(client.responseText.substr(0, 7) == 'context')",
                        "jrc.sendData('loaded', 2);",
                        "};",
                        "client.send();"), wait = 3)
  expect_equal(loaded, 2)
  ses$close()
  
  ses <- app$openPage()
  
  png(str_c(tempdir(), .Platform$file.sep, "testImage.png"))
  plot(1:10, 1:10)
  dev.off()
  
  print(app$allowDirectories())
  ses$sendCommand(str_c("var myImage = document.createElement('img');",
                        "myImage.src = 'testImage.png';",
                        "myImage.onload = function() {",
                        "jrc.sendData('loaded', 3);",
                        "};",
                        "document.body.appendChild(myImage);"), wait = 3)
  expect_equal(loaded, 3)
  
  closePage()
})

test_that("Files outside working directories can't be accessed", {
  
})

test_that("Directories can be added to the list of allowed directories", {
  
})