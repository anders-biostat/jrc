context("Security limits")

test_that("Number of active connection can be limited", {
  app <- openPage(connectionNumber = 2)
  
  app$openPage(FALSE)
  app$openPage(FALSE)
  
  expect_equal(length(app$getSessionIds()), 2)
  
  setLimits(maxCon = 3)
  app$openPage(FALSE)
  expect_equal(length(app$getSessionIds()), 3)
  
  closePage()
})

test_that("Storage size can be limited", {
  app <- openPage()
  ses <- app$getSession()
  ses$setLimits(list(storageSize = 200))
  
  sendCommand("jrc.sendCommand('print(\"xxx\")');", wait = 3)
  
  id <- ses$getMessageIds()
  expect_length(id, 1)
  
  sendCommand("jrc.sendCommand('print(\"xxx\")');", wait = 3)
  
  expect_length(ses$getMessageIds(), 1)
  expect_false(ses$getMessageIds() == id)
  
  ses$setLimits(list(storageSize = 400))
  sendCommand("jrc.sendCommand('print(\"xxx\")');", wait = 3)
  
  expect_length(ses$getMessageIds(), 2)
  
  closePage()
})

test_that("Number of stored messages can be limited", {
  app <- openPage()
  ses <- app$getSession()
  
  for(i in 1:3)
    sendCommand("jrc.sendCommand('print(\"xxx\")');", wait = 3)
  
  ses$setLimits(list(storedMsg = 2))
  
  sendCommand("jrc.sendCommand('print(\"xxx\")');", wait = 3)
  expect_length(ses$getMessageIds(), 3)
  
  ses$setLimits(list(storedMsg = 10))
  sendCommand("jrc.sendCommand('print(\"xxx\")');", wait = 3)
  
  expect_length(ses$getMessageIds(), 4)
  
  closePage()
})

test_that("Variable size can be limited", {
  x <- -1
  app <- openPage(allowedVariables = "x")
  
  setLimits(varSize = 70)
  ses <- app$openPage(FALSE)
  
  ses$sendCommand("jrc.sendData('x', [1, 1, 1]);", wait = 3)
  expect_length(x, 3)
  
  ses$sendCommand("jrc.sendData('x', [1, 1, 1, 1, 1]);", wait = 3)
  expect_length(x, 3)
  
  ses$setLimits(list(varSize = 100))
  ses$sendCommand("jrc.sendData('x', [1, 1, 1, 1, 1]);", wait = 3)
  expect_length(x, 5)
  
  closePage()
})

test_that("Number of received messages per second can be limited", {
  app <- openPage(allowedVariables = "x", useViewer = FALSE)
  ses <- app$getSession()
  
  x <- -1
  
  ses$setLimits(list(msgPerSec = 2))
  ses$sendCommand("i = 0; while(i < 4) {jrc.sendData('x', i); i++;}")
  
  t <- 0
  while(t < 2) {
    httpuv::service(0.1)
    t <- t + 0.1
  }
  expect_equal(x, 1)
  
  Sys.sleep(1)
  ses$setLimits(list(msgPerSec = 3))
  ses$sendCommand("j = 0; while(j < 10) {jrc.sendCommand('print(1)'); j++;}")

  t <- 0
  while(t < 2) {
    httpuv::service(0.1)
    t <- t + 0.1
  }
  expect_length(ses$getMessageIds(), 3)
  
  closePage()
})
