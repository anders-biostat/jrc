context("Handle multiple sessions")

test_that("Messages from different sessions are properly stored and can be executed via the wrapper function", {
  app <- openPage()
  
  ses1 <- app$getSession()
  ses2 <- app$openPage(browser = "google-chrome")
  #ses2 <- app$openPage(browser = "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe")
  
    
  expect_length(app$getSessionIds(), 2)
  
  ses1$sendCommand("jrc.sendCommand('k <- 1')", wait = 1)
  ses1$sendCommand("jrc.sendCommand('k <- 2')", wait = 3)
  ids1 <- ses1$getMessageIds()
  
  ses2$sendCommand("jrc.sendCommand('k <- 3')", wait = 3)
  ids2 <- ses2$getMessageIds()
  
  expect_length(ids1, 2)
  expect_length(ids2, 1)

  authorize(ses1$id, ids1[1])
  authorize(ses2$id)
  
  expect_length(ses1$getMessageIds(), 1)
  expect_length(ses2$getMessageIds(), 0)
  
  app$stopServer()
})

test_that("Each session can store and use its own state", {
  app <- App$new()
  app$startServer()
  
  ses1 <- app$openPage()
  ses2 <- app$openPage(browser = "google-chrome")
  #ses2 <- app$openPage(browser = "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe")
  
  ses1$sendCommand("jrc.sendCommand('k <- 1')", wait = 3)
  ses1$authorize()
  ses2$sendCommand("jrc.sendCommand('k <- 2')", wait = 3)
  ses2$authorize()
  
  k1 <- ses1$sessionVariables(varName = "k")
  k2 <- ses2$sessionVariables(varName = "k")
  
  expect_equal(k1, 1)
  expect_equal(k2, 2)
  
  f <- function() {k * 3}
  app$allowFunctions("f")
  app$allowVariables("k")
  
  ses1$sendCommand("jrc.callFunction('f', [], 'k')", wait = 3)
  ses2$sendCommand("jrc.callFunction('f', [], 'k')", wait = 3)
  
  expect_false(exists("k", inherits = FALSE))
  
  k1 <- ses1$sessionVariables(varName = "k")
  k2 <- ses2$sessionVariables(varName = "k")
  
  expect_equal(k1, 3)
  expect_equal(k2, 6)
    
  app$stopServer()
})

test_that("One can specify default actions for each new page", {
  app <- openPage(onStart = function(session) {
    session$sendCommand(paste0("f = function() {jrc.sendData('k', '", session$id, "', false)}"))
  }, allowedVariables = "k")
  ses1 <- app$getSession()
  
  ses2 <- app$openPage(useViewer = F)
  ses3 <- app$openPage(F)
  
  k <- -1
  
  ses1$callFunction("f", wait = 3)
  expect_equal(k, ses1$id)

  ses2$callFunction("f", wait = 3)
  expect_equal(k, ses2$id)
  
  closePage()
})