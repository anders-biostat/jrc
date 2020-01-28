context("Start server and open page")

test_that("Pages can be opened in different browsers", {
  app <- App$new()
  app$startServer()
  
  app$openPage()
  expect_length(app$getSessionIds(), 1)
  
  app$openPage(browser = "google-chrome")
  #app$openPage(browser = "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe")
  expect_length(app$getSessionIds(), 2)
  
  app$openPage(browser = "firefox")
  #app$openPage(browser = "C:\\Program Files\\Mozilla Firefox\\firefox.exe")
  expect_length(app$getSessionIds(), 3)

  app$openPage(useViewer = F)
  expect_length(app$getSessionIds(), 4)
    
  ids <- app$getSessionIds()
  expect_type(ids, "character")
  
  app$closeSession(ids[1:2])
  expect_length(app$getSessionIds(), 2)
  
  expect_warning(app$closeSession(ids[1]), "There is no session with ID")
  
  session <- app$getSession(ids[3])
  expect_true("Session" %in% class(session))
  
  app$closeSession(session$id)
  expect_length(app$getSessionIds(), 1)
  
  app$closeSession()
  expect_length(app$getSessionIds(), 0)
  
  expect_message(app$stopServer(), "Server has been stopped.")
})

test_that("Wrapper functions to open and close pages are working", {
  openPage()
  expect_length(getSessionIds(), 1)
  expect_message(closePage(), "Server has been stopped.")
  expect_message(closePage(), "There is no opened page.")
})

test_that("Port is freed after page is closed", {
  port <- httpuv::randomPort()
  app <- App$new()
  app$startServer(port = port)
  expect_error(app$startServer(port = port))
  expect_message(app$stopServer(), "Server has been stopped.")
  expect_silent(app$startServer(port = port))
  expect_message(app$stopServer(), "Server has been stopped.")
  
  openPage(port = port)
  expect_message(openPage(port = port), "Server has been stopped.")
  expect_message(closePage(), "Server has been stopped.")
})

test_that("Sessions can be closed according to there time stamps", {
  start <- Sys.time()
  openPage()
  
  app <- getPage()
  time <- Sys.time()
  
  app$openPage(F)
  app$openPage(F)
  
  expect_error(app$closeSession(), "There is more than one active session.")
  
  app$closeSession(old = Sys.time() - start)
  expect_length(app$getSessionIds(), 3)
  
  app$closeSession(old = Sys.time() - time)
  expect_length(app$getSessionIds(), 2)
  
  time <- Sys.time()
  session <- app$getSession(app$getSessionIds()[1])
  session$sendCommand("jrc.sendCommand('print(\"Hi!\")')", wait = 3)
  
  app$closeSession(inactive = Sys.time() - time)
  expect_length(app$getSessionIds(), 1)
  
  app$stopServer()
})