context("Start server and open page")

test_that("Pages can be opened in different browsers", {
  app <- App$new()
  app$startServer()
  
  app$openPage()
  expect_equal(nrow(app$getSessionIds()), 1)
  
  app$openPage(useViewer = F, browser = "google-chrome")
  expect_equal(nrow(app$getSessionIds()), 2)
  
  app$openPage(useViewer = F, browser = "firefox")
  expect_equal(nrow(app$getSessionIds()), 3)
  
  ids <- app$getSessionIds()
  expect_output(str(ids), "3 obs")
  expect_output(str(ids), "3 variables")
  
  expect_type(ids$id, "character")
  
  app$closeSession(ids$id[1])
  expect_equal(nrow(app$getSessionIds()), 2)
  
  expect_error(app$closeSession(ids$id[1]), "There is no session with this ID")
  
  app$closeSession(ids$id[2])
  expect_equal(nrow(app$getSessionIds()), 1)
  
  session <- app$getSession(ids$id[3])
  expect_true("Session" %in% class(session))
  
  app$closeSession(session)
  expect_equal(nrow(app$getSessionIds()), 0)
  
  expect_message(app$stopServer(), "Server has been stopped.")
})

test_that("Wrapper functions to open and close pages are working", {
  openPage()
  expect_equal(nrow(getPage()$getSessionIds()), 1)
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