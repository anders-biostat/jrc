context("Call functions and assign their results to variables")

test_that("Fucntions can be called on both sides", {
  openPage()
  
  k <- -1
  f <- function() k <<- 10
  sendCommand("jrc.callFunction('f')", wait = 3)
  id <- getPage()$getSessionIds()$id
  mid <- authorize(id)
  authorize(id, mid)
  expect_equal(k, 10)
  
  callFunction("Math.random", assignTo = "x")
  allowVariables("x")
  sendCommand("jrc.sendData('x', x, false)", wait = 3)
  expect_gte(x, 0)
  expect_lte(x, 1)
  
  expect_message(closePage(), "Server has been stopped.")
})

test_that("External variables are taken from the correct environment", {
  openPage()
  
  x <- 1
  y <- 0
  e <- new.env()
  e$x <- 10
  
  f <- function() x * 2
  id <- getPage()$getSessionIds()$id
  session <- getPage()$getSession(id)
  
  allowFunctions("f")
  allowVariables("y")
  sendCommand("jrc.callFunction('f', undefined, 'y')", wait = 3)
  expect_equal(session$getSessionVariable("y"), 2)
  
  setEnvironment(e)
  sendCommand("jrc.callFunction('f', undefined, 'y')", wait = 3)
  expect_equal(session$getSessionVariable("y"), 20)
  
  setSessionVariables(list(x = 100), id)
  sendCommand("jrc.callFunction('f', undefined, 'y')", wait = 3)
  expect_equal(session$getSessionVariable("y"), 200)
  
  expect_message(closePage(), "Server has been stopped.")
})

test_that("Function from a specified package can be called", {
  openPage()
  a <- ""
  session <- getPage()$getSession(getSessionIds()$id)
  
  allowFunctions("str_c")
  allowVariables("a")
  sendCommand("jrc.callFunction('str_c', ['abc', 'def'], 'a', 'stringr')", wait = 3)
  expect_equal(session$getSessionVariable("a"), "abcdef")
  
  expect_message(closePage(), "Server has been stopped.")  
})