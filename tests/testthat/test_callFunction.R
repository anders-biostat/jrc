context("Call functions and assign their results to variables")

test_that("Fucntions can be called on both sides", {
  openPage()
  
  k <- -1
  f <- function() k <<- 10
  sendCommand("jrc.callFunction('f')", wait = 3)
  authorize()
  expect_equal(k, 10)
  
  callFunction("Math.random", assignTo = "x")
  allowVariables("x")
  sendCommand("jrc.sendData('x', x)", wait = 3)
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

  allowFunctions("f")
  allowVariables("y")
  sendCommand("jrc.callFunction('f', undefined, 'y')", wait = 3)
  expect_equal(y, 2)
  
  setEnvironment(e)
  sendCommand("jrc.callFunction('f', undefined, 'y')", wait = 3)
  expect_equal(e$y, 20)
  
  setSessionVariables(list(x = 100))
  sendCommand("jrc.callFunction('f', undefined, 'y')", wait = 3)
  expect_equal(e$y, 200)
  
  expect_message(closePage(), "Server has been stopped.")
})

test_that("Function from a specified package can be called", {
  openPage()
  a <- ""

  allowFunctions("str_c")
  allowVariables("a")
  sendCommand("jrc.callFunction('str_c', ['abc', 'def'], 'a', 'stringr')", wait = 3)
  expect_equal(getSessionVariable("a"), "abcdef")
  
  expect_message(closePage(), "Server has been stopped.")  
})

test_that("Methods of an object can be called", {
  Accum <- R6::R6Class("Accum", public = list(
    addone = function(){
      private$count <- private$count + 1
    },
    show = function() {
      private$count
    }
  ),
  private = list(
    count = 0
  ))
  obj <- Accum$new()
  
  openPage(allowedFunctions = c("obj$addone"))
  sendCommand("jrc.callFunction('obj$addone')", wait = 3)
  
  expect_equal(obj$show(), 1)
  
  closePage()
})