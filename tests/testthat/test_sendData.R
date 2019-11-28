context("Send and receive data")

test_that("Data types are converted appropriately on the server side", {
  openPage()
  allowVariables(c("type", "size", "names"))
  
  sendData("x", 10)
  sendCommand("jrc.sendData('type', typeof x)", wait = 3)
  expect_equal(type, "number")
  
  sendData("y", "abc")
  sendCommand("jrc.sendData('type', typeof y)", wait = 3)
  expect_equal(type, "string")
  
  sendData("z", 1:10)
  sendCommand("jrc.sendData('type', Array.isArray(z) ? 'Array' : 'something else')", wait = 3)
  sendCommand("jrc.sendData('size', z.length)", wait = 3)
  expect_equal(type, "Array")
  expect_equal(size, 10)

  sendData("k", c("a", "b", "c")) 
  sendCommand("jrc.sendData('type', Array.isArray(k) ? 'Array' : 'something else')", wait = 3)
  sendCommand("jrc.sendData('size', k.length)", wait = 3)
  expect_equal(type, "Array")
  expect_equal(size, 3)
  
  sendData("f", as.factor(1:10)) # factor -> Array of strings
  sendCommand("jrc.sendData('type', typeof f[0])", wait = 3)
  expect_equal(type, "string")
  
  sendData("m", matrix(1:12, nrow = 4)) # matrix -> Array of Arrays
  sendCommand("jrc.sendData('size', [m.length, m[0].length])", wait = 3)
  expect_equal(size, c(4, 3))
  
  sendData("df", data.frame(a = 1:10, b = 11:20)) # data.frame -> Array of Objects (rowwise)
  sendCommand("jrc.sendData('size', df.length)", wait = 3)
  expect_equal(size, 10)
  sendCommand("jrc.sendData('names', Object.keys(df[0]))", wait = 3)
  expect_equal(names, c("a", "b"))
  
  sendData("df2", data.frame(a = 1:10,            # data.frame -> Array of Objects (rowwise) (factor -> character)
                             b = paste0("a", 1:10), 
                             c = as.factor(rep(c("a", "b"), times = 5)),
                             stringsAsFactors = F))
  sendCommand("jrc.sendData('type', [typeof df2[0].a, typeof df2[0].b, typeof df2[0].c])", wait = 3)
  expect_equal(type, c("number", "string", "string"))
  
  ####columnwise#####
  sendData("m", matrix(1:12, nrow = 4), rowwise = F) # matrix -> Array of Arrays
  sendCommand("jrc.sendData('size', [m.length, m[0].length])", wait = 3)
  expect_equal(size, c(3, 4))
  
  sendData("df", data.frame(a = 1:10, b = 11:20), rowwise = FALSE) # data.frame -> Object with each column as Array
  sendCommand("jrc.sendData('size', df.a.length)", wait = 3)
  sendCommand("jrc.sendData('type', typeof df.b[0])", wait = 3)
  expect_equal(size, 10)
  expect_equal(type, "number")
  
  sendData("df2", data.frame(a = 1:10,            # data.frame -> Object with each column as Array
                               b = paste0("a", 1:10), 
                               c = as.factor(rep(c("a", "b"), times = 5)),
                               stringsAsFactors = F), rowwise = F)
  sendCommand("jrc.sendData('names', Object.keys(df2))", wait = 3)
  sendCommand("jrc.sendData('type', [typeof df2.a[0], typeof df2.b[0], typeof df2.c[0]])", wait = 3)
  expect_equal(type, c("number", "string", "string"))
  expect_equal(names, c("a", "b", "c"))
  
  sendData("l", list(a = 1, b = 1:10, c = c("a", "b", "c"), d = as.factor(1:10))) # list of vectors -> Object of Arrays
  sendCommand("jrc.sendData('names', Object.keys(l))", wait = 3)
  sendCommand("jrc.sendData('type', typeof l.a)", wait = 3)
  sendCommand("jrc.sendData('size', l.c.length)", wait = 3)
  expect_equal(type, "number")
  expect_equal(names, c("a", "b", "c", "d"))
  expect_equal(size, 3)

  #keepAsVector = TRUE
  sendData("l", list(a = 1, b = 1:10), keepAsVector = TRUE) # list of vectors -> Object of Arrays
  sendCommand("jrc.sendData('size', l.a.length)", wait = 3)
  expect_equal(size, 1)

  sendData("l2", list(data.frame(a = 1:10,                                        # unnamed list -> Array of Arrays and Objects
                                 b = paste0("a", 1:10), 
                                 c = as.factor(rep(c("a", "b"), times = 5)),
                                 stringsAsFactors = F),
                      matrix(1:12, nrow = 4),
                      list(a = 1, b = 1:10, c = c("a", "b", "c"), d = as.factor(1:10))))
  sendCommand("jrc.sendData('type', Array.isArray(l2) ? 'Array' : 'something else')", wait = 3)
  expect_equal(type, "Array")
  sendCommand("jrc.sendData('type', typeof l2[2].a)", wait = 3)
  expect_equal(type, "number")
  
  expect_message(closePage(), "Server has been stopped.")
})

test_that("Data types are converted appropriately on the R side", {
  openPage(allowedVariables = c("x", "y", "z", "k", "m", "l"))
  
  sendCommand('jrc.sendData("x", 10)', wait = 3)  # number -> number
  expect_true(is.numeric(x))
  
  sendCommand('jrc.sendData("y", "abc")', wait = 3) # string -> character
  expect_type(y, "character")
  
  sendCommand('jrc.sendData("z", [1, 2, 3, 4, 5, 6])', wait = 3) # Array -> vector of numerics
  expect_true(is.vector(z))
  expect_true(is.numeric(z))
  
  sendCommand('jrc.sendData("k", ["a", "b", "c"])', wait = 3)
  expect_true(is.character(k))
  expect_length(k, 3)
  
  sendCommand('jrc.sendData("m", [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]])', wait = 3) # Array of numeric Arrays -> matrix (rowwise)
  expect_equal(dim(m), c(4, 3))
  
  sendCommand('jrc.sendData("l", {a: [1, 2, 3, 4, 5, 6], b: [7, 8, 9, 10, 11, 12]})', wait = 3)
  expect_type(l, "list")
  expect_length(l$a, 6)
  expect_true(is.numeric(l$b))
  
  expect_message(closePage(), "Server has been stopped.")
})

test_that("Data messages can be stored for later execution", {
  openPage()
  
  sendCommand('jrc.sendData("x", 15, false)', wait = 3)
  
  app <- getPage()
  session <- app$getSession(app$getSessionIds()$id)
  
  expect_length(session$getMessageIds(), 1)
  
  messageId <- session$getMessageIds()
  session$authorize(messageId)
  
  expect_equal(x, 15)
  
  expect_length(session$getMessageIds(), 0)
  expect_message(closePage(), "Server has been stopped.")
})

test_that("Variables are assigned in a correct environment", {
  
  openPage(allowedVariables = c(".x_ut", ".y_ut", ".z_ut"))
  e <- new.env()
  setEnvironment(e)
  
  sendCommand('jrc.sendData(".x_ut", 18, false)', wait = 3)
  expect_false(exists(".x_ut"))
  expect_equal(e$.x_ut, 18)
  
  sendCommand('jrc.sendData(".y_ut", 22, true)', wait = 3)
  expect_false(exists(".y_ut"))
  expect_false(exists(".y_ut", envir = e))
  
  id <- getPage()$getSessionIds()$id
  session <- getPage()$getSession(id)
  y <- session$getSessionVariable(".y_ut")
  expect_equal(y, 22)
  
  setEnvironment(environment())
  session$setSessionVariables(list(.z_ut = 10))
  sendCommand('jrc.sendData(".z_ut", 19)', wait = 3)
  expect_false(exists(".z_ut"))
  z <- session$getSessionVariable(".z_ut")
  expect_equal(z, 19)
  
  expect_message(closePage(), "Server has been stopped.")
})