context("Send and receive data")

test_that("Data types are converted appropriately on the server side", {
  openPage()
  allowVariables(c("type", "size", "names"))
  
  sendData("x", 10)
  sendCommand("jrc.sendData('type', typeof x, false)", wait = 3)
  expect_equal(type, "number")
  
  sendData("y", "abc")
  sendCommand("jrc.sendData('type', typeof y, false)", wait = 3)
  expect_equal(type, "string")
  
  sendData("z", 1:10)
  sendCommand("jrc.sendData('type', Array.isArray(z) ? 'Array' : 'something else', false)", wait = 3)
  sendCommand("jrc.sendData('size', z.length, false)", wait = 3)
  expect_equal(type, "Array")
  expect_equal(size, 10)

  sendData("k", c("a", "b", "c")) 
  sendCommand("jrc.sendData('type', Array.isArray(k) ? 'Array' : 'something else', false)", wait = 3)
  sendCommand("jrc.sendData('size', k.length, false)", wait = 3)
  expect_equal(type, "Array")
  expect_equal(size, 3)
  
  sendData("f", as.factor(1:10)) # factor -> Array of strings
  sendCommand("jrc.sendData('type', typeof f[0], false)", wait = 3)
  expect_equal(type, "string")
  
  sendData("m", matrix(1:12, nrow = 4)) # matrix -> Array of Arrays
  sendCommand("jrc.sendData('size', [m.length, m[0].length], false)", wait = 3)
  expect_equal(size, c(4, 3))
  
  sendData("df", data.frame(a = 1:10, b = 11:20)) # data.frame -> Array of Objects (rowwise)
  sendCommand("jrc.sendData('size', df.length, false)", wait = 3)
  expect_equal(size, 10)
  sendCommand("jrc.sendData('names', Object.keys(df[0]), false)", wait = 3)
  expect_equal(names, c("a", "b"))
  
  sendData("df2", data.frame(a = 1:10,            # data.frame -> Array of Objects (rowwise) (factor -> character)
                             b = paste0("a", 1:10), 
                             c = as.factor(rep(c("a", "b"), times = 5)),
                             stringsAsFactors = F))
  sendCommand("jrc.sendData('type', [typeof df2[0].a, typeof df2[0].b, typeof df2[0].c], false)", wait = 3)
  expect_equal(type, c("number", "string", "string"))
  
  ####columnwise#####
  sendData("m", matrix(1:12, nrow = 4), rowwise = F) # matrix -> Array of Arrays
  sendCommand("jrc.sendData('size', [m.length, m[0].length], false)", wait = 3)
  expect_equal(size, c(3, 4))
  
  sendData("df", data.frame(a = 1:10, b = 11:20), rowwise = FALSE) # data.frame -> Object with each column as Array
  sendCommand("jrc.sendData('size', df.a.length, false)", wait = 3)
  sendCommand("jrc.sendData('type', typeof df.b[0], false)", wait = 3)
  expect_equal(size, 10)
  expect_equal(type, "number")
  
  sendData("df2", data.frame(a = 1:10,            # data.frame -> Object with each column as Array
                               b = paste0("a", 1:10), 
                               c = as.factor(rep(c("a", "b"), times = 5)),
                               stringsAsFactors = F), rowwise = F)
  sendCommand("jrc.sendData('names', Object.keys(df2), false)", wait = 3)
  sendCommand("jrc.sendData('type', [typeof df2.a[0], typeof df2.b[0], typeof df2.c[0]], false)", wait = 3)
  expect_equal(type, c("number", "string", "string"))
  expect_equal(names, c("a", "b", "c"))
  
  sendData("l", list(a = 1, b = 1:10, c = c("a", "b", "c"), d = as.factor(1:10))) # list of vectors -> Object of Arrays
  sendCommand("jrc.sendData('names', Object.keys(l), false)", wait = 3)
  sendCommand("jrc.sendData('type', typeof l.a, false)", wait = 3)
  sendCommand("jrc.sendData('size', l.c.length, false)", wait = 3)
  expect_equal(type, "number")
  expect_equal(names, c("a", "b", "c", "d"))
  expect_equal(size, 3)
  
  sendData("l2", list(data.frame(a = 1:10,                                        # unnamed list -> Array of Arrays and Objects
                                 b = paste0("a", 1:10), 
                                 c = as.factor(rep(c("a", "b"), times = 5)),
                                 stringsAsFactors = F),
                      matrix(1:12, nrow = 4),
                      list(a = 1, b = 1:10, c = c("a", "b", "c"), d = as.factor(1:10))))
  sendCommand("jrc.sendData('type', Array.isArray(l2) ? 'Array' : 'something else', false)", wait = 3)
  expect_equal(type, "Array")
  sendCommand("jrc.sendData('type', typeof l2[2].a, false)", wait = 3)
  expect_equal(type, "number")
  
  expect_message(closePage(), "Server has been stopped.")
})