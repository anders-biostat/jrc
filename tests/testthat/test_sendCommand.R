context("Send and receive commands")

test_that("Commands can be send, received and stored", {
  app <- App$new()
  app$startServer()

  app$openPage()
  
  session <- app$getSession()
  
  session$sendCommand(paste0("jrc.sendCommand('message(\"Hi\")')"), wait = 3)
  expect_length(session$getMessageIds(), 1)
  session$sendCommand(paste0("jrc.sendCommand('message(\"Hi\")')"), wait = 3)
  
  messageIds <- session$getMessageIds()
  expect_length(messageIds, 2)
  
  expect_message(session$authorize(messageIds[1]), "Hi")

  expect_length(session$getMessageIds(), 1)
  session$removeMessage(messageIds[2])
  expect_length(session$getMessageIds(), 0)
  
  k_send <- sample(1000, 1)
  k_received <- -1
  
  session$sendCommand(paste0("jrc.sendCommand('k_received <<- ", k_send, "')"), wait = 3)
  session$authorize()   
  
  expect_equal(k_received, k_send)  
  
  expect_message(app$stopServer(), "Server has been stopped.")
})

test_that("Commands can be evaluated in the given environment", {
  app <- App$new()
  app$startServer()
  
  e <- new.env()
  app$setEnvironment(e)
  
  app$openPage()
  
  session <- app$getSession()
  
  e$k_send <- sample(1000, 1)
  e$k_received <- -1
  
  session$sendCommand(paste0("jrc.sendCommand('k_received <<- ", e$k_send, "')"), wait = 3)
  messageId <- session$getMessageIds()
  session$authorize(messageId)
  
  expect_equal(e$k_received, e$k_send)
  
  expect_message(app$stopServer(), "Server has been stopped.")  
})

test_that("Wrapper function is working", {
  openPage()
  
  k_send <- sample(1000, 1)
  k_received <- -1
  
  sendCommand(paste0("jrc.sendCommand('k_received <<- ", k_send, "')"), wait = 3)
  session <- getSession()
  messageId <- getMessageIds()

  authorize(session$id, messageId)
  
  expect_equal(k_received, k_send)  

  expect_message(closePage(), "Server has been stopped.")
})
