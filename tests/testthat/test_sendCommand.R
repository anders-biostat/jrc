context("Send and receive commands")

test_that("Commands can be send, received and stored", {
  app <- App$new()
  app$startServer()

  app$openPage()
  
  session <- app$getSession(app$getSessionIds()$id)
  
  session$sendCommand(paste0("jrc.sendCommand('message(\"Hi\")')"), wait = 3)
  expect_equal(length(session$getMessageIds()), 1)
  session$sendCommand(paste0("jrc.sendCommand('message(\"Hi\")')"), wait = 3)
  
  messageIds <- session$getMessageIds()
  expect_equal(length(messageIds), 2)
  
  expect_message(session$authorize(messageIds[1]), "Hi")

  expect_equal(length(session$getMessageIds()), 1)
  session$removeMessage(messageIds[2])
  expect_equal(length(session$getMessageIds()), 0)
  
  expect_message(app$stopServer(), "Server has been stopped.")
})

test_that("Default environment is the right one", {
  app <- App$new()
  app$startServer()
  
  app$openPage()
  
  session <- app$getSession(app$getSessionIds()$id)
  
  k_send <- sample(1000, 1)
  k_received <- -1
  
  session$sendCommand(paste0("jrc.sendCommand('k_received <<- ", k_send, "')"), wait = 3)
  messageId <- session$getMessageIds()
  session$authorize(messageId)   
  
  expect_equal(k_received, k_send)
  
  expect_message(app$stopServer(), "Server has been stopped.")      
})

test_that("Assignment operator works in a given environment", {
  app <- App$new()
  app$startServer()
  
  e <- new.env()
  app$setEnvironment(e)
  
  app$openPage()
  
  session <- app$getSession(app$getSessionIds()$id)
  
  e$k_send <- sample(1000, 1)
  e$k_received <- -1
  
  session$sendCommand(paste0("jrc.sendCommand('k_received <<- ", e$k_send, "')"), wait = 3)
  messageId <- session$getMessageIds()
  session$authorize(messageId)
  
  expect_equal(e$k_received, e$k_send)
  
  expect_message(app$stopServer(), "Server has been stopped.")  
})

