context("Send and receive messages")

test_that("Commands can be send, received and stored", {
  app <- App$new()
  app$startServer()
  app$openPage()
  
  session <- app$getSession(app$getSessionIds()$id)
  
  k_send <- sample(1000, 1)
  
  session$sendCommand(paste0("jrc.sendCommand(' k_received <<- ", k_send, "')"), wait = 3)
  messageIds <- session$getMessageIds()
  expect_equal(length(messageIds), 1)
  session$sendCommand(paste0("jrc.sendCommand('print(\"Hi!\"')"), wait = 3)
  expect_equal(length(session$getMessageIds()), 2)
  
  session$authorize(messageIds)
  expect_equal(k_received, k_send)
  expect_equal(length(session$getMessageIds()), 1)
  
  expect_message(app$stopServer(), "Server has been stopped.")
})