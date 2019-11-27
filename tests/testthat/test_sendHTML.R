context("Sending HTML to web page")

test_that("HTML can be added to the web page", {
  openPage(allowedVariables = "a")
  
  sendHTML('<input type = "button" id = "inp" onclick = "jrc.sendData(\'a\', \'click\', false)">')
  
  sendCommand(paste0('document.getElementById("inp").click()'), wait = 3)
  expect_equal(a, "click")
  
  closePage()
})