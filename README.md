[![Travis Build Status](https://travis-ci.org/anders-biostat/jrc.svg?branch=master)](https://travis-ci.org/anders-biostat/jrc)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/jrc)](https://cran.r-project.org/package=jrc)

## jrc

An R package to exchange commands between R and JavaScript. It opens a new or an existing web page and establishes a websocket connection
to the currently running R session. 

## Installation

``` r
install.packages("jrc")
``` 

## Usage

Main R functions are `sendData()`, `sendCommand()`, and `sendHTML()`. The first one sends a variable
from the R session to the web page, the second one executes a JavaScript code, and the last one 
adds some HTML to the web page.
Their JavaScript counterparts are `jrc.sendData()` and `jrc.sendCommand()`.

`openPage()` and `closePage()` respectively opens and closes a web page.

This example opens a new page and adds there a button, that increases value of `k` by one in the
R session.

``` r
library(jrc)

k <- 0
openPage()
sendCommand(paste0("button = document.createElement('input');",
              "button.type = 'button';",
              "button.addEventListener('click', function() {jrc.sendCommand('k <<- k + 1')});", 
              "button.value = '+1';",
              "document.body.appendChild(button);", collapse = "\n"))
```



