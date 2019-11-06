## Resubmission

This is a new version that fixes some existing bugs, such as crashing when one attempts to send data with NAs present,
and inability to use the package on RStudio Server. For more details, please, check NEWS.md.

## Reverse dependencies

None is affected.

## Test environments
* local ubuntu 18.04 LST, R 3.6.1
* win-builder: R-devel
* ubuntu 14.04.5 LTS, R-devel, R 3.5.2, R 3.4.4 (travis ci)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

#Previous cran-comments

## Resubmission

This is a new version of the package. Here, we introduce functionality to limit the amount of control
user can get over the currently running R session from the web page. As a part of this, `callFunction`
function added to be used instead of `sendCommand` whenever possible and some bugs in variable types, when
they are exchanged between R and JavaScript fixed. For more details, please, check NEWS.md.

This resubmition will be followed by the resubmition of this package's reverse dependency `sleepwalk`. 

## Test environments
* local ubuntu 18.04 LST, R 3.6.1
* win-builder: R-devel
* ubuntu 14.04.5 LTS, R-devel, R 3.5.2, R 3.4.4 (travis ci)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

## Test environments
* local ubuntu 16.04 LST, R 3.5.1
* win-builder: R-devel
* ubuntu 14.04.5 LTS, R 3.5.2 (travis ci)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

## Resubmission

* unused import ('run_now' from 'later') removed.
* DESCRIPTION eddited.
* A typo that resulted in "Unexecutable code in man/sendCommand.Rd" error fixed.

>> Most of your examples are wrapped in \donttest{} and hence not tested. Please unwrap the examples if that is feasible and if they can be executed in < 5 sec for each Rd file or create additionally small toy examples.
Unfortunately, running even the most minimalistic examples requires to open a web page and establish a websocket connection. While this works locally, devtools::check_rhub() always produces errors about not being able to open a web browser.
