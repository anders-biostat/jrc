## Test environments
* local ubuntu 16.04 LST, R 3.5.1
* win-builder: R-devel


## Resubmission

* unused import ('run_now' from 'later') removed.
* DESCRIPTION eddited.
* A typo that resulted in "Unexecutable code in man/sendCommand.Rd" error fixed.

>> Most of your examples are wrapped in \donttest{} and hence not tested. Please unwrap the examples if that is feasible and if they can be executed in < 5 sec for each Rd file or create additionally small toy examples.
Unfortunately, running even the most minimalistic examples requires to open a web page and establish a websocket connection. While this works locally, devtools::check_rhub() always produces errors about not being able to open a web browser.
