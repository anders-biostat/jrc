## Resubmission

This is a new version of the package. It adds a function to block the R session and listen to the server. It is needed to use jrc apps, for instance, in Jupyter Notebooks or in other cases, where the R
session is not interactive.

Regarding the auto-check issues at https://www.stats.ox.ac.uk/pub/bdr/donttest/jrc.out
  
I've replaced all donttest examples with dontrun and added a comment for the users that one needs a browser to run the code.

  >> Dear CRAN Team,

  >> The error occurs because all the provided examples require a browser to be installed, which is not always the case for automated checks. The examples are checked locally (manually in RStudio and with R CMD check --run-donttest). Also, they work when testing for Windows platform with rhub, or check_win_devel. In other cases (rhub Ubuntu and Fedora) I set _R_CHECK_DONTTEST_EXAMPLES_=FALSE. Should I do something like that for the CRAN auto-check? Or maybe replace donttest with dontrun?

  >Yes, please \dontrun{} plus an exlplanation as comment.
  >Or perhaps protect by if(interactive()) in case it should only run for users working interactivbely with the package.

  >Best,
  >Uwe Ligges

## Test environments
* local Ubuntu 23.04, R 4.2.2
* win-builder: R-devel
* Ubuntu Linux 20.04.1 LTS, R-release; Windows Server 2022, R-devel, 64 bit; Fedora Linux, R-devel (R-hub)

There were 2 NOTES when testing the package for Windows with R-hub:

```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

Both are mentioned in the open issues of the rhub package.


There was 1 NOTE when testing on R-hub (for both Ubuntu and Fedora), which, as far as I undertood, is caused by some promlem on the side of the testing platform.

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

For these two checks, _R_CHECK_DONTTEST_EXAMPLES_ was also set to false, since all the examples require presence of a browser. All other checks include running the examples.

# Previous cran-comments

## Resubmission

This is a new version of the package. It introduces minor changes in functionality and also fixes a
couple of existing issues. See NEWS.md for more details.

## Test environments
* local Ubuntu 21.10, R 4.0.4
* win-builder: R-devel
* Ubuntu Linux 20.04.1 LTS, R-release; Windows Server 2022, R-devel, 64 bit; Fedora Linux, R-devel (R-hub)

There is a NOTE when testing for Windows Server:
```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
However, I can't neither understand the cause of this note, nor reproduce it locally on my Windows machine (Windows 10, 64 bit, R 4.0.2, MiKTeX installed). There are also no notes or errors in the win-builder. So I decided to submit the package anyway.

## Resubmission
This is a new version of the package. It introduces minor changes in functionality and also fixes a
couple of existing issues. See NEWS.md for more details.

## Test environments
* local ubuntu 20.04 LST, R 4.0.3
* win-builder: R-devel
* ubuntu 16.04.6 LTS, R-devel, R 4.0.2, R 3.6.3 (travis ci)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

## Resubmission
This is a new version of the package. It fixes a major security issue by restricting server access to only to specified directories.

All the examples were checked locally by both R CMD check --run-donttest and running them manually in RStudio. 
However, all other checks were performed with _R_CHECK_DONTTEST_EXAMPLES_=FALSE.
Examples, wrapped in donttest{}, are functional and don't produce errors, but require some browser to be installed. 
I would rather not change `donttest` to `dontrun`, since it can confuse users.

## Test environments
* local ubuntu 20.04 LST, R 4.0.2
* win-builder: R-devel
* ubuntu 16.04.6 LTS, R-devel, R 4.0.2, R 3.6.3 (travis ci)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

## Resubmission

This is a new version that fixes some existing bugs. Other than that, it doesn't change any functinality. For more details, please, check NEWS.md.

## Test environments
* local ubuntu 18.04 LST, R 3.6.2
* win-builder: R-devel
* ubuntu 16.04.6 LTS, R-devel, R 3.6.2, R 3.5.3 (travis ci)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

## Resubmission

This is a new version that introduces major restructarization of the package. It now supports multiple connections to the interactive apps and
introduces functions to manage client sessions. The app itself is now contained within a single R6 object, which allows to start multiple apps
inside one R session.

## Reverse dependencies

Backwards compatibility is fully maintained.

## Test environments
* local ubuntu 18.04 LST, R 3.6.2
* win-builder: R-devel
* ubuntu 16.04.6 LTS, R-devel, R 3.6.2, R 3.5.3 (travis ci)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

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
