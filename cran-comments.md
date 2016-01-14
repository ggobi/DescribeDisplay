## Test environments and R CMD check results

* local OS X install (x86_64-apple-darwin13.4.0), R 3.2.3
  * There were no ERRORs or WARNINGs.
  * There is one NOTE. I have updated my email address.
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Di Cook <dicook@monash.edu>’

    New maintainer:
      Di Cook <dicook@monash.edu>
    Old maintainer(s):
      Di Cook <dicook@iastate.edu>

    License components with restrictions and base license permitting such:
      MIT + file LICENSE
    File 'LICENSE':
      YEAR: 2006-2014
      COPYRIGHT HOLDER: Hadley Wickham; Di Cook; Andreas Buja; Barret Schloerke

* ubuntu 12.04 (on travis-ci, x86_64-pc-linux-gnu), R 3.2.3
  * There were no ERRORs, WARNINGs.  
  * There are two NOTEs. But they seem to be false positives.
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Di Cook <dicook@monash.edu>’
    License components with restrictions and base license permitting such:
      MIT + file LICENSE
    File 'LICENSE':
      YEAR: 2006-2014
      COPYRIGHT HOLDER: Hadley Wickham; Di Cook; Andreas Buja; Barret Schloerke
    Checking URLs requires 'libcurl' support in the R build

    * checking package dependencies ... NOTE
      No repository set, so cyclic dependency check skipped

* win-builder (devel and release)
  * There were no ERRORs, WARNINGs.  
  * There are no NOTEs.


## Downstream dependencies
There are no downstream dependencies. (Yay!)
