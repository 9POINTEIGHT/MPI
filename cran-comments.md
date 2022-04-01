# Test environments
 * local Windows 10 install, R 4.0.3
 * R-hub Windows Server 2022, R-devel, 64 bit
 * R-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC
 * R-hub Fedora Linux, R-devel, clang, gfortran 
 * win-builder (devel and release)

# R CMD check results
0 errors | 0 warnings | 0 notes 

# R-hub platforms 
There are 2 NOTES

The first NOTE, which appears in all R-hub and win-builder, is because this package is the new submission on CRAN.
```r
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kittiya Kukiattikun <kittiya.contact@gmail.com>'

New submission
```
The second Note, which appears only in R-hub Windows Server 2022, is might be a bug/crash in miktex, as is noted in 
<a href="https://github.com/r-hub/rhub/issues/503">R-hub issue #503</a> .
```r
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  ```