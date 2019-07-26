# ellipse

<!-- badges: start -->
<!-- badges: end -->

This package contains ellipse drawing routines designed for pairwise 
confidence regions, including distorted ellipses for nonlinear
regression regions.  It also includes a routine "plotcorr" for 
plotting correlation matrices using ellipses.


## Installation

You can install the released version of ellipse from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ellipse")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ellipse)
## basic example code
```

The examples require the MASS, nls and ts packages.

### Release History:

* 0.2.12 (Jan 2004) - Bug fix for missing values in plotcorr, doc error

* 0.2.14 (Aug 2004) - Allowed separate colors for each ellipse in plotcorr
            and fixed some .Rd errors.

* 0.2.15 (May 2005) - Allowed plotcorr to plot just upper or lower triangle
            of matrix.
            
* 0.3.1 (June 2005) - Added NAMESPACE, package overview topic

* 0.3.2 (Feb 2006)  - Remove deprecated install.R

* 0.3.3 (Nov 2006)  - Nicer colors in plotcorr() example (from Gregor Gorjanc)

* 0.3.4 (Nov 2006)  - Update to strict test compliance

* 0.3.5 (Sep 2007)  - License clarification

* 0.3.6 (Apr 2009)  - Bug fix:  ... was not being passed from methods to
           ellipse.default().
           
* 0.3.7 (Mar 2012)  - Minor fix (clean up DESCRIPTION file)

* 0.3.8 (Apr 2013)  - Minor fix (no more \synopsis tag in help)
            
* 0.4.1 (Jan 2018)  - At request of CRAN, added pairs() generic function to avoid
       clash with MASS::pairs.profile, and other minor cleanups.
