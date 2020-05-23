## Overview

Sometimes it would be useful to make completely standalone Rmarkdown documents that do not depend on data in external files. One important example of this is scientific publications written in Rmarkdown for which we often would like to supply the source document with data to ensure results are reproducible.

This package provides for a new `data` language engine to facilitate the creation of standalone Rmarkdown documents. Instead of putting code inside `data` chunks, one puts the contents of the data file that one wishes to use in your Rmarkdown document. This data can then be directly read from these chunks into R and used in the Rmarkdown script.

The detailed functioning of the package is described in a vignette accompanying the package.

## Installation

This package can be installed from Github executing the
following command in R:

```r
remotes::install_github("dmkaplan2000/knitrdata",build_vignettes=TRUE)
```

## Use

To use the package, load the library in the `setup` chunk
at the beginning of your Rmarkdown document:

```r
libary(knitrdata)
```

After this, you can add `data` chunks to your Rmarkdown document. See
package documentation, vignettes and examples for more details.
