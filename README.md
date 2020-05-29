## Overview

Sometimes it would be useful to make completely standalone Rmarkdown documents that do not depend on data in external files. One important example of this is scientific publications written in Rmarkdown for which we often would like to supply the source document with data to ensure results are reproducible.

This package extends the `knitr` package, providing a new `data` language engine to facilitate the creation of standalone Rmarkdown documents. Instead of putting code inside `data` chunks, one puts the contents of the data file that one wishes to use in your Rmarkdown document. This data can then be directly read from these chunks into the Rmarkdown R session and used in the Rmarkdown script. Tools are included for creating both text (e.g., CSV) and binary (e.g., RDS files) `data` chunks.

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

After this, you can add `data` chunks to your Rmarkdown document. For example, you could add the following chunk to your Rmarkdown document to load the CSV data in the chunk into the Rmarkdown R session under the variable name `d`:

````
```{data output.var="d",loader.function=read.csv}
site,density
a,1.2
b,3.4
c,5.6
```
````

See package documentation, vignettes and examples for more details.
