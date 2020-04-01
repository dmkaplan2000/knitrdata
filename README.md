## Installation

This package can currently be installed from Github executing the
following command in R:

```r
remotes::install_github("dmkaplan2000/knitrdata")
```

## Use

To use the package, add the following two lines to the `setup` chunk
at the beginning of your Rmarkdown document:

```r
libary(knitrdata)
add_data_language_engine()
```

After this, you can add `data` chunks to your Rmarkdown document. See
package documentation, vignettes and examples for more details.
