## Overview

Sometimes it would be useful to make completely standalone Rmarkdown documents that do not depend on data or other information in external files. One important example of this is scientific publications written in Rmarkdown for which we often would like to supply the source document with data to ensure results are reproducible.

This package extends the `knitr` package, providing a new `data` language engine to facilitate the creation of standalone Rmarkdown documents. Instead of putting code inside `data` chunks, one puts the contents of the data file that one wishes to use in your Rmarkdown document. This data can then be directly read from these chunks into the Rmarkdown R session and used in the Rmarkdown script. Tools are included for creating both text (e.g., CSV) and binary (e.g., RDS files) `data` chunks.

The detailed functioning of the package is described in a vignette accompanying the package.

## Installation & Use

The latest version of this package can be installed from Github executing the
following command in R:

```r
remotes::install_github("dmkaplan2000/knitrdata",build_vignettes=TRUE)
```

A recent version of this package can also be installed from
[CRAN](https://cran.r-project.org/package=knitrdata):

```r
install.packages("knitrdata")
```

To use the package, load the library in the `setup` chunk
at the beginning of your Rmarkdown document:

```r
libary(knitrdata)
```

## Examples

After the package has been loaded in the Rmarkdown document, you can add `data` chunks to your document. `knitrdata` works along much the same principles as standalone HTML web pages or emails with attachments: data, encoded as text if binary, are included in special chunks with a bit of header information explaining how to process the data. For example, you could read standard CSV-formatted data into your Rmarkdown document by adding the following chunk:

````
```{data output.var="d",loader.function=read.csv}
site,density
a,1.2
b,3.4
c,5.6
```
````

This will load the CSV data in the chunk into the Rmarkdown R session under the variable name `d`. R chunks after this `data` chunk can use `d` as a normal R variable. 

With appropriate options, `data` chunks can handle CSV data with arbitrary delimiters and white-space delimited data, essentially anything for which an appropriate "loader function" exists in R.

One can achieve the same thing and much more using a chunk containing a binary RDS file:

````
```{data output.var = "d", format = "binary", loader.function = readRDS, md5sum='e326bdd310818f4f223f3a89e8f18dd5'}
H4sIAAAAAAAAA4vgYmBgYGZgZgNiViCTgTU0xE3XAigmDOQwAWlesAIGBkYIH8Rm
YQLzWRg4gTRbTmpZak4xkCUAloWIMibCGEkwRjKaRtbknMRimD64aWmJySX5RUDW
PyDmA5lo/9kYDBy4obRYGhigG5eXmJsKM44JKshSnFmSCmWzp6TmAbmVEH1M/9Fs
5kpJLEnUSysCmoJmMmdRfrkezHRQcDA1AIn/////BTkTAOOlUIFCAQAA
```
````

The contents of this chunk are the base64 encoding of a binary RDS file containing the data table above. The chunk header also includes an optional `md5sum` of the decoded chunk contents that is checked during processing to assure accuracy of the encoded data. Again, with appropriate options, essentially any type of binary data can be loaded with `data` chunks.

One can also use GPG to encrypt the chunk contents so that only users with the decryption key can have access to the data:

````
```{data output.var = "d", format = "binary", encoding="gpg", loader.function = readRDS, md5sum='e326bdd310818f4f223f3a89e8f18dd5'}
-----BEGIN PGP MESSAGE-----
Version: GnuPG v2

hQEMA+gixT2HKBy2AQf+MlmKnZjj4d0ajbTXI843LAPXWM+9OMklNh/YxtOnmg9m
u4f4lN3pjH+dDx2Y1CakKi4VtpCeksKPmizyKbLu3NJvrz/Dyz2KzyBHqgvfnst7
iwkCj5lKctL7RxYMxDkFzJe2ZyrnZWY8cGZe07ONdVT2y7kjv4JfWT8vqLXioY0j
JiwV/fGNVBiqHS164sHhLrSnQcL/RV6KykqODlorg9Qasxag5M0Y0ROvC13NLrFQ
XoK7VMg+7Fs4QOsi1EQrw/TMT6ffke+EhYVyAF7c5qBVWVJRCoytglaAGd/E59r6
k6WaNlNqD+btJRjAj8KH1MwSVPRkJ1b4ezMck6C899LAOQGfR6w8XdnB1856jhBM
RoBsPgiH51IknLocdLh3gZLGFrrUq+p/8Yf5iX12UnqK+rzzbFE3WN9//no4T1fJ
LGkN+93jI9aQhhY4moR5Eihoul9rgGLxEos8ZvsXyWmm5Y8LD0ZCMrnKbhKVE2yR
61SnaLg2Vnp1DlGvxcpGD8aYrqTjipxROCcmjuyCjzJDTef2YnnTDlF4Rzuaz8PO
vhXRg3BgLGtu3OWTaVuU7NpCB7/7A/EPHddoyUo/+xzi6/+t7Z6TSZFwIqq2jRlZ
Zz8yfmCGaBcn/VNJdrpZTOxHNel/RLMiuHnkDaia/hdKVYs9ObnB6gIt9Q==
=pyWi
-----END PGP MESSAGE-----

```
````

`knitrdata` includes the `data_encode`, `create_chunk` and `insert_chunk` helper functions to facilitate encoding of data, creation and insertion of `data` chunks, respectively.

`data` chunks are not limited to scientific data, but can also include images, text and text documents. For example, the following would export the given BibTeX references to the file `references.bib`:

````
```{data output.file="references.bib"}
@article{MeynardTestingmethodsspecies2019,
  ids = {MeynardTestingmethodsspecie,MeynardTestingmethodsspeciesinpress},
  title = {Testing Methods in Species Distribution Modelling Using Virtual Species: What Have We Learnt and What Are We Missing?},
  shorttitle = {Testing Methods in Species Distribution Modelling Using Virtual Species},
  author = {Meynard, Christine N. and Leroy, Boris and Kaplan, David M.},
  year = {2019},
  month = dec,
  volume = {42},
  pages = {2021--2036},
  issn = {0906-7590, 1600-0587},
  doi = {10.1111/ecog.04385},
  file = {/home/dmk/papers/meynard.et.al.2019.testing_methods_in_species_distribution_modelling_using_virtual_species.pdf},
  journal = {Ecography},
  keywords = {artificial species,environmental niche models,niche,simulations,species distribution modelling,virtual ecologist},
  language = {en},
  number = {12}
}

@article{SantosConsequencesdriftcarcass2018,
  title = {Consequences of Drift and Carcass Decomposition for Estimating Sea Turtle Mortality Hotspots},
  author = {Santos, Bianca S. and Kaplan, David M. and Friedrichs, Marjorie A. M. and Barco, Susan G. and Mansfield, Katherine L. and Manning, James P.},
  year = {2018},
  month = jan,
  volume = {84},
  pages = {319--336},
  issn = {1470-160X},
  doi = {10.1016/j.ecolind.2017.08.064},
  copyright = {All rights reserved},
  file = {/home/dmk/papers/santos.et.al.2018.consequences_of_drift_and_carcass_decomposition_for_estimating_sea_turtle.pdf},
  journal = {Ecological Indicators},
  keywords = {Carcass decomposition,Chesapeake bay,Conservation,Drift leeway,Drift simulations,Endangered species,Sea turtle mortality,Sea turtle strandings}
}
```
````

If the following line is in the YAML header of the document:

```
bibliography: references.bib
```

then the contents of this file will be used to generate citations and the bibliography of the document in the final formatting step of the knitting process. This can be done even if the external file `references.bib` did not exist when the knitting process was initiated. This references `data` chunk can be placed anywhere in the Rmarkdown document, even after references have been cited in the text (e.g., the end of the document, which is often the most convenient place). 

The same process can be used to embed all ancillary formatting files (e.g., LaTeX .cls style files, bibliography .csl syle files, CSS files, LaTeX header files) inside an Rmarkdown document using `data` chunks, obviating the need for external files.

See package vignettes, documentation and examples for more details, including a full list of chunk options and more usage examples.
