knitrdata 0.6.1
---------------------------------------------------------------------

- Minor changes to vignette.
- Fixed strange RStudio bug associated with addin described [here](https://github.com/rstudio/rstudioapi/issues/211) by moving all rstudioapi code into Shiny dialog

knitrdata 0.6.0
---------------------------------------------------------------------

- Adding remove_chunks function for command link chunk removal.
- Fix to empty line removal in create/insert_data_chunk_dialog.

knitrdata 0.5.3
---------------------------------------------------------------------

- Better tests and documentation of text data + md5sum checks
- Fixed text file issues with remove and insert RStudio addins
- Added newline character determination to text file chunk insert via addin

knitrdata 0.5.2
---------------------------------------------------------------------

- Fixing text file + md5sum mess

knitrdata 0.5.1
---------------------------------------------------------------------

- Added TOC to vignette
- Moved encoding/decoding of base64 to use functions in xfun
- Removed dependency on base64enc package

knitrdata 0.5.0
---------------------------------------------------------------------

- Changing to insertText in Rstudio addins to speed insertion
- New non-interactive addin to create empty data chunk
- Better handling of missing password for GPG-encrypted chunks during knit
- knitrdata instructional video

knitrdata 0.4.1
---------------------------------------------------------------------

- Fix to use radio buttons instead of checkboxes in addings
- Fix to avoid adding extra newline to decoded text files
- Implementation of gpg encoding in Rstudio addins

knitrdata 0.4.0
---------------------------------------------------------------------

- Added Rstudio addins for adding and removing data chunks

knitrdata 0.3.1
---------------------------------------------------------------------

- Added max.echo chunk option
- Added script for testing of large data chunks
- Fix for global assignment issue based on persistent data

knitrdata 0.3.0
---------------------------------------------------------------------

- Added md5sum checks on `data` chunks
- Added `create_chunk` and `insert_chunk` to facilitate placing chunks in Rmarkdown docs
- Added `list_rmd_chunks` and `splice_rmd_by_chunk` to facilitate removing and decoding `data` chunks
- Expanded README
- Vignette update for new features

knitrdata 0.2.1
---------------------------------------------------------------------

- Improving vignette and changing `eval=FALSE` functioning.
- Changing `newline` chunk option to `line.sep`.
- Fixing instructions regarding install with `remotes` to avoid missing vignettes problems.


knitrdata 0.2.0
---------------------------------------------------------------------

- Adding loader.function chunk option

knitrdata 0.1.0
---------------------------------------------------------------------

- Initial release
