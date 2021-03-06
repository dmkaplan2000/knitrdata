% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rstudio_addins.R
\encoding{UTF-8}
\name{insert_data_chunk_template}
\alias{insert_data_chunk_template}
\title{Insert an empty data chunk template in active source document}
\usage{
insert_data_chunk_template()
}
\value{
Invisibly returns the chunk contents as a character vector, one line
  of text per element.
}
\description{
This function is essentially the equivalent for data chunks of the "Insert a
new code chunk" menu item available in Rstudio when a Rmarkdown document is
open. It places at the current cursor location an empty \code{data} chunk
that can then be modified and filled in by hand.
}
\examples{
\dontrun{
insert_data_chunk_template()
}

}
\seealso{
Other Chunk tools: 
\code{\link{create_chunk}()},
\code{\link{create_data_chunk_dialog}()},
\code{\link{list_rmd_chunks}()},
\code{\link{remove_chunks_dialog}()}
}
\author{
David M. Kaplan \email{dmkaplan2000@gmail.com}
}
\concept{Chunk tools}
