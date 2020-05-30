# Functions for creating a data chunk with appropriate header ------------------

#' Tools for creating (data) chunks and inserting them into Rmarkdown documents
#'
#' These helper functions allow one to add the chunk header and tail to text containing
#' chunk contents and then insert that into a Rmarkdown document.
#'
#' \code{create_chunk}
#' takes in the (possibly encoded by \code{data_encode})
#' contents of a chunk and adds the chunk header and closer, invisibly returning
#' entire chunk contents as a character string.
#'
#' \code{insert_chunk} takes the chunk contents and inserts it at the given line number
#' in the \code{rmd.text} or \code{rmd.file}.
#'
#' Note that the additional arguments to \code{create_chunk} (\dots) are not
#' evaluated, but rather they are placed in the chunk header as they appear in the
#' function call as additional chunk options.
#'
#' @param text Character vector with contents of chunk.
#' @param file Path to file containing chunk contents. Ignored if \code{text}
#'   argument supplied. As a consequence, this means that all arguments must be named
#'   if the \code{file} argument is supplied to \code{create_chunk}.
#' @param chunk_label Character string giving the label to be used for the chunk.
#' @param chunk_type Character string giving the chunk type. Defaults to \code{"data"}.
#' @param \dots Additional chunk options. These are not evaluated, but rather included
#'   in the function call as they are entered in the function call.
#' @param chunk_options_string Character vector with additional chunk options that will
#'   be included in the header after the arguments in \dots.
#' @param chunk Character string with chunk contents including header and tail.
#' @param line Line number where chunk to be inserted.
#' @param rmd.text Text of Rmarkdown document where chunk contents are to be inserted.
#' @param rmd.file Filename of Rmarkdown document where chunk contents are to be inserted.
#'   Ignored if \code{rmd.text} is supplied.
#'
#' @export
#'
#' @describeIn create_chunk Silently returns chunk text contents.
#' @family create_chunks
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
#'
#' @example tests/test.create_chunks.R
create_chunk = function(text=readLines(file),
                        ...,chunk_label=NULL,
                        chunk_type="data",
                        file=NULL,chunk_options_string=NULL) {
  text = paste(text,collapse="\n")

  # Get additional input options as character string
  args = match.call(expand.dots=FALSE)$...
  argvalues = vapply(args, deparse, character(1L))
  dots = paste(ifelse(names(args)=="",argvalues,
                      paste(names(args), argvalues, sep = ' = ')),
               collapse = ', ')

  if (!is.null(chunk_options_string)) {
    dots = ifelse(dots=="",
                  chunk_options_string,
                  paste(dots,chunk_options_string,sep=", "))
  }

  if (!is.null(chunk_label))
    chunk_label = paste0(chunk_label,",")

  header = paste0("```{",chunk_type," ",chunk_label,dots,"}")
  tail = "```"

  chunk = paste(header,text,tail,sep="\n")

  invisible(chunk)
}

#' @export
#'
#' @describeIn create_chunk Invisibly returns the contents of the modified Rmarkdown
#'   as a character vector with each line in an element of the vector
#'   including the chunk at the appropriate line number.
insert_chunk = function(chunk,line,rmd.text=readLines(rmd.file),rmd.file=NULL) {
  if (line > length(rmd.text))
    stop("line argument beyond end of input file.")

  y = c(rmd.text[1:(line-1)],chunk,rmd.text[line:length(rmd.text)])

  invisible(y)
}