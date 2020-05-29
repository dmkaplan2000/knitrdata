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
#' in the \code{input.file}. By default, it will overwrite \code{input.file} unless the
#' \code{output.file} argument is supplied and is different from \code{input.file}.
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
#' @param chunk_options_string Character vector with text of chunk options. If given,
#'   additional function arguments (\dots) will be ignored with a warning if any exist.
#' @param chunk Character string with chunk contents including header and tail.
#' @param input.file Rmarkdown file where chunk contents are to be inserted.
#' @param line Line number where chunk to be inserted.
#' @param output.file Where to write final Rmarkdown document with chunk spliced in.
#'   Defaults to \code{input.file}.
#' @param overwrite Whether or not to overwrite the \code{output.file}. Defaults to
#'   \code{TRUE} if \code{input.file=output.file}, otherwise \code{FALSE}.
#'
#' @export
#'
#' @describeIn create_chunk Silently returns chunk text contents.
#' @family create_chunks
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
#'
#' @example tests/test.create_chunks.R
create_chunk = function(text=paste(readLines(file),collapse="\n"),
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
    if (!dots=="")
      warning("Ignoring extra arguments and using chunk_options_string.")
    dots = chunk_options_string
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
#' @describeIn create_chunk Inserts chunk contents into a Rmarkdown file
#'   at the specified line number.
insert_chunk = function(chunk,input.file,line,output.file=input.file,
                        overwrite = input.file == output.file) {
  tf = tempfile()
  on.exit(file.remove(tf))

  x = readLines(input.file)

  if (line > length(x))
    stop("line argument beyond end of input file.")

  y = c(x[1:(line-1)],chunk,x[line:length(x)])

  writeLines(y,tf)
  invisible(file.copy(tf,output.file,overwrite=overwrite))
}
