# Help function to try to break up lines correctly -------------
# This is messy as it requires writing the text to disk and reading it back in,
# but there are so many edge cases for newlines that it is hard to do things correctly otherwise.
# In particular, readLines(textConnection(text)) does not produce the same result as this as it
# adds a newline to each element of text even if text already has one.
break.lines = function(text) {
  tf = tempfile()
  on.exit(file.remove(tf))

  writeChar(text,tf,useBytes=TRUE,eos=NULL)
  return(readLines(tf,warn=FALSE))
}

# Functions for creating a data chunk with appropriate header ------------------

#' Tools for creating (data) chunks and inserting them into Rmarkdown documents
#'
#' These helper functions allow one to add the chunk header and tail to text
#' containing chunk contents and then insert that into a Rmarkdown document.
#'
#' \code{create_chunk} takes in the (possibly encoded by \code{data_encode})
#' contents of a chunk and adds the chunk header and closer, invisibly returning
#' entire chunk contents as a character string.
#'
#' \code{insert_chunk} takes the chunk contents and inserts it at the given line
#' number in the \code{rmd.text} or \code{rmd.file}.
#'
#' Note that the additional arguments to \code{create_chunk} (\dots) are not
#' evaluated, but rather they are placed in the chunk header as they appear in
#' the function call as additional chunk options.
#'
#' @param text Character vector with contents of chunk, one line per element of
#'   vector. If the character vector has just a single element, then an attempt
#'   will be made to split it into lines using \code{readLines}.
#' @param file Path to file containing chunk contents. Ignored if \code{text}
#'   argument supplied. As a consequence, this means that all arguments must be
#'   named if the \code{file} argument is supplied to \code{create_chunk}.
#' @param chunk_label Character string giving the label to be used for the
#'   chunk.
#' @param chunk_type Character string giving the chunk type. Defaults to
#'   \code{"data"}.
#' @param \dots Additional chunk options. These are not evaluated, but rather
#'   included in the function call as they are entered in the function call.
#' @param chunk_options_string Character vector with additional chunk options
#'   that will be included in the header after the arguments in \dots.
#' @param split_lines Boolean indicating whether or not the chunk contents
#'   should be split along line breaks before returning. Defaults to
#'   \code{TRUE}.
#' @param newline Character string used to end lines of text. Only relevant if
#'   \code{split_lines=FALSE}. Defaults to \code{platform.newline()}.
#' @param chunk Character vector with chunk contents including header and tail.
#'   If the character vector has just a single element, then an attempt will be
#'   made to split it into lines using \code{readLines}.
#' @param line Line number where chunk to be inserted.
#' @param rmd.text Character vector with contents of Rmarkdown document where
#'   chunk contents are to be inserted. It should have one element per line of
#'   text (as returned by \code{readLines}).  If the character vector has just a
#'   single element, then an attempt will be made to split it into lines using
#'   \code{readLines}.
#' @param rmd.file Filename of Rmarkdown document where chunk contents are to be
#'   inserted. Ignored if \code{rmd.text} is supplied.
#'
#' @export
#'
#' @describeIn create_chunk Silently returns chunk text contents.
#' @family Chunk tools
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
#'
#' @example tests/test.create_chunks.R
create_chunk = function(text=readLines(file),
                        ...,chunk_label=NULL,
                        chunk_type="data",
                        file=NULL,chunk_options_string=NULL,
                        split_lines=TRUE,
                        newline=platform.newline()) {
  # If single element text vector given, try to split into lines
  if (length(text) == 1)
    text = break.lines(text)

  # Get additional input options as character string
  args = match.call(expand.dots=FALSE)$...
  argvalues = vapply(args, deparse, character(1L))
  if (is.null(names(args))) { # Happens if only unnamed dots arguments
    dots = paste(argvalues,collapse=", ")
  } else { # At least one dots argument has a name
    dots = paste(ifelse(names(args)=="",
                        argvalues,
                        paste(names(args), argvalues, sep = ' = ')),
                 collapse = ', ')
  }

  if (dots == "")
    dots = NULL

  # Make string with all optional arguments to chunk
  ops = paste0(c(chunk_label,dots,chunk_options_string),
               collapse=", ")

  header = paste0("```{",chunk_type,ifelse(ops=="" || is.null(ops),""," "),ops,"}")
  tail = paste0("```")

  chunk = c(header,text,tail)

  if (!split_lines)
    chunk = paste0(chunk,newline,collapse="")

  invisible(chunk)
}

#' @export
#'
#' @describeIn create_chunk Invisibly returns the contents of the modified Rmarkdown text
#'   as a character vector with each line in an element of the vector
#'   including the chunk at the appropriate line number.
insert_chunk = function(chunk,line,rmd.text=readLines(rmd.file),rmd.file=NULL) {
  # If single element text vector given, try to split into lines
  if (length(chunk) == 1)
    chunk = break.lines(chunk)
  if (length(rmd.text) == 1)
    rmd.text = break.lines(rmd.text)

  if (line > length(rmd.text))
    stop("line argument beyond end of input file.")

  y = c(rmd.text[1:(line-1)],chunk,rmd.text[line:length(rmd.text)])

  invisible(y)
}

# Functions for identifying and removing chunks --------------------------------------

#' Tools for working with existing chunks in Rmarkdown documents
#'
#' These helper functions allow one to identify all the chunks in a Rmarkdown
#' document, split the document into pieces by a specific chunk so that one can
#' either work with the chunk contents or remove the chunk, and remove several
#' chunks at once.
#'
#' \code{list_rmd_chunks} takes a Rmarkdown document and returns a
#' \code{data.frame} listing the essential information of every chunk, including
#' chunk type (language engine), label and start and end line numbers.
#'
#' \code{split_rmd_by_chunk} takes a Rmarkdown document and a chunk label or
#' number and returns the Rmarkdown document split into 4 pieces: the part
#' before the chunk, the chunk header, the chunk contents, the chunk tail and
#' the part after the chunk. These can then be used to either work with the
#' chunk contents or remove the chunk from the Rmarkdown document.
#'
#' \code{remove_chunks} removes several chunks, designated by their text or
#' numeric labels, all at once from a Rmarkdown document.
#'
#' Note that the regular expression used by default to identify chunk starts is
#' not guaranteed to be exactly the same as that used by \code{knitr} and may
#' not work if the Rmarkdown document has unusual chunks. In particular, each
#' chunk must have the chunk type and chunk options enclosed in curly braces. If
#' code chunks exist without curly braces, then these will generally be ignored,
#' but they could potentially cause problems in unusual cases.
#'
#' @param text Character vector with contents of chunk, one element per line of
#'   text. If the character vector has just a single element, then an attempt
#'   will be made to split it into lines using \code{readLines}.
#' @param file Path to file containing chunk contents. Ignored if \code{text}
#'   argument supplied. As a consequence, this means that all arguments must be
#'   named if the \code{file} argument is supplied.
#' @param chunk.start.pattern Regular expression used to identify chunk starts.
#'   The default looks for lines beginning with three back quotes, followed by
#'   curly braces with some sort of text between them and then only spaces till
#'   the end of the line. This should generally work, but if the Rmarkdown
#'   document has chunks that have unusual headers, then this argument can be
#'   useful. In particular, if the document has chunks that begin without curly
#'   braces, these will not be recognized.
#' @param chunk.end.pattern Regular expression used to identify the chunk end.
#'   Default should generally work.
#'
#' @export
#'
#' @describeIn list_rmd_chunks Returns a data frame with 4 columns: the chunk
#'   type, the chunk label, the line number of the beginning of the chunk and
#'   the line number of the end of the chunk.
#' @family Chunk tools
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
#'
#' @example tests/test.create_chunks.R
list_rmd_chunks = function(text=readLines(file),file=NULL,
                           chunk.start.pattern="^```[{](.+)[}] *$",
                           chunk.end.pattern="^``` *$") {
  # If single element text vector given, try to split into lines
  if (length(text) == 1)
    text = break.lines(text)

  starts = grep(chunk.start.pattern,text)

  if (length(starts) < 1) {
    warning("No chunks found.")
    return(NULL)
  }

  # more robust search for endings that should work even if some
  # code chunks lack curly braces
  ends = c()
  for (k in 1:length(starts)) {
    st = starts[k]+1
    en = ifelse(k == length(starts),length(text),starts[k+1]-1)

    ends[k] = st - 1 + grep(chunk.end.pattern,text[st:en])[1]
  }

  if (any(is.na(ends)))
    stop("There seems to be a mismatch between chunk starts and chunk end.")

  x = text[starts]
  h = sub(chunk.start.pattern,"\\1",x)

  chunk_type = sapply(strsplit(h," "),function(x) x[1])

  h = trimws(sub("[^ ]+","",h)) # Remove chunk type

  chunk_label = sapply(strsplit(h,","),function(x) x[1])
  chunk_label = ifelse(grepl("=",chunk_label),"",chunk_label)
  chunk_label = trimws(chunk_label,"both")

  chunk_label[is.na(chunk_label)] = ""

  res = data.frame(type=chunk_type,label=chunk_label,
                   start=starts,end=ends)
  return(res)
}

#' @param chunk_label Character string giving the chunk label or the chunk number
#'   (as returned by \code{list_rmd_chunks}.
#' @param \dots Additional arguments to be passed to \code{list_rmd_chunks}
#'   (e.g., \code{chunk.start.pattern}).
#'
#' @export
#'
#' @describeIn list_rmd_chunks Returns a list with the contents of the Rmarkdown
#'   document broken into 4 pieces: pre-chunk, chunk header, chunk contents,
#'   chunk tail, and post-chunk.
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
split_rmd_by_chunk = function(text=readLines(file),chunk_label,file=NULL,...) {
  # If single element text vector given, try to split into lines
  if (length(text) == 1)
    text = break.lines(text)

  ch = list_rmd_chunks(text=text,...)

  if (is.character(chunk_label))
    chunk_label = which(ch$label==chunk_label)

  x = ch[chunk_label,]

  pre_chunk = text[1:(x$start-1)]
  header = text[x$start]
  tail = text[x$end]

  if (x$end-x$start<=1)
    content=NULL
  else
    content = text[(x$start+1):(x$end-1)]

  post_chunk = NULL
  if (x$end < length(text))
    post_chunk = text[(x$end+1):length(text)]

  return(list(pre_chunk=pre_chunk,header=header,content=content,
              tail=tail,post_chunk=post_chunk))
}

#' @param chunk_labels A vector of numeric or character chunk labels
#'   (as returned by \code{list_rmd_chunks}.
#' @param output.file Name of a file where Rmd document with desired chunks removed is to be saved.
#'
#' @export
#'
#' @describeIn list_rmd_chunks Silently returns a character vector with the contents of
#'   the Rmd file after having removed the desired chunks
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
remove_chunks = function(text=readLines(file),chunk_labels,file=NULL,output.file=NULL,...) {
  # If single element text vector given, try to split into lines
  if (length(text) == 1)
    text = break.lines(text)

  ch = list_rmd_chunks(text=text,...)

  # Convert character chunk labels into numbers
  # Warn if any unidentified chunks
  if (is.character(chunk_labels)) {
    cl = chunk_labels
    chunk_labels = which(ch$label %in% chunk_labels)
    sd = setdiff(cl,ch$label[chunk_labels])
    if (!length(sd)==0)
      warning("The following chunk labels were not found:",paste0('"',sd,'"',collapse=", "))
  }

  # Return immediately if no chunk labels around
  if (length(chunk_labels)==0)
    return(invisible(text))

  # Error if chunk numeric identifiers beyond end of file
  if (max(chunk_labels) > nrow(ch))
    stop("Chunk numeric identifier exceeds number of chunks in Rmarkdown document.")

  # Get full list of line numbers to remove
  se = as.data.frame(t(ch[chunk_labels,c("start","end"),drop=FALSE]))
  lns = do.call(c,lapply(se,function(x) x[1]:x[2]))

  # Remove rows
  text = text[-1*lns]

  # Write to output if needed
  if (!is.null(output.file))
    writeLines(text=text,con=output.file)

  return(invisible(text))
}
