# Data language engine for knitr --------------------------------------

# Helper functions to decode & encode data ----------------------------

# If as_text=TRUE, then returns a character string
# If as_text=FALSE, then returns a raw vector


#' Decode and encode text and binary data files
#'
#' These helper functions allow one to encode as text a binary or text data file
#' using either \code{base64} or \code{gpg} encoding (\code{data_encode}) and decode
#' text-encoded data back into its original binary or text format (\code{data_decode}).
#'
#' Encoding and decoding in \code{base64} format uses functionality from the
#' \code{\link[base64enc:base64encode]{base64enc}} package, whereas encoding and decoding using \code{gpg}
#' uses functionality from the \code{\link[gpg:gpg_encrypt]{gpg}} package. See those packages for more
#' details on the encoding and decoding process and setting up a \code{gpg} keyring.
#'
#' \code{data_encode} takes the name of a file containing the binary or text data
#' to be encoded and (silently) returns the encoded data as a character string. If
#' the \code{output} input argument is \code{NULL}, then the encoded data is
#' returned on the command line using \code{cat} so that it can be copied and
#' pasted into an Rmarkdown \code{data} chunk. This is only practical for small data
#' files. For larger data files, set the \code{output} argument to a path where the
#' encoded data will be stored.
#'
#' \code{data_encode} takes a character string of encoded data and returns either
#' a character string of decoded data (if \code{as_text=TRUE}) or a raw vector
#' of decoded binary data (if \code{as_text=FALSE}).
#'
#' For both functions, the \code{options} input argument takes a list of
#' additional input arguments that are passed directly to the encoding
#' or decoding functions in the respective packages that handle the actual
#' data translation. See \code{\link[base64enc]{base64encode}} and
#' \code{\link[gpg]{gpg_encrypt}} for details.
#'
#' For \code{gpg} encoding and decoding, in addition to installing the
#' \code{\link[gpg:gpg_encrypt]{gpg}} package, a \code{gpg} keyring must
#' be installed and properly configured. For encoding,
#' the \code{receiver} and potentially
#' the \code{signer} arguments must be supplied as elements of the \code{options}
#' input argument.
#'
#' @param data Encoded data as a character string
#' @param file Path to file containing data to be encoded
#' @param encoding Either \code{'base64'} or \code{'gpg'}
#' @param as_text A boolean indicating if decoded data should be treated as text
#'   (\code{TRUE}) or binary (\code{FALSE}). Defaults to \code{FALSE}, meaning binary.
#' @param output Path where encoded data is to be stored. Option, if \code{NULL} then
#'   encoded data will not be written to a file.
#' @param options A list containing extra argument for the encoding/decoding functions.
#'   See \code{\link[base64enc]{base64encode}} and \code{\link[gpg]{gpg_encrypt}}
#'   for details and the description below for required options for \code{gpg}
#'   encryption.
#'
#' @return Returns either the decoded data (\code{data_decode}) or the encoded data (\code{data_encode}).
#' @export
#'
#' @describeIn data_decode Returns decoded data as either a character
#'   string (\code{as_text=TRUE}) or raw vector (\code{as_text=FALSE}).
#' @family decode encode
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
#'
#' @example tests/test.data_encode_decode.R
data_decode = function(data,encoding,as_text=FALSE,options=list()) {
  if (!is.list(options))
    stop("options must be a list.")

  switch(
    encoding,
    base64 = {
      x = base64enc::base64decode(data)
      if (as_text)
        x = rawToChar(x)
      x
    },
    gpg = {
      if (!requireNamespace("gpg"))
        stop("gpg package must be installed and configured for encryption/decryption to work.")

      tf = tempfile()
      writeLines(data,tf)
      on.exit(file.remove(tf))
      do.call(gpg::gpg_decrypt,c(data=tf,as_text=as_text,options))
    },
    stop("Uknown encoding: ",encoding)
  )
}

#' @export
#'
#' @describeIn data_decode Returns data encoded as a character string using
#'   \code{base64} or \code{gpg} encoding.
data_encode = function(file,encoding,options=list(),output=NULL) {
  if (!is.list(options))
    stop("options must be a list.")

  data = switch(
    encoding,
    base64 =
      do.call(base64enc::base64encode,
              c(what=file,options,linewidth=64,newline="\n")),
    gpg = {
      if (!requireNamespace("gpg"))
        stop("gpg package must be installed and configured for encryption/decryption to work.")

      if (is.null(options$receiver))
        stop("Missing GPG receiver in options list. See ?gpg::gpg_encrypt for details.")
      do.call(gpg::gpg_encrypt,c(data=file,options))
    },
    stop("Uknown encoding: ",encoding)
  )

  if(is.null(output)) {
    cat(data)
  } else {
    writeLines(data,output)
  }

  invisible(data)
}

# Data engine itself -------------------------------------------------

eng_data = function(options) {
  output = ''

  if (is.null(options$output.var) && is.null(options$output.file))
    stop("One of output.var or output.file must be supplied in data chunk options.")

  code = options$code

  # Option to include external file
  # Useful to keep initial file size small and readable.
  if (!is.null(options$external.file)) {
    if (!is.null(code))
      warning("Non-empty data chunk, but given external.file chunk option. Using external file and ignoring data chunk contents.")

    code = readLines(options$external.file)
  }

  # Do nothing if told not to evaluate
  if (!options$eval)
    return(knitr::engine_output(options,code,output))

  format = options$format
  if (is.null(format))
    format = 'text'
  if (!is.character(format) || !(format %in% c("text","binary")))
    stop("format must be either 'text' or 'binary'.")

  encoding = options$encoding
  if (is.null(encoding)) {
    encoding = switch(
      format,
      text = 'asis',
      binary = 'base64'
    )
  }
  if (!is.character(encoding) || !(encoding %in% c("asis","base64","gpg")))
    stop("encoding must be one of: 'asis', 'base64', 'gpg'.")

  decoding.ops = options$decoding.ops
  if (is.null(decoding.ops))
    decoding.ops = list()
  if (!is.list(decoding.ops))
    stop("decoding.ops should be a list. Got object of class ",class(decoding.ops)[1])

  if (encoding == "asis") {
    data = paste(code,collapse=ifelse(is.null(options$newline),"\n",options$newline))
  } else {
    data = data_decode(code,encoding,as_text=(format=="text"),options=decoding.ops)
  }

  # Assign to output.var and/or write to file output.file
  if (!is.null(options$output.var))
    assign(options$output.var, data, envir = knitr::knit_global())
  if (!is.null(options$output.file))
    switch(format,
           text = writeLines(data,options$output.file),
           binary = writeBin(data,options$output.file)
    )

  knitr::engine_output(options,code,output)
}

# Loader/Unloader functions ----------------------------------------------

# Activate data language engine for use in Rmarkdown documents
.onLoad = function(libname,pkgname) {
  knitr::knit_engines$set(data=eng_data)
}

# Remove data language engine on package unload
.onUnload = function(libname,pkgname) {
  knitr::knit_engines$delete("data")
}
