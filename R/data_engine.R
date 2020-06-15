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
#' \code{\link[base64enc:base64]{base64enc}} package, whereas encoding and decoding using \code{gpg}
#' uses functionality from the \code{\link[gpg:gpg_encrypt]{gpg}} package. See those packages for more
#' details on the encoding and decoding process and setting up a \code{gpg} keyring.
#'
#' \code{data_encode} takes the name of a file containing the binary or text data
#' to be encoded and returns the encoded data as a character string.
#' The encoded data is returned silently to avoid outputing to the screen large
#' amounts of encoded data. If you want to visualize the encoded data, use the
#' \code{cat} function.
#' For larger data files, set the \code{output} argument to a path where the
#' encoded data will be stored.
#'
#' \code{data_encode} takes a character string of encoded data and returns either
#' a character string of decoded data (if \code{as_text=TRUE}) or a raw vector
#' of decoded binary data (if \code{as_text=FALSE}).
#'
#' For both functions, the \code{options} input argument takes a list of
#' additional input arguments that are passed directly to the encoding
#' or decoding functions in the respective packages that handle the actual
#' data translation. See \code{\link[base64enc:base64]{base64encode}} and
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
#' @param output Path where encoded data is to be stored. Optional; if \code{NULL} then
#'   encoded data will not be written to a file.
#' @param options A list containing extra argument for the encoding/decoding functions.
#'   See \code{\link[base64enc:base64]{base64encode}} and \code{\link[gpg]{gpg_encrypt}}
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
#' @seealso See also \code{\link[base64enc:base64]{base64encode}} and \code{\link[gpg]{gpg_encrypt}}.
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
      return(x)
    },
    gpg = {
      if (!requireNamespace("gpg"))
        stop("gpg package must be installed and configured for encryption/decryption to work.")

      tf = tempfile()
      writeLines(data,tf)
      on.exit(file.remove(tf))
      return(do.call(gpg::gpg_decrypt,c(data=tf,as_text=as_text,options)))
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
    base64 = {
      if (is.null(options$linewidth))
        options$linewidth = 64
      if (is.null(options$newline))
        options$newline = "\n"

      do.call(base64enc::base64encode,
              c(what=file,options))
    },
    gpg = {
      if (!requireNamespace("gpg"))
        stop("gpg package must be installed and configured for encryption/decryption to work.")

      if (is.null(options$receiver))
        stop("Missing GPG receiver in options list. See ?gpg::gpg_encrypt for details.")
      do.call(gpg::gpg_encrypt,c(data=file,options))
    },
    stop("Uknown encoding: ",encoding)
  )

  if(!is.null(output)) {
    writeLines(data,output)
  }

  invisible(data)
}

# Data engine itself -------------------------------------------------

eng_data = function(options) {
  output = ''
  code = options$code

  # Do nothing if told not to evaluate
  if (!options$eval)
    return(knitr::engine_output(options,code,output))

  if (is.null(options$output.var) && is.null(options$output.file))
    stop("One of output.var or output.file must be supplied in data chunk options.")

  # Option to include external file
  # Useful to keep initial file size small and readable.
  if (!is.null(options$external.file)) {
    if (!is.null(code))
      warning("Non-empty data chunk, but given external.file chunk option. Using external file and ignoring data chunk contents.")

    code = readLines(options$external.file)
  }

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
    data = paste(code,collapse=ifelse(is.null(options$line.sep),"\n",options$line.sep))
  } else {
    data = data_decode(code,encoding,as_text=(format=="text"),options=decoding.ops)
  }

  output.file = options$output.file

  # Create temp file if using loader function
  if (is.null(output.file) &&
      (!is.null(options$loader.function) || !is.null(options$md5sum))) {
    output.file = tempfile()
    on.exit(file.remove(output.file))
  }

  # Save decoded data to file if desired
  if (!is.null(output.file))
    switch(format,
           text = writeLines(data,output.file),
           binary = writeBin(data,output.file)
    )

  # Check md5sum if desired
  if (!is.null(options$md5sum) && options$md5sum != tools::md5sum(output.file))
    stop("Given md5sum does not match contents of decoded chunk.")

  # Apply loader function to data if desired
  if (!is.null(options$loader.function)) {
    loader.ops = options$loader.ops
    if (is.null(loader.ops))
      loader.ops = list()
    if (!is.list(loader.ops))
      stop("loader.ops should be a list. Got object of class ",
           class(loader.ops)[1])

    data = do.call(options$loader.function,
                   c(output.file,loader.ops))
  }

  # Assign to output.var
  if (!is.null(options$output.var)) {
    assign(options$output.var, data, envir = .knitrdata_env())
    #assign(options$output.var, data, envir = knitr::knit_global())
    #knitr::assign_knit_global(options$output.var,data) # Solution to avoid CRAN filters that needs to be implemented in knitr
  }

  # Reduce echo of long data
  if (is.null(options$max.echo))
    options$max.echo=20

  if (length(code)>options$max.echo)
    code = c(code[1:options$max.echo],
             paste0("-- ",length(code)-options$max.echo," more lines of data ommitted --"))

  knitr::engine_output(options,code,output)
}
