# Function to determine if a file is binary or text -------------------------

#' Functions to assess if files are binary, DOS text or UNIX text format
#'
#' These functions attempt to determine if a file is binary or text. In
#' addition, \code{file.type} attempts to determine the newline character(s)
#' used in the file.
#'
#' A file is assessed to be binary using a heuristic based on finding more than
#' \code{nbin} ASCII control (i.e., non-printing) characters in the first
#' \code{nbytes} of the file. This works well for standard ASCII text, but it
#' may be less effective for complex UTF8 text (e.g., Chinese).
#'
#' For text files, line endings are assessed by \code{file.type} by searching
#' first for DOS line endings (\code{\\r\\n}) in the first \code{nbytes} of the
#' input file, and then by searching for UNIX line endings (\code{\\n}). If
#' neither is found, then \code{NA_character_} is returned for the line ending.
#'
#' @param file The path to the file to be examined
#' @param bin.ints List of integers with the ASCII values of control characters
#'   that are to be considered when when looking for signs a file is binary.
#'   Default includes most ASCII control characters except things like NULL, LF,
#'   CR and HT that might actually appear in an ASCII file.
#' @param nbytes Number of bytes to read in from the beginning of the file.
#' @param nbin An integer indicating the threshold on the number of control
#'   characters above which a file is considered binary. Defaults to 2.
#'
#' @return For \code{is.file.binary}, a boolean value is returned, whereas a
#'   list is returned for \code{file.type}.
#'
#' @describeIn is.file.binary A boolean that will be \code{TRUE} if a file is considered to be
#'   binary.
#' @family binary text tests
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
#' @seealso See also \code{\link{platform.newline}}.
#' @export
is.file.binary = function(file,bin.ints=c(1:8,14:25),nbytes=1000,nbin=2) {
  x = as.integer(readBin(file,"raw",nbytes))
  n = sum(x %in% bin.ints)

  return(n>nbin)
}

#' @export
#'
#' @describeIn is.file.binary Returns a list with up to two elements:
#'   \code{type} & \code{newline}. \code{type} can either by \code{"binary"} or
#'   \code{"text"}. \code{newline} will be \code{NULL} for binary files,
#'   \code{"\\r\\n"} for DOS formatted text files, \code{"\\n"} for UNIX
#'   formatted text files and \code{NA_character_} for text files without any
#'   newline characters in the first \code{nbytes} of the file.
file.type = function(file,bin.ints=c(1:8,14:25),nbytes=1000,nbin=2) {
  x = readBin(file,"raw",nbytes)
  n = sum(as.integer(x) %in% bin.ints)

  r = list()
  r$type=ifelse(n>nbin,"binary","text")

  if (r$type=="text") {
    x = rawToChar(x)

    if (grepl("\r\n",x)) {
      r$newline = "\r\n"
    } else {
      if (grepl("\n",x)) {
        r$newline = "\n"
      } else {
        r$newline = NA_character_
      }
    }
  }

  return(r)
}

# Helper function for handling text file newline mess --------

#' Platform independent newline string
#'
#' A simple function to determine the appropriate newline string for a given
#' operating system.
#'
#' @param os Name of the operating system. Defaults to \code{.Platform$OS.type}
#'
#' @return For Windows, this should return \code{'\r\n'}, whereas for
#' other operating system it will return \code{'\n'}.
#'
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
#' @seealso See also \code{\link{file.type}}.
#' @export
platform.newline = function(os=.Platform$OS.type) ifelse(grepl("windows",tolower(os)),"\r\n","\n")

# Help function to try to get gpg key password into keyring memory -------

#' Functions that attempts to unlock a gpg key for decryption
#'
#' This function will attempt to unlock a specific GPG key by first encrypting a
#' small amount of information using the (public) key and then immediately
#' decrypting it using the (private) key, thereby causing the keyring to
#' temporarily store the key passphrase. This can be helpful if one is trying
#' to knit a document with encrypted data chunks, but the key for those
#' data chunks is locked with a passphrase. See the package vignette section
#' \code{Workarounds for GPG data chunk error: Password callback did not return a string value}
#' for more details.
#'
#' @param id Identifier of the GPG key
#' @param name Name associated with the desired GPG key
#' @param email Email associated with the desired GPG key
#'
#' @return Will return the identifier of the GPG key that was unlocked.
#'
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
#' @seealso See also \code{\link{data_encode}}, \code{\link[gpg]{gpg_encrypt}}.
#' @export
unlock_gpg_key_passphrase = function(
  id=ifelse(is.null(name),
            ifelse(is.null(email),NULL,
                   gpg::gpg_list_keys()$id[gpg::gpg_list_keys()$email==email]),
            gpg::gpg_list_keys()$id[gpg::gpg_list_keys()$name==name]),
  name=NULL,email=NULL) {
  if (is.null(id) || length(id)>1)
    stop("No id or multiple ids given.")

  tf = tempfile()
  on.exit(file.remove(tf))
  cat("hi",file=tf)
  data_decode(
    data_encode(tf,"gpg",options=list(receiver=id)),
    "gpg"
  )

  return(id)
}
