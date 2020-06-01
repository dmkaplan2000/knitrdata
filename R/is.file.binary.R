# Function to determine if a file is binary or text -------------------------

#' Determine if a file is binary or text
#'
#' This function attempts to determine if file is binary or text. It does this
#' using a heuristic based on finding at least \code{nbin} ASCII control
#' (i.e., non-printing) characters in the first \code{nchars} of the file.
#' This works well for standard ASCII text, but I have no idea how it will
#' work with complex UTF8 text (e.g., Chinese).
#'
#' @param file The path to the file to be examined
#' @param bin.chars List of control characters that are to be considered when
#'   when looking for signs a file is binary. Default includes most ASCII
#'   control characters except things like NULL, LF, CR and HT that might actually
#'   appear in an ASCII file.
#' @param nchars Number of bytes to read in from the beginning of the file.
#' @param nbin An integer indicating the threshold on the number of control characters
#'   above which a file is considered binary. Defaults to 2.
#'
#' @return A boolean that will be \code{TRUE} if a file is considered to be binary.
#'
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
is.file.binary = function(file,bin.chars=c(1:8,14:25),nchars=1000,nbin=2) {
  x = as.integer(readBin(file,"raw",nchars))
  n = sum(x %in% bin.chars)

  return(n>nbin)
}
