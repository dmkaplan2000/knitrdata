# Contains a number of convenience language engines included with knitrdata

# Lists of standard options for data chunks --------------
standard.ops = c("code","eval","echo","include","results",
                 "format","encoding","decoding.ops","external.file","md5sum","output.var",
                 "output.file","loader.function","loader.ops","line.sep","max.echo")

# generic engine for wrapping eng_data -----------------------------------------
eng_generic = function(
  options,
  # Below allows one to set defaults for key parameters #
  format = NULL,
  encoding = NULL,
  loader.function = NULL,
  loader.ops = list(),
  loader.ops.names = NULL) {

  # Set format, encoding and loader function if not already set
  if (is.null(options$format))
    options$format = format
  if (is.null(options$encoding))
    options$encoding = encoding
  if (is.null(options$loader.function))
    options$loader.function = loader.function

  # Add loader.ops if none
  if (is.null(options$loader.ops))
    options$loader.ops = loader.ops

  # Move recognized options for loader.function into loader.ops
  n = names(options)[names(options) %in% setdiff(loader.ops.names,standard.ops)]
  options$loader.ops[n] = options[n]
  options[n] = NULL

  return(eng_data(options))
}

# Extra engines -----------------------------------
table.ops = setdiff(methods::formalArgs(read.table),c("file",standard.ops))
eng_csv = function(options)
  eng_generic(options,format="text",loader.function="read.csv",loader.ops.names=table.ops)
eng_csv2 = function(options)
  eng_generic(options,format="text",loader.function="read.csv2",loader.ops.names=table.ops)
eng_rds = function(options)
  eng_generic(options,format="binary",encoding="base64",loader.function="readRDS",
              loader.ops.names="refhook")
