# Functions meant to be used as Rstudio addins ---------------------------------------

# Function for creating a data chunk --------------------
create_data_chunk_dialogue = function (title="Data chunk creator") {
  if (!requireNamespace("shiny",quietly=TRUE) || !requireNamespace("miniUI",quietly=TRUE))
    stop("This function requires that the shiny and miniUI packages be installed. Please install them before running.")

  ui <- miniPage(
    gadgetTitleBar(title,left = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("done", "Create chunk", primary = TRUE)),
    miniContentPanel(
      fileInput("filename","Data file: "),
      textInput("chunk_label","Chunk label: ",placeholder="mydatachunk",width="100%"),
      fillRow(
        height="40pt",
        checkboxGroupInput("format","Format: ",choices=c("text","binary"),selected="binary",inline=TRUE),
        checkboxGroupInput("encoding","Encoding: ",choices=c("asis","base64"),select="base64",inline=TRUE)
      ),
      fillRow(
        height="40pt",
        checkboxInput("md5sum","Do MD5 sum check? ",value=TRUE),
        checkboxInput("encode","Encode the data? ",value=TRUE)
      ),
      textInput("output.var","Output variable name (no quotes): ",placeholder="df",width="100%"),
      textInput("output.file","Output file name (no quotes): ",placeholder="filepath/filename.ext",
                width="100%"),
      textInput("loader.function","Loader function: ",placeholder="read.csv",width="100%"),
      fillRow(
        height="40pt",
        checkboxInput("echo","Echo chunk contents? ",value=FALSE),
        checkboxInput("eval","Evaluate chunk? ",value=TRUE)
      ),
      textInput("chunk_options_string","Additional chunk options (as they would appear in chunk header): ",
                placeholder="loader.ops=list(header=FALSE), max.echo=5",width="100%")
    )
  )

  server <- function(input, output, session) {
    rv = reactiveValues(makechunk=FALSE)

    isemp = function(x) is.null(x) || (x=="")

    # When data file set, determine defaults
    observeEvent(input$filename,{
      fn = input$filename$datapath

      isbin = is.file.binary(fn)

      updateCheckboxGroupInput(session,"format",selected=ifelse(isbin,"binary","text"))
      updateCheckboxGroupInput(session,"encoding",selected=ifelse(isbin,"base64","asis"))
      updateCheckboxInput(session,"encode",value=isbin)
      updateTextInput(session,"loader.function",
                      placeholder=ifelse(isbin,"readRDS","read.csv"))
    })

    # When the "create chunk" button is clicked, return a value
    observeEvent(input$done, {
      makechunk = TRUE

      fn = input$filename$datapath
      if (is.null(fn)) {
        makechunk = FALSE
        showModal(modalDialog("Data file must be specified."))
      }

      if (isemp(input$output.var) && isemp(input$output.file)) {
        makechunk = FALSE
        showModal(modalDialog("At least one of output variable or output file must be given."))
      }

      rv$makechunk = makechunk
    })

    observeEvent(rv$makechunk, {
      if (rv$makechunk) {
        fn = input$filename$datapath

        md5sum = NULL
        if (input$md5sum)
          md5sum = tools::md5sum(fn)
        names(md5sum) = NULL

        # Read and encode data
        if (input$encode && input$encoding != 'asis') {
          chunk = data_encode(fn,encoding = input$encoding)
        } else {
          chunk = readLines(fn)
        }

        chunk = do.call(c,strsplit(chunk,"\n")) # Break into lines

        args = list(text=chunk,chunk_label=input$chunk_label,
                    format=input$format,encoding=input$encoding)

        if (!isemp(input$output.var))
          args$output.var=input$output.var
        if (!isemp(input$output.file))
          args$output.file=input$output.file
        if (!isemp(input$loader.function))
          args$loader.function=input$loader.function
        if (!isemp(input$chunk_options_string))
          args$chunk_options_string=input$chunk_options_string

        args$echo=input$echo
        args$eval=input$eval

        if (!isemp(md5sum))
          args$md5sum = md5sum

        chunk = do.call(create_chunk,args)

        stopApp(chunk)
      }
    })

    # When the cancel button is clicked, return null
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
  }

  chunk = runGadget(app=ui, server=server,
                    viewer = dialogViewer("insert_data_chunk"))

  invisible(chunk)
}

# Insert data chunk ------------------------------
insert_data_chunk_dialogue = function (title="Data chunk inserter") {
  if (!requireNamespace("shiny",quietly=TRUE) || !requireNamespace("miniUI",quietly=TRUE) ||
      !requireNamespace("rstudioapi",quietly=TRUE))
    stop("The rstudioapi function must be installed.")

  context = rstudioapi::getSourceEditorContext()

  chunk = create_data_chunk_dialogue(title=title)

  if (is.null(chunk))
    return(FALSE)

  ln = context$selection[[1]]$range$start["row"]

  txt = insert_chunk(
    chunk = chunk,
    line = ln,
    rmd.text = context$contents
  )


  rstudioapi::setDocumentContents(paste(txt,collapse="\n"),context$id)
  rstudioapi::setCursorPosition(
    rstudioapi::document_position(ln,1),
    context$id)

  return(TRUE)
}

# Remove chunk ------------------------------

# Export chunk contents ---------------------

