# Functions meant to be used as Rstudio addins ---------------------------------------

# Some minor helper functions
isemp = function(x) is.null(x) || (x=="")
firstword = function(x,split="( |\t|\r?\n)") sapply(strsplit(x,split=split),function(y) y[1])

# Function for creating a data chunk --------------------

#' Invoke Shiny gadget to create a data chunk
#'
#' Opens a Shiny dialog allowing the user to specify the content and chunk
#' options of a data chunk. As different dialog elements are specified, other
#' elements will be modified to suggest likely desired values. For example, if a
#' binary file is uploaded, then the \code{format} will be set to \code{binary},
#' the \code{encoding} will be set to \code{base64} and the \code{Encode data?}
#' option will be checked. If these options are not appropriate, then they can
#' be altered afterwards.
#'
#' When the \code{Create chunk} button is clicked, the function will return the
#' chunk contents including header and tail. If \code{insert.editor.doc==TRUE},
#' then the resulting chunk will be inserted into the active document in the
#' RStudio editor at the cursor position.
#'
#' \code{insert_data_chunk_dialog} is a wrapper for
#' \code{create_data_chunk_dialog(insert.editor.doc=TRUE)}.
#'
#' @param insert.editor.doc Whether or not to insert output chunk into active
#'   RStudio editor document at cursor position. Defaults to \code{FALSE}.
#'
#' @describeIn create_data_chunk_dialog Invisibly returns the text of the data
#'   chunk as a character vector, one line of text per element.
#'
#' @examples
#' \dontrun{
#' create_data_chunk_dialog()
#' }
#'
#' @export
#'
#' @family Chunk tools
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
#' @encoding UTF-8
create_data_chunk_dialog = function (insert.editor.doc=FALSE) {
  # Default titles
  title="Data chunk creator"
  infobar="<big><b>Fill out, then click above to create chunk</b></big>"

  # Check for needed packages
  pkgs = c("shiny","miniUI")
  if (!all(sapply(pkgs,requireNamespace,quietly=TRUE)))
    stop("This function requires that the following packages be installed: ",
         paste(pkgs,collapse=", "))

  # Prepare if we are trying to insert chunk in RStudio
  if (insert.editor.doc) {
    # Check for rstudioapi and rstudio
    if (!requireNamespace("rstudioapi",quietly=TRUE))
      stop("The rstudioapi package must be installed to use this function.")

    if (!rstudioapi::isAvailable())
      stop("RStudio does not appear to be running.")

    # Active document stuff
    context = rstudioapi::getSourceEditorContext()
    editor.ln = context$selection[[1]]$range$start["row"]

    # Title
    title="Data chunk inserter"

    # Infobar contents
    infobar = paste0(
      "<big><b>",
      "Active document: ",ifelse(isemp(context$path),"<i>UNKNOWN</i>",context$path),
      "<br/>",
      "Line number: ",editor.ln,
      "</b></big>")
  }

  # User interface for dialog
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title,left = miniUI::miniTitleBarCancelButton(),
                   right = miniUI::miniTitleBarButton("done", "Create chunk", primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::uiOutput('infobar'),
      shiny::hr(),
      shiny::fileInput("filename","Data file: "),
      shiny::textInput("chunk_label","Chunk label: ",placeholder="mydatachunk",width="100%"),
      shiny::fillRow(
        height="40pt",
        shiny::radioButtons("format","Format: ",choices=c("text","binary"),selected="binary",inline=TRUE),
        shiny::radioButtons("encoding","Encoding: ",choices=c("asis","base64","gpg"),select="base64",inline=TRUE)
      ),
      shiny::fillRow(
        height="40pt",
        shiny::checkboxInput("md5sum","Do MD5 sum check? ",value=TRUE),
        shiny::checkboxInput("encode","Encode the data? ",value=TRUE)
      ),
      shiny::uiOutput("line_sep"),
      shiny::uiOutput("gpg_receivers"),
      shiny::textInput("output.var","Output variable name (no quotes): ",placeholder="df",width="100%"),
      shiny::textInput("output.file","Output file name (no quotes): ",placeholder="filepath/filename.ext",
                width="100%"),
      shiny::textInput("loader.function","Loader function: ",placeholder="read.csv",width="100%"),
      shiny::textInput("eval","Evaluate chunk? ",value="TRUE",width="100%"),
      shiny::checkboxInput("echo","Echo chunk contents? ",value=FALSE),
      shiny::textInput("chunk_options_string","Additional chunk options (as they would appear in chunk header): ",
                placeholder="loader.ops=list(header=FALSE), max.echo=5",width="100%")
    )
  )

  server <- function(input, output, session) {
    rv = shiny::reactiveValues(makechunk=FALSE)

    output$infobar = shiny::renderUI(shiny::HTML(infobar))

    # Add receivers list if GPG chosen
    shiny::observeEvent(list(input$encoding,input$encode),{
      if (input$encode && input$encoding=="gpg") {
        # If gpg not installed warned and return to encoding=base64
        if (!requireNamespace("gpg")) {
          shiny::showModal(shiny::modalDialog("gpg package required for gpg encoding."))
          shiny::updateRadioButtons(session,"encoding",selected="base64")
          return()
        }

        keys = gpg::gpg_list_keys()
        output$gpg_receivers = shiny::renderUI(
          shiny::selectInput("keys","GPG receivers: ",paste(keys$id,keys$name),multiple=TRUE)
        )
      } else {
        output$gpg_receivers = shiny::renderUI("")
      }
    })

    # If text file, add newline separator to dialog
    shiny::observeEvent(list(input$filename,input$encoding,input$format),{
      if (input$format=="text" && input$encoding=="asis") {
        line.sep = platform.newline()
        fn = input$filename$datapath
        if (!is.null(fn)) {
          ft = file.type(fn)
          if (ft$type == "text" && !is.na(ft$newline)) {
            line.sep = ft$newline
          }
        }

        line.sep = c(`\r\n`="\\r\\n",`\n`="\\n")[line.sep]
        output$line_sep = shiny::renderUI(
          shiny::textInput("line.sep","Newline character(s): ",value=line.sep,width="100%")
        )
      } else {
        output$line_sep = shiny::renderUI("")
      }
    })

    # When data file set, determine defaults
    shiny::observeEvent(input$filename,{
      fn = input$filename$datapath

      isbin = file.type(fn)$type == "binary"
      fnext = tolower(tools::file_ext(fn))

      shiny::updateRadioButtons(session,"format",selected=ifelse(isbin,"binary","text"))
      shiny::updateRadioButtons(session,"encoding",selected=ifelse(isbin,"base64","asis"))
      #shiny::updateCheckboxInput(session,"encode",value=isbin)
      shiny::updateCheckboxInput(session,"md5sum",value=isbin) # To avoid issues with files not ending in newline
      shiny::updateTextInput(session,"loader.function",
                      placeholder=ifelse(isbin,"readRDS","read.csv"))

      # Try to guess loader.function
      lfs = c(csv="read.csv",rds="readRDS")
      if (fnext %in% names(lfs))
        shiny::updateTextInput(session,"loader.function",
                               value=as.character(lfs[fnext]))
    })

    shiny::observeEvent(input$output.file,{
      if (!isemp(input$output.file)) {
        shiny::updateTextInput(
          session,"eval",
          value=paste0("!file.exists(\"",as.character(input$output.file),"\")")
        )
      }
    })

    # When the "create chunk" button is clicked, return a value
    shiny::observeEvent(input$done, {
      makechunk = TRUE

      fn = input$filename$datapath
      if (is.null(fn)) {
        makechunk = FALSE
        shiny::showModal(shiny::modalDialog("Data file must be specified."))
      }

      if (isemp(input$output.var) && isemp(input$output.file)) {
        makechunk = FALSE
        shiny::showModal(shiny::modalDialog("At least one of output variable or output file must be given."))
      }

      if (isemp(input$output.var) && !isemp(input$loader.function)) {
        makechunk = FALSE
        shiny::showModal(shiny::modalDialog("Loader function only has value if output variable name specified."))
      }

      if (input$encoding=="gpg" && isemp(input$keys)) {
        makechunk = FALSE
        shiny::showModal(shiny::modalDialog("One or more GPG receivers must be specified for GPG encoding."))
      }

      rv$makechunk = makechunk
    })

    shiny::observeEvent(rv$makechunk, {
      if (rv$makechunk) {
        fn = input$filename$datapath

        md5sum = NULL
        if (input$md5sum)
          md5sum = tools::md5sum(fn)
        names(md5sum) = NULL

        # Read and encode data
        if (input$encode && input$encoding != 'asis') {
          ops = list()
          if (input$encoding == "gpg")
            ops = list(receiver=firstword(input$keys))

          chunk = data_encode(fn,encoding = input$encoding,options=ops)
        } else {
          chunk = readLines(fn)
        }

        chunk_label = NULL
        if (!isemp(input$chunk_label))
          chunk_label = input$chunk_label

        args = list(text=chunk,
                    chunk_label=chunk_label,
                    format=input$format,encoding=input$encoding)

        if (!isemp(input$output.var))
          args$output.var=input$output.var
        if (!isemp(input$output.file))
          args$output.file=input$output.file
        args$echo=input$echo

        if (!isemp(md5sum))
          args$md5sum = md5sum

        # Things going in extra args
        ev = NULL
        if (!isemp(input$eval))
          ev=paste0("eval=",input$eval)

        nl = NULL
        if (!isemp(input$line.sep))
          nl=paste0('line.sep="',input$line.sep,'"')

        lf = NULL
        if (!isemp(input$loader.function))
          lf=paste0("loader.function=",input$loader.function)

        co = NULL
        if (!isemp(input$chunk_options_string))
          co = input$chunk_options_string

        co=paste(c(ev,nl,lf,co),collapse=",")

        if (!isemp(co))
          args$chunk_options_string = co

        chunk = do.call(create_chunk,args)

        if (insert.editor.doc) {
          # Insert text
          rstudioapi::insertText(location=c(editor.ln,1),
                                 text = paste0(chunk,platform.newline(),collapse=""),
                                 id = context$id)

          # Set position
          rstudioapi::setCursorPosition(position=c(editor.ln,1),id=context$id)
        }

        shiny::stopApp(chunk)
      }
    })

    # When the cancel button is clicked, return null
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  chunk = shiny::runGadget(
    app=ui, server=server,
    viewer = shiny::dialogViewer(title,width=600,height=900))

  return(invisible(chunk))
}

# Insert data chunk ------------------------------

#' @describeIn create_data_chunk_dialog Invisibly returns the text of the data
#'   chunk as a character vector, one line of text per element, if a chunk was
#'   inserted. Returns \code{NULL} otherwise.
#'
#' @examples
#' \dontrun{
#' insert_data_chunk_dialog()
#' }
#'
#' @export
#'
#' @encoding UTF-8
insert_data_chunk_dialog = function (insert.editor.doc=TRUE)
  create_data_chunk_dialog(insert.editor.doc=insert.editor.doc)

# Empty data chunk template ----------------------------

#' Insert an empty data chunk template in active source document
#'
#' This function is essentially the equivalent for data chunks of the "Insert a
#' new code chunk" menu item available in Rstudio when a Rmarkdown document is
#' open. It places at the current cursor location an empty \code{data} chunk
#' that can then be modified and filled in by hand.
#'
#' @return Invisibly returns the chunk contents as a character vector, one line
#'   of text per element.
#'
#' @examples
#' \dontrun{
#' insert_data_chunk_template()
#' }
#'
#' @export
#'
#' @family Chunk tools
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
#' @encoding UTF-8
insert_data_chunk_template = function() {
  chunk = create_chunk(
    paste(
      sep=platform.newline(),
      "# Instructions:",
      "# 1) Fill in at least one of these chunk options: output.var & output.file",
      "# 2) Add or modify other chunk options",
      "# 3) Delete any unused chunk options (e.g., output.file)",
      "# 4) Delete these instructions and replace with data"
    ),
    format="text",encoding="asis",output.var=,output.file=,loader.function=NULL,echo=TRUE)

  # Check for rstudioapi and rstudio
  if (!requireNamespace("rstudioapi",quietly=TRUE))
    stop("The rstudioapi package must be installed to use this function.")

  if (!rstudioapi::isAvailable())
    stop("RStudio does not appear to be running.")

  # Active document stuff
  context = rstudioapi::getSourceEditorContext()
  editor.ln = context$selection[[1]]$range$start["row"]

  # Insert text
  rstudioapi::insertText(location=c(editor.ln,1),
                         text = paste0(chunk,platform.newline(),collapse=""),
                         id = context$id)

  # Set position
  rstudioapi::setCursorPosition(position=c(editor.ln,1),id=context$id)

  return(invisible(chunk))
}

# Remove chunks ------------------------------

#' Invoke Rstudio addin to remove chunks from the active source document
#'
#' The dialog will present a data table list of chunks in the source document. Select the rows
#' that correspond to the chunks that you wish to remove and hit the \code{Remove chunks} button
#' to remove them.
#'
#' When the dialog is started, if the cursor is positioned inside a chunk in the source document,
#' then the row corresponding to this chunk will be selected by default.
#'
#' @return Returns \code{TRUE} if one or more chunks were removed, \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' remove_chunks_dialog()
#' }
#'
#' @export
#'
#' @family Chunk tools
#' @author David M. Kaplan \email{dmkaplan2000@@gmail.com}
#' @encoding UTF-8
remove_chunks_dialog = function () {
  title="Eliminate (data) chunks"

  # Check for needed packages
  pkgs = c("shiny","miniUI","DT","rstudioapi")
  if (!all(sapply(pkgs,requireNamespace,quietly=TRUE)))
    stop("This function requires that the following packages be installed: ",
         paste(pkgs,collapse=", "))

  if (!rstudioapi::isAvailable())
    stop("RStudio does not appear to be running.")

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title,left = miniUI::miniTitleBarCancelButton(),
                           right = miniUI::miniTitleBarButton("done", "Remove chunks", primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::h3(shiny::textOutput('actdoc')),
      shiny::hr(),
      DT::dataTableOutput('chunklist')
    )
  )

  server <- function(input, output, session) {
    context = rstudioapi::getSourceEditorContext()

    output$actdoc = shiny::renderText(paste0("Active document: ",context$path))

    chunks = list_rmd_chunks(context$contents)
    rv = shiny::reactiveValues(chunks = chunks)

    # See if highlighted area touches any chunks
    se = c(context$selection[[1]]$range$start["row"],
           context$selection[[1]]$range$end["row"])

    row = which(chunks$start <= se[2] & se[1] <= chunks$end)

    if (length(row) > 0) {
      proxy = DT::dataTableProxy("chunklist",session)
      DT::selectRows(proxy,selected=row)
    }

    # render data table when list of chunks changed
    shiny::observeEvent(rv$chunks,{
      output$chunklist = DT::renderDataTable(rv$chunks,server=FALSE)
    })

    shiny::observeEvent(input$done, {
      rows = input$chunklist_rows_selected

      if(is.null(rows) || length(rows)==0)
        shiny::stopApp(FALSE)

      contents = remove_chunks(text=context$contents,chunk_labels=rows)

      # Put into document
      rstudioapi::setDocumentContents(paste0(contents,platform.newline(),collapse=""),context$id)
      rstudioapi::setCursorPosition(
        rstudioapi::document_position(rv$chunks$start[rows[1]]-1,1),
        context$id)

      shiny::stopApp(TRUE)
    })

    # When the cancel button is clicked, return null
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(FALSE)
    })
  }

  res = shiny::runGadget(
    app=ui, server=server,
    viewer = shiny::dialogViewer(title))

  return(invisible(res))
}
