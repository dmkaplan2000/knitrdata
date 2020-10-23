library(knitrdata)
library(magrittr) # For pipe operator

# Use a temporary directory ----------------------------
owd = getwd()
td = tempdir()
setwd(td)

# Create a simple text file ------------------
txt = paste0("Hello",platform.newline(),"Goodbye",platform.newline())
txt.fn = "test.md5sum_chunks.txt"
writeChar(txt,txt.fn,useBytes=TRUE,eos=NULL)
txt.md5 = tools::md5sum(txt.fn)

# Create a simple binary file ----------------
d = data.frame(x=1:3,y=letters[1:3])
rds.fn = "test.md5sum_chunks.RDS"
saveRDS(d,rds.fn)
rds.md5 = tools::md5sum(rds.fn)

# Create Rmarkdown doc --------------
rmd.fn = "test.md5sum_chunks.Rmd"
if (file.exists(rmd.fn))
  file.remove(rmd.fn)
rmarkdown::draft(rmd.fn,"github_document","rmarkdown",edit=FALSE)

# Create chunks ------------------

loadlib = create_chunk("library(knitrdata)",chunk_type="r",chunk_label="loadlib")

lines = readLines(txt.fn)

co = paste0('md5sum="',txt.md5,'"')
loadtxt =
  create_chunk(lines,output.var="txt",output.file="output.md5sum_chunks.txt",
               format="text",echo=TRUE,
               chunk_label="loadtxt",chunk_options_string = co
  )

loadtxt64 = data_encode(txt.fn,"base64") %>%
  create_chunk(output.var="txt64",output.file="output.md5sum_chunks.64.txt",
               format="text",echo=TRUE,encoding="base64",
               chunk_label="loadtxt64",chunk_options_string = co
  )

co = paste0('md5sum="',rds.md5,'"')
loaddata = data_encode(rds.fn,"base64") %>%
  create_chunk(output.var="d",output.file="output.md5sum_chunks.RDS",
               format="binary",loader.function=readRDS,echo=TRUE,
               chunk_label="loadrds",chunk_options_string = co
  )

showdata = create_chunk("cat(txt)\ncat(txt64)\nd",chunk_type="r",
                        chunk_label="showdata")

# Push chunks into document ------------------

# Insert in reverse order to not have to figure out line number
rmd.text = readLines(rmd.fn)

rmd.text = insert_chunk(showdata,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loaddata,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadtxt64,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadtxt,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadlib,11,rmd.text=rmd.text)

writeLines(rmd.text,rmd.fn)

# List all chunks in document
chunklst = list_rmd_chunks(file=rmd.fn)
chunklst

# View file in Rstudio ----------
# rstudioapi::navigateToFile(rmd.fn)

# Clean environment ------
l = ls()
l = l[!(l %in% c("rmd.fn","owd"))]
rm(list=l)

# Render document to test -------
if (rmarkdown::pandoc_available(version="1.12.3"))
  system.time(
    rmarkdown::render(rmd.fn)
  )

# Clean up --------------
setwd(owd)
