library(knitrdata)
library(magrittr) # For pipe operator

# Use a temporary directory ----------------------------
owd = getwd()
td = tempdir()
setwd(td)

# Create simple text files ------------------

# Text file with platform line separator
pn = platform.newline()
txt = paste0(pn,"Hello",pn,pn,"Goodbye",pn,pn)
txt.fn = "test.md5sum_chunks.txt"
writeChar(txt,txt.fn,useBytes=TRUE,eos=NULL)
txt.md5 = tools::md5sum(txt.fn)

# Text file with opposite line separator
opn = ifelse(pn=="\n","\r\n","\n")
txt2 = paste0(opn,"Goodbye",opn,opn,"Hello",opn,opn)
txt2.fn = "test2.md5sum_chunks.txt"
writeChar(txt2,txt2.fn,useBytes=TRUE,eos=NULL)
txt2.md5 = tools::md5sum(txt2.fn)

# Chinese text
ch.txt = "发动机测谎报告"
ch.fn = "test.md5sum_chunks.ch.txt"
writeLines(ch.txt,ch.fn,useBytes=TRUE)
ch.md5 = tools::md5sum(ch.fn)
print(ch.md5)
# MD5 sums of Chinese text do not work on Windows, likely due to character encoding translation issues!!!
# This webpage seems pertinent: https://stackoverflow.com/questions/38230026/how-to-print-chinese-letters-from-r

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
lines2 = readLines(txt2.fn)

co = paste0('md5sum="',txt.md5,'"')
loadtxt =
  create_chunk(lines,output.var="txt",output.file="output.md5sum_chunks.txt",
               format="text",echo=TRUE,
               chunk_label="loadtxt",chunk_options_string = co
  )

# Text chunk specifying explicitly newline character to use
oopn = ifelse(opn=="\n","\\n","\\r\\n")
co2 = paste0('md5sum="',txt2.md5,'",line.sep="',oopn,'"')
loadtxt2 =
  create_chunk(lines2,output.var="txt2",output.file="output2.md5sum_chunks.txt",
               format="text",echo=TRUE,
               chunk_label="loadtxt2",chunk_options_string = co2
  )

loadtxt64 = data_encode(txt.fn,"base64") %>%
  create_chunk(output.var="txt64",output.file="output.md5sum_chunks.64.txt",
               format="text",echo=TRUE,encoding="base64",
               chunk_label="loadtxt64",chunk_options_string = co
  )

co2 = paste0('md5sum="',txt2.md5,'"')
loadtxt264 = data_encode(txt2.fn,"base64") %>%
  create_chunk(output.var="txt264",output.file="output2.md5sum_chunks.64.txt",
               format="text",echo=TRUE,encoding="base64",
               chunk_label="loadtxt264",chunk_options_string = co2
  )

co = paste0('md5sum="',rds.md5,'"')
loaddata = data_encode(rds.fn,"base64") %>%
  create_chunk(output.var="d",output.file="output.md5sum_chunks.RDS",
               format="binary",loader.function=readRDS,echo=TRUE,
               chunk_label="loadrds",chunk_options_string = co
  )

showdata = create_chunk(c("cat(txt)","cat(txt2)","cat(txt64)","cat(txt264)","d"),
                        chunk_type="r",
                        chunk_label="showdata")

# Chinese text - md5sum test fails on Windows
co = paste0('md5sum="',ch.md5,'"')
loadch =
  create_chunk(file=ch.fn,output.var="chtxt",output.file="output.md5sum_chunks.ch.txt",
               format="text",echo=TRUE,
               chunk_label="loadchtxt"
               #,chunk_options_string = co # MD5 sum fails on Windows
  )

showch = create_chunk(c("cat(chtxt)","tools::md5sum('output.md5sum_chunks.ch.txt')"),
                        chunk_type="r",
                        chunk_label="showch")

# Push chunks into document ------------------

# Insert in reverse order to not have to figure out line number
rmd.text = readLines(rmd.fn)

rmd.text = insert_chunk(showch,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadch,11,rmd.text=rmd.text)
rmd.text = insert_chunk(showdata,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loaddata,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadtxt264,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadtxt64,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadtxt2,11,rmd.text=rmd.text)
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
if (rmarkdown::pandoc_available(version="1.12.3")) {
  system.time(
    rmarkdown::render(rmd.fn)
  )

  # Check MD5 sum of Chinese text before and after
  print(tools::md5sum('output.md5sum_chunks.ch.txt'))
}

# Clean up --------------
setwd(owd)
