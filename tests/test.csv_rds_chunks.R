library(knitrdata)
library(magrittr) # For pipe operator

# Use a temporary directory ----------------------------
owd = getwd()
td = tempdir()
setwd(td)

# Create simple data files ------------------
df = data.frame(a=letters[1:3],b=1.1:3.1)

csv.fn = "test.csv_rds_chunks.csv"
csv2.fn = "test.csv_rds_chunks.csv2"
odd.fn = "test.csv_rds_chunks.odd"
rds.fn = "test.csv_rds_chunks.RDS"

write.csv(df,csv.fn,row.names=FALSE)
write.csv2(df,csv2.fn,row.names=FALSE)
write.table(df,odd.fn,row.names=FALSE,sep="|",dec=":")
saveRDS(df,rds.fn)

# Create chunks ------------------
loadlib = create_chunk("library(knitrdata)",chunk_type="r",chunk_label="loadlib")

lines = readLines(csv.fn)
lines2 = readLines(csv2.fn)
lines3 = readLines(odd.fn)

loadcsv =
  create_chunk(lines,output.var="df.csv",echo=TRUE,chunk_label="loadcsv",chunk_type="csv")
loadcsv2 =
  create_chunk(lines2,output.var="df.csv2",echo=TRUE,chunk_label="loadcsv2",chunk_type="csv2")
loadodd =
  create_chunk(lines3,output.var="df.odd",echo=TRUE,
               sep="|",dec=":",chunk_label="loadodd",chunk_type="csv")

loadrds = data_encode(rds.fn,"base64") %>%
  create_chunk(output.var="df.rds",echo=TRUE,chunk_label="loadrds",chunk_type="rds")

showdata = create_chunk(c("df.csv","df.csv","df.odd","df.rds"),
                        chunk_type="r",
                        chunk_label="showdata")

# Create Rmarkdown doc --------------
rmd.fn = "test.csv_rds_chunks.Rmd"
if (file.exists(rmd.fn))
  file.remove(rmd.fn)
rmarkdown::draft(rmd.fn,"github_document","rmarkdown",edit=FALSE)

# Push chunks into document ------------------

# Insert in reverse order to not have to figure out line number
rmd.text = readLines(rmd.fn)

rmd.text = insert_chunk(showdata,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadrds,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadodd,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadcsv2,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadcsv,11,rmd.text=rmd.text)
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
