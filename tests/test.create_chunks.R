# Use a temporary directory ----------------------------
owd = getwd()
td = tempdir(check=TRUE)
setwd(td)

# Test --------------
library(knitrdata)
library(magrittr) # For pipe operator

# Create new Rmarkdown document
if (file.exists("test.create_chunks.Rmd"))
  file.remove("test.create_chunks.Rmd")
rmarkdown::draft("test.create_chunks.Rmd","github_document","rmarkdown",
                 edit=FALSE)

# Create some binary data
x = data.frame(a=1:10,b=(1:10)^2)
saveRDS(x,"test.create_chunks.RDS")

# Push chunks into Rmarkdown document
# Insert in reverse order to not have to figure out line number
txt = create_chunk("x\nplot(b~a,data=x)",chunk_type="r") %>%
  insert_chunk(11,rmd.file="test.create_chunks.Rmd")
txt = data_encode("test.create_chunks.RDS","base64") %>%
  create_chunk(output.var="x",format="binary",loader.function=readRDS) %>%
  insert_chunk(11,txt)
txt = create_chunk("library(knitrdata)",chunk_type="r") %>%
  insert_chunk(11,txt)

writeLines(txt,"test.create_chunks.Rmd")

# List all chunks in documents
chunklst = list_rmd_chunks(file="test.create_chunks.Rmd")
chunklst

# Remove the pressure chunk
xx = split_rmd_by_chunk(file="test.create_chunks.Rmd",chunk_label="pressure")
txt = c(xx$pre_chunk,xx$post_chunk)
writeLines(txt,"test.create_chunks.Rmd")

# List chunks again
chunklst = list_rmd_chunks(file="test.create_chunks.Rmd")
chunklst

# Render document to test
rmarkdown::render("test.create_chunks.Rmd")

# Clean up --------------
file.remove("test.create_chunks.Rmd","test.create_chunks.RDS",
            "test.create_chunks.md","test.create_chunks.html")
unlink("test.create_chunks_files",recursive=TRUE)

setwd(owd)
