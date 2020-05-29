library(knitrdata)
library(tidyr) # For pipe operator

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
create_chunk("plot(b~a,data=x)",chunk_type="r") %>%
  insert_chunk("test.create_chunks.Rmd",11)
data_encode("test.create_chunks.RDS","base64") %>%
  create_chunk(output.var="x",format="binary",loader.function=readRDS) %>%
  insert_chunk("test.create_chunks.Rmd",11)
create_chunk("library(knitrdata)",chunk_type="r") %>%
  insert_chunk("test.create_chunks.Rmd",11)

# Render document to test
rmarkdown::render("test.create_chunks.Rmd")

# Clean up
file.remove("test.create_chunks.Rmd","test.create_chunks.RDS",
            "test.create_chunks.md","test.create_chunks.html")
