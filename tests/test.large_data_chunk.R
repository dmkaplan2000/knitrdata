# Use a temporary directory ----------------------------
owd = getwd()
td = tempdir()
setwd(td)

# Create lots of data to insert into doc ------------------
x = runif(3e5)
y = 4*x + 3 + rnorm(3e5)

d = data.frame(x=x,y=y)
rds.fn = "test.large_data_chunk.RDS"
saveRDS(d,rds.fn)

# ~3.7 MB of data
# Rstudio won't open a file larger than 5 MB...
# But render always works...

# Create Rmarkdown doc --------------
library(knitrdata)
library(magrittr) # For pipe operator

# Create new Rmarkdown document
rmd.fn = "test.large_data_chunk.Rmd"
if (file.exists(rmd.fn))
  file.remove(rmd.fn)
rmarkdown::draft(rmd.fn,"github_document","rmarkdown",edit=FALSE)

# Create chunks ------------------

loadlib = create_chunk("library(knitrdata)",chunk_type="r",chunk_label="loadlib")
loaddata = data_encode(
  rds.fn,"base64",
  options=list(linewidth=300)) %>% # Make lines longer to help with visualization
  create_chunk(output.var="d",format="binary",loader.function=readRDS,
               echo=TRUE, # Test max.echo option of knitrdata
               chunk_label="loaddata")
analyzedata = create_chunk("summary(lm(y~x,data=d))",chunk_type="r",chunk_label="analyzedata")

# Push chunks into document ------------------

# Insert in reverse order to not have to figure out line number
rmd.text = readLines(rmd.fn)

rmd.text = insert_chunk(analyzedata,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loaddata,11,rmd.text=rmd.text)
rmd.text = insert_chunk(loadlib,11,rmd.text=rmd.text)

writeLines(rmd.text,rmd.fn)

# List all chunks in document
chunklst = list_rmd_chunks(file=rmd.fn)
chunklst

# View file in Rstudio ----------
# rstudioapi::navigateToFile(rmd.fn)

# Rstudio will warn about file size and will take some time to open it,
# but it does eventually work. If you collapse down the visualization
# of the data chunk, then moving around and editing the documents works
# rapidly without any problems.

# Render document to test -------
if (rmarkdown::pandoc_available(version="1.12.3"))
  system.time(
    rmarkdown::render(rmd.fn)
  )

# Clean up --------------
file.remove(rmd.fn,rds.fn,
            sub("[.][^.]+$",".md",rmd.fn),sub("[.][^.]+$",".html",rmd.fn))
unlink(sub("[.][^.]+$","_files",rmd.fn),recursive=TRUE)

setwd(owd)
