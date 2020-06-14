# Use a temporary directory ----------------------------
owd = getwd()
td = tempdir(check=TRUE)
setwd(td)

# Do some data encoding and decoding ------------------
library(knitrdata)

x = data.frame(a=1:5,b=letters[1:5])
write.csv(x,"test.csv")
saveRDS(x,"test.RDS")

enccsv = data_encode("test.csv","base64")
encrds = data_encode("test.RDS","base64")

csv = data_decode(enccsv,"base64",as_text=TRUE)
cat(csv)

rds = data_decode(encrds,"base64",as_text=FALSE)
writeBin(rds,"test_output.RDS")
y = readRDS("test_output.RDS")
y

params = list(run_gpg=FALSE)
if (requireNamespace("gpg") && params$run_gpg) {
  k = gpg::gpg_keygen("test","test@test.org")
  encgpg = data_encode("test.csv","gpg",options = list(receiver=k))

  cat(data_decode(encgpg,"gpg",as_text=TRUE))

  gpg::gpg_delete(k,secret=TRUE)
}

# Cleanup ------------------------------------
file.remove("test.csv","test.RDS","test_output.RDS")

setwd(owd)
