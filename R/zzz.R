# Loader/Unloader functions ----------------------------------------------

# Create some persistent data for storing knit_global environment
.knitrdata_env_func = function(env) { function(e=NULL) if (is.null(e)) env else env <<- e }
.knitrdata_env = .knitrdata_env_func(new.env())

# Activate data language engine for use in Rmarkdown documents
.onLoad = function(libname,pkgname) {
  # Push knit_global into persistent data
  .knitrdata_env(knitr::knit_global())

  knitr::knit_engines$set(data=eng_data)

  # Extra engines
  knitr::knit_engines$set(csv=eng_csv,csv2=eng_csv2,rds=eng_rds)
}

# Remove data language engine on package unload
.onUnload = function(libname,pkgname) {
  knitr::knit_engines$delete("data")

  # Extra engines
  knitr::knit_engines$delete(c("csv","csv2","rds"))
}
