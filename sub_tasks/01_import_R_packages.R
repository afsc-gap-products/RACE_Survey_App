## Load packages -----------------------------------------------------------

pkg <- c( "stringr",
          "tidyr", 
          "dplyr",
          "magrittr",
          "googledrive",
          # "googlesheets4",
          "janitor", 
          "rmarkdown", 
          "distill", 
          
          # read url
          "XML",
          "fansi", 
          
          # read csv
          "readr",
          
          # print web pages
          "pagedown",
          
          # make tables
          "kableExtra", # devtools::install_github(repo="haozhu233/kableExtra", ref="a6af5c0")
          "flextable")

for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    1
    require(p, character.only = TRUE)
  }
}

if (packageVersion(pkg = "kableExtra") != "1.3.4.9000") {
  detach(name = "package:kableExtra", unload = TRUE)
  devtools::install_github(repo = "haozhu233/kableExtra", ref = "a6af5c0")
  library(kableExtra)
}

rm(p, pkg)
