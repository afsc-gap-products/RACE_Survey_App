## Load packages -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load( "stringr",
          "tidyr", 
          "dplyr",
          "magrittr",
          "googledrive",
          # "googlesheets4",
          "janitor", 
          "rmarkdown", 
          "markdown",
          "distill", 
          "writexl",
          
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

if (packageVersion(pkg = "kableExtra") != "1.3.4.9000") {
  detach(name = "package:kableExtra", unload = TRUE)
  devtools::install_github(repo = "haozhu233/kableExtra", ref = "a6af5c0")
  library(kableExtra)
}
