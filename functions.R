#' --------------------------------------
#' Project: RACE SURVEY APP
#' Developed by: Zach Oyafuso, Sarah Friedman, Emily Markowitz, Liz Dawson
#' Date: Feb 2022
#' --------------------------------------

## Load packages -----------------------------------------------------------
pkg <- c(#"tidyverse",
         "stringr",
         "tidyr", 
         "dplyr",
         "magrittr",
         "googledrive", 
         "janitor", 
         "rmarkdown", 
         "distill", 
         
         # read url
         "XML",
         
         # read csv
         "readr",
         
         # print web pages
         "pagedown",
         
         # make tables
         "kableExtra", 
         "flextable")

for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}

#' Check that your links work
#'
#' @param URLs A vector of strings with website URLs, local directories, and/or local files.
#' @param quiet default = FALSE. Will not return messages if = TRUE.
#'
#' @return
#' @export
#'
#' @examples
#' # Use test page URL:
#'   URLs <- c(
#'     "https://github.com",
#'     "http://steipe.biochemistry.utoronto.ca/abc/assets/testCheckLinks.html",
#'     "./",
#'     "./ex.txt",
#'     "./aa/")
#'  checkLinks(URLs)
checkLinks <- function(URLs,
                       quiet = FALSE) {
  
  # https://stackoverflow.com/questions/31214361/what-is-the-r-equivalent-for-excel-iferror
  
  URLs <- URLs[!is.na(URLs)] # remove empty rows
  
  notworking <- c()
  
  for (i in 1:length(URLs)){
    
    # Fix URLs if local directories
    URL <- URLs[i]
    
    if (substr(x = URL, start = 1, stop = 2) == "./") {
      if (URL == "./") {
        URL <- gsub(replacement = getwd(), pattern = "./",
                    x = URL, fixed = TRUE, useBytes = TRUE)
      } else {
        URL <- gsub(replacement = paste0(getwd(), "/"), pattern = "./",
                    x = URL, fixed = TRUE, useBytes = TRUE)
      }
      
    }
    
    if (substr(x = URL, start = nchar(URL), stop = nchar(URL)) == "/") {
      URL <- substr(x = URL, start = 1, stop = (nchar(URL)-1))
    }
    
    # download page and access URLs. If does not exist, collect it in "badurl"
    myPage <- try(expr = xml2::read_html(URL), silent = T)
    if (grepl(pattern = "Error", x = myPage[1],
              fixed = TRUE, useBytes = TRUE) &
        isFALSE(file.exists(URL))) {
      
      notworking <- c(notworking, URL)
    }
  }
  
  if (quiet == FALSE){
    if (length(notworking) == 0) {
      print("All links are good!")
    } else {
      print(paste0("There ",
                   ifelse(length(notworking)>1, "are ", "is "),
                   length(notworking),
                   " bad link",
                   ifelse(length(notworking)>1, "s", ""),
                   "."))
    }
  }
  
  return(notworking)
}
