#' --------------------------------------
#' Project: RACE SURVEY APP
#' Developed by: Zach Oyafuso, Sarah Friedman, Emily Markowitz, Liz Dawson
#' Date: Feb 2022
#' 
#' Notes: 1) Load Required Packages
#'        2) checkLinks() checks whether a URL is valid or not
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
         "fansi", 
         
         # read csv
         "readr",
         
         # print web pages
         "pagedown",
         
         # make tables
         "kableExtra", # devtools::install_github(repo="haozhu233/kableExtra", ref="a6af5c0")
         "flextable",
         
         #webpage enhancements
         "xaringanExtra" # devtools::install_github("gadenbuie/xaringanExtra")

         )

for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}

if(packageVersion("kableExtra") != "1.3.4.9000"){
  detach("kableExxtra")
  devtools::install_github(repo="haozhu233/kableExtra", ref="a6af5c0")
  library(kableExtra)
}

rm(p, pkg)

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
  # if (substr(x = URL, start = 1, stop = 2) == "./") {
      if (URL == "./") {
        URL <- gsub(replacement = getwd(), pattern = "./",
                    x = URL, fixed = TRUE, useBytes = TRUE)
      } else {
        URL <- gsub(replacement = paste0(getwd(), "/"), pattern = "./",
                    x = URL, fixed = TRUE, useBytes = TRUE)
      }
      
    }
    
    # fix for for a directory
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
  
  return(sort(unique(notworking)))
}


# function to extract link from formatted html link
html_to_link <- function(x){
  tmp <- paste0(#"..\\", 
                str_extract(x, "(?<=\\().*?(?=\\))")) 
  gsub(" ", "%20", tmp)
}


# clean out old htmls so we aren't collecting files we never use
clear_htmls <- function(){
  l <- list.files("./docs/", full.names = TRUE, pattern = ".html")
  invisible(file.remove(l))
  cat('all HTML files deleted from \'docs\' folder')
}



download_web_urls <- function(dat, col_in, dir_out) {
  
  dir.create(path = dir_out, showWarnings = FALSE)
  dat$col <- dat[,names(dat) == col_in]
  dat$col_out_link <- ""
  dat$col_out_link_txt <- ""
  dat$col_out_img <- ""
  dat$col_out_img_txt <- ""
  
  temp <- unique(dat$col[!is.na(dat$col)]) # links to download
  counter <- 0
  
  for (i in 1:length(temp)) { ## Loop over URLs -- start
    
    # if downloading a png
    if (grepl(pattern = ".png", x = temp[i], fixed = TRUE)) {
      counter <- 1 + counter
      dest <- paste0(dir_out, "dl_img_", counter, ".pdf")
      utils::download.file(url = temp[i], destfile = dest, mode = "wb")
      dat$col_out_img_txt[dat$col == temp[i]] <- "Downloaded image from web"
      dat$col_out_img[dat$col == temp[i]] <- dest
    }
    
    # if download google doc
    # if (grepl(pattern = "docs.google.com", x = temp[i])) {
    #   if (access_googledrive) {
    #     temp1 <- googledrive::drive_get(id = temp[i])
    #     type <- ifelse(grepl(pattern = "document", x = temp[i], ignore.case = TRUE),
    #                    "docx", "csv"
    #     )
    #     dest <- paste0(dir_out, temp1$name, ".", type)
    #     googledrive::drive_download(
    #       file = temp1$id,
    #       type = type,
    #       overwrite = TRUE,
    #       path = dest
    #     )
    #     dat$col_out_link_txt[dat$col == temp[i]] <- "Downloaded from google drive"
    #     dat$col_out_link[dat$col == temp[i]] <- dest
    #   }
    # }
    
    # if download google doc
    if (grepl(pattern = "docs.google.com", x = temp[i])) {
        ## Access metadata of the google doc so that you can specify a
        ## name of the destination file
        metadata <- googledrive::drive_get(id = temp[i])
        type <- ifelse(test = grepl(
          pattern = "document",
          x = temp[i],
          ignore.case = TRUE
        ),
        yes = "docx", ## Indicates a google doc
        no = "csv" ## Indicates a google spreadsheet?
        )
        dest <- paste0(dir_out, metadata$name)#, ".", type)

        ## Pull document from google drive, format it in the type specified,
        ## and write to dest path
        googledrive::drive_download(
          file = metadata$id,
          type = "pdf", #type,
          overwrite = TRUE,
          path = dest
        )
        
        ## Update the full_site info
        dat$col_out_link_txt[dat$col == temp[i]] <- "Downloaded from google drive"
        dat$col_out_link[dat$col == temp[i]] <- dest
    }
    
    # if downloading a webpage HTML
    if (grepl(pattern = ".html", x = temp[i], fixed = TRUE)) {
      counter <- 1 + counter
      dest <- paste0(dir_out, "dl_html_", counter, ".pdf")
      chrome_print(temp[i], output = dest)
      dat$col_out_link_txt[dat$col == temp[i]] <- "web page downloaded from web as pdf"
      dat$col_out_link[dat$col == temp[i]] <- dest
    }
    
    # if downloading a PDF from a webage
    # TOLEDO - need to make more accommodating to links without ".pdf" at the end
    if (grepl(pattern = ".pdf", x = temp[i], fixed = TRUE)) {
      counter <- 1 + counter
      dest <- paste0(dir_out, "dl_pdf_", counter, ".pdf")
      download.file(url = temp[i], destfile = dest, mode = "wb")
      dat$col_out_link_txt[dat$col == temp[i]] <- "PDF downloaded from web"
      dat$col_out_link[dat$col == temp[i]] <- dest
    }
  } ## Loop over URLs -- end
  return(dat)
}

