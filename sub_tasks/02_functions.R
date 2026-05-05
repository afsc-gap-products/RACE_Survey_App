#' --------------------------------------
#' Project: RACE SURVEY APP
#' Developed by: Zack Oyafuso, Sarah Friedman, Emily Markowitz
#' Date: Feb 2024
#' 
#' Notes: checkLinks() checks whether a URL is valid or not
#' --------------------------------------


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
#'  

checkLinks <- function(URLs, quiet = FALSE) {
  
  URLs <- URLs[!is.na(URLs) & URLs != ""]
  notworking <- character()
  
  for (URL in URLs) {
    
    original_URL <- URL
    
    # Handle relative paths
    if (startsWith(URL, "./")) {
      URL <- file.path(getwd(), sub("^\\./", "", URL))
    }
    
    if (startsWith(URL, "../")) {
      URL <- file.path(getwd(), sub("^\\.\\./", "", URL))
    }
    
    URL <- sub("/$", "", URL)  # remove trailing slash
    
    # Web URL
    if (grepl("^https?://", URL)) {
      ok <- tryCatch({
        resp <- httr2::request(URL) |> httr2::req_perform()
        status <- httr2::resp_status(resp)
        status < 400
      }, error = function(e) FALSE)
      
    } else {
      # Local file
      ok <- file.exists(URL)
    }
    
    if (!ok) {
      notworking <- c(notworking, original_URL)
    }
  }
  
  notworking <- sort(unique(notworking))
  
  if (!quiet) {
    if (length(notworking) == 0) {
      message("All links are good!")
    } else {
      message(length(notworking), " bad link(s) found.")
    }
  }
  
  return(notworking)
}

# function to extract link from formatted html link
html_to_link <- function(x){
  tmp <- paste0(#"..\\", 
                str_extract(x, "(?<=\\().*?(?=\\))")) 
  gsub(" ", "%20", tmp)
}


# clean out old htmls so we aren't collecting files we never use
clear_htmls <- function() {
  l <- list.files("./docs/", full.names = TRUE, pattern = ".html")
  
  if(remake_species_pages){
    invisible(file.remove(l))
  } else {
    z <- list.files("./docs/", full.names = TRUE, pattern = "zz_")
    invisible(file.remove(l[!l %in% z]))
  }
  
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

