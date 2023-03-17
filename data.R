#' --------------------------------------
#' Project: RACE SURVEY APP
#' Developed by: Zack Oyafuso, Sarah Friedman, Emily Markowitz, Liz Dawson
#' Date: Feb 2022
#' --------------------------------------

# Load Data from Google Drive --------------------------------------------------

# How to find the google drive ID of a document:
# Google Drive File ID is a unique identifier of the file on Google Drive.
# File IDs are stable throughout the lifespan of the file, even if the file
# name changes. To locate the File ID, right-click on the name of the file,
# choose the Get Shareable Link option, and turn on Link Sharing if needed.
# You will see the link with a combination of numbers and letters at the end,
# and what you see after `id =`  is the File ID.
# https://drive.google.com/open?id=***ThisIsFileID***
#
# If your file is already open in a browser, you can obtain File ID from 
# its link:
# https://docs.google.com/spreadsheets/d/***ThisIsFileID***/edit#gid=123456789

## Laod data
## Google Drive File ID pointing to the RACE survey app data spreadsheet
dir_data <- "1AIQ0JEUA20D-g32uRQfRMZb0wW4SXl2n8Lwb_62uW-o"
dir_species_guides <- "172nNe_qrK0CWGNC4kR9gh27B-nyTgEj03uf4opGv5iU"

## Import input data and save locally to the data/ folder

if (access_to_internet) {
  
  ## Download Main entries data
  survey_app_data <- 
    googlesheets4::read_sheet(ss = googledrive::as_id(dir_data), 
                              sheet = "entries", 
                              skip = 1)
  write.csv(x = survey_app_data, 
            file = "data/survey_app_data.csv", 
            row.names = F)
  
  ## Download task list data
  task_list_data <- 
    googlesheets4::read_sheet(ss = googledrive::as_id(dir_data), 
                              sheet = "task_list_data", 
                              skip = 1)
  write.csv(x = task_list_data, 
            file = "data/task_list_data.csv", 
            row.names = F)
  
  ## Download minimum ID list
  min_ID_list <- 
    googlesheets4::read_sheet(ss = googledrive::as_id(dir_data), 
                              sheet = "min_ID_list")
  write.csv(x = min_ID_list, 
            file = "data/min_id.csv", 
            row.names = F)
  
  ## Download minimum ID list
  taxa_guides <- 
    googlesheets4::read_sheet(ss = googledrive::as_id(dir_data), 
                              sheet = "taxa_guides", skip = 1)
  write.csv(x = taxa_guides, 
            file = "data/taxa_guides.csv", 
            row.names = F)
  
  ## Download Species Guides data
  googledrive::drive_download(
    file = as_id(dir_species_guides),
    type = "csv",
    overwrite = TRUE,
    path = paste0("./data/id_guides_data.csv")
  )
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Clean up website content
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean up entries dataframe to format we need it for printing
website_content <- read.csv("data/survey_app_data.csv")
# website_content[is.na(website_content)] <- ""
website_content <- website_content %>%
  # bind description bullets together
  tidyr::unite(
    tidyr::starts_with("description"),
    col = "descrip",
    sep = "\n - ",
    remove = FALSE,
    na.rm = TRUE
  ) %>%
  dplyr::mutate(
    descrip = ifelse(test = descrip != "",
                     yes = paste0("\n- ", descrip),
                     no = ""),
    section = ifelse(test = is.na(section), yes = "",
                     no = stringr::str_to_title(section)),
    subsection = ifelse(test = is.na(subsection), yes = "",
                        no = stringr::str_to_sentence(subsection)),
    
    # Hyperlinked titles URL links
    title_link = ifelse(test = url_loc == "",
                        yes = title,
                        no = paste0("[", title, "](../", url_loc, ")")
    ),
    Links = ifelse(test = url_web == "",
                   yes = title_link,
                   no = paste0(title_link, " \n\n ([Web link](", url_web, "))")
    ),
    title_link_inline = ifelse(test = url_web == "",
                               yes = title_link,
                               no = paste0(title_link, " ([Web link](", 
                                           url_web, "))")
    )
  ) %>%
  dplyr::arrange(page, sub_page) %>%
  dplyr::relocate(
    page,
    sub_page,
    section,
    subsection, title, title_link_inline, subtitle, descrip,
    dplyr::starts_with("svy_")
  )
