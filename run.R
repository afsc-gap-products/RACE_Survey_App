#' --------------------------------------
#' Project: RACE SURVEY APP
#' Developed by: Zack Oyafuso, Sarah Friedman, Emily Markowitz, Liz Dawson
#' --------------------------------------

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import packages, authenticate google drive
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("sub_tasks/01_import_R_packages.R")
googledrive::drive_deauth()
googledrive::drive_auth() 
1

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import helper functions
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("sub_tasks/02_functions.R")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Clear the html files in docs/ folder. Since it takes a while to create
##   all of the fish ID pages, remake_species_pages can be set to F to skip
##   remaking those pages. In clear_htmls() the species id pages (those that
##   start with "zz") are not cleared from the docs/ folder. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
remake_species_pages <- FALSE
clear_htmls() #removes all existing htmls in docs folder

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import and clean up data. If access_to_internet == TRUE, a local copy
##   of the various data input are saved in the data/ folder.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
access_to_internet <- TRUE
source("sub_tasks/03_data.R")
1

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Check that links work: listed below are links that do not work
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
checkLinks(URLs = website_content$url_loc)
checkLinks(URLs = website_content$url_web)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Find combination of webpages using the generic template. dir_pdfs is a 
## vector of subpages that link to pdfs and not htmls so we note that here. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dir_pdfs <- c("Codebook", "Emergency Flow Chart")

comb <- website_content %>%
  dplyr::select(page, sub_page) %>%
  dplyr::distinct() %>%
  dplyr::mutate(template_rmd = "template.rmd") %>%
  dplyr::mutate(web_page = gsub(x = paste0(page, "_", sub_page, ".html"),
                                pattern = " ", 
                                replacement = "_"))
comb$template_rmd <- ifelse(comb$sub_page %in% dir_pdfs, 
                            yes = "", 
                            no = comb$template_rmd)
comb$web_page[match(x = dir_pdfs, table = comb$sub_page)] <-
  website_content$url_loc[match(x = dir_pdfs, table = website_content$sub_page)]

## Add comb information for webpages that use a custom template
custom_comb <-
  data.frame(page = c("FPC and Deck Lead", "FPC and Deck Lead", 
                      "FPC and Deck Lead", "Personnel", "Species ID", 
                      "Species ID"),
             sub_page = c("Tasklist 1 - Beginning of Survey or Leg",
                          "Tasklist 2 - End of Leg",
                          "Tasklist 3 - End of Survey", "Flight Itineraries",
                          "Minimum ID", "Guides"),
             template_rmd = c("tasklist.rmd", "tasklist.rmd", "tasklist.rmd",
                              "personnel_flight_itineraries.Rmd",
                              "species_id_minimum_id.Rmd", 
                              "species_id_guides.Rmd"))
custom_comb$web_page <- 
  gsub(x = paste0(custom_comb$page, "_", custom_comb$sub_page, ".html"),
       pattern = " ", 
       replacement = "_")

comb <- rbind(comb, custom_comb)
source("sub_tasks/04_render_main_page.R")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Look over comb df and render each page
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (jj in 1:nrow(comb)) { ## Loop over pages -- start
  if (comb$template_rmd[jj] == "") next #direct pdfs
  
  ## Page specific information
  page_title <- comb$page[jj]
  page_desc <- ifelse(test = is.na(x = comb$sub_page[jj]),
                      yes = "Parent directory",
                      no = comb$sub_page[jj])
  page_dat <- website_content %>%
    dplyr::filter(page == page_title &
                    sub_page == page_desc &
                    (title != "" | Links != ""))
  
  ## Render document
  rmarkdown::render(input = paste0("templates/", comb$template_rmd[jj]),
                    output_dir = "docs/",
                    output_file =  comb$web_page[jj])
  
} ## Loop over pages -- end

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Render ID by Taxa page. 
##   If remake_species_pages == TRUE, remake species pages
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("sub_tasks/05_render_species_pages.R")
