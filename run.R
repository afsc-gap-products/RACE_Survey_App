# --------------------------------------
# Project: RACE SURVEY APP
# Developed by: Zach Oyafuso, Sarah Friedman, Emily Markowitz, Liz Dawson
# Date: Feb 2022
# --------------------------------------

# TO DO
 # fix how images are shown on pages
 # fix how check_links checks local links
 # fix how descrip column works in template pages 
 # page_desc <- "blank" # need to change
 # add periods at the end of the title/subtitles before links
 # [DONE - EHM] automate yaml to match heirarchy in spreadsheet

# Knowns ----------------------------------------------------------------------

dir_googledrive <- "1AIQ0JEUA20D-g32uRQfRMZb0wW4SXl2n8Lwb_62uW-o"
access_googledrive <- TRUE
this_year <- "2021" # just doing this for proof of concept

page_data <- data.frame(file_name = c("main", "travel_itinerary"), 
                        name = c("main", "travel_itinerary"), 
                        descr = c("Welcome to the website. I hope you enjoy it!", "travel_itinerary"), 
                        file_folder = c(NA, paste0("forms_files/Travel/flight itineraries/", this_year, "/")), 
                        template = c(T, F))

# Helper files ----------------------------------------------------------------
googledrive::drive_deauth()
googledrive::drive_auth() 
1
source("./functions.R")
source("./data.R")

# Check that links work --------------------------------------------------------

# listed below are links that do not work

checkLinks(URLs = full_site0$url_loc)

checkLinks(URLs = full_site0$url_web)

checkLinks(URLs = full_site0$img)

# create all pages -------------------------------------------------------------

# CHECK! - define here pages that you don't want to use the template for!
# non-template pages MUST be named with the web_page name listed in the 'comb' object
comb <- comb %>% 
  dplyr::mutate(
    template = dplyr::case_when(
      page0 == "index" ~ FALSE, 
      sub_page0 == "flight_itineraries" ~ FALSE, 
      TRUE ~ TRUE))

for (jj in 1:nrow(comb)){
  
  page_desc <- "blank" # TOLEDO, need to change
  
  # if the the page requires a non-template page structure
  if (comb$template[jj] == FALSE) {
    rmarkdown::render(gsub(pattern = ".html", replacement = ".rmd", x = comb$web_page[jj]),
                      output_dir = "./docs/",
                      output_file =  comb$web_page[jj])
  } else { 
  
  # page content
      page_dat <- site %>% 
        dplyr::filter(page0 == comb$page0[jj] &
                        sub_page0 == comb$sub_page0[jj])
      
    # list of subpages (if the main page and not a subpage)
    if (sum(site$sub_page[site$page0==comb$page0[jj]]=="")>0) {
      subpages <- site %>% 
        dplyr::filter(page0 == comb$page0[jj])  %>%
        dplyr::filter(sub_page!="") %>% 
        dplyr::select(survey, page, page0, sub_page, sub_page0, web_page) %>% 
        dplyr::distinct() %>% 
        dplyr::arrange(sub_page0)
    }
    
    rmarkdown::render(paste0("template.Rmd"),
                      output_dir = "./docs/",
                      output_file =  comb$web_page[jj])
  }
}


# rmarkdown::render(paste0("index.Rmd"),
#                   output_dir = "./docs/",
#                   output_file =  "index.html")
# 
# rmarkdown::render(paste0("survey_team.Rmd"),
#                   output_dir = "docs/",
#                   output_file =  "survey_team.html")
# 
# rmarkdown::render(paste0("flight_itineraries.Rmd"),
#                   output_dir = "./docs/",
#                   output_file =  "flight_itineraries.html")
# 
# rmarkdown::render(paste0("safety.Rmd"),
#                   output_dir = "docs/",
#                   output_file =  "safety.html")
# 
# rmarkdown::render(paste0("safety_work_environment.Rmd"),
#                   output_dir = "docs/",
