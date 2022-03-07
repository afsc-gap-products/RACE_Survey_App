#' --------------------------------------
#' Project: RACE SURVEY APP
#' Developed by: Zack Oyafuso, Sarah Friedman, Emily Markowitz, Liz Dawson
#' Started date: Feb 2022
#' Current date: March 2022
#' --------------------------------------

# Knowns ----------------------------------------------------------------------

dir_googledrive <- "1AIQ0JEUA20D-g32uRQfRMZb0wW4SXl2n8Lwb_62uW-o"
dir_species_guides <- "172nNe_qrK0CWGNC4kR9gh27B-nyTgEj03uf4opGv5iU"

access_to_internet <- TRUE

this_year <- "2021" # just doing this for proof of concept


# CHECK! - define here pages that you don't want to use the template for!
# non-template pages MUST be named with the web_page name listed in the 'comb' object

no_templ <- c("survey_team", "flight_itineraries", "contacts",
              "covid", "work_environment", "Inventory", "guides",
              "Software & Drivers", "General Info",
              "checklist_in", "checklist_out", "checklist_end")


# Helper files ----------------------------------------------------------------
googledrive::drive_deauth()
googledrive::drive_auth() 
1
source("./functions.R")
source("./data.R")

# Check that links work --------------------------------------------------------

# listed below are links that do not work

# checkLinks(URLs = full_site0$url_loc)
# 
# checkLinks(URLs = full_site0$url_web)
# 
# checkLinks(URLs = full_site0$img)


# Create all pages -------------------------------------------------------------

comb <- comb %>% 
  dplyr::mutate(
    template = dplyr::case_when(
      page0 == "index" ~ FALSE, 
      sub_page0 %in% no_templ ~ FALSE, 
      TRUE ~ TRUE))


# clear_htmls() #removes all existing htmls in docs folder

for (jj in 1:nrow(comb)){
  
  print(comb[jj,])
  
  page_title <- stringr::str_to_title(comb$page[jj])
  page_desc <- ifelse(
      is.na(comb$sub_page[jj]), 
      "Parent directory", 
      stringr::str_to_title(comb$sub_page[jj]))
  
  # if the the page requires a non-template page structure
  # to use, simply name the rmd with the name in the comb$web_page column, so it knows what to grab
  if (comb$template[jj] == FALSE) {
    rmarkdown::render(gsub(pattern = ".html", 
                           replacement = ".rmd", 
                           x = comb$web_page[jj]),
                      output_dir = "./docs/",
                      output_file =  comb$web_page[jj])
  } else { 
  
  # page content
      page_dat <- site %>% 
        dplyr::filter(page0 == comb$page0[jj] &
                        sub_page0 == comb$sub_page0[jj] &
                        (title != "" | Links != "")) 
      
    # # list of subpages (if the main page and not a subpage)
    # if (sum(site$sub_page[site$page0==comb$page0[jj]]=="")>0) {
    #   subpages <- site %>% 
    #     dplyr::filter(page0 == comb$page0[jj])  %>%
    #     dplyr::filter(sub_page!="") %>% 
    #     dplyr::select(page, page0, sub_page, sub_page0, web_page, 
    #                   dplyr::starts_with("srvy_")) %>% 
    #     dplyr::distinct() %>% 
    #     dplyr::arrange(sub_page0)
    # }
    
    rmarkdown::render(paste0("template.Rmd"),
                      output_dir = "./docs/",
                      output_file =  comb$web_page[jj])
  }
}

