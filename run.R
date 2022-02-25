#' --------------------------------------
#' Project: RACE SURVEY APP
#' Developed by: Zack Oyafuso, Sarah Friedman, Emily Markowitz, Liz Dawson
#' Date: Feb 2022
#' --------------------------------------

# Knowns ----------------------------------------------------------------------

dir_googledrive <- "1AIQ0JEUA20D-g32uRQfRMZb0wW4SXl2n8Lwb_62uW-o"
access_googledrive <- TRUE
this_year <- "2021" # just doing this for proof of concept


# CHECK! - define here pages that you don't want to use the template for!
# non-template pages MUST be named with the web_page name listed in the 'comb' object

no_templ <- c("survey_team", "flight_itineraries", "checklist_in",
              "checklist_out", "checklist_end")


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

# create all pages -------------------------------------------------------------

comb <- comb %>% 
  dplyr::mutate(
    template = dplyr::case_when(
      page0 == "index" ~ FALSE, 
      sub_page0 %in% no_templ ~ FALSE, 
      TRUE ~ TRUE))


# clear_htmls() #removes all existing htmls in docs folder


for (jj in 1:nrow(comb)){
  
  page_desc <- "blank" # TOLEDO, need to change
  page_title <- ifelse(
    page_dat$sub_page[1]=="", 
    page_dat$page[1],
    stringr::str_to_title(paste0(page_dat$page[1], ' | ', 
                                 page_dat$sub_page[1])))
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
