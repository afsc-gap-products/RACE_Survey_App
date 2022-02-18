## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", "googledrive", "janitor", "rmarkdown", "distill", "kableExtra")

for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}




# Sign into google drive-------------------------------------------------------

googledrive::drive_deauth()
googledrive::drive_auth() #only need to run this once per session
1


# Load Data --------------------------------------------------------------------
# How to find the google drive ID of a document
# Google Drive File ID is a unique identifier of the file on Google Drive. File IDs are stable throughout the lifespan of the file, even if the file name changes.
# 
# To locate the File ID, right-click on the name of the file, choose the Get Shareable Link option, and turn on Link Sharing if needed.
# 
# You will see the link with a combination of numbers and letters at the end, and what you see after `id =`  is the File ID.
# https://drive.google.com/open?id=***ThisIsFileID***
#   
# If your file is already open in a browser, you can obtain File ID from its link:
# https://docs.google.com/spreadsheets/d/***ThisIsFileID***/edit#gid=123456789 

googledrive::drive_download(file = as_id("1AIQ0JEUA20D-g32uRQfRMZb0wW4SXl2n8Lwb_62uW-o"), 
                            type = "csv", 
                            overwrite = TRUE, 
                            path = paste0("./survey_app_data.csv"))

full_site <- read_csv("./survey_app_data.csv", skip = 1) %>%
  clean_names()


site <- full_site %>%
  filter(in_survey_app == TRUE)



# Render Site ------------------------------------------------------------------

# ## we need a column in the spreadsheet to auto-generate proper htmls. Currently they are being manually generated and don't match up with spreadsheet; can use this loop when we have updated
# 
# sections <- unique(site$page)
# 
# 
# # Knit the HTML version
# for (i in 1:length(sections)) {
#   rmarkdown::render(paste0("./", sections[i], ".Rmd"),
#                     output_dir = "./docs/",
#                     output_file = paste0(sections[i], ".html"))
# }




rmarkdown::render(paste0("index.Rmd"),
                  output_dir = "./docs/",
                  output_file =  "index.html")

rmarkdown::render(paste0("survey_team.Rmd"),
                  output_dir = "docs/",
                  output_file =  "survey_team.html")

rmarkdown::render(paste0("flight_itineraries.Rmd"),
                  output_dir = "docs/",
                  output_file =  "flight_itineraries.html")

rmarkdown::render(paste0("safety.Rmd"),
                  output_dir = "docs/",
                  output_file = "safety.html")

rmarkdown::render(paste0("safety_work_environment.Rmd"),
                  output_dir = "docs/",
                  output_file =  "safety_work_environment.html")

rmarkdown::render(paste0("safety_covid.Rmd"),
                  output_dir = "docs/",
                  output_file =  "safety_covid.html")

# rmarkdown::render(paste0("gears.Rmd"),
#                   output_dir = "docs/",
#                   output_file =  "gears.html")
# 
# rmarkdown::render(paste0("special_projects.Rmd"),
#                   output_dir = "docs/",
#                   output_file =  "special_projects.html")
# 
# rmarkdown::render(paste0("id_guides.Rmd"),
#                   output_dir = "docs/",
#                   output_file =  "id_guides.html")
# 
# rmarkdown::render(paste0("manuals.Rmd"),
#                   output_dir = "docs/",
#                   output_file =  "manuals.html")


