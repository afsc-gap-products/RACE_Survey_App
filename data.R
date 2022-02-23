#' --------------------------------------
#' Project: RACE SURVEY APP
#' Developed by: Zach Oyafuso, Sarah Friedman, Emily Markowitz, Liz Dawson
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
# If your file is already open in a browser, you can obtain File ID from its link:
# https://docs.google.com/spreadsheets/d/***ThisIsFileID***/edit#gid=123456789

if (access_googledrive) {
  googledrive::drive_download(
    file = as_id(dir_googledrive),
    type = "csv",
    overwrite = TRUE,
    path = paste0("./data/survey_app_data.csv")
  )
}

full_site0 <- full_site <- readr::read_csv(file = "./data/survey_app_data.csv", 
                                           skip = 1)

full_site$url_web[is.na(full_site$url_web)] <- ""
full_site$url_loc[is.na(full_site$url_loc)] <- ""

# download links from online ---------------------------------------------------

temp <- unique(full_site0$url_web[!is.na(full_site0$url_web)]) # links to download
counter_pdf <- 0
for (i in 1:length(temp)) { ## Loop over URLs -- start
  
  # if downloading a png
  if (grepl(pattern = ".png", x = temp[i], fixed = TRUE)) {
    counter_pdf <- 1 + counter_pdf
    dest <- paste0("./downloaded/downloadedimg_", counter_pdf, ".pdf")
    download.file(url = temp[i], destfile = dest, mode = "wb")
    full_site$img_txt[full_site$url_web == temp[i]] <- 
      "Downloaded image from web"
    full_site$img[full_site$url_web == temp[i]] <- dest
  }
  
  # if download google doc
  if (grepl(pattern = "docs.google.com", x = temp[i])) {
    if (access_googledrive) {
      
      ## Access metadata of the google doc so that you can specify a
      ## name of the destination file
      metadata <- googledrive::drive_get(id = temp[i])
      type <- ifelse(test = grepl(pattern = "document", 
                                  x = temp[i], 
                                  ignore.case = TRUE),
                     yes = "docx",  ## Indicates a google doc
                     no = "csv"     ## Indicates a google spreadsheet?
      )
      dest <- paste0("./downloaded/", metadata$name, ".", type)
      
      ## Pull document from google drive, format it in the type specified,
      ## and write to dest path
      googledrive::drive_download(
        file = metadata$id,
        type = type,
        overwrite = TRUE,
        path = dest)
      
      ## Update the full_site info 
      full_site$url_loc_txt[full_site$url_web == temp[i]] <- 
        "Downloaded from google drive"
      full_site$url_loc[full_site$url_web == temp[i]] <- dest
    }
  }
  
  # if downloading a webpage HTML
  if (grepl(pattern = ".html", x = temp[i], fixed = TRUE)) {
    counter_pdf <- 1 + counter_pdf
    dest <- paste0("./downloaded/downloadedpdf_", counter_pdf, ".pdf")
    chrome_print(temp[i], output = dest)
    full_site$url_loc_txt[full_site$url_web == temp[i]] <- "web page downloaded from web as pdf"
    full_site$url_loc[full_site$url_web == temp[i]] <- dest
  }
  
  # if downloading a PDF from a webage
  # TOLEDO - need to make more accommodating to links without ".pdf" at the end
  if (grepl(pattern = ".pdf", x = temp[i], fixed = TRUE)) {
    counter_pdf <- 1 + counter_pdf
    dest <- paste0("./downloaded/downloadedpdf_", counter_pdf, ".pdf")
    download.file(temp[i],
                  dest,
                  mode = "wb")
    full_site$url_loc_txt[full_site$url_web == temp[i]] <- 
      "PDF downloaded from web"
    full_site$url_loc[full_site$url_web == temp[i]] <- dest
  }
}## Loop over URLs -- end


# wrangle data -----------------------------------------------------------------

# Clean up entries dataframe to format we need it for printing
full_site <- full_site %>%
  janitor::clean_names() %>%
  dplyr::filter(!is.na(page)) %>%
  # bind description bullets together
  # dplyr::mutate(across(starts_with('description'), ifelse())) %>% # TOLEDO, add periods to ends of sentences without them in description_ columns
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
                     no =  ""),
    
    # page and subpage management
    page = gsub( pattern = ".html", 
                 replacement = "",
                 x = page, 
                 fixed = TRUE),
    page = stringr::str_to_title(gsub(pattern = "_", 
                                      replacement = " ", 
                                      x = page, 
                                      fixed = TRUE)),
    page0 = tolower(gsub(pattern = " ", 
                         replacement = "_", 
                         x = page, 
                         fixed = TRUE)),
    sub_page = ifelse(test = is.na(sub_page), yes = "", no = sub_page),
    sub_page = stringr::str_to_title(gsub(pattern = "_",
                                          replacement = " ", 
                                          x = sub_page, 
                                          fixed = TRUE)),
    sub_page0 = tolower(gsub(pattern = " ", 
                             replacement = "_", 
                             x = sub_page, 
                             fixed = TRUE)),
    section = ifelse(test = is.na(section), yes = "", no = section),
    subsection = ifelse(test = is.na(subsection), yes = "", no = subsection),
    survey = ifelse(test = is.na(survey), yes = "", no = survey),
    in_survey_app = ifelse(test = is.na(in_survey_app), yes = FALSE, no = TRUE),
    
    # Images
    images = ifelse(test = is.na(img), 
                    yes = "", 
                    no = paste0("![*", img_txt, "*](", img, "){width='400px'}")
    ),
    
    # URL links
    url_web_txt = ifelse(test = is.na(url_web_txt) & !is.na(url_web), 
                         yes = "Web link", 
                         no = url_web_txt
    ),
    url_loc_txt = ifelse(test = is.na(url_loc_txt) & !is.na(url_loc),
                         yes = "Local link", 
                         no = url_loc_txt
    ),
    Links = ifelse(test = url_loc == "", 
                   yes = "",
                   no = paste0("[", url_loc_txt, "](", url_loc, ")")
    ),
    Links = ifelse(test = url_web == "", 
                   yes = Links,
                   no = paste0(Links, " \n\n [", url_web_txt, "](", url_web, ")")
    ),
    Links_inline = ifelse(test = Links == "", 
                          yes = "", 
                          no = paste0("Links: ", gsub(
                            pattern = " \n\n ",
                            replacement = ", ", 
                            x = Links ), "")
    )
  ) %>%
  dplyr::select(-starts_with("url_"), 
                -starts_with("img"), 
                -starts_with("description_")) %>%
  dplyr::arrange(page0, sub_page0) %>%
  dplyr::relocate(survey, page, page0, sub_page, sub_page0, section, 
                  subsection, title, subtitle, descrip, images, Links_inline)

# create general pages for each page if not already specified
for (jj in 1:length(unique(full_site$page0))) {
  page_dat <- full_site %>%
    dplyr::filter(page0 == unique(full_site$page0)[jj]) %>%
    dplyr::select(page, page0, sub_page, sub_page0) %>%
    dplyr::distinct()
  if (sum(is.na(page_dat$sub_page)) == 0) { # there is no info for a general page
    temp <- as_tibble(data.frame(matrix(data = "", 
                                        nrow = 1, 
                                        ncol = ncol(full_site))))
    names(temp) <- names(full_site)
    temp <- temp %>%
      dplyr::mutate(
        page = page_dat$page[1],
        page0 = page_dat$page0[1],
        in_survey_app = TRUE
      )
    
    full_site <- dplyr::bind_rows(full_site, temp)
  }
}

full_site <- full_site %>%
  dplyr::mutate(web_page = ifelse(test = (sub_page0 != ""),
                                  yes = paste0(page0, "_", sub_page0, ".html"),
                                  no = paste0(page0, ".html")
  ))

# subset of data we will actually use ------------------------------------------

site <- full_site %>%
  dplyr::filter(in_survey_app == TRUE) %>%
  dplyr::select(-in_survey_app) %>%
  dplyr::select(-starts_with("url_"), -starts_with("img"), -starts_with("description_")) %>%
  dplyr::distinct()


# Write yml --------------------------------------------------------------------

# find combinations of pages
comb <- site %>%
  dplyr::select(page0, page, sub_page0, sub_page, web_page) %>%
  dplyr::distinct() %>%
  dplyr::arrange(page0, sub_page0)

# Write _site.yml
site_yml <- base::readLines("_site_template.txt")

comb0 <- comb %>%
  dplyr::filter(page0 != "index")

a <- paste0(
  ifelse(comb0$sub_page0 == "", "    - ", "        - "),
  'text: "',
  ifelse(comb0$sub_page0 == "", comb0$page, comb0$sub_page), '"
',
  ifelse(comb0$sub_page0 == "", "      ", "          "),
  "href: ", comb0$web_page, "
",
ifelse(comb0$sub_page0 == "", "      menu:
", ""),
collapse = ""
)

site_yml <- gsub(
  pattern = "INSERT_NAVIGATION",
  replacement = a,
  x = site_yml, fixed = TRUE
)

# write new yml file
utils::write.table(
  x = site_yml,
  file = "_site.yml",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

# make comb neat :)
comb <- comb %>%
  dplyr::select(page0, page, sub_page0, sub_page, web_page)

## Remove misc variables
rm(comb0, full_site, metadata, temp, a, counter_pdf,
   dest, i, jj, site_yml, type)
