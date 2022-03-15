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
# If your file is already open in a browser, you can obtain File ID from its link:
# https://docs.google.com/spreadsheets/d/***ThisIsFileID***/edit#gid=123456789

if (access_to_internet) {
  googledrive::drive_download(
    file = as_id(dir_pagecontent),
    type = "csv",
    overwrite = TRUE,
    path = paste0("./data/survey_app_data.csv")
  )
  
  googledrive::drive_download(
    file = as_id(dir_species_guides),
    type = "csv",
    overwrite = TRUE,
    path = paste0("./data/id_guides_data.csv")
  )
}

full_site0 <- full_site <- readr::read_csv("./data/survey_app_data.csv", skip = 1)

full_site$url_web[is.na(full_site$url_web)] <- ""
full_site$url_loc[is.na(full_site$url_loc)] <- ""

# download links from online ---------------------------------------------------

if (access_to_internet) { # aka, access to internet?
  download_web_urls(dat = full_site, col_in = "url_web", dir_out = "./downloaded/")
}
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
                     no = ""),
    
    # page name management
    page = gsub(
      pattern = ".html",
      replacement = "",
      x = page,
      fixed = TRUE
    ),
    page = stringr::str_to_title(gsub(
      pattern = "_",
      replacement = " ",
      x = page,
      fixed = TRUE
    )),
    page0 = tolower(gsub(
      pattern = " ",
      replacement = "_",
      x = page,
      fixed = TRUE
    )),
    # subpage name management
    sub_page = ifelse(test = is.na(sub_page), yes = "", no = sub_page),
    sub_page = stringr::str_to_title(gsub(
      pattern = "_",
      replacement = " ",
      x = sub_page,
      fixed = TRUE
    )),
    sub_page0 = tolower(gsub(
      pattern = " ",
      replacement = "_",
      x = sub_page,
      fixed = TRUE
    )),
    # section and subsection name management
    section = ifelse(test = is.na(section), yes = "", 
                     no = #stringr::str_to_sentence
                     (section)),
    subsection = ifelse(test = is.na(subsection), yes = "", 
                        no = #stringr::str_to_sentence
                          (subsection)),
    # which surveys do each of these documents belong to?
    # srvy_all = ifelse(test = is.na(survey), yes = TRUE, no = FALSE),
    survey = ifelse(test = is.na(survey), 
                    yes = NA,
                    no = gsub(pattern = " ",
                              replacement = "",
                              fixed = TRUE,
                              x = paste0(",", survey,","))), # TOLEDO - can delete?
    # by having "svy_*" before it, we can use `dplyr::starts_with("svy_")`` to collect all of these columns
    srvy_ebs = ifelse(
      grepl(pattern = ",all,", x = survey, ignore.case = TRUE) |
        grepl(pattern = ",bs,", x = survey, ignore.case = TRUE) | 
                       grepl(pattern = ",ebs,", x = survey, ignore.case = TRUE), 
                     TRUE, FALSE), 
    srvy_nbs = ifelse(grepl(pattern = ",all,", x = survey, ignore.case = TRUE) | 
                       grepl(pattern = ",bs,", x = survey, ignore.case = TRUE) | 
                       grepl(pattern = ",nbs,", x = survey, ignore.case = TRUE), 
                     TRUE, FALSE), 
    srvy_bss = ifelse(grepl(pattern = ",all,", x = survey, ignore.case = TRUE) | 
                       grepl(pattern = ",bs,", x = survey, ignore.case = TRUE) | 
                       grepl(pattern = ",bss,", x = survey, ignore.case = TRUE), 
                     TRUE, FALSE), 
    srvy_ai = ifelse(grepl(pattern = ",all,", x = survey, ignore.case = TRUE) | 
                      grepl(pattern = ",ai,", x = survey, ignore.case = TRUE), 
                    TRUE, FALSE), 
    srvy_goa = ifelse(grepl(pattern = ",all,", x = survey, ignore.case = TRUE) |
                       grepl(pattern = ",goa,", x = survey, ignore.case = TRUE), 
                     TRUE, FALSE), 
    # in_survey_app = ifelse(in_survey_app == TRUE, TRUE, FALSE),
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
                   no = paste0("[", url_loc_txt, "](../", url_loc, ")")
    ),
    Links = ifelse(test = url_web == "",
                   yes = Links,
                   no = paste0(Links, " \n\n [", url_web_txt, "](../", url_web, ")")
    ),
    Links_inline = ifelse(test = Links == "",
                          yes = "",
                          no = paste0("Links: ", gsub(
                            pattern = " \n\n ",
                            replacement = ", ",
                            x = Links
                          ), "")
    )
  ) %>%
  dplyr::select(
    # -survey,
    -starts_with("url_"),
    -starts_with("img"),
    -starts_with("description_")
  ) %>%
  dplyr::arrange(page0, sub_page0) %>%
  dplyr::relocate(
    # survey, 
    page, page0, sub_page, sub_page0, section,
    subsection, title, subtitle, descrip, images, Links_inline, 
    dplyr::starts_with("svy_")
  )


# create survey-specific collection pages --------------------------------------
temp <- full_site %>% 
  dplyr::filter((page0 == "collections" &
                  sub_page0 == "specific_collections"))
# TOLEDO - will there be a time when there are no surveys id'ed for this

data_to_insert <- data.frame()
for (i in 1:nrow(temp)) {
  dat <- temp[i,] 
  temp1 <- dat %>% 
    dplyr::select(dplyr::starts_with("srvy_")) %>% 
    unlist()
  srvys <- gsub(pattern = "srvy_", x = names(temp1[which(temp1 == TRUE)]), replacement = "")
  # print(srvys) # check the line above is working
  if (length(srvys)==1) {
    data_to_insert <- dplyr::bind_rows(
      data_to_insert, 
      dat %>% 
        dplyr::mutate(sub_page0 = paste0(srvys, "_", sub_page0), 
                      sub_page = paste0(toupper(srvys), " ", sub_page)))
  } else {
    for (ii in 1:length(srvys)) {
      dat0 <- dat %>% 
          dplyr::mutate(sub_page0 = paste0(srvys[ii], "_", sub_page0), 
                        sub_page = paste0(toupper(srvys[ii]), " ", sub_page))
      dat0[, grepl(pattern = "srvy_", x = names(dat0))] <- FALSE
      dat0[, paste0("srvy_", srvys[ii])] <- TRUE      
      
      data_to_insert <- dplyr::bind_rows(
        data_to_insert, 
        dat0)
    }
  }
}

# only use srvy pages that are used this year
# not the most efficent, but wanna be done!
data_to_insert1 <- data.frame()
for (i in 1:nrow(data_to_insert)){
  for (ii in 1:length(this_year_surveys)) {
   if (grepl(pattern = this_year_surveys[ii], 
            x = data_to_insert$sub_page[i], 
            ignore.case = TRUE)) {
     data_to_insert1 <- dplyr::bind_rows(data_to_insert1, data_to_insert[i,])
   }
  }
}

full_site <- dplyr::bind_rows(
  full_site %>% 
    dplyr::filter(!(page0 == "collections" &
                  sub_page0 == "specific_collections")), 
  data_to_insert1) 
# need to think about how to remove srvy content we are not using (e.g., 2022 goa)


# create general pages for each page if not already specified ------------------
for (jj in 1:length(unique(full_site$page0))) {
  page_dat <- full_site %>%
    dplyr::filter(page0 == unique(full_site$page0)[jj]) %>%
    dplyr::select(page, page0, sub_page, sub_page0) %>%
    dplyr::distinct()
  if (sum(is.na(page_dat$sub_page)) == 0) { # there is no info for a general page
    temp <- as_tibble(data.frame(matrix(
      data = "",
      nrow = 1,
      ncol = ncol(full_site)
    )))
    names(temp) <- names(full_site)
    temp <- temp %>%
      dplyr::mutate(
        page = page_dat$page[1],
        page0 = page_dat$page0[1],
        order = as.numeric(order),
        across(dplyr::starts_with("srvy_"), as.logical),
        in_survey_app = TRUE
      )
    full_site <- dplyr::bind_rows(full_site, temp)
  }
}



## Remove rows not included in survey app (yet)
full_site <- subset(x = full_site, subset = in_survey_app == TRUE)


dir_pdfs <- make_clean_names(c("codebook", "emergency_flowchart",
                               "survey_protocol")) # link directly to a non-html file in drop down menu

no_templ <- janitor::make_clean_names(no_templ)

full_site <- full_site %>%
  dplyr::mutate(web_page = case_when(
    make_clean_names(title) %in% no_templ ~ 
      paste0(page0, "_", make_clean_names(title), ".html"), # direct link to htmls on subpages
    sub_page0 %in% dir_pdfs ~ html_to_link(Links), #create direct links to a file on menu
    sub_page0 == "" ~  paste0(page0, ".html"),
    TRUE ~ paste0(page0, "_", sub_page0, ".html")
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
  dplyr::filter(page0 != "index") %>%
  dplyr::distinct(across(page0:sub_page), .keep_all = TRUE)

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
rm(
  comb0, full_site, a, i, jj, site_yml
)
