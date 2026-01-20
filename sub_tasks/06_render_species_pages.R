##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Render all of the taxa ID pages
## Author:        Sarah Friedman (sarah.friedman@noaa.gov)
## Note:          Takes a bit of time (~ 45-60 min) to run.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import data, fix url locations
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

species_data <- readr::read_csv(file = "data/species_id_data.csv") %>%
  dplyr::mutate(size_cm = as.character(x = size_cm),
                species_code = as.character(x = species_code)) %>%
  replace(is.na(.), "") 
species_data$url_loc <- gsub(x = species_data$url_loc, 
                            pattern = "./files/", 
                            replacement = "../files/")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Clean up data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grps <- species_data %>%
  dplyr::select(group_short, group_long) %>%
  unique() %>%
  dplyr::mutate(
    group_page = paste0("./zz_", 
                        janitor::make_clean_names(string = group_short), 
                        ".html"),
    link = paste0("[", group_long, "](", group_page, ")")
  ) %>%
  dplyr::arrange(group_short)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Render the ID by Taxa page which is embedded in the Guides subtab
##   in Species ID
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rmarkdown::render("templates/species_id_id_by_taxa.Rmd", 
                  output_dir = "docs/",
                  output_file = "Species_ID_ID_by_Taxa.html")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Render pages if remake_species_pages = TRUE
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (remake_species_pages) {
  for (k in 1:nrow(x = grps)) { ## Loop over groupings -- start
    group_data <- guides_data %>%
      dplyr::filter(group_short == grps$group_short[k])
    
    group_title <- janitor::make_clean_names(string = group_data$group_short[1])
    
    fam <- group_data$group_long[1]
    spp <- unique(x = group_data$species)
    spp <- spp[spp != "subgroup"]
    
    
    ## making subgroup pages
    subgrps <- unique(x = group_data$subgroup)
    subgrps[is.na(x = subgrps)] <- ""
    subgrps <- sort(x = subgrps, decreasing = TRUE) #blank subgrps last
    
    if (length(x = subgrps) > 1) { # generates subgroup level pages
      if(fam == "Rockfishes"){ # RFs are a strange case with nested subgroups
        cols <- c("red", "red/black", "black", "striped/spotted")
        for(ii in 1:length(x = cols)){
          fam_page <- TRUE
          subtitle <- stringr::str_to_title(string = cols[ii])
          rf_data <- group_data %>%
            tidyr::separate(subgroup, 
                            into = c("color_cat", "subgroup"), 
                            sep = " ", 
                            extra = "merge") %>%
            dplyr::mutate(
              subgroup = tolower(
                x = str_remove(string = subgroup, 
                               pattern = "category; |category")),
              subgroup = ifelse(test = is.na(x = subgroup), 
                                yes = "", 
                                no = subgroup)) 
          
          page_data <- rf_data %>%
            dplyr::filter(species != "subgroup") %>%
            dplyr::filter(tolower(x = color_cat) == cols[ii]) %>%
            dplyr::group_by(species) %>%
            dplyr::mutate(n = 1:n()) %>%
            dplyr::filter(n == 1)
          
          page <- paste0("zz_", group_title, "_", 
                         str_remove(string = cols[ii], pattern = "/"), 
                         ".html")
          fp <- FALSE
          
          subgrps <- unique(x = tolower(x = page_data$subgroup))
          
          rmarkdown::render("templates/template_family_pages.Rmd",
                            output_dir = "docs/",
                            output_file = page)
          rm(ncols, colnms, page)
          
          for (jj in 1:length(x = subgrps)){
            subtitle <- subgrps[jj]
            fam_page <- FALSE
            page_data <- rf_data %>%
              dplyr::filter(tolower(x = color_cat) == cols[ii]) %>%
              dplyr::filter(tolower(x = subgroup) == subgrps[[jj]])
            
            page <- paste0("zz_", group_title, "_", 
                           str_remove(string = cols[ii], pattern = "/"),
                           "_subpg_", jj, ".html")
            
            rmarkdown::render("templates/template_family_pages.Rmd",
                              output_dir = "docs/",
                              output_file = page)
            rm(ncols, colnms, page)
          }
        } 
      } else {
        
        fp <- FALSE
        fam_page <- FALSE
        for (kk in 1:length(x = subgrps)) {
          subtitle <- stringr::str_to_title(string = subgrps[kk])
          
          page_data <- group_data %>%
            dplyr::mutate(subgroup = ifelse(test = is.na(x = subgroup), 
                                            yes = "", 
                                            no = subgroup)) %>%
            dplyr::filter(subgroup == subgrps[kk]) %>%
            dplyr::group_by(species) %>%
            dplyr::mutate(n = 1:n()) %>%
            dplyr::filter(n == 1)
          
          page <- paste0("zz_", group_title, "_subpg", kk, ".html")
          
          rmarkdown::render("templates/template_family_pages.Rmd",
                            output_dir = "docs/",
                            output_file = page)
          
          rm(ncols, colnms, page)
        }
        
      }
    }
    
    # generates group-level page
    fam_page <- TRUE
    
    if (fam == "Rockfishes") {
      fp <- TRUE
      subtitle <- "Color Categories"
      page_data <- rf_data %>%
        dplyr::mutate(subgroup = tolower(x = color_cat))
      subgrps <- cols
      
    } else {
      
      if (all(grepl(pattern = group_data$family[[1]], 
                    x = group_data$family))) {
        subtitle <- group_data$family[1]
      } else {
        subtitle <- ""
      }
      
      page_data <- group_data
      
    }
    
    page <- paste0("zz_", group_title, ".html")
    
    rmarkdown::render(paste0("templates/template_family_pages.Rmd"),
                      output_dir = "docs/",
                      output_file = page)
    
    # making species pages
    for (ll in 1:length(x = spp)) {
      species_page <- group_data %>%
        dplyr::filter(species != "subgroup") %>%
        dplyr::filter(species == spp[ll])
      
      title <- stringr::str_to_title(species_page$common_name[1])
      subtitle <- paste0("*", species_page$species[1], "*")
      
      #trying to shorten file name
      sp_pg <- 
        stringr::str_replace(
          string = stringr::str_remove(string = species_page$species[1],
                                       pattern = "[a-z]+"),
          pattern = "\\s", 
          replacement = "")
      
      page <- paste0("zz_", group_title, "_", sp_pg, ".html")
      
      rmarkdown::render(paste0("templates/template_species_pages.Rmd"),
                        output_dir = "docs/",
                        output_file = page
      )
    }
  } ## Loop over groupings -- end
}  

rm(species_data, links)