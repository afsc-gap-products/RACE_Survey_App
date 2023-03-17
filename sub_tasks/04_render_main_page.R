##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Render the main page (index.html)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Write yaml script using a template and then going by the structure
##   of the comb dataframe
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site_yml <- base::readLines(con = "templates/_site_template.txt")

a <- ""
for (ipage in unique(x = comb$page)) {
  a <- paste(a, (paste0('   - text: "', ipage, '"
    ', '  menu: 
')))
  
  temp_subpages <- 
    subset(x = comb, subset = page == ipage)
  
  for (isubpage in 1:nrow(temp_subpages)) {
    a <- 
      paste(a, (paste0('      - text: "', temp_subpages$sub_page[isubpage], '"
    ', '     href: ', temp_subpages$web_page[isubpage], "
")))
  }
}

site_yml <- gsub(pattern = "INSERT_NAVIGATION",
                 replacement = a,
                 x = site_yml, 
                 fixed = TRUE)

# write new yml file
utils::write.table(x = site_yml,
                   file = "templates/_site.yml",
                   row.names = FALSE,
                   col.names = FALSE,
                   quote = FALSE)

rm(a, ipage, isubpage, site_yml, temp_subpages)

rmarkdown::render(input = "templates/index.Rmd",
                  output_dir = "docs/",
                  output_file =  "index.html")
