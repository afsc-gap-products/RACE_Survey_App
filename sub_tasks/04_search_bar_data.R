# Create search index from spreadsheet -----------------------------------------
# We map the columns you want to search to a consistent format
search_data_entries <- website_content %>%
  mutate(site_loc = paste(page, "→", sub_page, "→", section)) %>%
  select(site_loc, title, section = subsection , subtitle, url_loc) %>%
  mutate(site_loc = ifelse(section == "", site_loc, paste(site_loc, "→"))) %>%
  mutate(source = "entries") %>%
  distinct(url_loc, .keep_all = TRUE) %>% 
  filter(!(grepl("Tasklists", section) & grepl("data", url_loc))) %>%
  mutate(url_loc = ifelse(grepl("Tasklists", section), gsub("docs/", "", url_loc), url_loc))

search_data_taxa <- taxa_guides %>%
  mutate(site_loc = paste("Species Info → Species ID Guides →"),
         sec = ifelse(is.na(subsection), section, paste(section, "→", subsection))) %>%
  select(site_loc, title, section = sec, url_loc) %>%
  mutate(source = "guides") %>%
  distinct(url_loc, .keep_all = TRUE)


# Combine them
full_search_index <- bind_rows(search_data_entries, search_data_taxa) %>%
  mutate(url_loc = str_replace(url_loc, "./files/", "../files/")) %>%
  filter(!grepl("\\.\\.\\.", url_loc))

# Convert to JSON string
json_string <- jsonlite::toJSON(full_search_index, auto_unbox = TRUE, pretty = TRUE)

# Wrap it in a JavaScript variable assignment
js_content <- paste0("var searchData = ", json_string, ";")

# Save as a .js file
writeLines(js_content, "docs/js/search_data.js")
