# Create search index from spreadsheet -----------------------------------------
# We map the columns you want to search to a consistent format
search_data_entries <- website_content %>%
  select(title, section = subsection , subtitle, url_loc) %>%
  mutate(source = "entries") %>%
  distinct(url_loc, .keep_all = TRUE)

search_data_taxa <- taxa_guides %>%
  select(title, section, subtitle = subsection, url_loc) %>%
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
