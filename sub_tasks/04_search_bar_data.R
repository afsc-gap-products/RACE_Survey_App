# Create search index from spreadsheet -----------------------------------------
# We map the columns you want to search to a consistent format
search_data_entries <- website_content %>%
  filter(!(grepl("data", url_loc) & (grepl("Tasklists", section) | grepl("Tasklists", subsection)))) %>%
  mutate(
    site_loc = if_else(!is.na(sub_page), paste(page, "→", sub_page), page),
    site_loc = if_else(!is.na(section), paste(site_loc, "→", section), site_loc),
    site_loc = if_else(subsection != "", paste(site_loc, "→", subsection), site_loc),
    search_terms = paste(subsection, "/", section)
  ) %>%
  select(site_loc, title, section = subsection, subtitle, url_loc, search_terms) %>%
  mutate(
    source = "entries",
    url_loc = if_else(grepl("Tasklists", site_loc), gsub("docs/", "", url_loc), url_loc)
  ) %>%
  distinct(url_loc, .keep_all = TRUE)


search_data_taxa <- taxa_guides %>%
  mutate(section = ifelse(is.na(section), "Inverts", section),
         site_loc = paste("Species Info → Species ID Guides →", section),
         site_loc = ifelse(is.na(subsection) | section == subsection, site_loc, paste(site_loc, "→", subsection)),
         search_terms = ifelse(is.na(subsection) | section == subsection, section, paste(subsection, "/", section))
         ) %>%
  select(site_loc, title, section, url_loc, search_terms) %>%
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

