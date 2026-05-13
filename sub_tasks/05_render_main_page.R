site_yml <- readLines("templates/_site_template.txt")

yaml_escape <- function(x) {
  x <- gsub('"', '\\"', x, fixed = TRUE)
  paste0('"', x, '"')
}

# Build navigation as character vector (one line per element)
nav_lines <- comb %>%
  dplyr::filter(!is.na(sub_page), sub_page != "") %>%
  dplyr::group_by(page) %>%
  dplyr::group_split() %>%
  purrr::map(function(df) {
    c(
      paste0("    - text: ", yaml_escape(df$page[1])),
      "      menu:",
      purrr::map2_chr(
        df$sub_page,
        df$web_page,
        ~ paste0(
          "        - text: ", yaml_escape(.x), "\n",
          "          href: ", yaml_escape(.y)
        )
      )
    )
  }) %>%
  unlist()

# Find placeholder line and replace it cleanly
insert_line <- which(site_yml == "INSERT_NAVIGATION")

site_yml <- append(
  site_yml[-insert_line],
  values = nav_lines,
  after = insert_line - 1
)

writeLines(site_yml, "templates/_site.yml")

rmarkdown::render(
  input = "templates/index.Rmd",
  output_dir = "docs",
  output_file = "index.html",
  quiet = TRUE
)