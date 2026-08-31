# ================================================================
# FILE INTEGRITY CHECKS FOR SURVEY APP CONTENT
# ================================================================
# This script:
# 1. Verifies that all files referenced in the survey app,
#    task list, and taxa guides exist in the /files directory.
# 2. Checks "Annual Updates" file maintenance status.
# 3. Identifies files in /files that are NOT referenced anywhere
#    in the app (including indirectly via folders).
# 4. Detects duplicate filenames and files (by hashes) across different directories.
# 5. Detects files that have not been updated in the last 10 years
# ================================================================


# Recommendation to run Steps 1 & 2 annually. Other steps can be run as needed/desired.


# ----------------------------------------------------------------
# FUNCTION: standardize file paths for consistent comparison
# ----------------------------------------------------------------
clean_paths <- function(x, base = "./files") {
  x |>
    gsub("\\\\", "/", x = _) |>                         # Windows → Unix slashes
    gsub("^\\./", "", x = _) |>                         # remove leading "./"
    gsub("//+", "/", x = _) |>                          # collapse repeated slashes
    (\(p) normalizePath(p, winslash = "/", mustWork = FALSE))() |>
    gsub(paste0("^", normalizePath(base, winslash = "/")), "", x = _) |>
    gsub("^/", "", x = _) |>                              # ensure relative paths
    tolower()
}


# ----------------------------------------------------------------
# STEP 1: Validate that all app links point to real files
# ----------------------------------------------------------------
app_links <- list(
  website_content = website_content$url_loc[website_content$in_survey_app],
  task_list_data  = task_list_data$url_loc,
  taxa_guides     = taxa_guides$url_loc
)

results <- lapply(names(app_links), function(source_name) {
  checkLinks(URLs = app_links[[source_name]], quiet = TRUE)
})

names(results) <- names(app_links)

message("\nBroken links in survey_app_data spreadsheet:\n")
print(results)


# ----------------------------------------------------------------
# STEP 2: Annual update audit (missing + stale files)
# ----------------------------------------------------------------

# Download latest tracking sheet if needed
if (access_to_internet) {
  with_drive_quiet(
    googledrive::drive_download(
      file = googledrive::as_id("1V-jY808DWnWPu_90WDWBxW8Ih91XwlcGRWwzmopv9os"),
      path = "data/annual_updates.xlsx",
      overwrite = TRUE
      
    )
  )
}


# Load tracking table
annual_updates <- readxl::read_excel(
  path = "data/annual_updates.xlsx",
  sheet = "Files to update",
  range = "B3:I100",
  trim_ws = TRUE
) |>
  janitor::clean_names() |>
  dplyr::filter(status == "Updated") |>
  dplyr::select(file, path, importance, status) |>
  dplyr::filter(!is.na(path))


# Current year for comparison
current_year <- format(Sys.Date(), "%Y")

# File system metadata
file_info <- file.info(annual_updates$path)

# Combine metadata with tracking sheet
annual_audit <- annual_updates |>
  dplyr::mutate(
    modified = file_info$mtime,
    year = format(file_info$mtime, "%Y"),
    exists = !is.na(file_info$mtime),
    status_flag = dplyr::case_when(
      !exists ~ "missing",
      year != current_year ~ "stale",
      TRUE ~ "current"
    )
  )


message("\nBroken links in the annual audit spreadsheet:")

# Show only files needing attention
annual_audit |>
  dplyr::filter(status_flag != "current") |>
  dplyr::select(file, path, importance, status, status_flag, modified) |>
  print()



# ----------------------------------------------------------------
# Helpers to run the subsequent steps
# ----------------------------------------------------------------

# Function to exclude paths matching any of the specified patterns (case-insensitive)
exclude_paths <- function(paths, patterns) {
  regex <- paste0(tolower(patterns), collapse = "|")
  paths[!grepl(regex, tolower(paths))]
}


# List of manual exclusions for each check (case-insensitive patterns to exclude from each analysis)
EXCLUSIONS <- list(
  orphan = c(
    "collections/special_projects/",
    "speciesID/fishid",
    "travel/flight_itineraries/",
    "software/r/gapsurvey",
    "safety_and_health/accidents",
    "metis/required_files"
  ),

  duplicates = c(
    "software/r/gapsurvey",
    "collections/special_projects/",
    "computer_programs/timezero/",
    "computer_programs/olex_and_opencpn/",
    "training"
  ),

  old_files = c(
    "safety_and_health/accidents",
    "collections/special_projects/",
    "computer_programs/gps",
    "sensors/light_meters"
  )
)


# ----------------------------------------------------------------
# STEP 3: Identify paths that exceed character limit
# ----------------------------------------------------------------
# Windows limits file paths to 260 characters by default and will not copy files that exceed this limit, therefore the path will break for anyone that tries to copy the survey app to their computer. Rename these files to something shorter or reorganize to shorten the path and that will solve the issue

# All files on disk
all_files <- list.files("./files/", recursive = TRUE, full.names = TRUE)

exceeding_names <- all_files[nchar(all_files) > 230]

message("\nResources that exceed character limit:")
exceeding_names

# 
# 
# # ----------------------------------------------------------------
# # STEP 4: Identify files in /files not referenced in the app
# # ----------------------------------------------------------------
# 
# # remove hidden/temp/system files
# all_files_clean <- clean_paths(all_files)
# all_files_clean <- all_files_clean[
#   !grepl("(^|/)\\.", all_files_clean) &
#     !grepl("~$", all_files_clean) &
#     !grepl("thumbs\\.db$|desktop\\.ini$", all_files_clean)
# ]
# 
# 
# # All app-referenced files (combined)
# files_in_app <- clean_paths(unlist(app_links))
# 
# 
# # Split app references into:
# # - direct file references
# # - folder references (indirect links)
# app_files <- files_in_app[grepl("\\.[^./]+$", files_in_app)]
# app_folders <- sub("/$", "", files_in_app[!grepl("\\.[^./]+$", files_in_app)])
# app_folders <- app_folders[!is.na(app_folders)]
# 
# # Files not referenced anywhere in the app
# files_not_in_app <- all_files_clean[
#   grepl("\\.[^./]+$", all_files_clean) &          # keep real files only
#     !all_files_clean %in% app_files &             # exclude direct references
#     !vapply(all_files_clean, function(f) {        # exclude folder-based references
#       any(startsWith(f, paste0(app_folders, "/")))
#     }, logical(1))
# ]
# 
# # Optional: additional manual exclusions
# orphan_files <- exclude_paths(files_not_in_app, EXCLUSIONS$orphan)
# 
# 
# # list of files in app but not linked anywhere
# orphan_files
# 
# 
# # ----------------------------------------------------------------
# # STEP 5: Detect duplicate files by filename (case-insensitive)
# # ----------------------------------------------------------------
# 
# # files without manual exclusions for duplicates check
# trim_files   <- exclude_paths(all_files_clean, EXCLUSIONS$duplicates)
# 
# # Group by filename
# dup_list <- split(trim_files, basename(trim_files))
# 
# # Keep only duplicates
# dup_list <- dup_list[sapply(dup_list, length) > 1]
# 
# # Sort each group by directory for easier cleanup
# dup_list <- lapply(dup_list, function(paths) {
#   paths[order(dirname(paths))]
# })
# 
# # Sort list alphabetically by filename
# dup_filenames <- dup_list[order(names(dup_list))]
# 
# # list of duplicate files by name
# dup_filenames
# 
# 
# # ----------------------------------------------------------------
# # STEP 5b: Detect duplicate files by hashes
# # ----------------------------------------------------------------
# 
# file_paths <- paste0("./files/", trim_files)
# 
# file_hashes <- vapply(file_paths, function(f) {
#   if (!file.exists(f)) return(NA_character_)
# 
#   size <- file.info(f)$size
# 
#   con <- file(f, "rb")
#   raw <- readBin(con, what = "raw", n = size)
#   close(con)
# 
#   digest::digest(raw, algo = "md5")
# }, character(1))
# 
# 
# dup_df <- data.frame(
#   path = file_paths,
#   hash = file_hashes,
#   size = file.info(file_paths)$size,
#   stringsAsFactors = FALSE
# ) %>%
#   filter(!is.na(hash))   # remove missing files
# 
# 
# dup_files <- dup_df %>%
#   group_by(hash) %>%
#   filter(n() > 1) %>%
#   ungroup() %>%
#   mutate(
#     file = basename(path),
#     folder = dirname(path)
#   ) %>%
#   group_by(hash) %>%
#   mutate(
#     group_id = cur_group_id(),
#     n_duplicates = n()
#   ) %>%
#   ungroup() %>%
#   arrange(file, group_id, folder) %>%
#   group_by(group_id) %>%
#   mutate(
#     keep = ifelse(folder == min(folder), "KEEP", "REVIEW")
#   ) %>%
#   ungroup() %>%
#   select(group_id, file, path, size, keep) %>%
#   arrange(group_id)
# 
# # list of duplicate files based on hashes
# View(dup_files)
# 
# 
# 
# # ----------------------------------------------------------------
# # STEP 6: Files not updated in the past 10 years
# # ----------------------------------------------------------------
# 
# # File system metadata
# all_file_info <- file.info(all_files, ignore.case = TRUE)
# all_file_info <- data.frame(all_file_info) |>
#   tibble::rownames_to_column("path") |>
#   tibble()
# 
# old_files <- all_file_info |>
#   dplyr::mutate(year = as.numeric(format(mtime, "%Y"))) |>
#   filter(as.numeric(current_year) - year > 10) |>
#   select(path, year) |>
#   arrange(year) |>
#   filter(!grepl(
#     "Manuals/Globe/|Safety/Accidents/|
#      Collections/Special projects/|Manuals/GPSs|Manuals/Light meters/"
#   , path))
# 
# 
# View(old_files)
# 
# 
# # ----------------------------------------------------------------
# # AUDIT SUMMARY
# # ----------------------------------------------------------------
# 
# cat("
# ============================
# FILE AUDIT SUMMARY
# ============================
# 
# Orphan files: ", length(orphan_files), "
# Duplicate names: ", length(dup_filenames), "
# Duplicate hashes: ", nrow(dup_files), "
# Old files: ", nrow(old_files), "
# 
# ")