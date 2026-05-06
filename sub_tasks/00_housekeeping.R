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
checkLinks(URLs = website_content$url_loc[website_content$in_survey_app])
checkLinks(URLs = task_list_data$url_loc)
checkLinks(URLs = taxa_guides$url_loc)


# ----------------------------------------------------------------
# STEP 2: Annual update audit (missing + stale files)
# ----------------------------------------------------------------

# Download latest tracking sheet if needed
if (access_to_internet) {
  googledrive::drive_download(
    file = googledrive::as_id("1V-jY808DWnWPu_90WDWBxW8Ih91XwlcGRWwzmopv9os"),
    path = "data/annual_updates.xlsx",
    overwrite = TRUE
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

# Show only files needing attention
annual_audit |>
  dplyr::filter(status_flag != "current") |>
  dplyr::select(file, path, importance, status, status_flag, modified) |>
  View()



# # ----------------------------------------------------------------
# # STEP 3: Identify files in /files not referenced in the app
# # ----------------------------------------------------------------
# 
# # All files on disk
# all_files <- list.files("./files/", recursive = TRUE, full.names = TRUE)
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
# files_in_app <- clean_paths(c(
#   website_content$url_loc[website_content$in_survey_app],
#   task_list_data$url_loc,
#   taxa_guides$url_loc
# ))
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
# orphan_files <- files_not_in_app[
#   !grepl(tolower(
#     "Collections/Special projects/|SpeciesID/|Travel/flight itineraries/|Manuals/R/GAPsurvey/|Safety/Accidents/slide|Metis PC Required Directories and Control Files"),
#     files_not_in_app
#   )
# ]
# 
# # list of files in app but not linked anywhere
# orphan_files
# 
# 
# # ----------------------------------------------------------------
# # STEP 4: Detect duplicate files by filename (case-insensitive)
# # ----------------------------------------------------------------
# 
# trim_files <- all_files_clean[
#   !grepl(tolower(
#     "Manuals/Globe/|Manuals/R/GAPsurvey/|Manuals/MARPORT/|
#      Collections/Special projects/|Manuals/TimeZero/|Critical habitat/shapefiles/|
#      Manuals/Olex and OpenCPN/|Collections/Special projects/|Prior Years Training Powerpoints and Resources"
#     ),
#     all_files_clean
#   )
# ]
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
# # STEP 4b: Detect duplicate files by hashes
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
# # STEP 5: Files not updated in the past 10 years
# # ----------------------------------------------------------------
# 
# # File system metadata
# all_file_info <- file.info(all_files, ignore.case = TRUE)
# 
# old_files <- data.frame(all_file_info) |>
#   tibble::rownames_to_column("path") |>
#   tibble() |>
#   dplyr::mutate(year = as.numeric(format(mtime, "%Y"))) |>
#   filter(as.numeric(current_year) - year > 10) |>
#   select(path, year) |>
#   arrange(year) |>
#     "Manuals/Globe/|Manuals/MARPORT/|Safety/Accidents/|
#      Collections/Special projects/|Manuals/GPSs|Manuals/Light meters/"
#   , path))
# 
# 
# View(old_files)
