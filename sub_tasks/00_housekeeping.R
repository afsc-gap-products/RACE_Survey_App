# ================================================================
# FILE INTEGRITY CHECKS FOR SURVEY APP CONTENT
# ================================================================
# This script:
# 1. Verifies that all files referenced in the survey app,
#    task list, and taxa guides exist in the /files directory.
# 2. Identifies files in /files that are NOT referenced anywhere
#    in the app (including indirectly via folders).
# 3. Detects duplicate filenames and files (by hashes) across different directories.
# 4. Checks "Annual Updates" file maintenance status.
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
    gsub("^/", "", x = _)                               # ensure relative paths
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
  dplyr::filter(!status %in% c("Not needed", "Not started")) |>
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
# all_files <- clean_paths(
#   list.files("./files/", recursive = TRUE, full.names = TRUE)
# )
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
# 
# # Files not referenced anywhere in the app
# files_not_in_app <- all_files[
#   grepl("\\.[^./]+$", all_files) &          # keep real files only
#     !grepl("(^|/)\\.", all_files) &         # remove hidden files/folders
#     !grepl("~$", all_files) &               # remove temp files
#     !all_files %in% app_files &             # exclude direct references
#     !vapply(all_files, function(f) {        # exclude folder-based references
#       any(startsWith(f, paste0(app_folders, "/")))
#     }, logical(1))
# ]
# 
# # Optional: additional manual exclusions
# orphan_files <- files_not_in_app[
#   !grepl(
#     "Collections/Special projects/|SpeciesID/|Travel/flight itineraries/|Thumbs.db|Manuals/R/GAPsurvey/|archive|Safety/Accidents/slide|Metis PC Required Directories and Control Files",
#     files_not_in_app
#   )
# ]
# 
# orphan_files
# 
# 
# # ----------------------------------------------------------------
# # STEP 4: Detect duplicate files by filename (case-insensitive)
# # ----------------------------------------------------------------
# 
# trim_files <- tolower(all_files)[
#   !grepl(
#     "Manuals/Globe/|Manuals/R/GAPsurvey/|
#      Trainings/Prior Years Training Powerpoints and Resources/|
#      Thumbs.db|archive|Manuals/MARPORT/|
#      Collections/Special projects/|Manuals/TimeZero/|Critical habitat/shapefiles/|
#      Manuals/Olex and OpenCPN/|Thumbs.db|Collections/Special projects/|Prior Years Training Powerpoints and Resources",
#     all_files
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
# View(dup_files)







