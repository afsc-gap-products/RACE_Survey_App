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
# REPORT INITIALIZATION HELPERS
# ----------------------------------------------------------------
report_entries <- list()

add_section <- function(title) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat(" ", toupper(title), "\n")
  cat(" ", paste(rep("=", 60), collapse = ""), "\n\n")
}

print_status <- function(status_type, message_text) {
  symbol <- switch(status_type,
                   "PASS" = "[ PASS ]",
                   "WARN" = "[ WARN ]",
                   "FAIL" = "[ FAIL ]",
                   "INFO" = "[ INFO ]"
  )
  cat(sprintf("%-10s %s\n", symbol, message_text))
}

clean_paths <- function(x, base = "./files") {
  x |>
    gsub("\\\\", "/", x = _) |>
    gsub("^\\./", "", x = _) |>
    gsub("//+", "/", x = _) |>
    (\(p) normalizePath(p, winslash = "/", mustWork = FALSE))() |>
    gsub(paste0("^", normalizePath(base, winslash = "/")), "", x = _) |>
    gsub("^/", "", x = _) |>
    tolower()
}

exclude_paths <- function(paths, patterns) {
  regex <- paste0(tolower(patterns), collapse = "|")
  paths[!grepl(regex, tolower(paths))]
}

EXCLUSIONS <- list(
  orphan = c(
    "collections/special_projects/", "speciesID/fishid", 
    "travel/flight_itineraries/", "software/r/gapsurvey", 
    "safety_and_health/accidents", "metis/required_files"
  ),
  duplicates = c(
    "software/r/gapsurvey", "collections/special_projects/", 
    "computer_programs/timezero/", "computer_programs/olex_and_opencpn/", "training"
  ),
  old_files = c(
    "safety_and_health/accidents", "collections/special_projects/", 
    "computer_programs/gps", "sensors/light_meters"
  )
)

current_year <- format(Sys.Date(), "%Y")
last_year  <- as.numeric(current_year) - 1

# ================================================================
# START OF REPORT
# ================================================================

cat("\n")
cat("================================================================\n")
cat("            SURVEY APP CONTENT FILE INTEGRITY REPORT             \n")
cat("            Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("            Mode: ", ifelse(RUN_FULL_AUDIT, "FULL AUDIT", "STANDARD ROUTINE"), "\n")
cat("================================================================\n")


# ----------------------------------------------------------------
# STEP 1: Validate App Links
# ----------------------------------------------------------------
add_section("1. Survey App Links Validation")

app_links <- list(
  website_content = website_content$url_loc[website_content$in_survey_app],
  task_list_data  = task_list_data$url_loc,
  taxa_guides     = taxa_guides$url_loc
)

results <- lapply(names(app_links), function(source_name) {
  checkLinks(URLs = app_links[[source_name]], quiet = TRUE)
})
names(results) <- names(app_links)
filtered_results <- Filter(function(x) length(x) > 0, results)

if (length(filtered_results) > 0) {
  print_status("FAIL", "Broken links detected in survey_app_data spreadsheet:")
  print(filtered_results)
} else {
  print_status("PASS", "All spreadsheet links functional.")
}


# ----------------------------------------------------------------
# STEP 2: Annual Update Audit
# ----------------------------------------------------------------
add_section("2. Annual Updates Spreadsheet Audit")

if (exists("access_to_internet") && access_to_internet) {
  with_drive_quiet(
    googledrive::drive_download(
      file = googledrive::as_id("1V-jY808DWnWPu_90WDWBxW8Ih91XwlcGRWwzmopv9os"),
      path = "data/annual_updates.xlsx",
      overwrite = TRUE
    )
  )
  
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
  
  file_info <- file.info(annual_updates$path)
  
  annual_audit <- annual_updates |>
    dplyr::mutate(
      modified = file_info$mtime,
      year = format(file_info$mtime, "%Y"),
      exists = !is.na(file_info$mtime),
      status_flag = dplyr::case_when(
        !exists ~ "missing",
        year != current_year ~ "stale",
        grepl(last_year, path) ~ "outdated link",
        TRUE ~ "current"
      )
    )
  
  annual_audit_broken <- annual_audit |>
    dplyr::filter(status_flag != "current" & grepl("/", path)) |>
    dplyr::select(file, path, importance, status, status_flag, modified)
  
  if (nrow(annual_audit_broken) > 0) {
    print_status("WARN", sprintf("%d files in annual audit require attention:", nrow(annual_audit_broken)))
    print(as.data.frame(annual_audit_broken), row.names = FALSE)
  } else {
    print_status("PASS", "All annual audit links exist and are up to date.")
  }
} else {
  print_status("INFO", "Skipped: No internet access detected.")
}


# ----------------------------------------------------------------
# STEP 3: Path Length Audit
# ----------------------------------------------------------------
add_section("3. Character Length Limit Check (>230 chars)")

all_files <- list.files("./files/", recursive = TRUE, full.names = TRUE)
exceeding_names <- all_files[nchar(all_files) > 230]

if (length(exceeding_names) > 0) {
  print_status("WARN", sprintf("%d file paths exceed 230 characters:", length(exceeding_names)))
  print(exceeding_names)
} else {
  print_status("PASS", "All file paths are within safe character limits.")
}


# ----------------------------------------------------------------
# STEP 4: Last Year's Document Maintenance
# ----------------------------------------------------------------
add_section("4. Documents Needing Annual Rollover")

files_last_year <- all_files[grepl(last_year, basename(all_files)) &
                               !grepl(paste0("/", last_year, "/"), all_files)]

if (length(files_last_year) > 0) {
  print_status("WARN", sprintf("Found %d file(s) on disk referencing %d:", length(files_last_year), last_year))
  print(files_last_year)
} else {
  print_status("PASS", "No disk files found needing year updates.")
}

names_last_year <- website_content[grepl(last_year, website_content$title), 2:6]
if (nrow(names_last_year) > 0) {
  print_status("WARN", sprintf("Found %d website content title(s) referencing %d:", nrow(names_last_year), last_year))
  print(as.data.frame(names_last_year), row.names = FALSE)
} else {
  print_status("PASS", "No website titles found needing year updates.")
}


# ================================================================
# CONDITIONAL EXECUTION: STEPS 5 - 7
# ================================================================

orphan_files  <- character(0)
dup_filenames <- list()
dup_files     <- data.frame()
old_files     <- data.frame()

if (RUN_FULL_AUDIT) {
  
  # ----------------------------------------------------------------
  # STEP 5: Orphaned Files
  # ----------------------------------------------------------------
  add_section("5. Orphaned File Identification")
  
  all_files_clean <- clean_paths(all_files)
  all_files_clean <- all_files_clean[
    !grepl("(^|/)\\.", all_files_clean) &
      !grepl("~$", all_files_clean) &
      !grepl("thumbs\\.db$|desktop\\.ini$", all_files_clean)
  ]
  
  files_in_app <- clean_paths(gsub("\\.\\.", ".", na.omit(unlist(app_links))))
  app_files <- files_in_app[grepl("\\.[^./]+$", files_in_app)]
  app_folders <- sub("/$", "", files_in_app[!grepl("\\.[^./]+$", files_in_app)])
  app_folders <- app_folders[!is.na(app_folders)]
  
  files_not_in_app <- all_files_clean[
    grepl("\\.[^./]+$", all_files_clean) &
      !all_files_clean %in% app_files &
      !vapply(all_files_clean, function(f) {
        any(startsWith(f, paste0(app_folders, "/")))
      }, logical(1))
  ]
  
  orphan_files <- exclude_paths(files_not_in_app, EXCLUSIONS$orphan)
  
  if (length(orphan_files) > 0) {
    print_status("WARN", sprintf("Found %d unreferenced (orphan) files:", length(orphan_files)))
    print(orphan_files)
  } else {
    print_status("PASS", "No orphan files identified.")
  }
  
  # ----------------------------------------------------------------
  # STEP 6: Duplicate Files (Names & Hashes)
  # ----------------------------------------------------------------
  add_section("6. Duplicate Files Check")
  
  trim_files <- exclude_paths(all_files_clean, EXCLUSIONS$duplicates)
  dup_list   <- split(trim_files, basename(trim_files))
  dup_list   <- dup_list[sapply(dup_list, length) > 1]
  dup_list   <- lapply(dup_list, function(paths) paths[order(dirname(paths))])
  dup_filenames <- dup_list[order(names(dup_list))]
  
  if (length(dup_filenames) > 0) {
    print_status("WARN", sprintf("Found %d duplicate file names across directories:", length(dup_filenames)))
    print(dup_filenames)
  } else {
    print_status("PASS", "No duplicate file names found.")
  }
  
  file_paths  <- paste0("./files/", trim_files)
  file_hashes <- vapply(file_paths, function(f) {
    if (!file.exists(f)) return(NA_character_)
    size <- file.info(f)$size
    con  <- file(f, "rb")
    raw  <- readBin(con, what = "raw", n = size)
    close(con)
    digest::digest(raw, algo = "md5")
  }, character(1))
  
  dup_df <- data.frame(
    path = file_paths,
    hash = file_hashes,
    size = file.info(file_paths)$size,
    stringsAsFactors = FALSE
  ) |> dplyr::filter(!is.na(hash))
  
  dup_files <- dup_df |>
    dplyr::group_by(hash) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      file = basename(path),
      folder = dirname(path)
    ) |>
    dplyr::group_by(hash) |>
    dplyr::mutate(
      group_id = dplyr::cur_group_id(),
      n_duplicates = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(file, group_id, folder) |>
    dplyr::group_by(group_id) |>
    dplyr::mutate(
      keep = ifelse(folder == min(folder), "KEEP", "REVIEW")
    ) |>
    dplyr::ungroup() |>
    dplyr::select(group_id, file, path, size, keep) |>
    dplyr::arrange(group_id)
  
  if (nrow(dup_files) > 0) {
    print_status("WARN", sprintf("Found %d identical files (MD5 match):", nrow(dup_files)))
    print(as.data.frame(dup_files), row.names = FALSE)
  } else {
    print_status("PASS", "No identical content hashes detected.")
  }
  
  # ----------------------------------------------------------------
  # STEP 7: Files >10 Years Old
  # ----------------------------------------------------------------
  add_section("7. Stale Files (>5 Years Old)")
  
  all_file_info <- file.info(all_files, ignore.case = TRUE)
  all_file_info <- data.frame(all_file_info) |>
    tibble::rownames_to_column("path") |>
    tibble::as_tibble()
  
  old_files <- all_file_info |>
    dplyr::mutate(year = as.numeric(format(mtime, "%Y"))) |>
    dplyr::filter(as.numeric(current_year) - year > 5) |>
    dplyr::select(path, year) |>
    dplyr::arrange(year) |>
    dplyr::filter(!grepl(
      "Manuals/Globe/|Safety/Accidents/|Collections/Special projects/|Manuals/GPSs|Manuals/Light meters/", 
      path
    ))
  
  if (nrow(old_files) > 0) {
    print_status("WARN", sprintf("Found %d files modified over 5 years ago:", nrow(old_files)))
    print(as.data.frame(old_files), row.names = FALSE)
  } else {
    print_status("PASS", "No stale (>5yr) files found.")
  }
  
} else {
  add_section("5-7. Extended Integrity Audit")
  print_status("INFO", "Extended audit (Orphans, Duplicates, Old Files) was SKIPPED.")
  print_status("INFO", "To run extended checks, set `RUN_FULL_AUDIT <- TRUE` at script top.")
}

# ================================================================
# AUDIT SUMMARY EXECUTIVE BLOCK
# ================================================================

cat("\n")
cat("================================================================\n")
cat("                      AUDIT SUMMARY METRICS                     \n")
cat("================================================================\n")
cat(sprintf(" Exceeding Path Limits: %d\n", length(exceeding_names)))
cat(sprintf(" Rollover Files (Year): %d\n", length(files_last_year)))
cat(sprintf(" Rollover Titles (Year):%d\n", nrow(names_last_year)))

if (RUN_FULL_AUDIT) {
  cat(sprintf(" Orphan Files:          %d\n", length(orphan_files)))
  cat(sprintf(" Duplicate Names:       %d\n", length(dup_filenames)))
  cat(sprintf(" Duplicate Hashes:      %d\n", nrow(dup_files)))
  cat(sprintf(" Files >10 Years Old:   %d\n", nrow(old_files)))
} else {
  cat(" Extended Checks:       [ NOT RUN ]\n")
}
cat("================================================================\n\n")