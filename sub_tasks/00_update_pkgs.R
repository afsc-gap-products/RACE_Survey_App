# ==============================================================================
# Script: R Package Downloader & Archive Utility
# Description: Identifies existing local source packages, resolves their CRAN 
#              dependencies, clears outdated local archives, downloads updated 
#              CRAN and GitHub packages, and grabs additional documentation zip.
# ==============================================================================

# ---- 1. Directory Setup & Configuration ----

# Target folder for downloaded source packages (.tar.gz)
folder <- "files/software/r/library/"
# dir.create(folder, showWarnings = FALSE, recursive = TRUE)

# # Target folder for external GAPsurvey documentation (deprecated 2026)
# doc_folder <- "files/software/r/gapsurvey/"
# # dir.create(doc_folder, showWarnings = FALSE, recursive = TRUE)

# Base URL for custom GitHub package repositories (e.g., AFSC GAP products)
github_repo <- "https://github.com/afsc-gap-products/"

# Optional custom GitHub packages to include (currently empty)
# Example: AKpkgs <- c("akgfmaps", "GAPsurvey", "akfishcondition", "navmaps", "gapctd")
AKpkgs <- c()


# ---- 2. Identify Packages & Resolve Dependencies ----

# Extract unique package names from existing .tar.gz archives in the target folder, and ensure 'RSQLite' is included in the base set
existing_pkg_files <- list.files(folder, pattern = "\\.tar.gz$", full.names = FALSE)
base_pkgs <- unique(c(sub("_.+$", "", existing_pkg_files), "RSQLite"))

# Retrieve available CRAN packages database once for fast lookup
cran_db <- available.packages()

# Recursively resolve all CRAN dependencies for the base package list
deps <- tools::package_dependencies(
  base_pkgs,
  db = cran_db,
  recursive = TRUE
)

# Combine target base packages, their dependencies, and any extra GitHub packages
all_pkgs <- unique(c(base_pkgs, unlist(deps), AKpkgs))


# ---- 3. Helper Function: Construct Package Metadata ----

#' Generates a list with download URL and target file name for a given package name.
#' Checks GitHub releases first, then CRAN database. Returns NULL if not found.
get_pkg_info <- function(pkg) {
  if (pkg %in% AKpkgs) {
    list(
      url  = paste0(github_repo, pkg, "/releases/latest/download/", pkg, ".tar.gz"),
      file = paste0(pkg, ".tar.gz")
    )
  } else if (pkg %in% rownames(cran_db)) {
    version  <- cran_db[pkg, "Version"]
    repo_url <- cran_db[pkg, "Repository"]
    
    list(
      url  = paste0(repo_url, "/", pkg, "_", version, ".tar.gz"),
      file = paste0(pkg, "_", version, ".tar.gz")
    )
  } else {
    NULL
  }
}

# Build metadata list for all requested packages and remove NULL entries
pkg_info <- lapply(all_pkgs, get_pkg_info)
pkg_info <- pkg_info[!vapply(pkg_info, is.null, logical(1))]


# ---- 4. Remove Existing Archive Files ----

# Scan target directory for all existing files to clean up before downloading
existing_files <- list.files(folder, full.names = TRUE)

# Identify old matching .tar.gz package files
old_files <- unlist(lapply(all_pkgs, function(pkg) {
  grep(paste0("^", pkg, "_|^", pkg, "\\.tar\\.gz"), existing_files, value = TRUE)
}))

# Delete matching files if any exist
if (length(old_files) > 0) {
  file.remove(old_files)
}


# ---- 5. Download Packages ----

# Extend download timeout to 5 minutes to accommodate larger packages
options(timeout = 300)

for (info in pkg_info) {
  message("Downloading: ", info$file)
  tryCatch(
    download.file(
      url      = info$url,
      destfile = file.path(folder, info$file),
      mode     = "wb",
      quiet    = TRUE
    ),
    error = function(e) message("Failed: ", info$file)
  )
}


# # ---- 6. Download Extra GAPsurvey Documentation ----
# 
# # Download latest master branch archive for GAPsurvey into dedicated directory
# gitt_url <- paste0(github_repo, "GAPsurvey/archive/master.zip")
# 
# tryCatch(
#   download.file(
#     url      = gitt_url,
#     destfile = file.path(doc_folder, "Rprograms.zip"),
#     mode     = "wb"
#   ),
#   error = function(e) message("Failed to download GAPsurvey master archive.")
# )