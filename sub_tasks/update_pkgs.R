folder <- "G:/RACE_Survey_App/files/Manuals/R/R Program/"
dir.create(folder, showWarnings = FALSE, recursive = TRUE)

files <- list.files(folder, pattern = "\\.tar\\.gz$", full.names = FALSE)
pkgs  <- unique(c(sub("_.+$", "", files), "RSQLite"))

# GAP GitHub packages
repo   <- "https://github.com/afsc-gap-products/"
# AKpkgs <- c("akgfmaps", "GAPsurvey", "akfishcondition", "navmaps", "gapctd")
AKpkgs <- c()

# CRAN package metadata (once)
cran_db <- available.packages()

# ---- Resolve dependencies properly ----
deps <- tools::package_dependencies(
  pkgs,
  db = cran_db,
  recursive = TRUE
)

all_pkgs <- unique(c(pkgs, unlist(deps), AKpkgs))

# ---- Pre-list existing files once ----
existing_files <- list.files(folder, full.names = TRUE)

# ---- Helper: build download info ----
get_pkg_info <- function(pkg) {
  if (pkg %in% AKpkgs) {
    list(
      url = paste0(repo, pkg, "/releases/latest/download/", pkg, ".tar.gz"),
      file = paste0(pkg, ".tar.gz")
    )
  } else if (pkg %in% rownames(cran_db)) {
    version <- cran_db[pkg, "Version"]
    repo_url <- cran_db[pkg, "Repository"]
    
    list(
      url  = paste0(repo_url, "/", pkg, "_", version, ".tar.gz"),
      file = paste0(pkg, "_", version, ".tar.gz")
    )
  } else {
    NULL
  }
}

# ---- Build download table ----
pkg_info <- lapply(all_pkgs, get_pkg_info)
pkg_info <- pkg_info[!vapply(pkg_info, is.null, logical(1))]

# ---- Remove old versions (vectorized) ----
old_files <- unlist(lapply(all_pkgs, function(pkg) {
  grep(paste0("^", pkg, "_|^", pkg, "\\.tar\\.gz"),
       existing_files,
       value = TRUE)
}))

if (length(old_files)) file.remove(old_files)

# ---- Download all packages ----
options(timeout = 300)

for (info in pkg_info) {
  message("Downloading: ", info$file)
  tryCatch(
    download.file(info$url,
                  destfile = file.path(folder, info$file),
                  mode = "wb",
                  quiet = TRUE),
    error = function(e) message("Failed: ", info$file)
  )
}

# ---- Update GAPsurvey docs ----
gitt <- paste0(repo, "GAPsurvey/archive/master.zip")
folderr <- "G:/RACE_Survey_App/files/Manuals/R/"

download.file(
  url = gitt,
  destfile = file.path(folderr, "Rprograms.zip"),
  mode = "wb"
)
