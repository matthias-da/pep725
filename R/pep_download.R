#' Download Synthetic PEP725 Data
#'
#' Downloads a pre-generated synthetic version of PEP725 phenological data
#' from an external repository. The data is cached locally after the first
#' download to avoid repeated downloads.
#'
#' @param url Character string specifying the URL to download from.
#'   Default uses the official pep725 package data repository.
#' @param cache Logical. If \code{TRUE} (default), cache the downloaded data
#'   locally for future use.
#' @param force Logical. If \code{TRUE}, force re-download even if cached
#'   data exists. Default is \code{FALSE}.
#' @param quiet Logical. If \code{TRUE}, suppress download progress messages.
#'   Default is \code{FALSE}.
#'
#' @return A \code{pep} object containing synthetic phenological data with
#'   the same structure as original PEP725 data.
#'
#' @details
#' The synthetic data preserves the statistical properties and structure of
#' real PEP725 data (stations, species, phases, temporal patterns) while
#' ensuring data privacy. It is suitable for:
#' \itemize{
#'   \item Learning and testing package functions
#'   \item Reproducing examples from vignettes
#'   \item Developing analysis workflows before accessing real data
#' }
#'
#' The data is cached in a platform-appropriate location:
#' \itemize{
#'   \item Windows: \code{\%LOCALAPPDATA\%/R/pep725/}
#'   \item macOS: \code{~/Library/Caches/R/pep725/}
#'   \item Linux: \code{~/.cache/R/pep725/}
#' }
#'
#' @section Data Access:
#' If you have access to real PEP725 data, you can import it using
#' \code{\link{pep_import}} instead.
#'
#' @examples
#' \dontrun{
#' # Download synthetic data
#' pep <- pep_download()
#'
#' # Explore the data
#' print(pep)
#' summary(pep)
#'
#' # Force re-download
#' pep <- pep_download(force = TRUE)
#' }
#'
#' @seealso
#' \code{\link{simulate_pep}} for creating your own synthetic data,
#' \code{\link{pep_import}} for importing real PEP725 data
#'
#' @author Matthias Templ
#' @export
pep_download <- function(url = NULL,
                         cache = TRUE,
                         force = FALSE,
                         quiet = FALSE) {

 # Default URL - update this to your actual data repository
 if (is.null(url)) {
    url <- "https://github.com/matthias-da/pep725_releases/raw/main/downloads/v0.1.0/pep_synth.rda"
  }

  # Determine cache directory
 cache_dir <- tools::R_user_dir("pep725", which = "cache")
  cache_file <- file.path(cache_dir, "pep_synth.rda")

  # Check if cached version exists
  if (cache && file.exists(cache_file) && !force) {
    if (!quiet) {
      message("Loading cached synthetic PEP data from: ", cache_file)
    }

    pep <- tryCatch({
      env <- new.env()
      load(cache_file, envir = env)
      get(ls(env)[1], envir = env)
    }, error = function(e) {
      warning("Cached file appears corrupted. Re-downloading...", call. = FALSE)
      NULL
    })

    if (!is.null(pep)) {
      # Ensure it has the pep class
      if (!inherits(pep, "pep")) {
        class(pep) <- c("pep", class(pep))
      }
      return(pep)
    }
  }

  # Download the data
  if (!quiet) {
    message("Downloading synthetic PEP data from: ", url)
  }

  # Create temp file for download
  temp_file <- tempfile(fileext = ".rda")

  # Download with error handling
  download_success <- tryCatch({
    utils::download.file(
      url = url,
      destfile = temp_file,
      mode = "wb",
      quiet = quiet
    )
    TRUE
  }, error = function(e) {
    FALSE
  }, warning = function(w) {
    # download.file returns warnings for HTTP errors
    FALSE
  })
  if (!download_success || !file.exists(temp_file) || file.size(temp_file) == 0) {
    stop(
      "Failed to download synthetic PEP data.\n",
      "Please check your internet connection and try again.\n",
      "URL: ", url, "\n\n",
      "Alternatively, you can create synthetic data using simulate_pep() ",
      "if you have access to real PEP725 data.",
      call. = FALSE
    )
  }

  # Read the downloaded data
  pep <- tryCatch({
    env <- new.env()
    load(temp_file, envir = env)
    get(ls(env)[1], envir = env)
  }, error = function(e) {
    stop(
      "Downloaded file could not be read. It may be corrupted.\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })

  # Convert to data.table if needed
  if (!inherits(pep, "data.table")) {
    pep <- data.table::as.data.table(pep)
  }

  # Add pep class
  if (!inherits(pep, "pep")) {
    class(pep) <- c("pep", class(pep))
  }

  # Cache the data
  if (cache) {
    # Create cache directory if it doesn't exist
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    tryCatch({
      save(pep, file = cache_file)
      if (!quiet) {
        message("Data cached to: ", cache_file)
      }
    }, error = function(e) {
      warning("Could not cache data: ", e$message, call. = FALSE)
    })
  }

  # Clean up temp file
  unlink(temp_file)

  if (!quiet) {
    message("Download complete. ",
            format(nrow(pep), big.mark = ","), " observations loaded.")
  }

  pep
}


#' Clear Cached PEP Data
#'
#' Removes cached synthetic PEP data from the local cache directory.
#'
#' @param quiet Logical. If \code{TRUE}, suppress messages. Default is \code{FALSE}.
#'
#' @return Invisible \code{TRUE} if cache was cleared, \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' # Clear the cache
#' pep_cache_clear()
#'
#' # Next download will fetch fresh data
#' pep <- pep_download()
#' }
#'
#' @seealso \code{\link{pep_download}}
#'
#' @author Matthias Templ
#' @export
pep_cache_clear <- function(quiet = FALSE) {
  cache_dir <- tools::R_user_dir("pep725", which = "cache")
  cache_file <- file.path(cache_dir, "pep_synth.rda")

  if (file.exists(cache_file)) {
    unlink(cache_file)
    if (!quiet) {
      message("Cache cleared: ", cache_file)
    }
    return(invisible(TRUE))
  } else {
    if (!quiet) {
      message("No cached data found.")
    }
    return(invisible(FALSE))
  }
}


#' Get Cache Information
#'
#' Returns information about the cached PEP data.
#'
#' @return A list with cache information, or NULL if no cache exists.
#'
#' @examples
#' \dontrun{
#' # Check cache status
#' pep_cache_info()
#' }
#'
#' @author Matthias Templ
#' @export
pep_cache_info <- function() {
  cache_dir <- tools::R_user_dir("pep725", which = "cache")
  cache_file <- file.path(cache_dir, "pep_synth.rda")

  if (file.exists(cache_file)) {
    info <- file.info(cache_file)
    list(
      path = cache_file,
      size_mb = round(info$size / 1024^2, 2),
      modified = info$mtime,
      exists = TRUE
    )
  } else {
    list(
      path = cache_file,
      size_mb = NA,
      modified = NA,
      exists = FALSE
    )
  }
}
