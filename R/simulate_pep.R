utils::globalVariables(c("med_day", "i.med_day"))

#' Simulate Synthetic PEP Data
#'
#' Generates synthetic phenological observations by fitting a GAM smooth
#' (\code{day ~ s(year)}) per station-species-phase group and replacing the
#' observed \code{day} values with predictions plus Gaussian noise.  The
#' station-year-species-phase structure of the input data is kept intact.
#'
#' @param pep A \code{pep} object or \code{data.table} with at least the
#'   columns \code{species}, \code{phase_id}, \code{s_id}, \code{year}, and
#'   \code{day}.
#' @param min_obs Integer. Minimum number of observations required per
#'   station-species-phase group to attempt GAM fitting.
#'   Groups with fewer observations keep their original \code{day} values
#'   unchanged. Default is 20.
#' @param seed Integer. Random seed for reproducibility. Default is 42.
#' @param progress Logical. Show a progress bar? Default \code{TRUE}.
#'
#' @return A \code{\link[=new_pep]{pep}} object with the same rows and columns
#'   as the input.  The \code{day} column is overwritten with synthetic values
#'   for groups that meet the \code{min_obs} threshold; other rows retain their
#'   original values.
#'
#' @details
#' For each qualifying group (station x species x phase with at least
#' \code{min_obs} observations), a GAM is fitted and synthetic day-of-year
#' values are generated as \code{round(predicted + rnorm(n, sd = residual_sd))}.
#'
#' Groups where the GAM fit fails receive a fallback: the species-phase median
#' day plus Gaussian jitter (sd = 5 days).
#'
#' Groups below the \code{min_obs} threshold are left unchanged.  A message
#' reports how many rows were not simulated.
#'
#' @author Matthias Templ
#' @export
#' @examples
#' \dontrun{
#' # Download PEP data first
#' pep <- pep_download()
#'
#' # Use subset for faster simulation
#' pep_ch <- pep[country == "Switzerland"]
#'
#' # Generate synthetic data
#' pep_synth <- simulate_pep(pep_ch)
#'
#' # Use with analysis functions
#' normals <- pheno_normals(pep_synth, period = 1990:2015,
#'                          by = c("country", "phase_id"))
#' }
simulate_pep <- function(pep, min_obs = 20, seed = 42, progress = TRUE) {

  # Input validation
  if (!inherits(pep, "data.frame")) {
    stop("'pep' must be a data.frame or data.table", call. = FALSE)
  }

  required_cols <- c("species", "phase_id", "s_id", "year", "day")
  missing_cols <- setdiff(required_cols, names(pep))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  set.seed(seed)
  pep <- copy(pep)
  setDT(pep)

  # Grouping
  groups <- pep[, .N, by = .(species, phase_id, s_id)][N >= min_obs]
  n_groups <- nrow(groups)

  if (n_groups == 0) {
    warning("No groups have >= ", min_obs,
            " observations. Returning data unchanged.", call. = FALSE)
    return(new_pep(pep))
  }

  # Track how many rows are in qualifying groups
  n_total <- nrow(pep)

  # Init progress bar and time
  if (progress) {
    pb <- txtProgressBar(min = 0, max = n_groups, style = 3)
    start_time <- Sys.time()
  }

  # Loop over groups
  for (i in seq_len(n_groups)) {
    group <- groups[i]
    sub <- pep[species == group$species & phase_id == group$phase_id & s_id == group$s_id]

    fit <- try(mgcv::gam(day ~ s(year), data = sub), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      pred <- predict(fit, newdata = sub)
      res_sd <- sd(residuals(fit))
      pep[species == group$species & phase_id == group$phase_id & s_id == group$s_id,
          day := round(pred + rnorm(.N, sd = res_sd))]
    }

    # Progress update
    if (progress) {
      setTxtProgressBar(pb, i)

      if (i %% 500 == 0 || i == n_groups) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        avg_time <- elapsed / i
        remaining <- round(avg_time * (n_groups - i))
        message(sprintf(" | Est. time left: %ds", remaining))
      }
    }
  }

  if (progress) close(pb)

  # Fallback: replace NAs (rare/failed groups) with species-phase median + jitter
  medians <- pep[!is.na(day), .(med_day = median(day)), by = .(species, phase_id)]
  pep[medians, on = .(species, phase_id), med_day := i.med_day]
  pep[is.na(day) & !is.na(med_day), day := round(med_day + rnorm(.N, mean = 0, sd = 5))]
  pep[, med_day := NULL]

  # Report coverage
  all_groups <- pep[, .N, by = .(species, phase_id, s_id)]
  n_small <- sum(all_groups$N < min_obs)
  if (n_small > 0) {
    n_unchanged <- pep[all_groups[N < min_obs], on = .(species, phase_id, s_id), .N]
    message(sprintf(
      "Note: %d group(s) had fewer than %d observations. %d row(s) (%.1f%%) retain original day values.",
      n_small, min_obs, n_unchanged, 100 * n_unchanged / n_total
    ))
  }

  new_pep(pep)
}
