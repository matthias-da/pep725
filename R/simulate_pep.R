#' Simulate synthetic pep data for a given phenology dataset
#' keeping station-year-species-phase structure intact
#'
#' @param pep A data.table with PEP-like structure
#' @param min_obs Minimum number of observations to allow simulation per group
#' @param seed Random seed
#' @param progress Logical. Show progress bar? Default TRUE.
#' @return data.table with same rows as pep, synthetic day values
#' @author Matthias Templ
#' @export
#' @examples
#' \dontrun{
#' # Download or load PEP data first
#' pep <- pep_download()
#'
#' # Generate synthetic data
#' pep_synth <- simulate_pep(pep)
#'
#' # Use with analysis functions
#' out <- regional_box_ts(pep = pep_synth,
#'                        species_name = "Triticum aestivum",
#'                        phase = 10)
#' }
simulate_pep <- function(pep, min_obs = 20, seed = 42, progress = TRUE) {
  library(data.table)
  library(mgcv)

  set.seed(seed)
  pep <- copy(pep)
  setDT(pep)

  # Output synthetic column
  pep[, doy_synth := NA_real_]

  # Grouping
  groups <- pep[, .N, by = .(species, phase_id, s_id)][N >= min_obs]
  n_groups <- nrow(groups)

  # Init progress bar and time
  if (progress) {
    pb <- txtProgressBar(min = 0, max = n_groups, style = 3)
    start_time <- Sys.time()
  }

  # Loop over groups
  for (i in seq_len(n_groups)) {
    group <- groups[i]
    sub <- pep[species == group$species & phase_id == group$phase_id & s_id == group$s_id]

    fit <- try(gam(day ~ s(year), data = sub), silent = TRUE)
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

  # Fallback: jitter for rare/failed groups
  pep[is.na(day), day := day + rnorm(.N, mean = 0, sd = 5)]

  return(pep)
}
