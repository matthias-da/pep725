utils::globalVariables(c("med_day", "i.med_day"))

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
#' # Download PEP data first
#' pep <- pep_download()
#'
#' # Use subset for faster simulation
#' pep_ch <- pep[country == "Switzerland"]
#'
#' # Generate synthetic data
#' pep_synth <- simulate_pep(pep_ch)
#'
#' # Use with analysis functions (grapevine has longest records)
#' # Load GISS data from hail package
#' data(giss, package = "hail")
#' out <- regional_box_ts(pep = pep_synth, giss = giss,
#'                        species_name = "Vitis vinifera",
#'                        phase = 65)
#' }
simulate_pep <- function(pep, min_obs = 20, seed = 42, progress = TRUE) {
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

  return(pep)
}
