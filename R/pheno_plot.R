#' Plot Phenological Trends and Climate Sensitivities
#'
#' Generates diagnostic plots from processed phenology–climate data. Supports three types of visualizations: multi-source time series comparison, smoothed phase–temperature trends using GAM or LOESS, and robust linear sensitivity analysis. Optional filtering by specific phenological phase is available.
#'
#' @param data_list A named list of prepared data objects, typically output from \code{\link{regional_box_ts}}. Must contain elements like \code{ts_tidy} and \code{pep_giss}.
#' @param type Type of plot to generate. One of \code{"timeseries"}, \code{"giss_smooth"}, or \code{"giss_sensitivity"}.
#' @param alpha_lines Alpha transparency for time series lines (default is \code{0.6}).
#' @param linewidth Line width for time series lines (default is \code{0.7}).
#' @param smooth Smoothing method for \code{type = "giss_smooth"}: either \code{"gam"} or \code{"loess"}.
#' @param span Smoothing span for LOESS (default is \code{0.3}).
#' @param gam_bs Basis type for GAM smoother. See \code{mgcv::s} for options (default is \code{"tp"}).
#' @param gam_k Optional \code{k} parameter (basis dimension) for GAM smoothing. If \code{NULL}, the default from \code{mgcv::gam} is used.
#' @param se Logical. Whether to include confidence bands in smoothed plots (default is \code{FALSE}).
#'
#' @return A \code{ggplot} object for visual inspection and further customization.
#'
#' @details
#' \itemize{
#'   \item \strong{type = "timeseries"}: Shows DOY trends by source (PEP725, MeteoSwiss) with faceting by phenophase and spatial scope.
#'   \item \strong{type = "giss_smooth"}: Plots phenological timing (DOY) against GISS global temperature anomalies, applying the selected smoother.
#'   \item \strong{type = "giss_sensitivity"}: Fits robust linear models (via \code{robustbase::lmrob}) to estimate phenological sensitivity (days/degC).
#' }
#' Use \code{phase_select} to restrict to one phenophase for clearer interpretation or focused analysis.
#'
#' @seealso \code{\link{regional_box_ts}}, \code{\link[mgcv]{gam}}, \code{\link[robustbase]{lmrob}}
#'
#' @examples
#' \dontrun{
#' # Requires pep_download() data - see regional_box_ts()
#' pep <- pep_download()
#' data(giss)
#'
#' # Using functional groups:
#' d1 <- regional_box_ts(pep, giss, phase = 10, year_min = 1950,
#'                       functional_group = "C4_summer_cereals",
#'                       pep_for_giss = "near",
#'                       lon_min = 8.0,  lon_max = 8.1,
#'                       lat_min = 44.7, lat_max = 47.9)
#' pC <- pheno_plot(d1, type = "giss_smooth")
#' pD <- pheno_plot(d1, type = "giss_sensitivity")
#' pC | pD
#'
#' # Without functional groups:
#' out <- regional_box_ts(pep, giss, species_name = "Triticum aestivum", phase = 10)
#' p2 <- pheno_plot(out, type = "giss_smooth", smooth = "gam")
#' p3 <- pheno_plot(out, type = "giss_sensitivity")
#' p2 | p3
#' }


#'
#' @import ggplot2
#' @importFrom mgcv gam
#' @importFrom robustbase lmrob
#' @author Matthias Templ
#' @export
pheno_plot <- function(
    data_list,
    type = c("timeseries", "giss_smooth", "giss_sensitivity"),
 #   phase_select = NULL,              # NEW: filter for a specific phase
    alpha_lines = 0.6,
    linewidth = 0.7,
    smooth = c("gam", "loess"),       # method for smoothing
    span = 0.3,                       # loess tuning
    gam_bs = "tp",                    # GAM basis
    gam_k = NULL,                     # optional GAM k
    se = FALSE                        # show confidence interval
) {
  type <- match.arg(type)
  smooth <- match.arg(smooth)
  phase <- data_list$phase

  # ── Timeseries Plot ──────────────────────────────────────────────────────
  if (type == "timeseries") {
    ts_tidy <- data_list$ts_tidy

    p <- ggplot(ts_tidy, aes(year, DOY, color = source)) +
      geom_line(aes(linetype = source), linewidth = linewidth, alpha = alpha_lines, na.rm = TRUE) +
      facet_grid(phase ~ panel, scales = "free_y") +
      #month_scale +
      labs(x = "", y = NULL, title = "Winter wheat phenology at/near Changins vs PEP725") +
      theme_bw() +
      theme(legend.position = "top") +
      scale_linetype_manual(values = c(
        "Obs. Changins (MeteoCH)"   = "dashed",
        "PEP725 (aggregated)"       = "solid",
        "PEP725 (near Changins)"    = "solid"
      ))
    return(p)
  }


  # --- Helper: flexible phase name mapping + warnings ---------------------
  phase_lookup <- c(
    `0`   = "Dry seed",
    `7`   = "Coleoptile emerged from caryopsis",
    `10`  = "Leaf development",
    `11`  = "First true leaf",
    `15`  = "True leaves",
    `30`  = "Stem elongation",
    `31`  = "Stem 10% of final length",
    `51`  = "Inflorescence or flower buds visible",
    `60`  = "Heading",
    `61`  = "Start of flowering: 10% of flowers open",
    `65`  = "Anthesis",
    `69`  = "End of flowering",
    `70`  = "Milk development",
    `75`  = "50% of fruits have reached final size",
    `80`  = "Dough development",
    `81`  = "Beginning of ripening or fruit colouration",
    `90`  = "Maturity",
    `95`  = "50% of leaves fallen",
    `100` = "Harvest",
    `111` = "First cut for silage winning",
    `131` = "First cut for hay winning",
    `151` = "Start of harvest for silage (corn, grass)",
    `205` = "Autumnal leaf colouring >= 50%"
  )
  phase_name <- function(id) {
    res <- as.character(phase_lookup[as.character(id)])
    if (any(is.na(res))) {
      warning(sprintf("Unmapped phase_id(s): %s",
                      paste(setdiff(id, as.integer(names(phase_lookup))), collapse = ", ")),
              call. = FALSE)
    }
    res
  }

  phase_select <- phase_name(phase)


  # ── GISS Smooth Plot ─────────────────────────────────────────────────────
  if (type == "giss_smooth") {
    pep_giss <- data_list$pep_giss
    phase_select <- data_list$phase_select
    if (!is.null(phase_select)) {
      pep_giss <- pep_giss[phase == phase_select]
    }

    # choose smoother
    layer_smooth <- switch(
      smooth,
      gam = {
        gam_formula <- if (is.null(gam_k)) {
          as.formula(sprintf("y ~ s(x, bs = '%s')", gam_bs))
        } else {
          as.formula(sprintf("y ~ s(x, bs = '%s', k = %d)", gam_bs, gam_k))
        }
        geom_smooth(method = mgcv::gam, formula = gam_formula, se = se)
      },
      loess = {
        geom_smooth(method = "loess", span = span, se = se)
      }
    )

    p <- ggplot(pep_giss, aes(dTgl, DOY, color = phase)) +
      geom_point(alpha = 0.8) +
      layer_smooth +
      # month_scale +
      labs(
        x = expression(paste(Delta, "T"[gl], " [", degree, "C]")),
        y = NULL,
        title = sprintf("PEP725 (aggregated): \n%s vs global temperature (GISS)",
                        ifelse(is.null(phase_select), "Phenology", phase_select))
      ) +
      theme_bw() +
      theme(legend.position = "top")

    return(p)
  }

  # ── GISS Sensitivity Plot ────────────────────────────────────────────────
  if (type == "giss_sensitivity") {
    pep_giss <- data_list$pep_giss
    if (!is.null(phase_select)) {
      pep_giss <- pep_giss[phase == phase_select]
    }

    # robust slopes (days / degC)
    slopes <- pep_giss[, {
      fit <- robustbase::lmrob(DOY ~ dTgl)
      .(slope = unname(coef(fit)[2]))
    }, by = phase]

    lbls <- slopes[, sprintf("%s: robust sensitivity = %.1f days/degC", phase, slope)]

    p <- ggplot(pep_giss, aes(dTgl, DOY, color = phase)) +
      geom_point(alpha = 0.8) +
      geom_smooth(method = "lmrob", se = FALSE) +
      # month_scale +
      labs(
        x = expression(paste(Delta, "T"[gl], " [", degree, "C]")),
        y = NULL,
        title = sprintf("Sensitivity of %s to global temperature",
                        ifelse(is.null(phase_select), "phenophases", phase_select))
      ) +
      theme_bw() +
      theme(legend.position = "top")


    # Single phase: add one label
    y_loc <- median(pep_giss$DOY, na.rm = TRUE)
    x_loc <- max(pep_giss$dTgl, na.rm = TRUE)
    p <- p + annotate("text",
                      x = x_loc,
                      y = y_loc,
                      hjust = 1, size = 4.2,
                      label = lbls)


    return(p)
  }
}





