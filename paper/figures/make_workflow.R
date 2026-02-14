library(ggplot2)
library(grid)

# ============================================================
# Workflow diagram v8 — updated function names + larger fonts
# - Consistent pep_/pheno_ naming convention
# - Increased font sizes for readability
# ============================================================

# --- colour palette ---
col_data    <- "#3B82F6"   # blue
col_pep     <- "#EF4444"   # red
col_sub     <- "#9CA3AF"   # gray
col_qual    <- "#F59E0B"   # amber
col_core    <- "#10B981"   # green
col_adv     <- "#047857"   # dark green
col_viz     <- "#8B5CF6"   # purple
col_arrow   <- "#374151"   # dark gray
bg          <- "white"

# --- helper: rectangle ---
rrect <- function(xmin, xmax, ymin, ymax, fill, border = "grey30",
                  lwd = 0.6) {
  annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
           fill = fill, colour = border, linewidth = lwd)
}

# --- helper: arrow ---
arr <- function(x, y, xend, yend, colour = col_arrow, lwd = 0.55) {
  annotate("segment", x = x, y = y, xend = xend, yend = yend,
           colour = colour, linewidth = lwd,
           arrow = arrow(length = unit(2.5, "mm"), type = "closed"))
}

# --- layout ---
# Row 1 (y ~ 9): Data Access -> pep -> Subsetting
# Row 2 (y ~ 5-7.5): Quality | Core | Advanced
# Row 3 (y ~ 3): Visualization on pep + Utilities
# Footer (y ~ 1.5): S3 note

# Box half-widths
w_da  <- 1.5
w_pep <- 1.15
w_sub <- 1.5
w_an  <- 2.45
w_viz <- 2.45

# X positions (wider canvas)
x_da   <- 1.9
x_pep  <- 5.5
x_sub  <- 9.1

x_qual <- 2.6
x_core <- 7.5
x_adv  <- 12.4

x_viz  <- 2.6
x_util <- 10.0

# ---- build plot ----
p <- ggplot() +
  coord_cartesian(xlim = c(0, 15.2), ylim = c(0.5, 10.8), clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = bg, colour = NA),
        plot.margin = margin(10, 10, 10, 10))

# Title
p <- p +
  annotate("text", x = 7.6, y = 10.4, label = "pep725 Package Workflow",
           fontface = "bold", size = 6.0, colour = "grey20")

# ===== ROW 1: Data Access -> pep object -> Subsetting =====

# Data Access
p <- p +
  rrect(x_da - w_da, x_da + w_da, 8.15, 9.75, fill = alpha(col_data, 0.15), border = col_data) +
  annotate("text", x = x_da, y = 9.45, label = "Data Access",
           fontface = "bold", size = 4.3, colour = col_data) +
  annotate("text", x = x_da, y = 8.75,
           label = "pep_import()\npep_download()\nas.pep()\npep_simulate()",
           size = 3.0, colour = "grey25", lineheight = 0.88)

# pep object
p <- p +
  rrect(x_pep - w_pep, x_pep + w_pep, 8.35, 9.55, fill = alpha(col_pep, 0.15),
        border = col_pep, lwd = 1.0) +
  annotate("text", x = x_pep, y = 9.22, label = "pep object",
           fontface = "bold", size = 4.6, colour = col_pep) +
  annotate("text", x = x_pep, y = 8.72,
           label = "data.table + S3 class\nplot() | summary() | print()",
           size = 2.7, colour = "grey40", lineheight = 0.88)

# Subsetting
p <- p +
  rrect(x_sub - w_sub, x_sub + w_sub, 8.35, 9.55, fill = alpha(col_sub, 0.12), border = col_sub) +
  annotate("text", x = x_sub, y = 9.22, label = "Subsetting",
           fontface = "bold", size = 4.3, colour = "grey40") +
  annotate("text", x = x_sub, y = 8.72,
           label = "pep[species == ...]\npep[country %in% ...]\n\u2192 preserves pep class",
           size = 2.9, colour = "grey40", lineheight = 0.88)

# Row 1 arrows
p <- p +
  arr(x_da + w_da, 8.95, x_pep - w_pep, 8.95) +
  arr(x_pep + w_pep, 8.95, x_sub - w_sub, 8.95)

# ===== ROW 2: Three analysis groups =====

# Arrows from Subsetting down to each group
p <- p +
  arr(x_sub - 0.9, 8.35, x_qual + 0.3, 7.72) +
  arr(x_sub,       8.35, x_core,        7.72) +
  arr(x_sub + 0.9, 8.35, x_adv - 0.3,  7.72)

# Quality & Validation
p <- p +
  rrect(x_qual - w_an, x_qual + w_an, 4.85, 7.68,
        fill = alpha(col_qual, 0.12), border = col_qual) +
  annotate("text", x = x_qual, y = 7.38, label = "Quality & Validation",
           fontface = "bold", size = 3.8, colour = col_qual) +
  annotate("text", x = x_qual, y = 6.2,
           label = "pep_quality()\npep_completeness()\npep_coverage()\npep_check_phases()\npep_flag_outliers() + pep_plot_outliers()\npep_second_events()\npep_check_connectivity()",
           size = 2.7, colour = "grey25", lineheight = 0.92) +
  annotate("text", x = x_qual, y = 5.1,
           label = "all results \u2192 plot() | summary() | print()",
           size = 2.4, colour = col_qual, fontface = "italic")

# Core Analysis
p <- p +
  rrect(x_core - w_an, x_core + w_an, 4.85, 7.68,
        fill = alpha(col_core, 0.12), border = col_core) +
  annotate("text", x = x_core, y = 7.38, label = "Core Analysis",
           fontface = "bold", size = 3.8, colour = col_core) +
  annotate("text", x = x_core, y = 6.45,
           label = "pheno_normals()\npheno_anomaly()\npheno_gradient()\npheno_synchrony()",
           size = 2.8, colour = "grey25", lineheight = 0.92) +
  annotate("text", x = x_core, y = 5.6,
           label = "all results \u2192 plot() | summary() | print()",
           size = 2.4, colour = col_core, fontface = "italic")

# Advanced Analysis
p <- p +
  rrect(x_adv - w_an, x_adv + w_an, 4.85, 7.68,
        fill = alpha(col_adv, 0.12), border = col_adv) +
  annotate("text", x = x_adv, y = 7.38, label = "Advanced Analysis",
           fontface = "bold", size = 3.8, colour = col_adv) +
  annotate("text", x = x_adv, y = 6.3,
           label = "pheno_combine()\npheno_trend_turning()\npheno_pls()\npheno_regional()\n   \u2192 pheno_plot()\n   \u2192 pheno_plot_hh()",
           size = 2.8, colour = "grey25", lineheight = 0.92) +
  annotate("text", x = x_adv, y = 5.1,
           label = "all results \u2192 plot() | summary() | print()",
           size = 2.4, colour = col_adv, fontface = "italic")

# ===== ROW 3: Visualization on pep + Utilities =====

# Dashed arrow from pep object down to Visualization
p <- p +
  annotate("segment", x = x_pep, y = 8.35, xend = x_pep, yend = 3.95,
           colour = col_viz, linewidth = 0.45, linetype = "dashed") +
  arr(x_pep, 3.95, x_viz + w_viz, 3.55, colour = col_viz)

# Visualization on pep
p <- p +
  rrect(x_viz - w_viz, x_viz + w_viz, 2.6, 3.9,
        fill = alpha(col_viz, 0.10), border = col_viz) +
  annotate("text", x = x_viz, y = 3.62, label = "Visualization (on pep)",
           fontface = "bold", size = 3.8, colour = col_viz) +
  annotate("text", x = x_viz, y = 3.05,
           label = "pheno_map()   pheno_leaflet()\npheno_plot_timeseries()",
           size = 2.8, colour = "grey25", lineheight = 0.88)

# Utilities
p <- p +
  rrect(x_util - 2.8, x_util + 2.8, 2.6, 3.9,
        fill = alpha("grey80", 0.12), border = "grey60") +
  annotate("text", x = x_util, y = 3.62, label = "Utilities",
           fontface = "bold", size = 3.8, colour = "grey45") +
  annotate("text", x = x_util, y = 3.05,
           label = "bbch_description()   calc_daylength()   calc_thermal_units()\nadd_country()   select_phase()   kendall_tau()",
           size = 2.7, colour = "grey25", lineheight = 0.88)

# ===== FOOTER =====
p <- p +
  annotate("text", x = 7.6, y = 1.7,
           label = "S3 design: every analysis function returns a classed object with dedicated print(), summary(), and plot() methods.\nFour vignettes provide reproducible workflows for data access, quality assessment, spatial analysis, and phenological patterns.",
           size = 2.9, colour = "grey35", fontface = "italic", lineheight = 0.9)

ggsave("paper/figures/workflow_diagram.png", p, width = 11.5, height = 8,
       dpi = 300, bg = "white")
cat("Done: workflow_diagram.png saved\n")
