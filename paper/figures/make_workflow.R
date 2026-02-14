library(ggplot2)
library(grid)

# ============================================================
# Workflow diagram v7 — redesigned per feedback
# - No enclosing box around analysis groups
# - Standalone viz connects from pep object
# - Each analysis group notes plot() availability
# - pheno_plot/pheno_plot_hh shown under regional_box_ts
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
           arrow = arrow(length = unit(2.2, "mm"), type = "closed"))
}

# --- layout ---
# Row 1 (y ~ 8.5): Data Access -> pep -> Subsetting
# Row 2 (y ~ 5.5-7): Quality | Core | Advanced
# Row 3 (y ~ 3): Visualization on pep + Utilities
# Footer (y ~ 1.5): S3 note

# Box half-widths
w_da  <- 1.35
w_pep <- 1.05
w_sub <- 1.35
w_an  <- 2.15
w_viz <- 2.15

# X positions
x_da   <- 1.7
x_pep  <- 5.0
x_sub  <- 8.3

x_qual <- 2.3
x_core <- 6.75
x_adv  <- 11.2

x_viz  <- 2.3
x_util <- 9.0

# ---- build plot ----
p <- ggplot() +
  coord_cartesian(xlim = c(0, 13.6), ylim = c(0.8, 10.2), clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = bg, colour = NA),
        plot.margin = margin(8, 8, 8, 8))

# Title
p <- p +
  annotate("text", x = 6.8, y = 9.85, label = "pep725 Package Workflow",
           fontface = "bold", size = 5.0, colour = "grey20")

# ===== ROW 1: Data Access -> pep object -> Subsetting =====

# Data Access
p <- p +
  rrect(x_da - w_da, x_da + w_da, 7.85, 9.25, fill = alpha(col_data, 0.15), border = col_data) +
  annotate("text", x = x_da, y = 9.0, label = "Data Access",
           fontface = "bold", size = 3.7, colour = col_data) +
  annotate("text", x = x_da, y = 8.38,
           label = "pep_import()\npep_download()\nas.pep()\nsimulate_pep()",
           size = 2.5, colour = "grey25", lineheight = 0.85)

# pep object
p <- p +
  rrect(x_pep - w_pep, x_pep + w_pep, 8.0, 9.1, fill = alpha(col_pep, 0.15),
        border = col_pep, lwd = 1.0) +
  annotate("text", x = x_pep, y = 8.78, label = "pep object",
           fontface = "bold", size = 3.9, colour = col_pep) +
  annotate("text", x = x_pep, y = 8.3,
           label = "data.table + S3 class\nplot() | summary() | print()",
           size = 2.2, colour = "grey40", lineheight = 0.85)

# Subsetting
p <- p +
  rrect(x_sub - w_sub, x_sub + w_sub, 8.0, 9.1, fill = alpha(col_sub, 0.12), border = col_sub) +
  annotate("text", x = x_sub, y = 8.78, label = "Subsetting",
           fontface = "bold", size = 3.7, colour = "grey40") +
  annotate("text", x = x_sub, y = 8.32,
           label = "pep[species == ...]\npep[country %in% ...]\n\u2192 preserves pep class",
           size = 2.5, colour = "grey40", lineheight = 0.85)

# Row 1 arrows
p <- p +
  arr(x_da + w_da, 8.55, x_pep - w_pep, 8.55) +
  arr(x_pep + w_pep, 8.55, x_sub - w_sub, 8.55)

# ===== ROW 2: Three analysis groups (NO enclosing box) =====

# Arrows from Subsetting down to each group
p <- p +
  arr(x_sub - 0.8, 8.0, x_qual + 0.3, 7.32) +
  arr(x_sub,       8.0, x_core,        7.32) +
  arr(x_sub + 0.8, 8.0, x_adv - 0.3,  7.32)

# Quality & Validation
p <- p +
  rrect(x_qual - w_an, x_qual + w_an, 4.85, 7.28,
        fill = alpha(col_qual, 0.12), border = col_qual) +
  annotate("text", x = x_qual, y = 7.0, label = "Quality & Validation",
           fontface = "bold", size = 3.3, colour = col_qual) +
  annotate("text", x = x_qual, y = 5.95,
           label = "pep_quality()\npep_completeness()\ncoverage()\ncheck_phases()\nflag_outliers()  +  plot_outliers()\ndetect_second_events()\ncheck_connectivity()",
           size = 2.35, colour = "grey25", lineheight = 0.88) +
  annotate("text", x = x_qual, y = 5.05,
           label = "all results \u2192 plot() | summary() | print()",
           size = 2.0, colour = col_qual, fontface = "italic")

# Core Analysis
p <- p +
  rrect(x_core - w_an, x_core + w_an, 4.85, 7.28,
        fill = alpha(col_core, 0.12), border = col_core) +
  annotate("text", x = x_core, y = 7.0, label = "Core Analysis",
           fontface = "bold", size = 3.3, colour = col_core) +
  annotate("text", x = x_core, y = 6.15,
           label = "pheno_normals()\npheno_anomaly()\npheno_gradient()\npheno_synchrony()",
           size = 2.35, colour = "grey25", lineheight = 0.88) +
  annotate("text", x = x_core, y = 5.35,
           label = "all results \u2192 plot() | summary() | print()",
           size = 2.0, colour = col_core, fontface = "italic")

# Advanced Analysis
p <- p +
  rrect(x_adv - w_an, x_adv + w_an, 4.85, 7.28,
        fill = alpha(col_adv, 0.12), border = col_adv) +
  annotate("text", x = x_adv, y = 7.0, label = "Advanced Analysis",
           fontface = "bold", size = 3.3, colour = col_adv) +
  annotate("text", x = x_adv, y = 6.05,
           label = "pheno_combine()\npheno_trend_turning()\npls_phenology()\nregional_box_ts()\n   \u2192 pheno_plot()\n   \u2192 pheno_plot_hh()",
           size = 2.35, colour = "grey25", lineheight = 0.88) +
  annotate("text", x = x_adv, y = 5.05,
           label = "all results \u2192 plot() | summary() | print()",
           size = 2.0, colour = col_adv, fontface = "italic")

# ===== ROW 3: Visualization on pep + Utilities =====

# Dashed arrow from pep object down to Visualization
p <- p +
  annotate("segment", x = x_pep, y = 8.0, xend = x_pep, yend = 3.95,
           colour = col_viz, linewidth = 0.45, linetype = "dashed") +
  arr(x_pep, 3.95, x_viz + w_viz, 3.55, colour = col_viz)

# Visualization on pep
p <- p +
  rrect(x_viz - w_viz, x_viz + w_viz, 2.7, 3.9,
        fill = alpha(col_viz, 0.10), border = col_viz) +
  annotate("text", x = x_viz, y = 3.65, label = "Visualization (on pep)",
           fontface = "bold", size = 3.3, colour = col_viz) +
  annotate("text", x = x_viz, y = 3.1,
           label = "map_pep()   leaflet_pep()\npheno_plot_timeseries()",
           size = 2.35, colour = "grey25", lineheight = 0.88)

# Utilities
p <- p +
  rrect(x_util - 2.6, x_util + 2.6, 2.7, 3.9,
        fill = alpha("grey80", 0.12), border = "grey60") +
  annotate("text", x = x_util, y = 3.65, label = "Utilities",
           fontface = "bold", size = 3.3, colour = "grey45") +
  annotate("text", x = x_util, y = 3.1,
           label = "bbch_description()   calc_daylength()   calc_thermal_units()\nadd_country()   select_phase()   kendall_tau()",
           size = 2.35, colour = "grey25", lineheight = 0.88)

# ===== FOOTER =====
p <- p +
  annotate("text", x = 6.8, y = 2.0,
           label = "S3 design: every analysis function returns a classed object with dedicated print(), summary(), and plot() methods.\nFour vignettes provide reproducible workflows for data access, quality assessment, spatial analysis, and phenological patterns.",
           size = 2.5, colour = "grey35", fontface = "italic", lineheight = 0.9)

ggsave("paper/figures/workflow_diagram.png", p, width = 10, height = 7.2,
       dpi = 300, bg = "white")
cat("Done: workflow_diagram.png saved\n")
