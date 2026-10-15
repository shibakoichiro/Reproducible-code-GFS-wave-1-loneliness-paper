# =============================================================================
# 05_main_figures.R
# Main-text Figures 1 and 2 (forest plots of country-specific estimates)
# Input : output/tbl3.output.csv (from 01) and output/tbl2.output.csv (from 02)
# Output: output/Figure1.pdf, output/Figure2.pdf (vector, one file per figure with
#         panels a and b, no legend) and PNG previews
# =============================================================================
#
# Packages: ggplot2, dplyr, readr, stringr, patchwork (all on CRAN).
# Alternating row shading is drawn with geom_rect, so ggforestplot is not needed.

library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(patchwork)

source("00_config.R")
in_dir  <- OUT_DIR
out_dir <- OUT_DIR

fmt2 <- function(x) formatC(x, digits = 2, format = "f")

clean_country <- function(x) {
  x <- str_trim(str_replace_all(x, "_", " "))
  if_else(str_detect(x, "rkiye|rkey"), "Turkey", x)
}

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------
dat1 <- read_csv(file.path(in_dir, "tbl3.output.csv"), show_col_types = FALSE) |>
  filter(Variable %in% c("GENDER", "Gender"), Category %in% c("Male", "Female")) |>
  transmute(Country = clean_country(COUNTRY), group = Category,
            yi = mean.pooled, ci.lb = ci.low, ci.ub = ci.up)

dat2 <- read_csv(file.path(in_dir, "tbl2.output.csv"), show_col_types = FALSE) |>
  filter(str_detect(Variable, "^Relationship with (mother|father)")) |>
  transmute(Country = clean_country(Country),
            group = if_else(str_detect(Variable, "mother"), "Mother", "Father"),
            yi = Est, ci.lb = ci.low, ci.ub = ci.up)

stopifnot(nrow(dat1) == 44, nrow(dat2) == 44)

# ---------------------------------------------------------------------------
# One forest-plot panel: estimates on the left, "Est (95% CI)" text on the right
# ---------------------------------------------------------------------------
base_size <- 7.5

forest_panel <- function(d, tag, title, xlab, xlim, ref_line = NULL) {
  title <- bquote(bold(.(tag)) ~ ~ .(title))
  d <- d |>
    arrange(yi) |>
    mutate(Country = factor(Country, levels = Country),
           row = row_number(),
           lab = paste0(fmt2(yi), " (", fmt2(ci.lb), ", ", fmt2(ci.ub), ")"))
  stripes <- d |> filter(row %% 2 == 0)

  p_plot <- ggplot(d, aes(y = Country)) +
    geom_rect(data = stripes, inherit.aes = FALSE,
              aes(xmin = -Inf, xmax = Inf, ymin = row - 0.5, ymax = row + 0.5),
              fill = "grey92") +
    { if (!is.null(ref_line)) geom_vline(xintercept = ref_line, linetype = "dashed",
                                         linewidth = 0.3, colour = "grey30") } +
    geom_linerange(aes(xmin = ci.lb, xmax = ci.ub), linewidth = 0.4) +
    geom_point(aes(x = yi), shape = 15, size = 1.6) +
    scale_x_continuous(limits = xlim, expand = expansion(mult = 0.01)) +
    scale_y_discrete(expand = expansion(add = 0.55)) +
    labs(x = xlab, y = NULL, title = title) +
    theme_classic(base_size = base_size) +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(colour = "black", face = "bold"),
          axis.text.x = element_text(colour = "black"),
          axis.line.x = element_line(linewidth = 0.3),
          axis.ticks.x = element_line(linewidth = 0.3),
          plot.title = element_text(size = base_size + 1, face = "plain",
                                    margin = margin(b = 3)),
          plot.title.position = "plot")

  p_text <- ggplot(d, aes(y = Country)) +
    geom_rect(data = stripes, inherit.aes = FALSE,
              aes(xmin = -Inf, xmax = Inf, ymin = row - 0.5, ymax = row + 0.5),
              fill = "grey92") +
    geom_text(aes(x = 0, label = lab), size = base_size / .pt, hjust = 0.5) +
    scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
    scale_y_discrete(expand = expansion(add = 0.55)) +
    labs(title = "Estimate (95% CI)") +
    theme_void(base_size = base_size) +
    theme(plot.title = element_text(size = base_size, hjust = 0.5,
                                    margin = margin(b = 3)))

  p_plot + p_text + plot_layout(widths = c(2.1, 1)) &
    theme(plot.margin = margin(1, 1, 1, 1))
}

# ---------------------------------------------------------------------------
# Panels (panel letter included in each panel title)
# ---------------------------------------------------------------------------

# Figure 1: mean frequency of loneliness by gender.
# x-axis trimmed to the range of the data (same range in both panels).
xlab1 <- "Mean frequency of loneliness (0 = never, 10 = always)"
xlim1 <- c(floor(min(dat1$ci.lb) * 2) / 2, ceiling(max(dat1$ci.ub) * 2) / 2)
panels <- list(
  Figure1a = forest_panel(filter(dat1, group == "Male"),   "a", "Men",   xlab1, xlim1),
  Figure1b = forest_panel(filter(dat1, group == "Female"), "b", "Women", xlab1, xlim1)
)

# Figure 2: associations of childhood relationships with parents
# (same x-axis range in both panels, trimmed to the data)
xlim2 <- c(floor(min(dat2$ci.lb) * 2) / 2, ceiling(max(dat2$ci.ub) * 2) / 2)
xlab2 <- "Regression coefficient (very/somewhat good vs very/somewhat bad)"
panels$Figure2a <- forest_panel(filter(dat2, group == "Mother"), "a",
                                "Relationship with mother", xlab2, xlim2, ref_line = 0)
panels$Figure2b <- forest_panel(filter(dat2, group == "Father"), "b",
                                "Relationship with father", xlab2, xlim2, ref_line = 0)

# ---------------------------------------------------------------------------
# Save: one file per figure, panels a and b stacked (120 mm x 188 mm,
# fits within one A4 page). Figure legends go in the manuscript, not here.
# ---------------------------------------------------------------------------
stack <- function(top, bottom) {
  wrap_elements(full = top) / plot_spacer() / wrap_elements(full = bottom) +
    plot_layout(heights = c(1, 0.03, 1))
}
figs <- list(
  Figure1 = stack(panels$Figure1a, panels$Figure1b),
  Figure2 = stack(panels$Figure2a, panels$Figure2b)
)

w <- 120; h <- 188  # mm
for (nm in names(figs)) {
  ggsave(file.path(out_dir, paste0(nm, ".pdf")), figs[[nm]], width = w, height = h,
         units = "mm", device = cairo_pdf)
  ggsave(file.path(out_dir, paste0(nm, "_preview.png")), figs[[nm]], width = w,
         height = h, units = "mm", dpi = 250, bg = "white")
}
