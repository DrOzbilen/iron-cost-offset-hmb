# =============================================================================
# IRON COST-OFFSET MODEL FOR HEAVY MENSTRUAL BLEEDING (HMB)
# Figure code - R version (base R graphics, no external packages)
# =============================================================================
#
# Produces the two figures selected for the manuscript:
#   FIGURE 1 - Enriched heatmap: prevention vs intravenous iron.
#              Colour = probability prevention is cheaper (PSA); number = fold difference.
#   FIGURE 2 - Break-even intravenous iron price across all four PBAC scenarios (line chart).
#
# Both are saved as 300-dpi PNG and PDF. Uses only base R + grDevices.
# =============================================================================


# ---- Shared parameters (current model) ----
MLP <- 0.80    # menstrual blood loss (mL) per PBAC point
CYC <- 13      # cycles per year
IV  <- 8.25    # IV iron cost (TL/mg)
FE  <- 0.43    # population blood iron content (mg/mL)

PBACS <- c(200, 400, 800, 1000)

# Prevention methods: name, MBL reduction, annual cost (TL), colour
methods <- c("LNG-IUS", "NSAID", "TXA", "COC (LNG/EE)", "COC (E2V/DNG)")
mbl     <- c(0.82, 0.35, 0.45, 0.45, 0.71)
tl      <- c(862,  709,  2345, 4556, 7344)
cols    <- c("#1b6ca8", "#2a9d3a", "#e8853a", "#9b59b6", "#c0392b")

# Prevented-mg cost = annual cost / (MBL reduction x annual iron loss)
prevented_mg <- function(i, pbac) tl[i] / (mbl[i] * FE * MLP * pbac * CYC)


# =============================================================================
# FIGURE 1 - Enriched heatmap: probability (colour) + fold difference (number)
# =============================================================================
# Colour encodes the PSA probability that prevention is cheaper than IV;
# the large number is the median fold difference. Two data values per cell.
#
# NOTE: the probability values below come from the PSA. Replace them with the
# output of YOUR run of the main calculation script (HMB_calc.R) so that the
# figure is consistent with the "R" methods statement. The fold values are
# point estimates and do not change between runs.

prob <- matrix(c(
  100, 100, 100, 100,   # LNG-IUS
  100, 100, 100, 100,   # NSAID
   86, 100, 100, 100,   # TXA
   18,  88, 100, 100,   # COC (LNG/EE)
   15,  88, 100, 100),  # COC (E2V/DNG)
  nrow = 5, byrow = TRUE)

fold <- matrix(c(
   7, 14, 28, 35,
   4,  7, 15, 18,
   1,  3,  6,  7,
   1,  1,  3,  4,
   1,  1,  3,  4),
  nrow = 5, byrow = TRUE)

# Yellow-green colour ramp (light = low probability, dark = high probability)
ramp <- colorRampPalette(c("#ffffe5", "#78c679", "#004529"))

draw_figure1 <- function() {
  par(mar = c(4, 8, 4, 6), xpd = FALSE)
  nr <- nrow(prob); nc <- ncol(prob)

  # empty plotting area with cell coordinates (row 1 at top)
  plot(NA, xlim = c(0.5, nc + 0.5), ylim = c(0.5, nr + 0.5),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  pal <- ramp(101)   # probability 0..100 -> palette index 1..101
  for (i in 1:nr) {
    yy <- nr - i + 1                       # flip so row 1 is at the top
    for (j in 1:nc) {
      p <- prob[i, j]
      rect(j - 0.5, yy - 0.5, j + 0.5, yy + 0.5,
           col = pal[round(p) + 1], border = "white", lwd = 2)
      txtcol <- if (p > 60) "white" else "#222222"
      # fold difference (large, upper)
      text(j, yy + 0.12, sprintf("%d\u00d7", fold[i, j]),
           col = txtcol, font = 2, cex = 1.25)
      # probability (small, lower)
      text(j, yy - 0.22, sprintf("%d%% < IV", p),
           col = txtcol, cex = 0.70)
    }
  }

  # axis labels
  axis(2, at = nr:1, labels = methods, las = 1, tick = FALSE, cex.axis = 0.95)
  axis(1, at = 1:nc, labels = paste("PBAC", PBACS), tick = FALSE, cex.axis = 0.95)

  title(main = "Prevention versus IV iron: probability (colour) and fold difference (number)",
        cex.main = 1.0, font.main = 2)
  mtext("Bleeding severity", side = 1, line = 2.5, cex = 0.95)

  # colour bar (right side)
  par(xpd = TRUE)
  bar_x <- nc + 0.75; bar_w <- 0.18
  bar_y0 <- 1; bar_y1 <- nr
  ny <- 100
  for (k in 1:ny) {
    yb <- bar_y0 + (bar_y1 - bar_y0) * (k - 1) / ny
    yt <- bar_y0 + (bar_y1 - bar_y0) * k / ny
    rect(bar_x, yb, bar_x + bar_w, yt, col = pal[k], border = NA)
  }
  rect(bar_x, bar_y0, bar_x + bar_w, bar_y1, border = "#888888")
  text(bar_x + bar_w + 0.05, bar_y0, "0",   cex = 0.7, pos = 4)
  text(bar_x + bar_w + 0.05, bar_y1, "100", cex = 0.7, pos = 4)
  text(bar_x + bar_w / 2, bar_y1 + 0.35, "P(<IV) %", cex = 0.7, font = 2)
}

png("Figure1_heatmap.png", width = 8, height = 5.2, units = "in", res = 300)
draw_figure1()
dev.off()

pdf("Figure1_heatmap.pdf", width = 8, height = 5.2)
draw_figure1()
dev.off()

cat("Figure 1 (enriched heatmap) saved.\n")


# =============================================================================
# FIGURE 2 - Break-even IV price across all four PBAC scenarios (line chart)
# =============================================================================
# The break-even IV price equals the prevented-mg cost: the IV price at which
# prevention just stops being cheaper. The actual IV price (8.25) sits far
# above every curve except COC at the lowest PBAC (shown honestly).

draw_figure2 <- function() {
  par(mar = c(5, 5, 4, 8), xpd = FALSE)

  # empty plot
  plot(NA, xlim = c(160, 1010), ylim = c(0, 12.5),
       xlab = "PBAC score (bleeding severity)",
       ylab = "Break-even IV price (TL/mg)",
       xaxt = "n", bty = "n", cex.lab = 1.1)
  axis(1, at = PBACS, labels = PBACS)

  # green shaded region: below the actual IV price -> prevention cheaper
  rect(160, 0, 1010, IV, col = rgb(0, 0.5, 0, 0.05), border = NA)

  # horizontal grid lines
  abline(h = seq(0, 12, 2), col = "#e5e5e5", lwd = 0.5)

  # actual IV price line
  abline(h = IV, col = "#c0392b", lty = 2, lwd = 2)
  text(1010, IV + 0.25, sprintf("Actual IV price %.2f TL/mg", IV),
       col = "#c0392b", font = 2, cex = 0.85, pos = 2)

  # one break-even curve per method
  for (i in seq_along(methods)) {
    y <- sapply(PBACS, function(p) prevented_mg(i, p))
    lines(PBACS, y, col = cols[i], lwd = 2.2)
    points(PBACS, y, col = cols[i], pch = 19, cex = 1.3)
    points(PBACS, y, col = "white", pch = 21, cex = 1.3, bg = cols[i], lwd = 0.8)
  }

  # legend (outside plot, right side)
  par(xpd = TRUE)
  legend(1035, 11, legend = methods, col = cols, lwd = 2.2, pch = 19,
         bty = "n", cex = 0.82, title = "Prevention method")

  # annotations
  text(175, IV * 0.5,
       "Green zone: break-even below\nactual IV price -> prevention cheaper",
       col = "#2a7d2a", cex = 0.7, font = 3, pos = 4)

  title(main = paste0("Break-even IV price: the level to which IV iron would have\n",
                      "to fall for prevention to lose its advantage"),
        cex.main = 1.05, font.main = 2)
}

png("Figure2_breakeven.png", width = 8.5, height = 5.5, units = "in", res = 300)
draw_figure2()
dev.off()

pdf("Figure2_breakeven.pdf", width = 8.5, height = 5.5)
draw_figure2()
dev.off()

cat("Figure 2 (break-even) saved.\n")
cat("\nBoth figures generated with base R (no external packages).\n")
