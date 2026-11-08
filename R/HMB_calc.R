# =============================================================================
# IRON COST-OFFSET MODEL FOR HEAVY MENSTRUAL BLEEDING (HMB)
# Calculation code - R version
# =============================================================================
#
# PURPOSE: To compare the economic value of reducing blood loss (prevention)
#          versus replacing lost iron (replacement), expressed as cost per
#          milligram of iron.
#
# THE CODE HAS TWO PARTS:
#   A) POINT ESTIMATES : closed-form formulas, no randomness.
#      -> Produces the SAME result on every run (fully reproducible).
#      -> Yields the manuscript's iron-loss table, prevented-mg price,
#         fold differences, threshold, and savings %.
#
#   B) PROBABILISTIC SENSITIVITY ANALYSIS (PSA) : fixed-seed Monte Carlo.
#      -> Computes the PROBABILITY that one method is cheaper than another,
#         plus 95% credible intervals. N = 200,000 iterations.
#      -> Yields the manuscript's P(<IV), P(<oral), and worst-case test.
#
# NOTE: Part A is mathematically IDENTICAL to the Python (numpy) version.
#       In Part B, because R and Python use different random-number
#       generators, probabilities may vary by ~+-1 point; N = 200,000
#       renders this difference negligible.
# =============================================================================


# ///////////////////////////////////////////////////////////////////////////
# PARAMETERS  (all inputs in one place - edit here to change assumptions)
# ///////////////////////////////////////////////////////////////////////////

MLP  <- 0.80    # Menstrual blood loss (mL) per PBAC point  [conversion factor]
CYC  <- 13      # Cycles per year (365 days / 28 days)
IV   <- 8.25    # IV iron cost (TL/mg): 2 x FCM vial + 1 infusion procedure / 1000 mg
USD  <- 47.2    # Exchange rate: 1 USD = 47.2 TL

FE_TARGET <- 0.50   # Target-state blood iron content (mg/mL) - iron-replete individual
FE_POP    <- 0.43   # Population mean (midpoint of 0.36-0.50 range) - used in point est. & PSA

BOX     <- 152.62   # Oral iron (ferrous sulfate) box price (TL) [30 tablets]
ELEMBOX <- 2400     # Elemental iron per box (mg) [30 tb x 80 mg]

PBACS <- c(200, 400, 800, 1000)   # PBAC (bleeding severity) scenarios examined

# Prevention methods: for each, (MBL reduction fraction, annual cost in TL)
#   mbl = method's menstrual-blood-loss reduction fraction (Cochrane/RCT)
#   tl  = method's annual cost (real national drug tariffs + public-hospital fees)
PREV <- list(
  "LNG-IUS"     = list(mbl = 0.82, tl = 862),   # Device (2808 + 1500 insertion)/5 yr
  "NSAID"       = list(mbl = 0.35, tl = 709),   # Naproxen: 2x1 x 4 days x 13 cycles
  "TXA"         = list(mbl = 0.45, tl = 2345),  # Tranexamic acid: 3x1 x 4 days x 13 cycles
  "COC-LNG/EE"  = list(mbl = 0.45, tl = 4556),  # Cheap-moderate COC: daily, 13 packs/yr
  "COC-E2V/DNG" = list(mbl = 0.71, tl = 7344)   # Expensive-effective COC: daily, 13 packs/yr
)

# Method processing / display order
ORD <- c("LNG-IUS", "NSAID", "TXA", "COC-LNG/EE", "COC-E2V/DNG")

# Oral iron pathway: (fractional absorption, IV-switch fraction on failure)
#   f  = fraction of ingested iron absorbed (Stoffel 2017)
#   pf = fraction who cannot tolerate / do not respond and switch to IV (Okam 2016)
ORAL <- list(
  "daily"        = list(f = 0.163, pf = 0.30),   # daily dosing
  "alternate-day" = list(f = 0.218, pf = 0.25)   # alternate-day dosing (higher absorption)
)


# ///////////////////////////////////////////////////////////////////////////
# HELPER FUNCTIONS
# ///////////////////////////////////////////////////////////////////////////

# Numeric formatter (fixed decimals, standard dot notation)
fmt <- function(x, d = 2) format(round(x, d), nsmall = d, trim = TRUE)

# Annual menstrual iron loss (mg) = blood iron content x mL/PBAC x PBAC x cycles
iron_year <- function(pbac, fe) fe * MLP * pbac * CYC


# ///////////////////////////////////////////////////////////////////////////
# PART A) POINT ESTIMATES  (analytic, no randomness - always the same result)
# ///////////////////////////////////////////////////////////////////////////

cat(strrep("=", 76), "\n", sep = "")
cat("A) POINT ESTIMATES (closed-form; identical on every run)\n")
cat(strrep("=", 76), "\n\n", sep = "")

# --- Cost per lost mg for each replacement pathway ---
# Oral pathway blended-cost formula:
#   cost = (1 - pf) x [box_price / (elemental_iron x absorption)]  +  pf x IV
#   i.e. continuing patients pay absorption-adjusted oral cost;
#        those who fail (fraction pf) switch to expensive IV.
oral_cost <- sapply(ORAL, function(o) {
  (1 - o$pf) * (BOX / (ELEMBOX * o$f)) + o$pf * IV
})
cheapest <- min(c(oral_cost, IV))   # cheapest replacement pathway

cat("Replacement pathways (per lost mg):\n")
for (k in names(oral_cost)) {
  cat("  oral ", formatC(k, width = -14), " ",
      fmt(oral_cost[[k]]), " TL/mg  ($",
      fmt(oral_cost[[k]] / USD, 3), ")\n", sep = "")
}
cat("  IV                 ", fmt(IV), " TL/mg  ($", fmt(IV / USD, 3), ")\n", sep = "")
cat("  cheapest replacement = ", fmt(cheapest), " TL/mg\n\n", sep = "")

# --- Iron-loss table: Annual iron loss (target state, fe = 0.50) ---
cat("Iron-loss table - Annual iron loss (fe = ", fmt(FE_TARGET), ", target state):\n", sep = "")
for (p in PBACS) {
  yr <- FE_TARGET * MLP * p * CYC   # annual iron loss (mg)
  cat("  PBAC ", formatC(p, width = 4), ": ",
      MLP * p, " mL, ",
      FE_TARGET * MLP * p, " mg/cycle, ",
      round(yr), " mg/yr, ",
      "IV-equivalent ", round(yr * IV), " TL ($", round(yr * IV / USD), ")\n", sep = "")
}
cat("\n")

# --- Prevented-mg price (fe = 0.43, population) ---
# Formula: prevented_mg_cost = annual_cost / (MBL_reduction x annual_iron_loss)
cat("Prevented-mg price (TL/mg, fe = ", fmt(FE_POP), "):\n", sep = "")
cat("  ", formatC("Method", width = -14), sep = "")
for (p in PBACS) cat(formatC(paste0("PBAC", p), width = -10), sep = "")
cat("\n")

PMC <- list()   # store results (reused for fold difference and threshold)
for (m in ORD) {
  mbl <- PREV[[m]]$mbl
  tl  <- PREV[[m]]$tl
  row <- sapply(PBACS, function(p) tl / (mbl * iron_year(p, FE_POP)))
  PMC[[m]] <- row
  cat("  ", formatC(m, width = -14), sep = "")
  for (v in row) cat(formatC(fmt(v), width = -10), sep = "")
  cat("\n")
}

# --- Fold difference vs IV (point) = IV / prevented-mg-cost ---
cat("\n  Fold difference vs IV (how many times cheaper):\n")
for (m in ORD) {
  cat("  ", formatC(m, width = -14), sep = "")
  for (v in PMC[[m]]) cat(formatC(round(IV / v), width = -10), sep = "")
  cat("\n")
}

# --- Savings % (total-cost metric) ---
# Closed form: savings = MBL_reduction - annual_cost / (annual_iron_loss x IV)
cat("\n  Savings % (conservative metric valuing residual with IV):\n")
for (m in ORD) {
  mbl <- PREV[[m]]$mbl
  tl  <- PREV[[m]]$tl
  cat("  ", formatC(m, width = -14), sep = "")
  for (p in PBACS) {
    sav <- (mbl - tl / (iron_year(p, FE_POP) * IV)) * 100
    cat(formatC(fmt(sav, 1), width = -10), sep = "")
  }
  cat("\n")
}

# --- Threshold (point): is prevented-mg cost < cheapest replacement? ---
cat("\nThreshold - Is prevented-mg cost < cheapest replacement (", fmt(cheapest),
    ")?\n", sep = "")
for (m in ORD) {
  cat("  ", formatC(m, width = -14), sep = "")
  for (v in PMC[[m]]) cat(formatC(ifelse(v < cheapest, "YES", "no"), width = -10), sep = "")
  cat("\n")
}


# ///////////////////////////////////////////////////////////////////////////
# PART B) PROBABILISTIC SENSITIVITY ANALYSIS (PSA - fixed-seed Monte Carlo)
# ///////////////////////////////////////////////////////////////////////////

cat("\n", strrep("=", 76), "\n", sep = "")
cat("B) PROBABILISTIC SENSITIVITY ANALYSIS (PSA; fixed seed = 12345, N = 200,000)\n")
cat(strrep("=", 76), "\n", sep = "")

set.seed(12345)     # fixed seed for reproducibility
N <- 200000         # number of iterations

# --- Helper distribution functions (from mean and standard deviation) ---
# Beta distribution: for 0-1 proportions (absorption, MBL reduction, switch rate)
rbeta_ms <- function(m, s) {
  v <- s^2
  k <- m * (1 - m) / v - 1
  rbeta(N, m * k, (1 - m) * k)
}
# Gamma distribution: for positive costs (annual TL, box price)
rgamma_ms <- function(m, s) {
  rgamma(N, shape = (m / s)^2, scale = s^2 / m)
}

# --- Uncertainty distributions ---
# NOTE: The call order together with the seed determines the result; DO NOT REORDER.
fe_s  <- runif(N, 0.36, 0.50)                          # blood iron content (mg/mL)
mlp_s <- pmin(pmax(rnorm(N, 0.80, 0.12), 0.45), 1.15)  # mL/PBAC (clipped to 0.45-1.15)
iv_s  <- rep(IV, N)                                    # IV fixed at 8.25 (conservative choice)
box_s <- rgamma_ms(152.62, 23)                         # oral iron box price

# --- Prevention distributions: efficacy (beta) and cost (gamma) ---
# SDs: narrow for efficacy (well established), ~25% uncertainty for cost
prev_ss <- list("LNG-IUS" = 0.05, "NSAID" = 0.05, "TXA" = 0.05,
                "COC-LNG/EE" = 0.05, "COC-E2V/DNG" = 0.04)
prev_cs <- list("LNG-IUS" = 215, "NSAID" = 180, "TXA" = 590,
                "COC-LNG/EE" = 1140, "COC-E2V/DNG" = 1830)

pr_r <- list()   # efficacy distribution per method
pr_c <- list()   # cost distribution per method
for (m in ORD) {
  pr_r[[m]] <- rbeta_ms(PREV[[m]]$mbl, prev_ss[[m]])
  pr_c[[m]] <- rgamma_ms(PREV[[m]]$tl, prev_cs[[m]])
}

# --- Oral iron pathway distributions (absorption + IV switch) ---
fd  <- rbeta_ms(0.163, 0.030)   # daily absorption
pfd <- rbeta_ms(0.30,  0.07)    # daily IV-switch rate
fa  <- rbeta_ms(0.218, 0.035)   # alternate-day absorption
pfa <- rbeta_ms(0.25,  0.07)    # alternate-day IV-switch rate
od  <- (1 - pfd) * (box_s / (ELEMBOX * fd)) + pfd * iv_s   # daily blended
oa  <- (1 - pfa) * (box_s / (ELEMBOX * fa)) + pfa * iv_s   # alternate-day blended
ch  <- pmin(od, oa, iv_s)                                  # cheapest replacement

# Annual iron-loss distribution (PBAC-dependent)
iron_year_s <- function(p) fe_s * mlp_s * p * CYC

# --- P(prevention < IV) ---
cat("\nP(prevention cheaper than IV):\n")
for (m in ORD) {
  cat("  ", formatC(m, width = -14), sep = "")
  for (p in PBACS) {
    pm   <- pr_c[[m]] / (pr_r[[m]] * iron_year_s(p))   # prevented-mg cost distribution
    prob <- round(mean(pm < iv_s) * 100)
    cat(formatC(paste0(prob, "%"), width = -10), sep = "")
  }
  cat("\n")
}

# --- P(prevention < cheapest oral pathway) ---
cat("\nP(prevention cheaper than cheapest oral pathway):\n")
for (m in ORD) {
  cat("  ", formatC(m, width = -14), sep = "")
  for (p in PBACS) {
    pm   <- pr_c[[m]] / (pr_r[[m]] * iron_year_s(p))
    prob <- round(mean(pm < ch) * 100)
    cat(formatC(paste0(prob, "%"), width = -10), sep = "")
  }
  cat("\n")
}

# --- Worst-case test: most expensive prevention < cheapest replacement? ---
# In each iteration pick the most expensive of the 5 methods, compare to cheapest replacement
cat("\nWorst-case test (most expensive prevention < cheapest replacement):\n")
for (p in PBACS) {
  # prevented-mg cost distribution per method -> N x 5 matrix
  mat <- sapply(ORD, function(m) pr_c[[m]] / (pr_r[[m]] * iron_year_s(p)))
  most_expensive <- apply(mat, 1, max)   # most expensive method per iteration
  prob <- mean(most_expensive < ch) * 100
  cat("  PBAC ", formatC(p, width = 4), ": ", fmt(prob, 1), "%\n", sep = "")
}

# --- 95% credible interval examples (to illustrate uncertainty) ---
cat("\nCredible-interval examples (LNG-IUS, per prevented mg):\n")
for (p in c(200, 800)) {
  pm <- pr_c[["LNG-IUS"]] / (pr_r[["LNG-IUS"]] * iron_year_s(p))
  cat("  PBAC ", p, ": 95% CrI ",
      fmt(quantile(pm, 0.025)), " - ", fmt(quantile(pm, 0.975)), " TL/mg\n", sep = "")
}

cat("\n", strrep("=", 76), "\n", sep = "")
cat("Note: Part A (point estimates) is mathematically identical to the Python version.\n")
cat("      Part B (PSA) yields practically the same results owing to the large N.\n")
cat(strrep("=", 76), "\n", sep = "")
