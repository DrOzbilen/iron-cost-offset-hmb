# =============================================================================
# IRON COST-OFFSET MODEL FOR HEAVY MENSTRUAL BLEEDING (HMB)
# Table generation code - R version
# =============================================================================
#
# Reproduces every table in the manuscript (Tables 1-7), prints each to the
# console, and writes them all to a single multi-sheet Excel workbook.
#
# Tables 1-2 are input tables (efficacy and unit costs); Tables 3-7 are
# computed from the model. Values match HMB_calc.R.
#
# EXCEL OUTPUT: writing .xlsx requires the 'openxlsx' package (the only
# external dependency in this archive). If it is not installed, the script
# still prints all tables to the console and writes CSV files as a fallback,
# so no functionality is lost without the package.
# =============================================================================


# ---- Parameters (identical to HMB_calc.R) ----
MLP <- 0.80; CYC <- 13; IV <- 8.25
FE_POP <- 0.43; FE_TARGET <- 0.50
USD <- 47.2; PPP <- 16.647
BOX <- 152.62; ELEMBOX <- 2400
PBACS <- c(200, 400, 800, 1000)

methods <- c("LNG-IUS", "NSAID", "TXA", "COC (LNG/EE)", "COC (E2V/DNG)")
mbl     <- c(0.82, 0.35, 0.45, 0.45, 0.71)
tl      <- c(862,  709,  2345, 4556, 7344)

prevented_mg <- function(i, p) tl[i] / (mbl[i] * FE_POP * MLP * p * CYC)
iron_year    <- function(p, fe) fe * MLP * p * CYC

# Oral replacement pathway (blended) cost per lost mg
ORAL <- list(daily = list(f = 0.163, pf = 0.30),
             alt   = list(f = 0.218, pf = 0.25))
oral_cost <- sapply(ORAL, function(o) (1 - o$pf) * (BOX / (ELEMBOX * o$f)) + o$pf * IV)
CHEAP_ORAL <- min(oral_cost)


# =============================================================================
# BUILD EACH TABLE AS A data.frame
# =============================================================================

# ---- Table 1: Base-case MBL reduction percentages ----
table1 <- data.frame(
  Method       = c("LNG-IUS", "Combined OC (upper end)", "Combined OC (lower end)",
                   "Tranexamic acid", "NSAIDs"),
  MBL_reduction = c("82%", "71%", "45%", "45%", "35%"),
  Range_source  = c("79-85%", "64-88%", "12-77%", "40-50%", "25-50%"),
  stringsAsFactors = FALSE
)

# ---- Table 2: Base-case unit costs ----
table2 <- data.frame(
  Preparation = c("LNG-IUS (Mirena)", "NSAID (naproxen)", "TXA (Transamin)",
                  "COC E2V/DNG (Qlairista)", "COC LNG/EE (Leverette)",
                  "FCM (Ferdepo)", "Iron sucrose (Ferroven)", "Oral iron (Tardyferon)"),
  In_model_TL   = c("862/yr", "709/yr", "2345/yr", "7344/yr", "4556/yr",
                    "8.25/mg", "5.96/mg", "2.28-2.75/mg"),
  In_model_USD  = c("18.3/yr", "15.0/yr", "49.7/yr", "155.6/yr", "96.5/yr",
                    "0.175/mg", "0.126/mg", "0.048-0.058/mg"),
  In_model_IntPPP = c("51.8/yr", "42.6/yr", "140.9/yr", "441.2/yr", "273.7/yr",
                      "0.496/mg", "0.358/mg", "0.137-0.165/mg"),
  stringsAsFactors = FALSE
)

# ---- Table 3: Menstrual iron loss by PBAC (computed) ----
table3 <- data.frame(
  PBAC          = PBACS,
  MBL_cycle_mL  = FE_TARGET * 0 + MLP * PBACS,           # 0.8 * PBAC
  Iron_cycle_mg = FE_TARGET * MLP * PBACS,               # 0.5 * 0.8 * PBAC
  Iron_year_mg  = round(FE_TARGET * MLP * PBACS * CYC),
  Fold_vs_200   = paste0(round(PBACS / 200, 1), "x"),
  IV_all_TL     = round(FE_TARGET * MLP * PBACS * CYC * IV),
  IV_all_USD    = round(FE_TARGET * MLP * PBACS * CYC * IV / USD),
  IV_all_IntPPP = round(FE_TARGET * MLP * PBACS * CYC * IV / PPP),
  stringsAsFactors = FALSE
)

# ---- Table 4: Cost per lost mg of replacement routes (computed) ----
table4 <- data.frame(
  Route        = c("Oral - daily (blended)", "Oral - alternate-day (blended)",
                   "Intravenous iron"),
  Median_TL_mg = c(round(oral_cost[["daily"]], 2), round(oral_cost[["alt"]], 2), IV),
  USD_mg       = c(round(oral_cost[["daily"]]/USD, 3), round(oral_cost[["alt"]]/USD, 3),
                   round(IV/USD, 3)),
  IntPPP_mg    = c(round(oral_cost[["daily"]]/PPP, 3), round(oral_cost[["alt"]]/PPP, 3),
                   round(IV/PPP, 3)),
  stringsAsFactors = FALSE
)

# ---- Table 5: Prevention vs IV — probability (%) and fold difference ----
# Probability values come from the PSA (see HMB_calc.R); fold = IV / prevented-mg cost.
prob_IV <- matrix(c(100,100,100,100, 100,100,100,100, 86,100,100,100,
                    18,88,100,100, 15,88,100,100), nrow = 5, byrow = TRUE)
table5 <- data.frame(Method = methods, stringsAsFactors = FALSE)
for (k in seq_along(PBACS)) {
  p <- PBACS[k]
  folds <- sapply(seq_along(methods), function(i) round(IV / prevented_mg(i, p)))
  table5[[paste0("PBAC", p, "_pct")]]  <- paste0(prob_IV[, k], "%")
  table5[[paste0("PBAC", p, "_fold")]] <- paste0(folds, "x")
}

# ---- Table 6: Prevention vs oral pathway — probability (%) and directional fold ----
prob_oral <- matrix(c(94,100,100,100, 44,94,100,100, 1,21,83,93,
                      0,1,23,44, 0,0,21,42), nrow = 5, byrow = TRUE)
dir_fold <- function(pm) {
  if (pm <= CHEAP_ORAL) { k <- CHEAP_ORAL / pm; sprintf("%sx^", ifelse(k>=1.5, round(k), round(k,1))) }
  else                  { k <- pm / CHEAP_ORAL; sprintf("%sxv", ifelse(k>=1.5, round(k), round(k,1))) }
}
# ^ = prevention cheaper (up), v = oral cheaper (down)
table6 <- data.frame(Method = methods, stringsAsFactors = FALSE)
for (k in seq_along(PBACS)) {
  p <- PBACS[k]
  folds <- sapply(seq_along(methods), function(i) dir_fold(prevented_mg(i, p)))
  table6[[paste0("PBAC", p, "_pct")]]  <- paste0(prob_oral[, k], "%")
  table6[[paste0("PBAC", p, "_fold")]] <- folds
}

# ---- Table 7: Total annual cost metric (PPP int$) (computed) ----
build_t7 <- function() {
  rows <- list()
  for (i in seq_along(methods)) {
    for (p in PBACS) {
      A     <- iron_year(p, FE_POP) * IV / PPP
      prevc <- tl[i] / PPP
      resid <- iron_year(p, FE_POP) * (1 - mbl[i]) * IV / PPP
      B     <- prevc + resid
      rows[[length(rows)+1]] <- data.frame(
        Method = methods[i], PBAC = p,
        A_IVonly = round(A), Prevention = round(prevc),
        Residual_IV = round(resid), B_total = round(B),
        Net_saving = round(A - B),
        Saving_pct = sprintf("%.1f%%", (A - B) / A * 100),
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}
table7 <- build_t7()


# =============================================================================
# PRINT ALL TABLES TO CONSOLE
# =============================================================================
tables <- list("Table1_MBL_reduction" = table1,
               "Table2_unit_costs"     = table2,
               "Table3_iron_loss"      = table3,
               "Table4_replacement"    = table4,
               "Table5_vs_IV"          = table5,
               "Table6_vs_oral"        = table6,
               "Table7_total_cost"     = table7)

for (nm in names(tables)) {
  cat("\n", strrep("=", 70), "\n", nm, "\n", strrep("=", 70), "\n", sep = "")
  print(tables[[nm]], row.names = FALSE)
}
cat("\nNote (Table 6): ^ = prevention cheaper (up); v = oral cheaper (down).\n")


# =============================================================================
# WRITE TO EXCEL (openxlsx) — with CSV fallback if package unavailable
# =============================================================================
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(tables)) {
    openxlsx::addWorksheet(wb, substr(nm, 1, 31))   # Excel sheet-name limit
    openxlsx::writeData(wb, substr(nm, 1, 31), tables[[nm]])
  }
  openxlsx::saveWorkbook(wb, "HMB_tables.xlsx", overwrite = TRUE)
  cat("\nExcel workbook written: HMB_tables.xlsx (7 sheets)\n")
} else {
  cat("\n'openxlsx' not installed — writing CSV files instead.\n")
  for (nm in names(tables)) {
    write.csv(tables[[nm]], paste0(nm, ".csv"), row.names = FALSE)
  }
  cat("CSV files written (one per table). Install openxlsx for a single .xlsx.\n")
}
