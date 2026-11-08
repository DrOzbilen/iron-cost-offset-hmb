# Iron as a currency: a cost-offset model for heavy menstrual bleeding (HMB)

Reproducible code for a pharmacoeconomic **cost-offset / cost-consequence model**
that compares *reducing menstrual blood loss* (prevention) with *replacing lost iron*
(oral or intravenous replacement), by expressing every option in a single common unit:
the **cost per milligram of iron** each intervention restores or prevents from being lost.

Menstrual blood loss is quantified with the **Pictorial Blood Loss Assessment Chart (PBAC)**
and converted to iron with a conservative coefficient. Costs use Türkiye national
reimbursement prices (2026 price year) and are reported in Turkish lira, market-rate USD,
and purchasing-power-parity international dollars (Int$).

> This repository contains **only model code and published input parameters**.
> It contains **no patient-level data**; the study is a computational model and
> required no individual participant data.

---

## What's here

```
iron-cost-offset-hmb/
├── R/                        # Primary implementation (base R, no external packages*)
│   ├── HMB_calc.R            #   Core model: point estimates + 200,000-iteration PSA
│   ├── HMB_tables.R          #   Reproduces every manuscript table (console + .xlsx*)
│   └── HMB_figures.R         #   Reproduces Figure 1 (heatmap) and Figure 2 (break-even)
├── python/
│   └── HMB_model_python.py   # Independent NumPy cross-check of the same model
├── figures/                  # Pre-rendered figures as shipped in the manuscript
│   ├── Figure1_heatmap.png
│   └── Figure2_breakeven.png
├── CITATION.cff
├── LICENSE
└── README.md
```

\* `HMB_calc.R` and `HMB_figures.R` use **base R only**. `HMB_tables.R` optionally uses
`openxlsx` to write an Excel workbook; without it, the script still prints every table to
the console and falls back to CSV, so no functionality is lost.

---

## Requirements

- **R ≥ 4.0** (tested with base R; no packages required to run the model or figures)
- Optional: `install.packages("openxlsx")` to export tables as `.xlsx`
- Optional cross-check: **Python ≥ 3.9** with **NumPy** (`pip install numpy`)

---

## How to run

From the repository root:

```bash
# 1) Core model — point estimates + probabilistic sensitivity analysis
Rscript R/HMB_calc.R

# 2) All manuscript tables (prints to console; writes tables.xlsx if openxlsx is installed)
Rscript R/HMB_tables.R

# 3) Figures (writes Figure1_heatmap.(png|pdf) and Figure2_breakeven.(png|pdf))
Rscript R/HMB_figures.R
```

Independent cross-check (optional):

```bash
python python/HMB_model_python.py
```

---

## Key model parameters (all editable at the top of each script)

| Parameter | Value | Meaning |
|---|---|---|
| PBAC→MBL factor | 0.80 mL/point | blood volume per PBAC point |
| Cycles / year | 13 | |
| Blood iron content | 0.43 mg/mL (base case); 0.36–0.50 (sensitivity) | conservative midpoint |
| IV iron unit cost | **8.25 TL/mg** | **ferric carboxymaltose** (base-case IV comparator) |
| Oral iron (blended) | ~2.3–2.8 TL/mg | ferrous sulfate, absorption- and failure-adjusted |
| PSA | 200,000 iterations, fixed seed **12345** | |
| Market rate | 1 USD = 47.2 TL | |
| PPP rate | 1 Int\$ = 16.647 TL (IMF WEO 2025) | |

**Note on the intravenous comparator.** Ferric carboxymaltose (8.25 TL/mg) is used as the
base-case IV route because it is the preparation most commonly used in contemporary practice.
Iron sucrose is reported in Table 2 for reference only (corrected unit cost 5.96 TL/mg for a
box of five 100 mg vials); it is seldom preferred today and is **not** used in the model.

---

## Reproducibility

- **Point estimates** (iron-loss table, cost per prevented mg, fold differences, thresholds,
  savings %) are closed-form and **identical on every run**.
- **Probabilistic results** (P[prevention < replacement], 95% credible intervals) use a
  **fixed seed (12345)** and 200,000 iterations, so they are stable to ~±1 percentage point.
- The R and Python implementations are mathematically identical in Part A; small differences
  in Part B reflect different random-number generators only and are negligible at N = 200,000.

---

## Citation

If you use this code, please cite the accompanying manuscript (see `CITATION.cff`).
A permanent DOI for this repository can be minted via Zenodo (see below).

## License

Released under the **MIT License** (see `LICENSE`). You may change this before publishing
if your journal or institution requires a different license.

## Authors

- **Muhammet Özbilen**, MD, FEFIM — Iron Deficiency Clinic, Department of Internal Medicine, Ordu University Faculty of Medicine, Ordu, Türkiye. ORCID: [0000-0001-6052-7486](https://orcid.org/0000-0001-6052-7486)
- **Deha Denizhan Keskin** — Department of Obstetrics and Gynaecology, Ordu University Faculty of Medicine, Ordu, Türkiye. ORCID: [0000-0002-6108-7619](https://orcid.org/0000-0002-6108-7619)
- **Osman Şenol** — Department of Health Management, Karadeniz Technical University, Trabzon, Türkiye. ORCID: [0000-0001-5626-2921](https://orcid.org/0000-0001-5626-2921)

## Correspondence

Muhammet Özbilen — drozbilen@gmail.com
