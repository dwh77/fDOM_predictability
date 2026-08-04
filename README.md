# fDOM Predictability

This repository predicts fluorescent dissolved organic matter (fDOM) in Carvins
Cove Reservoir (CCR) at 1–30 day horizons using four model types (AR, ARIMA,
NNETAR, and XGBoost), each fit with different combinations of in-reservoir
("water column") and catchment drivers. The goal is to compare the relative importance
of in-reservoir vs. catchment drivers for predicting fDOM, and how that
importance changes seasonally and under different hydrologic conditions.

## Repository structure

- **`Predictions/`** — the core analysis for this project: compiling the daily
  driver dataset, fitting the four forecasting model types, generating 1–30 day
  predictions across 2024–2025, and evaluating model and driver performance. See
  below for the full script-by-script breakdown and run order.
- **`Dissertation_Synthesis/`** — a separate project (broader fDOM variability
  synthesis across CCR/FCR/BVR); not part of this forecasting workflow.

---

## `Predictions/` folder

### `Predictions/Data/`

- The daily compiled dataset produced by `1_Data_Comp.R`
  (`Daily_catwalk_RH_2021_2026.csv`, plus a few supporting files) is **already
  provided** here — you don't need to rerun script 1 just to explore the data
  or jump straight to model fitting/evaluation.
- `Predictions/Data/Predictions/` holds the **already-generated prediction
  outputs** for all four models plus persistence, from the current rolling-
  window-refit workflow (scripts `3a`–`3d`). 
- Because both the compiled input data and the forecast outputs are already
  provided, you can jump in and review results at whichever stage you're
  interested in — you don't have to rerun the whole pipeline from scratch.

### `Predictions/Figures/`

Manuscript/SI figures produced by `2_Timeseries_Figures.R` and other scripts.

### `Predictions/Scripts/` — what each script does, and the order to run them

1. **`0_Install_Packages.R`** — installs every package used across this
   project's scripts. Run once, first.
2. **`1_Data_Comp.R`** — compiles the core daily dataset: fDOM,
   stratification/density, DO, chlorophyll-a, catchment discharge and DOC
   (from RHESSys). Sources `find_depths.R` internally
   to compute sensor depths. Produces `Daily_catwalk_RH_2021_2026.csv`, the
   input to every model script.
3. **`1b_USGS-HPB_flow.R`** — gap-fills the local HPB stream gauge record
   using a regression against the nearby USGS Tinker Creek gauge, and computes
   flow-percentile classes (high/low flow) used later for the high-vs-low-flow
   evaluation in script 4. Produces `HPB_USGS_Flows.csv`.
4. **`2_Timeseries_Figures.R`** — builds the manuscript timeseries figures
   and summary statistics from the compiled dataset. Depends on outputs of scripts 1 and 1b.
5. **`3a_Predict_AR_RollingRefit.Rmd`**, **`3b_Predict_ARIMA_RollingRefit.Rmd`**,
   **`3c_Predict_NNETAR_RollingRefit.Rmd`**, **`3d_Predict_XGB_RollingRefit.Rmd`**
   — one script per model type. Each fits the lake/catchment/lake+catchment
   model variants, then generates 30-day-ahead forecasts for every day in
   2024–2025 (~730 reference dates), refitting the model at each reference
   date on a rolling 2-year training window. These four scripts are
   independent of each other and can be run in any order.

   **These are the slow scripts.** Each contains a commented-out example call
   near the forecast function (`forecast_fdom_30day(...)`) that runs the
   prediction for a **single reference date only** — use that first to sanity-check
   the workflow. Running the **full** `run_forecasts()` loop across all
   ~730 reference dates for all three driver-combination models takes
   **multiple hours per script** (NNETAR in particular). The outputs of a full run are already
   provided in `Predictions/Data/Predictions/`, so you don't need to rerun
   these unless you're changing the models themselves.
6. **`4_Eval_predictions.Rmd`** — reads in the forecast outputs from all four
   models plus persistence, builds the multi-model ensemble, computes
   RMSE/skill metrics, and produces the evaluation figures.

Two supplementary scripts, not part of the core pipeline above and not
required to run in any particular order:

- **`fDOM_DOC.R`** — exploratory regressions relating fDOM to DOC chemistry
  across depths and reservoirs (CCR/FCR), including an iron-interference
  correction check.
- **`CCR_WRT.R`** — quick water residence time calculations for CCR/HPB from
  RHESSys discharge output.


