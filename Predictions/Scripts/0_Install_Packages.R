#### Install needed packages for fDOM_predictability analysis

## Core tidyverse / dates
install.packages('tidyverse')
install.packages('lubridate')

## Data compilation (1_Data_Comp.R, find_depths.R, USGS-HPB_flow.R, fDOM_DOC.R, Synthesis_fDOM_Variability.R)
install.packages('zoo')           # na.approx short-gap interpolation
install.packages('slider')        # rolling window means (slide_dbl)
install.packages('rLakeAnalyzer') # water.density() for stratification calcs
install.packages('akima')         # 2D interpolation for temp-profile heatmap
install.packages('astsa')         # acf2() diagnostic plots
install.packages('corrplot')      # driver correlation matrix figure
install.packages('plyr')          # rbind.fill() for met data with mismatched columns
install.packages('dataRetrieval') # USGS streamflow retrieval
install.packages('sf')            # dataRetrieval returns sf objects; st_drop_geometry()
install.packages('ggpmisc')       # stat_poly_line / stat_poly_eq regression annotations
install.packages('gt')            # summary tables (Synthesis_fDOM_Variability.R)

## Forecasting models (3a-3d Predict_*.Rmd and *_NoRefit.Rmd)
install.packages('fable')      # ARIMA() and NNETAR()
install.packages('fabletools') # refit(), model_sum(), other mable utilities
install.packages('feasts')     # gg_tsresiduals() residual diagnostics
install.packages('tsibble')    # tsibble time series structure used by fable
install.packages('urca')       # unit root tests used internally by fable::ARIMA()
install.packages('tidymodels') # XGBoost workflow: recipes, tune, workflows, rsample
install.packages('xgboost')    # gradient boosted trees engine
install.packages('vip')        # variable importance plotting helper

## Evaluation and figures (4_Eval_predictions.Rmd, 2_Timeseries_Figures.R)
install.packages('Metrics')  # rmse() / mae()
install.packages('patchwork') # combining ggplot panels (/ and | layout)
install.packages('ggpubr')    # ggarrange() / annotate_figure() shared-legend figures
install.packages('plotly')    # optional interactive versions of time series plots

## Needed to knit the .Rmd analysis scripts (3a-3d, 4_Eval_predictions.Rmd)
install.packages('rmarkdown')
install.packages('knitr')
