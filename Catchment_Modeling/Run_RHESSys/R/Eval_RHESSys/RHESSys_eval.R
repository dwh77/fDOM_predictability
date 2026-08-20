####  RHESSys model evaluation for fDOM prediction project

#### packages
library(tidyverse)
library(patchwork)


############################## eval metric helper functions
nse <- function(obs, mod) {
  1 - sum((obs - mod)^2, na.rm = TRUE) /
    sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
}

kge <- function(obs, mod) {
  r   <- cor(obs, mod, use = "complete.obs")
  alpha <- sd(mod, na.rm = TRUE) / sd(obs, na.rm = TRUE)
  beta  <- mean(mod, na.rm = TRUE) / mean(obs, na.rm = TRUE)
  1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
}

calc_metrics <- function(df) {
  df |>
    summarise(
      RMSE = sqrt(mean((Observed - Modeled)^2)),
      MAE  = mean(abs(Observed - Modeled)),
      R2   = cor(Observed, Modeled)^2,
      NSE  = nse(Observed, Modeled),
      KGE  = kge(Observed, Modeled)
      )
}


############################## function to read in outputs
load_rhessys_output <- function(grow_path,
                                h2o_path,
                                area_m2     = 45.83578 * 1000000,
                                filter_date = "2021-01-01") {

  output_grow <- read_delim(grow_path, delim = " ", col_names = TRUE)
  output_h2o  <- read_delim(h2o_path,  delim = " ", col_names = TRUE)

  left_join(output_h2o, output_grow, by = c("day", "month", "year", "basinID")) |>
    mutate(date = ymd(paste(year, month, day, sep = "-"))) |>
    filter(date >= ymd(filter_date)) |>
    select(date, streamflow, return, baseflow, streamflow_NO3, streamflow_NH4, streamflow_DOC, lai.y) |>
    rename(lai = lai.y) |>
    mutate(
      streamflow_m_day  = streamflow / 1000,
      streamflow_m3_day = streamflow_m_day * area_m2,
      DOC_mgL           = streamflow_DOC / streamflow_m_day,
      NO3_mgL           = streamflow_NO3 / streamflow_m_day,
      NH4_mgL           = streamflow_NH4 / streamflow_m_day
    ) |>
    select(date, lai, streamflow_m3_day, DOC_mgL, NO3_mgL)
}

# Usage
# output_h2o_grow <- load_rhessys_output(
#   grow_path   = "Catchment_Modeling/Run_RHESSys/out/ccrTR/HarvestNone/TR1850_2026_NOharvest_run_grow_basin.daily",
#   h2o_path    = "Catchment_Modeling/Run_RHESSys/out/ccrTR/HarvestNone/TR1850_2026_NOharvest_run_basin.daily",
#   area_m2     = 45.83578 * 1000000,   # optional, this is the default
#   filter_date = "2021-01-01"           # optional, this is the default
# )



################################################################################
#### Basin level ----

#### Note give large size of RHESSys outputs I am pushing a smaller output to github, but code that created those csvs is presented here

# output_h2o_grow <- output_h2o_grow <- load_rhessys_output(
#     grow_path   =  "Catchment_Modeling/Run_RHESSys/out/ccrTR/HarvestNone/TR1850_2026_NOharvest_run_grow_basin.daily",
#     h2o_path    =  "Catchment_Modeling/Run_RHESSys/out/ccrTR/HarvestNone/TR1850_2026_NOharvest_run_basin.daily",
#     area_m2     = 45.83578 * 1000000,   # optional, this is the default
#     filter_date = "2021-01-01"           # optional, this is the default
#   )
#
# write.csv(output_h2o_grow, "Catchment_Modeling/Run_RHESSys/out/TR_for_predictions/basin_output_2021_2026.csv", row.names = F)

output_h2o_grow <- read_csv("Catchment_Modeling/Run_RHESSys/out/TR_for_predictions/basin_output_2021_2026.csv")

output_h2o_grow |>
  select(date, lai, DOC_mgL, NO3_mgL) |>
  pivot_longer(-1) |>
  ggplot(aes(x = date, y = value))+ geom_point()+facet_wrap(~name, scales = "free_y")



################################################################################
#### Stream routing eval ----

#### Note give large size of RHESSys outputs I am pushing a smaller output to github, but code that created those csvs is presented here


# output_streamrouting <- read_delim(Catchment_Modeling/Run_RHESSys/out/ccrTR/HarvestNone/TR1850_2026_NOharvest_run_streamrouting.daily",
#                                    delim = " ", col_names = T)
#
# #get reaches of interest
# sr_df <- output_streamrouting  |>
#   mutate(date = as.Date(paste(year, month, day, sep = "-"))) |>
#   filter(date >= ymd("2021-01-01")) |>
#   # filter(reachID == 2) |>
#   filter(reachID %in% c(2,62,36,28)) |>
#   mutate(reach_ID_name = ifelse(reachID == 2, "CCR dam", NA),
#          reach_ID_name = ifelse(reachID == 62, "HPB", reach_ID_name),
#          reach_ID_name = ifelse(reachID == 36, "SMB", reach_ID_name),
#          reach_ID_name = ifelse(reachID == 28, "CCS", reach_ID_name)
#   ) |>
#   #Q_out units are in m3/day
#   #DOC_out units are in kg C/day
#   #DOC_out/Qout = kg/m3 == g/L
#   #so then multiply by 1000 to get mg/L
#   mutate(#NO3_mgL_mod = (NO3_out / Qout) * 1000,
#          DOC_mgL_mod = (DOC_out / Qout) * 1000,
#          #DON_mgL_mod = (DON_out / Qout) * 1000,
#          Q_m3day_mod = Qout, #convert to m3/day
#          ) |>
#   dplyr::select(date, reach_ID_name, Q_m3day_mod, DOC_mgL_mod) |>
#   pivot_longer(cols = c(Q_m3day_mod, DOC_mgL_mod), names_to = "variable", values_to = "value")
#
#
# write.csv(sr_df, "Catchment_Modeling/Run_RHESSys/out/TR_for_predictions/streamroute_output_2021_2026.csv", row.names = F)


sr_df <- read_csv("Catchment_Modeling/Run_RHESSys/out/TR_for_predictions/streamroute_output_2021_2026.csv")


#SR for a few vars
sr_df |>
  ggplot(aes(x = date, y = value, col = as.factor(reach_ID_name))) +
  geom_line() +
  theme_bw() + theme(legend.position = "top")+
  facet_wrap(~variable, scales = "free_y")



################################################################################
### Eval data
#### READ in and format eval data
target <- read_csv("Catchment_Modeling/Run_RHESSys/Target_Data_comp/TargetData_2020_2025.csv") |>
  select(-HPB_Q_PT_m3day, -HPB_Q_lm_m3day)

# eval <- target |>
#   mutate(date = as.Date(Date)) |>
#   filter(date >= ymd("2020-01-01")) |>
#   select(date, HPB_Q_lm_m3day, NO3_mgL, DOC_mgL, lai_MODIS) |>
#   rename(lai = lai_MODIS,
#          Q_m3_day = HPB_Q_lm_m3day )

flow_flags <- read_csv("Predictions/Data/HPB_USGS_Flows2.csv")

hpb_PT_eval <- flow_flags |>
  mutate(HPB_Q_PT_m3day = HPB_Q_cms *86400,
         HPB_Q_lm_m3day = HPB_Q_cms_filled *86400) |>
  select(Date,
         HPB_Q_PT_m3day,
         HPB_Q_lm_m3day )

#make HPB lm be only when HPB data is missing
target <- full_join(target, hpb_PT_eval, by = "Date")

################################################################################
#### LAI evals ----
#site 101 is only LAI
lai_eval <- target |>
  mutate(date = as.Date(Date)) |>
  filter(date >= ymd("2021-01-01")) |>
  select(date, lai_MODIS)

lai_SI <- output_h2o_grow |>
  select(date, lai) |> rename(lai_RHESSys = lai) |>
  left_join(lai_eval, by = "date") |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = lai_RHESSys, color = "Modeled")) +
  geom_point(aes(y = lai_MODIS, color = "Observed")) +
  scale_color_manual(values = c("Modeled" = "blue", "Observed" = "black")) +
  theme_bw() +
  labs(x = "Date", y = "LAI", color = NULL) +
  theme(legend.position = "top", text = element_text(size = 14))

lai_SI

### Stats
lai_stats_df <- output_h2o_grow |>
  select(date, lai) |>
  rename(Modeled = lai) |>
  left_join(lai_eval, by = "date") |>
  rename(Observed = lai_MODIS) |>
  filter(!is.na(Observed), !is.na(Modeled))

metrics_lai <- lai_stats_df |>
  calc_metrics()

metrics_lai

################################################################################
#### HPB Q evals ----
hpb_PT_eval <- target |>
  select(date = Date, HPB_Q_PT_m3day, HPB_Q_lm_m3day )

# Prepare data and convert m3/day to m3/s (divide by 86400)
hpb_plot <- sr_df |>
  filter(reach_ID_name == "HPB") |>
  pivot_wider(names_from = variable, values_from = value) |>
  left_join(hpb_PT_eval, by = "date") |>
  select(date, Q_m3day_mod, HPB_Q_PT_m3day, HPB_Q_lm_m3day) |>
  mutate(across(c(Q_m3day_mod, HPB_Q_PT_m3day, HPB_Q_lm_m3day), ~ . / 86400))

summary(hpb_plot$Q_m3day_mod)
summary(hpb_plot$HPB_Q_PT_m3day)
summary(hpb_plot$HPB_Q_lm_m3day)

## HPB Q eval metrics -- modeled vs HPB_Q_lm_m3day only (this is the one and
## only place Q gets evaluated; the flowmate-based stats below are DOC/NO3 only)
hpb_Q_stats <- hpb_plot |>
  select(Modeled = Q_m3day_mod, Observed = HPB_Q_lm_m3day) |>
  filter(!is.na(Observed), !is.na(Modeled)) |>
  calc_metrics()

hpb_Q_stats


#plot
Q_SI <- hpb_plot |>
  ggplot(aes(x = date)) +
  geom_point(aes(y = HPB_Q_lm_m3day, color = "Observed"), size = 1.5, alpha = 0.6) +
  geom_line(aes(y = Q_m3day_mod, color = "Modeled")) +
  scale_color_manual(values = c("Modeled" = "blue", "Observed" = "black")) +
  labs(#title = paste0("HPB: Modeled vs LM  |  RMSE = ", rmse_lm, "cms"),
       x = NULL, y = expression(Q~(m^3~s^-1)), color = NULL) +
  scale_y_log10() +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size = 14))

Q_SI

################################################################################
#### Stream chem eval ----
## Q is intentionally excluded here -- it's evaluated once, above, in the
## HPB Q evals section (modeled vs HPB_Q_lm_m3day only). This pipeline is
## DOC/NO3 only, so the flowmate-derived Q is never used for eval stats.

##set up obs data
obs_long <- target |>
  mutate(date = as.Date(Date)) |>
  filter(date >= ymd("2021-01-01"),
         Site != 101) |>
  mutate(Site_name = ifelse(Site == 100, "HPB", NA),
         Site_name = ifelse(Site == 300, "SMB", Site_name),
         Site_name = ifelse(Site == 200, "CCS", Site_name)
         ) |>
  select(date, Site_name, NO3_mgL, DOC_mgL) |>
  rename(Site = Site_name) |>
  pivot_longer(cols = c(DOC_mgL, NO3_mgL),
               names_to = "Variable", values_to = "Value") |>
  mutate(
    Data_type = "Observed",
    Variable  = case_when(
      str_detect(Variable, "DOC") ~ "DOC_mgL",
      str_detect(Variable, "NO3") ~ "NO3_mgL"
    )
  ) |>
  select(Date = date, Site, Data_type, Variable, Value)


# Pivot sr_df long, remove CCR dam and Q, clean variable names
sr_long <- sr_df |>
  filter(reach_ID_name != "CCR dam",
         !str_detect(variable, "Q")) |>
  rename(Site = reach_ID_name) |>
  mutate(
    Data_type = "Modeled",
    Variable  = case_when(
      str_detect(variable, "DOC") ~ "DOC_mgL",
      str_detect(variable, "NO3") ~ "NO3_mgL"
    ),
    Value = value
  ) |>
  select(Date = date, Site, Data_type, Variable, Value)

# Bind together
combined_long <- bind_rows(sr_long, obs_long)

## plot
# combined_long |>
# ggplot(aes(x = Date, y = Value)) +
#   geom_line(data = filter(combined_long, Data_type == "Modeled"), color = "lightblue") +
#   geom_point(data = filter(combined_long, Data_type == "Observed"), size = 1.5, alpha = 0.6) +
#   facet_grid(Variable~Site, scales = "free_y") +
#   labs(title = "Modeled vs Observed", y = NULL, color = "Site") +
#   theme_bw()

## only DOC
doc_SI <- combined_long |>
  filter(Variable == "DOC_mgL") |>
  ggplot(aes(x = Date, y = Value)) +
  geom_line(data = filter(combined_long, Data_type == "Modeled", Variable == "DOC_mgL"), color = "blue", alpha = 0.6) +
  geom_point(data = filter(combined_long, Data_type == "Observed", Variable == "DOC_mgL"), size = 1.5, alpha = 0.8) +
  facet_wrap(~Site, scales = "free_y") +
  labs(y = "DOC (mg/L) ", color = "Site") +
  theme_bw()+ theme( text = element_text(size = 14))

doc_SI

##SI RHESSys figure
(lai_SI / doc_SI / Q_SI) +  plot_annotation(tag_levels = c('a', 'b', 'c') )


## stats
stats_df <- combined_long |>
  summarise(Value = mean(Value, na.rm = TRUE),
            .by = c(Date, Site, Variable, Data_type)) |>
  pivot_wider(names_from = Data_type, values_from = Value) |>
  filter(!is.na(Observed), !is.na(Modeled))

# Aggregated across sites
metrics_overall <- stats_df |>
  group_by(Variable) |>
  calc_metrics()

metrics_overall

# # By site
# metrics_by_site <- stats_df |>
#   group_by(Variable, Site) |>
#   calc_metrics()
#
# metrics_by_site



