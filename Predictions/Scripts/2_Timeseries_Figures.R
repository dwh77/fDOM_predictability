#### Timeseries figure and stats for paper
## DWH

## libraries
library(tidyverse)
library(patchwork)


## Read in data
eval <- read_csv("./Predictions/Data/Daily_catwalk_RH_2021_2026.csv") |>
  mutate(Date = as.Date(Date)) |> filter(Date <= ymd("2026-01-31")) |>
  mutate(Train = ifelse(Date < ymd("2024-01-01"), "Train", "Pred"))


local_flow <- read_csv("./Predictions/Data/HPB_USGS_Flows.csv")



#### Stats and plots for paper ----
##fdom stats full TS
mean(eval$fDOM_1_QSU_daily , na.rm = T)
summary(eval$fDOM_1_QSU_daily)

(sd(eval$fDOM_1_QSU_daily, na.rm = T) / mean(eval$fDOM_1_QSU_daily, na.rm = T))*100

##stats for full TS
obs_stats_fullTS <- eval |> select(-fDOM_1m_lag1) |>
  pivot_longer(cols = -c(Date, Train),
               names_to = "Variable",
               values_to = "Value") |>
  group_by(Variable) |>
  summarise(
    Min   = min(Value,   na.rm = TRUE),
    Mean   = mean(Value,   na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Max   = max(Value,   na.rm = TRUE),
    SD     = sd(Value,     na.rm = TRUE),
    CV     = (SD / Mean) * 100,
    .groups = "drop"
  ) |>
  arrange(Variable) |>
  mutate(across(where(is.numeric), ~ round(., 1)))


##stats for training vs prediction
obs_stats_TrainPred <- eval |> select(-fDOM_1m_lag1) |>
  pivot_longer(cols = -c(Date, Train),
               names_to = "Variable",
               values_to = "Value") |>
  group_by(Train, Variable) |>
  summarise(
    Min   = min(Value,   na.rm = TRUE),
    Mean   = mean(Value,   na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Max   = max(Value,   na.rm = TRUE),
    SD     = sd(Value,     na.rm = TRUE),
    CV     = (SD / Mean) * 100,
    .groups = "drop"
  ) |>
  arrange(Variable)|>
  mutate(across(where(is.numeric), ~ round(., 1)))


## plotlys
#strat
densts <- eval |> select(Date, Diff_Dens_1_max) |> mutate(Diff_Dens_1_max = round(Diff_Dens_1_max, 2))

strat_plot <- eval |> ggplot(aes(x = Date, y = Diff_Dens_1_max))+ geom_point()
strat_plot
# plotly::ggplotly(strat_plot)

#chla
chla_plot <- eval |> ggplot(aes(x = Date, y = Chla_1_ugL_daily))+ geom_point()
chla_plot
# plotly::ggplotly(chla_plot)

#DO
DO_plot <- eval |> ggplot(aes(x = Date, y = DOsat_1_pct_daily))+ geom_point()
DO_plot
# plotly::ggplotly(DO_plot)


#### SI plot for all model inputs ----

## Display order: fDOM, stratification, DO, chla, Q, DOC
driver_order <- c("fDOM_1_QSU_daily", "Diff_Dens_1_max", "DOsat_1_pct_daily",
                   "Chla_1_ugL_daily", "RH_Q_cms", "RH_DOC_mgL")

## Y-axis label per variable
driver_labels <- list(
  "fDOM_1_QSU_daily"  = "fDOM \n (QSU)",
  "Diff_Dens_1_max"   =  expression(atop("Density", displaystyle(atop("Difference", "(kg m"^-3*")")))),
  "DOsat_1_pct_daily" = "DO \n (% sat)",
  "Chla_1_ugL_daily"  = expression(atop("Chl-a", "("*mu*"g L"^-1*")")),
  "RH_Q_cms"          = expression(atop("Modeled", displaystyle(atop("Discharge", "(m"^3*" s"^-1*")")))),
  "RH_DOC_mgL"        = "Modeled \n Stream DOC \n (mg/L)"
)

driver_long <- eval |>
  select(-fDOM_1m_lag1, -Train) |>
  filter(Date > ymd("2021-08-19"), Date < ymd("2026-01-31")) |>
  pivot_longer(-Date, names_to = "name", values_to = "value") |>
  mutate(name = factor(name, levels = driver_order))

driver_plots <- lapply(driver_order, function(v) {
  driver_long |>
    filter(name == v) |>
    ggplot(aes(x = Date, y = value)) +
    geom_point() +
    geom_vline(xintercept = ymd("2024-01-01"), linetype = 2, linewidth = 1.2, color = "red") +
    labs(x = NULL, y = driver_labels[[v]]) +
    theme_bw()
})

driverplot <- wrap_plots(driver_plots, ncol = 1) +
  plot_layout(axis_titles = "collect", axes = "collect_x") +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

driverplot
# plotly::ggplotly(driverplot)

# ggsave("./Predictions/Figures/fDOM_drivers_TS.png", driverplot,
#        height = 7, width = 6, units = "in")




#### SI figure for water level and HPB flow ####

##water level
waterlevel <- read_csv("./Predictions/Data/Extras_Daily_Cat_Met_RH_2021_2026.csv") |>
  select(Date, waterlevel_m) |>
  mutate(Date = as.Date(Date)) |>
  filter(Date >= ymd("2021-08-19"), Date <= ymd("2026-01-31"))


#plot
ccr_depth_plot <- waterlevel |>
  ggplot(aes(x = Date, y = waterlevel_m ))+
  geom_point()+
  labs(x = "Date", y = "CCR \n Water Level (m)")+
  geom_vline(xintercept = ymd("2024-01-01"), linetype = 2, linewidth = 1.2, color = "red")+
  theme_bw() + theme(legend.position = "none", text = element_text(size = 14),
                     axis.title.y.left = element_text(size = 12))

ccr_depth_plot


## HPB flow
hpb_flow_plot <- local_flow |>
  mutate(Interp = ifelse(is.na(HPB_Q_cms), "Interp", "Observed")) |>
  select(Date, HPBinterp_Q_cms, Interp) |>
  filter(Date >= ymd("2021-08-19"), Date <= ymd("2026-01-31")) |>
  ggplot(aes(x = Date, y = HPBinterp_Q_cms, shape = Interp ))+
  geom_point()+
  labs(x = "Date", y = "HPB \n Discharge (cms)")+
  scale_shape_manual(values = c("Observed" = 16, "Interp" = 5), guide = "none")+
  geom_vline(xintercept = ymd("2024-01-01"), linetype = 2, linewidth = 1.2, color = "red")+
  theme_bw() + theme(legend.position = "none", text = element_text(size = 14),
                     axis.title.y.left = element_text(size = 12))

hpb_flow_plot

#join
hydroSI <- wrap_plots(hpb_flow_plot, ccr_depth_plot, ncol = 1) +
  plot_layout(axis_titles = "collect", axes = "collect_x") +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 16, face = "bold"))

hydroSI

# ggsave("./Predictions/Figures/HydroSI.png", hydroSI,
#        height = 4, width = 6, units = "in")


#################### OLD ###########################

# #### Figure 3 for MS ----
# fdomTS <- eval |>
#   filter(Date <= ymd("2026-01-31")) |>
#   ggplot(aes(x = Date, y = fDOM_1_QSU_daily ))+
#   #ggplot(aes(x = Date, y = fDOM_1m_obs, col = HighVar))+
#   geom_point()+
#   labs(x = "Date", y = "fDOM (QSU)")+
#   #scale_color_manual(values = highvar_colors)+
#   geom_vline(xintercept = ymd("2024-01-01"), linetype = 2, linewidth = 1.2, color = "red")+
#   theme_bw() + theme(legend.position = "none", text = element_text(size = 18))
#
# fdomTS
#
# ## Density
# dens_colors <- c("Mixed" = "red", "Strat" = "black")
#
# stratTS <- eval |>
#   select(Date, Diff_Dens_1_max) |>
#   mutate(Strat = ifelse(Diff_Dens_1_max > 0.1, "Strat", "Mixed")) |>
#   filter(!is.na(Strat)) |>
#   ggplot(aes(x = Date, y = Diff_Dens_1_max, col = Strat))+
#   geom_point()+
#   labs(x = "Date", y = expression("Density \n Diff (kg m"^-3*")"), color = "Stratified")+
#   scale_color_manual(values = dens_colors)+
#   geom_vline(xintercept = ymd("2024-01-01"), linetype = 2, linewidth = 1.2, color = "red")+
#   #scale_x_date(date_labels = "%b %Y", date_breaks = "6 months")+
#   theme_bw() + theme(legend.position = "top", text = element_text(size = 18))
#
# stratTS
#
# ## Flow TS
# Flow_colors <- c("Low flow" = "#D55E00", "High flow" = "#0072B2") #, "Normal" = "black"
#
# flowTS <- local_flow |>
#   mutate(Interp = ifelse(is.na(HPB_Q_cms), "Interp", "Observed")) |>
#   select(Date, HPB_Q_cms_filled, flow_class_hpb, Interp) |>
#   filter(Date >= ymd("2021-08-19"), Date <= ymd("2026-01-31")) |>
#   #make non categorized flow classes not gray
#   mutate(flow_class_hpb = ifelse(is.na(flow_class_hpb), "Normal", flow_class_hpb)) |>
#   ggplot(aes(x = Date, y = HPB_Q_cms_filled, col = flow_class_hpb, shape = Interp))+
#   geom_point()+
#   labs(x = "Date", y = "Flow (cms)", color = "Flow Classification during predictions")+
#   scale_color_manual(values = Flow_colors)+
#   scale_shape_manual(values = c("Observed" = 16, "Interp" = 17), guide = "none")+
#   scale_y_log10()+
#   # scale_x_date(date_labels = "%b %y", date_breaks = "6 months")+
#   geom_vline(xintercept = ymd("2024-01-01"), linetype = 2, linewidth = 1.2, color = "red")+
#   theme_bw() + theme(legend.position = "top", text = element_text(size = 18))
#
# flowTS
#
#
# ##Join
# fdomTS / stratTS / flowTS +
#   plot_layout(axis_titles = "collect", axes = "collect_x") +
#   plot_annotation(tag_levels = "a") &
#   theme(plot.tag = element_text(size = 18, face = "bold"),
#         legend.position = "top")



