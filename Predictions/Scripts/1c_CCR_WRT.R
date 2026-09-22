#### Water residence time (WRT) calcs for CCR based on RHESSys

eval <- read.csv("./Predictions/Data/Daily_catwalk_RH_2021_2026.csv")

summary(eval$RH_Q_cms)

#### Quick WRT calcs assuming full pond ----
# ## CCR volume at full pond
# ccr_Vol_m3 <- 2.3*10^7
#
#
# ### Max and min discharge from full RHESSys CCR runs
# ccr_Q_m3day_max <- 2.14 * 86400
#
# ccr_Q_m3day_min <- 0.023 * 86400
#
# ccr_Q_m3day_25th <- 0.13 * 86400
#
# #### CCR fastest WRT
# ccr_Vol_m3 / ccr_Q_m3day_max
#
# #### CCR slowest WRT
# ccr_Vol_m3 / ccr_Q_m3day_min
#
# ccr_Vol_m3 / ccr_Q_m3day_25th
#
#
#
# ### HPB subset
#
# ## HPB subsection volume at full pont (trimming at dam)
# hpb_Vol_m3 <-1.1*10^6 #this is just the HPB arm before it opens up   # this value gets closer to island: 2.5*10^6
#
# #hpb q
# hpb_Q_m3day_max <- 1 * 86400
#
#
# #HPB fast wrt
# hpb_Vol_m3 / hpb_Q_m3day_max


######################## Get daily WRT based on bathymetry curve ######################

#### Set up bathy data ----

## Pulling in bathymetry
bathy_edi <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184?key=yltMpS4UEIk12AvB9L7OL5uRiG0")


#set up bathy so we have depth at dam and volume when reservoir is that deep
#flipping depth so that 0 depth isn't surface
ccr_bathy <- bathy_edi |>
  filter(Reservoir == "CCR") |>
  mutate(Depth_inverse = (Depth_m - 23)*-1) |>
  select(Depth_m = Depth_inverse,
         Volume_L = Volume_below_L)


#### Fit curve for Depth ~ Volume ----

#check linear
ggplot(ccr_bathy, aes(x = Depth_m, y = Volume_L)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "blue", se = TRUE)


#### fit pwoer law
library(nlme)

# fit power law with nls
bathy_power <- nls(Volume_L ~ a * Depth_m^b,
                   data  = ccr_bathy,
                   start = list(a = 1e6, b = 2))

summary(bathy_power)

# extract for annotation
a  <- round(coef(bathy_power)["a"], 0)
b  <- round(coef(bathy_power)["b"], 3)
r2 <- round(1 - sum(residuals(bathy_power)^2) /
              sum((ccr_bathy$Volume_L - mean(ccr_bathy$Volume_L))^2), 3)

# plot
ggplot(ccr_bathy, aes(x = Depth_m, y = Volume_L)) +
  geom_point(size = 2) +
  stat_function(fun = function(x) a * x^b, color = "blue", linewidth = 1) +
  annotate("text", x = Inf, y = -Inf,
           label = paste0("y = ", a, "x^", b, "\nR² = ", r2),
           hjust = 1.1, vjust = -0.5, size = 5) +
  labs(x = "Depth (m)", y = "Volume (L)", title = "CCR Bathymetry — Power Law Fit") +
  theme_bw()



#### Calc daily volume off of power law fit

#### Read in water level at fit to power law for volume
hydro <- read_csv("./Predictions/Data/Extras_Daily_Cat_Met_RH_2021_2026.csv") |>
  mutate(Strat = ifelse(Diff_Dens_1_max > 0.1, "Stratified", "Mixed")) |>
  select(Date, Strat, waterlevel_m, RH_Q_cms)

##fit power law and calc WRT
wrt <- hydro |>
  mutate(Volume_L = a * waterlevel_m^b) |>
  #set up units for WRT calc
  mutate(Volume_m3 = Volume_L / 1000,
         Q_m3_day = RH_Q_cms * 86400) |>
  mutate(WRT_days = Volume_m3 / Q_m3_day) |>
  select(Date, Strat, waterlevel_m, RH_Q_cms, WRT_days)

summary(wrt)
sd(wrt$WRT_days, na.rm = T)

#plots
wrt |>
  select(-Strat) |>
  pivot_longer(-1) |>
  ggplot(aes(x = Date, y = value))+
  geom_point()+
  scale_y_log10()+
  facet_wrap(~name, scales = "free_y", ncol = 1)


# #wrt v stratification
# wilcox_result <- wilcox.test(WRT_days ~ Strat, data = filter(wrt, !is.na(Strat)))
# p_val <- round(wilcox_result$p.value, 4)
# p_label <- ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", p_val))
#
# # plot
# wrt |>
#   filter(!is.na(Strat)) |>
#   ggplot(aes(x = Strat, y = WRT_days, fill = Strat)) +
#   geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # hide outliers since we plot all points
#   geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
#   # scale_y_log10()+
#   annotate("text", x = 1.5, y = max(wrt$WRT_days, na.rm = TRUE),
#            label = p_label, size = 5, fontface = "italic") +
#   labs(x = NULL, y = "Water Residence Time (days)") +
#   theme_bw() +
#   theme(legend.position = "none", text = element_text(size = 14))



#### WRT and flow as percentile comparisons
head(wrt)

usgs_hpb_flows <- read_csv("./Predictions/Data/HPB_USGS_Flows2.csv") |>
  select(Date, USGS_Q_cms, HPB_Q_cms,  HPB_Q_cms_filled)


hydro <- full_join(wrt, usgs_hpb_flows, by = "Date") |>
  filter(Date >= ymd("2021-08-19"),
         Date <= ymd("2026-02-01"))


#cdfs
# hydro |>
#   select(Date, RH_Q_cms, WRT_days, HPB_Q_cms_filled) |>
#   pivot_longer(-Date, names_to = "variable", values_to = "value") |>
#   filter(!is.na(value)) |>
#   ggplot(aes(x = value, color = variable)) +
#   stat_ecdf(linewidth = 1) +
#   facet_wrap(~variable, scales = "free_x") +
#   labs(x = "Value", y = "Cumulative Probability", color = NULL) +
#   theme_bw() + theme(legend.position = "top")

#hist
hydro |>
  select(Date, RH_Q_cms, WRT_days, HPB_Q_cms_filled, USGS_Q_cms) |>
  pivot_longer(-Date, names_to = "variable", values_to = "value") |>
  filter(!is.na(value)) |>
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.8) +
  facet_wrap(~variable, scales = "free") +
  labs(x = NULL, y = "Count", fill = NULL) +
  theme_bw() + theme(legend.position = "none")


#pdf
hydro |>
  select(Date, RH_Q_cms, WRT_days, HPB_Q_cms_filled, USGS_Q_cms) |>
  pivot_longer(-Date, names_to = "variable", values_to = "value") |>
  filter(!is.na(value)) |>
  ggplot(aes(x = value, fill = variable)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~variable, scales = "free") +
  scale_x_log10()+
  labs(x = NULL, y = "Density", fill = NULL) +
  theme_bw() + theme(legend.position = "none")

#calculate percentiles
hydro_deciles <- hydro |>
  filter(Date >= ymd("2024-01-01"),
         Date <= ymd("2026-02-01")) |>
  mutate(
    decile_RH_Q  = ntile(RH_Q_cms,        10),
    decile_WRT   = ntile(WRT_days,         10),
    decile_HPB   = ntile(HPB_Q_cms_filled, 10),
    decile_USGS  = ntile(USGS_Q_cms,       10)
  )


#plot some
hydro_deciles |>
  ggplot(aes(x = RH_Q_cms, y = WRT_days))+
  geom_point()+
  scale_y_log10()+ scale_x_log10()+
  geom_smooth(method = "lm")


hydro_deciles |>
  ggplot(aes(x = HPB_Q_cms_filled, y = WRT_days))+
  geom_point()+
  scale_y_log10()+ scale_x_log10()+
  geom_smooth(method = "lm")


# ## Histograms of WRT by HPB flows
# medians <- hydro_deciles |>
#   filter(Date > ymd("2024-01-01")) |>
#   filter(decile_HPB %in% c(1, 10)) |>
#   group_by(decile_HPB) |>
#   summarise(median_WRT = median(WRT_days, na.rm = TRUE),
#             sd_WRT = sd(WRT_days, na.rm = T)) |>
#   mutate(decile_HPB = factor(decile_HPB, labels = c("Low flow (D1)", "High flow (D10)")))
#
#
# hydro_deciles |>
#   filter(Date > ymd("2024-01-01")) |>
#   filter(decile_HPB %in% c(1, 10)) |>
#   mutate(decile_HPB = factor(decile_HPB, labels = c("Low flow (D1)", "High flow (D10)"))) |>
#   ggplot(aes(x = WRT_days, fill = decile_HPB, color = decile_HPB)) +
#   geom_density(alpha = 0.4) +
#   # scale_x_log10()+
#   geom_vline(data = medians, aes(xintercept = median_WRT, color = decile_HPB),
#               linetype = "dashed", linewidth = 1.2) +
#   labs(x = "Water Residence Time (days)", y = "Density", fill = NULL, color = NULL) +
#   theme_bw() +
#   theme(legend.position = "top", text = element_text(size = 14))
#
#
# #by RH Q
# medians <- hydro_deciles |>
#   filter(decile_RH_Q %in% c(1, 10)) |>
#   group_by(decile_RH_Q) |>
#   summarise(median_WRT = median(WRT_days, na.rm = TRUE),
#             sd_WRT = sd(WRT_days, na.rm = T)) |>
#   mutate(decile_RH_Q = factor(decile_RH_Q, labels = c("Low flow (D1)", "High flow (D10)")))
#
#
# hydro_deciles |>
#   filter(Date > ymd("2024-01-01")) |>
#   filter(decile_RH_Q  %in% c(1, 10)) |>
#   mutate(decile_RH_Q = factor(decile_RH_Q, labels = c("Low flow (D1)", "High flow (D10)"))) |>
#   ggplot(aes(x = WRT_days, fill = decile_RH_Q, color = decile_RH_Q)) +
#   geom_density(alpha = 0.4) +
#   # scale_x_log10()+
#   geom_vline(data = medians, aes(xintercept = median_WRT, color = decile_RH_Q),
#              linetype = "dashed", linewidth = 1.2) +
#   labs(x = "Water Residence Time (days)", y = "Density", fill = NULL, color = NULL) +
#   theme_bw() +
#   theme(legend.position = "top", text = element_text(size = 14))



#### Write csv of WRT time data ----
#wrtflow classes
wrt_flow_flags <- hydro_deciles |>
  select(Date, RH_Q_cms, decile_RH_Q, WRT_days, decile_WRT) |>
  mutate(flow_class_WRT = case_when(decile_WRT == 1  ~ "Fast WRT",  decile_WRT == 10 ~ "Slow WRT",
                                TRUE         ~ "Normal"),
         flow_class_RH_Q_cms = case_when(decile_RH_Q == 1  ~ "Low flow",  decile_RH_Q == 10 ~ "High flow",
                                    TRUE         ~ "Normal")
         )


write.csv(wrt_flow_flags, "./Predictions/Data/WRT_rhessys_estimates.csv", row.names = F)



