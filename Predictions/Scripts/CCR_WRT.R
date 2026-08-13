#### Water residence time (WRT) calcs for CCR based on RHESSys

eval <- read.csv("./Predictions/Data/Daily_catwalk_RH_2021_2026.csv")

summary(eval$RH_Q_cms)

## CCR volume at full pond
ccr_Vol_m3 <- 2.3*10^7


### Max and min discharge from full RHESSys CCR runs
ccr_Q_m3day_max <- 2.14 * 86400

ccr_Q_m3day_min <- 0.023 * 86400

ccr_Q_m3day_25th <- 0.13 * 86400

#### CCR fastest WRT
ccr_Vol_m3 / ccr_Q_m3day_max

#### CCR slowest WRT
ccr_Vol_m3 / ccr_Q_m3day_min

ccr_Vol_m3 / ccr_Q_m3day_25th



####### HPB subset ####

## HPB subsection volume at full pont (trimming at dam)
hpb_Vol_m3 <-1.1*10^6 #this is just the HPB arm before it opens up   # this value gets closer to island: 2.5*10^6

#hpb q
hpb_Q_m3day_max <- 1 * 86400


#HPB fast wrt
hpb_Vol_m3 / hpb_Q_m3day_max


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


#wrt v strat
wilcox_result <- wilcox.test(WRT_days ~ Strat, data = filter(wrt, !is.na(Strat)))
p_val <- round(wilcox_result$p.value, 4)
p_label <- ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", p_val))

# plot
wrt |>
  filter(!is.na(Strat)) |>
  ggplot(aes(x = Strat, y = WRT_days, fill = Strat)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # hide outliers since we plot all points
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  scale_y_log10()+
  annotate("text", x = 1.5, y = max(wrt$WRT_days, na.rm = TRUE),
           label = p_label, size = 5, fontface = "italic") +
  labs(x = NULL, y = "Water Residence Time (days)") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size = 14))







