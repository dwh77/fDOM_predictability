#### Compile data sets for CCR AR modeling


##packages
library(tidyverse)
library(rLakeAnalyzer) #for density
library(slider) # for rolling windows 'slide_dbl'


#### Daily Met ----
options(timeout = 600)  # 10 minutes
met <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1105/4/8ebf27393ccafe518328468a260d2e18")
range(met$DateTime)

met_githubL1 <- read.csv("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/refs/heads/ccre-dam-data-qaqc/ccre_met_L1.csv")
range(met_githubL1$DateTime)


# #Get daily summed rain and mean SW
daily_met <- plyr::rbind.fill(met, met_githubL1) |>
  select(DateTime, Rain_Total_mm, ShortwaveRadiationUp_Average_W_m2) |>
  mutate(Date = as.Date(DateTime)) |>
  group_by(Date) |>
  summarise(Rain_mm_daily = sum(Rain_Total_mm, na.rm = T),
            SW_Wm2_daily = mean(ShortwaveRadiationUp_Average_W_m2, na.rm = T))



#### EDI data sets ----
catwalk_EDI <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1069/4/42e6d8bb3d379d40a4a4fb566d4ff36e" )
summary(ymd_hms(catwalk_EDI$DateTime))

catwalk_githubL1 <- read.csv("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/refs/heads/ccre-dam-data-qaqc/ccre-waterquality_L1.csv")
summary(ymd_hms(catwalk_githubL1$DateTime))


catwalk <- rbind(catwalk_EDI, catwalk_githubL1) |>
  mutate(DateTime = ymd_hms(DateTime))


#get daily water level
waterlevel <- catwalk |>
  mutate(Date = as.Date(DateTime)) |>
  group_by(Date) |>
  summarise(waterlevel_m = mean(Modeled_Depth_m, na.rm = TRUE))

summary(waterlevel$waterlevel_m)


#### Format catwalk data ----

#### Get daily fDOM and EXO variables
p <- -0.01

catwalk_daily <- catwalk |>
  mutate(fdom1_TC = EXOfDOM_QSU_1/(1 + (p*(EXOTemp_C_1 - 20)) )
  ) |>
  mutate(Date = as.Date(DateTime)) |>
   group_by(Date) |>
  summarise(
    # fDOM (temperature corrected)
    fDOM_1_QSU_daily     = mean(fdom1_TC,              na.rm = TRUE),
    # Water temperature
    Temp_1_C_daily       = mean(EXOTemp_C_1,           na.rm = TRUE),
    # Chlorophyll-a
    Chla_1_ugL_daily     = mean(EXOChla_ugL_1,         na.rm = TRUE),
    # DO % saturation
    DOsat_1_pct_daily    = mean(EXODOsat_percent_1,    na.rm = TRUE),
    # # Specific conductance
    SC_1_uScm_daily  = mean(EXOSpCond_uScm_1,      na.rm = TRUE),
    #water level
    waterlevel_m = mean(Modeled_Depth_m, na.rm = TRUE)
  )


#### Get stratification values
source("./Predictions/Scripts/find_depths.R")
# depth_offsets_df <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1069/4/23caf92df7e665597ebc329d9e406637")

catwalk_EDI_link <- "https://pasta.lternet.edu/package/data/eml/edi/1069/4/42e6d8bb3d379d40a4a4fb566d4ff36e"
catwalk_github_link <- "https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/refs/heads/ccre-dam-data-qaqc/ccre-waterquality_L1.csv"
depth_offsets_link <- "https://pasta.lternet.edu/package/data/eml/edi/1069/4/23caf92df7e665597ebc329d9e406637"


depths_EDI <- find_depths(data_file = catwalk_EDI_link,
                      depth_offset = depth_offsets_link)

depths_git <- find_depths(data_file = catwalk_github_link,
                          depth_offset = depth_offsets_link)


unique(depths_EDI$variable)

##get daily depths
depths_daily <- rbind(depths_EDI, depths_git) |>
  filter(variable == "ThermistorTemp") |>
  mutate(depth1 = round(sensor_depth, 1)) |>
  mutate(Date = as.Date(DateTime)) |>
  group_by(Date, variable, Position) |>
  summarise(Temp_C = mean(observation, na.rm = T),
            sensor_depth_m = mean(sensor_depth, na.rm = T),
            .groups = "drop")


#get top and bottom temp
Dens_diff <- depths_daily %>%
  group_by(Date) %>%
  summarise(
    # Closest to 1 meter
    Depth_1m   = sensor_depth_m[which.min(abs(sensor_depth_m - 1))],
    Temp_1m    = Temp_C[which.min(abs(sensor_depth_m - 1))],
    # Max depth
    Depth_max  = max(sensor_depth_m),
    Temp_max   = Temp_C[which.max(sensor_depth_m)],
    .groups = "drop"
  ) |>
  mutate(depth_1m_check = 1 - Depth_1m) |>
  #Get strat metrics
  mutate(Diff_C_1_max = Temp_1m  - Temp_max,
         Diff_Dens_1_max = water.density(Temp_max) - water.density(Temp_1m))

## check how far off the 1m measurements are
summary(Dens_diff)

density_join <- Dens_diff |>
  select(Date, Diff_C_1_max, Diff_Dens_1_max)

density_join |>
  pivot_longer(-1) |>
  ggplot(aes(x = Date, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)+ theme_bw()


# ### make heatmap of temp profiles
# library(akima)  # for interpolation
#
# # Step 1: interpolate to 0.1m depth intervals for each date
# interp_df <- depths_daily |>
#   filter(!is.na(Temp_C), !is.na(sensor_depth_m)) |>
#   group_by(Date) |>
#   reframe({
#     if (n() >= 3) {
#       interp_result <- approx(
#         x    = sensor_depth_m,
#         y    = Temp_C,
#         xout = seq(0, max(sensor_depth_m), by = 0.1)  # start at 0
#       )
#       tibble(depth_interp = round(interp_result$x, 1),  # round to 1 decimal
#              Temp_interp  = interp_result$y)
#     } else {
#       tibble(depth_interp = round(sensor_depth_m, 1), Temp_interp = Temp_C)
#     }
#   }) |>
#   filter(!is.na(Temp_interp))  # drop NAs outside observed range
#
# # Step 2: heatmap
# ggplot(interp_df, aes(x = Date, y = depth_interp, fill = Temp_interp)) +
#   geom_tile() +
#   scale_y_reverse() +                          # surface at top
#   scale_fill_gradientn(
#     colors = c("blue", "cyan", "yellow", "red"),
#     name   = "Temp (°C)"
#   ) +
#   labs(x = "Date", y = "Depth (m)", title = "Water Temperature Heatmap") +
#   theme_bw() +
#   theme(text = element_text(size = 14))



#### Join exo to density ----
Catwalk_df <- full_join(catwalk_daily, density_join,  by = "Date")



#### Get RHESSys data and format to bind ----
##read in RHESSys
workpath <- "C:/Users/dwh18/OneDrive/Desktop/R_Projects/RHESSys_development/ccr_rhessys_dwh/out"  #ccr_rhessys/out/ccr_patch1500_cow1; ccr_patch1500_KEEP

output_grow <- read_delim(paste0(workpath, "/ccrTR/HarvestNone/TR1850_2026_NOharvest_run_grow_basin.daily"),
                          delim = " ", col_names = T)

output_h2o <- read_delim(paste0(workpath, "/ccrTR/HarvestNone/TR1850_2026_NOharvest_run_basin.daily"),
                         delim = " ", col_names = T)



#define area and format data
ccr_area_m2 <- 45.83578 * 1000000

output_h2o_grow <- left_join(output_h2o, output_grow, by = c("day", "month", "year", "basinID")) |>
  mutate(date = ymd(paste(year, month, day, sep = "-"))) |>
  filter(date >= ymd("2021-04-01")) |>
  select(date, streamflow, streamflow_NO3, streamflow_DOC, lai.y) |>
  rename(lai = lai.y) |>
  #Q unit conversions
  mutate(streamflow_m_day = streamflow / 1000) |> #convert mm/day to m/day
  mutate(streamflow_m3_day = streamflow_m_day * ccr_area_m2) |> #convert m/day to m3/day
  #chem conversions from g/m2/day to mg/L
  mutate(DOC_mgL = streamflow_DOC / streamflow_m_day,
         NO3_mgL = streamflow_NO3 / streamflow_m_day)


#format data for join
rhessys_df <- output_h2o_grow |>
  rename(Date = date) |>
  mutate(streamflow_cms = streamflow_m3_day / 86400) |>
  select(Date, streamflow_cms, DOC_mgL) |>
  rename(RH_Q_cms = streamflow_cms,
         RH_DOC_mgL = DOC_mgL)



#### format and export
datecheck <- seq(ymd("2021-08-19"), ymd("2026-02-01"), by = "day")

Catwalk_RH_df <- full_join(Catwalk_df, rhessys_df, by = "Date") |>
  full_join(daily_met, by = "Date") |>
  filter(Date >= ymd("2021-08-19"),
         Date <= ymd("2026-02-01")) |>
  arrange(Date) |>
  mutate(fDOM_1m_lag1 = lag(fDOM_1_QSU_daily , 1)) |>
  mutate(
    Chla_1_ugL_7day = slider::slide_dbl(Chla_1_ugL_daily, mean, .before = 7, .after = 0, .complete = F),
    Chla_1_ugL_14day = slider::slide_dbl(Chla_1_ugL_daily, mean, .before = 14, .after = 0, .complete = F),
    RH_Q_cms_7day = slider::slide_dbl(RH_Q_cms, mean, .before = 7, .after = 0, .complete = F),
    RH_Q_cms_14day = slider::slide_dbl(RH_Q_cms, mean, .before = 14, .after = 0, .complete = F),
  )
  # # Z-score
  # mutate(across(
  #   .cols = !Date,
  #   .fns  = ~ scale(.x)[,1],
  #   .names = "{.col}_ZS"
  # ))


#### Check lags and coor matrix

## ACF
library(astsa)
astsa::acf2(Catwalk_RH_df$fDOM_1_QSU_daily, xlim=c(1,20), na.action = na.pass) # Plots the ACF of x for lags 1 to 19
pacf(Catwalk_RH_df$fDOM_1_QSU_daily, xlim = c(1,20), na.action = na.pass)


##cor matrix
library(corrplot)

df_corr1m <- Catwalk_RH_df |>
  #select(-Date, fDOM_1m_lag1)
  select(-c(Date, fDOM_1m_lag1, Chla_1_ugL_7day, Chla_1_ugL_14day, RH_Q_cms_7day, RH_Q_cms_14day,
            Temp_1_C_daily, Diff_C_1_max, Rain_mm_daily, SW_Wm2_daily))


cor_matrix <- cor(df_corr1m, use = "pairwise.complete.obs")

# Step 1: plot with all numbers in normal weight
corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         tl.col      = "black",
         tl.srt      = 45,
         number.font = 1,          # all normal weight first
         col         = colorRampPalette(c("#D73027", "#FFFFBF", "#1A9850"))(200))

# Step 2: overlay bold numbers only where |r| >= 0.5
bold_matrix <- cor_matrix
bold_matrix[abs(cor_matrix) < 0.5] <- NA  # hide the weak ones

corrplot(bold_matrix,
         method      = "color",
         type        = "upper",
         add         = TRUE,       # overlay on existing plot
         addCoef.col = "black",
         tl.pos      = "n",        # suppress repeated labels
         cl.pos      = "n",        # suppress repeated legend
         number.font = 2,          # bold
         col         = colorRampPalette(c("#D73027", "#FFFFBF", "#1A9850"))(200))






### Select data for export
export_df <- Catwalk_RH_df |>
  dplyr::select(Date, fDOM_1_QSU_daily, fDOM_1m_lag1,
                Diff_Dens_1_max, DOsat_1_pct_daily, Chla_1_ugL_daily,
                RH_Q_cms, RH_DOC_mgL)


summary(export_df)


getwd()
#write.csv(export_df, "./Predictions/Data/Daily_catwalk_RH_2021_2026.csv", row.names = F)
# # also write df with all variables just to have daily data compiled
# write.csv(Catwalk_RH_df, "./Predictions/Data/Extras_Daily_Cat_Met_RH_2021_2026.csv", row.names = F)





