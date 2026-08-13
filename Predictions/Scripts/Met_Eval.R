## Compare met data products

library(tidyverse)

#### EDI met ----
options(timeout = 600)  # 10 minutes
met <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1105/4/8ebf27393ccafe518328468a260d2e18?key=yltMpS4UEIk12AvB9L7OL5uRiG0")
range(met$DateTime)

met_githubL1 <- read.csv("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/refs/heads/ccre-dam-data-qaqc/ccre_met_L1.csv")
range(met_githubL1$DateTime)

daily_met <- plyr::rbind.fill(met, met_githubL1) |>
  select(DateTime, Rain_Total_mm, AirTemp_C_Average) |>
  mutate(Date = as.Date(DateTime)) |>
  group_by(Date) |>
  summarise(Rain_mm_daily = sum(Rain_Total_mm, na.rm = T),
            Tmin_C = min(AirTemp_C_Average, na.rm = T),
            Tmax_C = max(AirTemp_C_Average, na.rm = T)) |>
  mutate(Data = "CCR_dam")




### ERA5 ----
era5 <- read_csv("C:/Users/dwh18/OneDrive/Desktop/R_Projects/RHESSys_development/ccr_rhessys_dwh/clim/era5_1jan1940_1may2026.csv") |>
  #convert rain from m to mm
  mutate(Rain_mm_daily = rain *1000) |>
  dplyr::select(Date = DATE,
         Rain_mm_daily,
         Tmin_C = tmin,
         Tmax_C = tmax)|>
  mutate(Data = "ERA5")



#### NOAA ROA airport weather ----

#set url to read in
station_id <- "USW00013741"   # Roanoke-Blacksburg Regional Airport, VA

url <- paste0(
    "https://www.ncei.noaa.gov/access/services/data/v1?",
    "dataset=daily-summaries",
    "&stations=", station_id,
    "&startDate=1948-01-01",
    "&endDate=", Sys.Date(),
    "&dataTypes=TMIN,TMAX,PRCP",
    "&units=metric",
    "&format=csv"
  )

#read in data
roa_met_df <- read_csv(url, show_col_types = FALSE)

#format
roa_met <- roa_met_df |>
  dplyr::select(Date = DATE,
                Rain_mm_daily = PRCP,
                Tmin_C = TMIN,
                Tmax_C = TMAX)|>
  mutate(Data = "NOAA_ROA")



##### Function to plot and compare ----
plot_met_compare <- function(df1, df2, var, title) {
  name1 <- unique(df1$Data)
  name2 <- unique(df2$Data)

  joined <- full_join(
    df1 |> select(Date, Rain_mm_daily, Tmin_C, Tmax_C),
    df2 |> select(Date, Rain_mm_daily, Tmin_C, Tmax_C),
    by = "Date",
    suffix = c(paste0("_", name1), paste0("_", name2))
  )

  x_col <- paste0(var, "_", name1)
  y_col <- paste0(var, "_", name2)

  plot_df <- joined |>
    select(x = all_of(x_col), y = all_of(y_col)) |>
    filter(!is.na(x), !is.na(y))

  fit <- lm(y ~ x, data = plot_df)
  print(summary(fit))
  cor_result <- cor.test(plot_df$x, plot_df$y, method = "pearson")
  print(cor_result)

  pearson_label <- paste0("Pearson's r = ", round(cor_result$estimate, 2))

  ggplot(plot_df, aes(x = x, y = y)) +
    geom_point(color = "black") +
    stat_poly_line(method = "lm", linewidth = 2, color = "black") +
    stat_poly_eq(small.p = TRUE, label.x = "left", label.y = "top", parse = TRUE,
                 aes(label = after_stat(paste(rr.label, p.value.label, sep = "~~~")))) +
    annotate(geom = "text_npc", npcx = 0.05, npcy = 0.83,
             label = pearson_label, hjust = 0) +
    labs(x = paste0(var, " (", name1, ")"), y = paste0(var, " (", name2, ")"), title = title) +
    theme_bw()
} #end function


#### Run function for plots ----
#ERA5~dam
SI_tmin <- plot_met_compare(era5, daily_met, "Tmin_C", "Tmin: ERA5 vs CCR_dam")
SI_tmax <- plot_met_compare(era5, daily_met, "Tmax_C", "Tmax: ERA5 vs CCR_dam")
SI_rain <- plot_met_compare(era5, daily_met, "Rain_mm_daily", "Rain: ERA5 vs CCR_dam")

SI_tmin | SI_tmax | SI_rain

(SI_tmin + labs(tag = "a")) |   (SI_tmax + labs(tag = "b")) |   (SI_rain + labs(tag = "c")) &
  theme(plot.tag = element_text(size = 14, face = "bold"))

#ERA5~ROA
plot_met_compare(era5, roa_met, "Tmin_C", "Tmin: ERA5 vs NOAA_ROA")
plot_met_compare(era5, roa_met, "Tmax_C", "Tmax: ERA5 vs NOAA_ROA")
plot_met_compare(era5, roa_met, "Rain_mm_daily", "Rain: ERA5 vs NOAA_ROA")

#dam~ROA
plot_met_compare(daily_met, roa_met, "Tmin_C", "Tmin: CCR_dam vs NOAA_ROA")
plot_met_compare(daily_met, roa_met, "Tmax_C", "Tmax: CCR_dam vs NOAA_ROA")
plot_met_compare(daily_met, roa_met, "Rain_mm_daily", "Rain: CCR_dam vs NOAA_ROA")





##############################################################################################
#percent of wet days
pct_rain_any <- mean(roa_met$Rain_mm_daily > 0, na.rm = TRUE) * 100
pct_rain_025 <- mean(roa_met$Rain_mm_daily >= 0.25, na.rm = TRUE) * 100
pct_rain_1 <- mean(roa_met$Rain_mm_daily >= 1, na.rm = TRUE) * 100

#### Temporal trends in long term data #####
roa_met_yearly <- roa_met |>
  filter(Date < ymd("2026-01-01")) |>
  mutate(Year = year(Date)) |>
  group_by(Year) |>
  summarise(Total_Rain = sum(Rain_mm_daily, na.rm = TRUE),
            N_Rain_Days = sum(Rain_mm_daily > 0, na.rm = TRUE),
            P10_Rain = quantile(Rain_mm_daily[Rain_mm_daily > 0], 0.10, na.rm = TRUE),
            P25_Rain = quantile(Rain_mm_daily[Rain_mm_daily > 0], 0.25, na.rm = TRUE),
            P50_Rain = quantile(Rain_mm_daily[Rain_mm_daily > 0], 0.50, na.rm = TRUE),
            P75_Rain = quantile(Rain_mm_daily[Rain_mm_daily > 0], 0.75, na.rm = TRUE),
            P90_Rain = quantile(Rain_mm_daily[Rain_mm_daily > 0], 0.90, na.rm = TRUE),
            .groups = "drop")





##plot
roa_met_yearly |>
  pivot_longer(-1) |>
  ggplot(aes(x = Year, y = value))+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_poly_eq(small.p = TRUE, label.x = "left", label.y = "top", parse = TRUE,
               aes(label = after_stat(paste(rr.label, p.value.label, sep = "~~~"))))+
  facet_wrap(~name, scales = "free_y")




