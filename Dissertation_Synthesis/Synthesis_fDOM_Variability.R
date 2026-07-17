#### Inter v Intra variability for Synthesis

##packages
library(tidyverse)





##################### Compile daily data ############################################
###  If compiled jump down to reading in csv ###

options(timeout = 600)  # 10 minutes

##get EDI data

fcr_catwalk <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136")

ccr_catwalk <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1069/4/42e6d8bb3d379d40a4a4fb566d4ff36e")

bvr_catwalk <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9")



## Trim data
p <- -0.01

#ccr
ccr_daily <- ccr_catwalk |>
  mutate(
    fdom1_TC = EXOfDOM_QSU_1/(1 + (p*(EXOTemp_C_1 - 20)) ),
    fdom9_TC = EXOfDOM_QSU_9/(1 + (p*(EXOTemp_C_9 - 20)) )
  ) |>
  mutate(Date = as.Date(DateTime)) |>
  group_by(Date) |>
  summarise(fDOM_1_QSU_daily = mean(fdom1_TC,  na.rm = TRUE),
            fDOM_9_QSU_daily = mean(fdom9_TC,  na.rm = TRUE)) |>
  mutate(Site = "CCR")

#fcr
fcr_daily <- fcr_catwalk |>
  mutate(fdom1_TC = EXOfDOM_QSU_1/(1 + (p*(EXOTemp_C_1 - 20)) )
  ) |>
  mutate(Date = as.Date(DateTime)) |>
  group_by(Date) |>
  summarise(fDOM_1_QSU_daily = mean(fdom1_TC,  na.rm = TRUE))|>
  mutate(Site = "FCR")

#bvr
bvr_daily <- bvr_catwalk |>
  mutate(fdom1_TC = EXOfDOM_QSU_1.5 / (1 + (p*(EXOTemp_C_1.5 - 20)) )
  ) |>
  mutate(Date = as.Date(DateTime)) |>
  group_by(Date) |>
  summarise(fDOM_1_QSU_daily = mean(fdom1_TC,  na.rm = TRUE))|>
  mutate(Site = "BVR")


#join daily
daily_fdom <- plyr::rbind.fill(ccr_daily, fcr_daily, bvr_daily) |>
  select(Date, Site, fDOM_1_QSU_daily, fDOM_9_QSU_daily)


head(daily_fdom)

getwd()

write.csv(daily_fdom, "./Dissertation_Synthesis/Daily_fDOM_data.csv", row.names = F)



##################### READ in csv from here ####################################


daily_fdom <- read.csv("./Dissertation_Synthesis/Daily_fDOM_data.csv") |>
  mutate(
    Year  = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Winter",
      Month %in% c(3, 4, 5)  ~ "Spring",
      Month %in% c(6, 7, 8)  ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Fall"
    ),
    Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall"))
  )



#### Seasons and year variability -------------------------------
library(tidyr)
library(gt)

# ---- Helper to compute summary stats ----
summarize_fdom <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(
      Mean   = mean(fDOM_1_QSU_daily, na.rm = TRUE),
      Median = median(fDOM_1_QSU_daily, na.rm = TRUE),
      Max    = max(fDOM_1_QSU_daily, na.rm = TRUE),
      Min    = min(fDOM_1_QSU_daily, na.rm = TRUE),
      SD     = sd(fDOM_1_QSU_daily, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Range = paste0(round(Min, 1), " - ", round(Max, 1)),
      CV    = round(SD / Mean, 2) * 100,
      Mean   = round(Mean, 2),
      Median = round(Median, 2)
    ) %>%
    select(-Max, -Min, -SD)
}

# ---- Table 1: by Site x Season (across all years) ----
season_table <- summarize_fdom(daily_fdom, Site, Season)

# ---- Table 2: by Site x Year ----
year_table <- summarize_fdom(daily_fdom, Site, Year)

# ---- Nice gt tables ----
season_gt <- season_table %>%
  arrange(Site, Season) %>%
  gt(groupname_col = "Site") %>%
  tab_header(title = "fDOM Summary by Site and Season") %>%
  cols_label(
    Season = "Season",
    Mean = "Mean",
    Median = "Median",
    Range = "Range (Max - Min)",
    CV = "CV"
  ) %>%
  fmt_number(columns = c(Mean, Median, CV), decimals = 1)

year_gt <- year_table %>%
  arrange(Site, Year) %>%
  gt(groupname_col = "Site") %>%
  tab_header(title = "fDOM Summary by Site and Year") %>%
  cols_label(
    Year = "Year",
    Mean = "Mean",
    Median = "Median",
    Range = "Range (Max - Min)",
    CV = "CV"
  ) %>%
  fmt_number(columns = c(Mean, Median, CV), decimals = 1)

season_gt
year_gt


## diff across season
season_table |>
  group_by(Site) |>
  summarize(range_median = max(Median) - min(Median))


year_table |>
  group_by(Site) |>
  summarize(range_median = max(Median) - min(Median))















