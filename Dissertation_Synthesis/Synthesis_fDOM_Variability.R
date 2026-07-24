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

#fdom
daily_fdom <- read.csv("./Dissertation_Synthesis/Daily_fDOM_data.csv") |>
  mutate(Date = as.Date(Date),
    Year  = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Winter",
      Month %in% c(3, 4, 5)  ~ "Spring",
      Month %in% c(6, 7, 8)  ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Fall"
    ),
    Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |>
  filter(Year > 2021)


#metab
# daily_fcr_metab <- read.csv("https://raw.githubusercontent.com/dwh77/FCR_Metab/refs/heads/main/Data/Model_Output/MetabOutput_QAQC_15_22.csv") |>
#   select(solarDay, GPP_QAQC, R_QAQC, NEM_QAQC) |>
#   mutate(R_QAQC = -R_QAQC,          #set R rates to negative
#          solarDay = ymd(solarDay)) |>
#   rename(Date = solarDay) |>
#   mutate(
#     Year  = year(Date),
#     Month = month(Date),
#     Season = case_when(
#       Month %in% c(12, 1, 2) ~ "Winter",
#       Month %in% c(3, 4, 5)  ~ "Spring",
#       Month %in% c(6, 7, 8)  ~ "Summer",
#       Month %in% c(9, 10, 11) ~ "Fall"
#     ),
#     Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall"))
#   )



#### Seasons and year variability -------------------------------
library(gt)


##fDOM by reservoir plot
daily_fdom |>
  ggplot(aes(x = Date, y = fDOM_1_QSU_daily))+
  geom_point()+
  facet_wrap(~Site, ncol = 1)+
  theme_bw()

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



#### Boxplots of CV by Year and Season, across reservoirs -------------------------------

# ---- CV per Site x Year x Season (gives a distribution of CV across years for each season) ----
season_year_table <- summarize_fdom(daily_fdom, Site, Year, Season)

# ---- Long-format df: one CV value per Site x Year, tagged by Group (Year or Season) ----
cv_boxplot_df <- bind_rows(
  year_table |>
    select(Site, Year, CV) |>
    mutate(Group = "Full Year"),
  season_year_table |>
    select(Site, Year, Season, CV) |>
    mutate(Group = as.character(Season))
) |>
  mutate(Group = factor(Group, levels = c("Full Year", "Spring", "Summer", "Fall", "Winter")))

# ---- Boxplot: CV by Year/Season grouping, one box per reservoir within each group ----
# points are jittered within each box's dodge position and shaped by Year
# (8 years in the record, so shapes are set manually since ggplot's default
# discrete shape scale only supports 6 before it starts dropping points)
# point colour is mapped to Site (shapes 0-7 have no fill, so fill alone
# doesn't distinguish points) using the same palette as the box fill so the
# two legends merge into one "Reservoir" key
site_colors <- c(BVR = "orange", CCR = "skyblue", FCR = "#009E73")

aaa <- cv_boxplot_df |>
  ggplot(aes(x = Group, y = CV, fill = Site)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  geom_point(
    aes(shape = factor(Year), group = Site),
    colour = "black",
    position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0, dodge.width = 0.75),
    size = 2
  ) +
  ylim(0,40)+
  scale_shape_manual(values = 15:18) +
  scale_fill_manual(values = site_colors) +
  labs(x = NULL, y = "CV (%)", fill = "Reservoir", shape = "Year") +
  theme_bw()

aaa

#### Boxplots of CV by Full Record vs individual Year, across reservoirs ---------------

# ---- Long-format df: one CV value per Site x Season, tagged by Group2 ----
# "All Years" uses season_table (CV pooled across the whole record, one point
# per season); each year column uses season_year_table filtered to that year
# (one point per season within that year). Update the year labels below if
# the filtered date range in daily_fdom changes.
cv_boxplot_by_year_df <- bind_rows(
  season_table |>
    select(Site, Season, CV) |>
    mutate(Group2 = "All Years"),
  season_year_table |>
    mutate(Group2 = sprintf("%02d", Year %% 100)) |>
    select(Site, Season, CV, Group2)
) |>
  mutate(Group2 = factor(Group2, levels = c("All Years", "22", "23", "24", "25")))

# ---- Boxplot: CV by Full-record/Year grouping, one box per reservoir within each group ----
# points are the season CVs within that grouping, shaped by Season, dodged to
# line up with each reservoir's box (group = Site keeps the dodge to 3 slots
# instead of splitting further by Season)
bbb <- cv_boxplot_by_year_df |>
  ggplot(aes(x = Group2, y = CV, fill = Site)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
  geom_point(
    aes(shape = Season, group = Site),
    colour = "black",
    position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0, dodge.width = 0.75),
    size = 2
  ) +
  ylim(0,40)+
  scale_shape_manual(values = 15:18) +
  scale_fill_manual(values = site_colors) +
  labs(x = NULL, y = "CV (%)", fill = "Reservoir", shape = "Season") +
  theme_bw()

bbb


aaa | bbb


#### CV (%) by Site and Year/Season grouping, pooled across 2022-2025 -----------------

# ---- Full-record CV per Site (all days, all years pooled, i.e. "Full Year") ----
overall_table <- summarize_fdom(daily_fdom, Site) |>
  mutate(Group = "Full Year")

# ---- Combine with season_table's pooled Site x Season CV ----
cv_point_df <- bind_rows(
  overall_table |> select(Site, Group, CV, Mean),
  season_table |> mutate(Group = as.character(Season)) |> select(Site, Group, CV, Mean)
) |>
  mutate(Group = factor(Group, levels = c("Full Year", "Spring", "Summer", "Fall", "Winter")))

# ---- Plot: one point per Site x Group, CV (%) mapped to point size ----
cv_point_df |>
  ggplot(aes(x = Group, y = Site, size = CV)) +
  geom_point() +
  scale_size(range = c(3, 14)) +
  labs(x = NULL, y = NULL, size = "CV (%)") +
  theme_bw()

## CV by size and color
cv_point_df |>
  ggplot(aes(x = Group, y = Site, fill = CV, size = CV)) +
  geom_point(shape = 21) +
  scale_size(range = c(3, 14)) +
  # scale_size(range = c(3, 14), breaks = c(10, 20, 30), limits = c(0, 40)) +
  labs(x = NULL, y = NULL, size = "CV (%)", color = "CV (%)") +
  #scale_color_viridis_c()+
  scale_fill_distiller(palette = "Blues", direction = 1)+
  theme_bw()


cv_point_df |>
  ggplot(aes(x = Group, y = CV, fill = Site)) +
  geom_jitter(shape = 21, size = 5, width = 0.2) +
  labs(x = "Time Frame", y = "CV (%)")+
  scale_fill_manual(values = site_colors) +
  theme_bw()


## mean by color and size
cv_point_df |>
  ggplot(aes(x = Group, y = Site, fill = Mean, size = CV)) +
  geom_point(shape = 21) +
  scale_size(range = c(5, 20)) +
  scale_size(range = c(3, 14), breaks = c(10, 20, 30), limits = c(10, 40)) +
  labs(x = NULL, y = NULL, size = "CV (%)", color = "CV (%)") +
  #scale_color_viridis_c()+
  scale_fill_distiller(palette = "RdYlGn", direction = -1)+
  theme_bw()











#### Raw daily fDOM boxplots by Year and Season, faceted by reservoir -----------------
library(patchwork)
library(FSA)         # dunnTest()
library(rcompanion)  # cldList() compact letter display

# ---- Helper: per-Site Kruskal-Wallis test + Dunn post-hoc compact letters ----
# returns $kw (one row per Site: p-value, label, label y-position) and
# $letters (one row per Site x group with significant post-hoc separation)
kw_dunn_by_site <- function(df, group_col) {
  df$.grp <- factor(df[[group_col]])
  form <- reformulate(".grp", response = "fDOM_1_QSU_daily")

  kw_df <- do.call(rbind, lapply(split(df, df$Site), function(sub) {
    p <- kruskal.test(form, data = sub)$p.value
    data.frame(
      Site     = unique(sub$Site),
      p_kw     = p,
      kw_label = paste0("KW p ", if (p < 0.001) "< 0.001" else paste0("= ", round(p, 3))),
      y        = max(sub$fDOM_1_QSU_daily, na.rm = TRUE) * 1.15
    )
  }))

  letters_list <- lapply(split(df, df$Site), function(sub) {
    p <- kruskal.test(form, data = sub)$p.value
    if (p >= 0.05) return(NULL)  # only post-hoc test significant KW results

    dunn_res  <- FSA::dunnTest(form, data = sub, method = "bh")$res
    cld       <- rcompanion::cldList(P.adj ~ Comparison, data = dunn_res, threshold = 0.05, remove.zero = FALSE)
    y_by_grp  <- tapply(sub$fDOM_1_QSU_daily, sub$.grp, max, na.rm = TRUE) * 1.05
    data.frame(
      Site   = unique(sub$Site),
      grp    = as.character(cld$Group),
      Letter = cld$Letter,
      y      = as.numeric(y_by_grp[as.character(cld$Group)])
    )
  })
  letters_df <- do.call(rbind, letters_list)
  if (is.null(letters_df)) {
    letters_df <- data.frame(Site = character(), grp = character(), Letter = character(), y = numeric())
  }
  names(letters_df)[names(letters_df) == "grp"] <- group_col

  list(kw = kw_df, letters = letters_df)
}

year_stats   <- kw_dunn_by_site(daily_fdom, "Year")
season_stats <- kw_dunn_by_site(daily_fdom, "Season")

# panel a: daily fDOM distribution by year (2022-2025), one facet per reservoir
fdom_by_year_plot <- daily_fdom |>
  ggplot(aes(x = factor(Year), y = fDOM_1_QSU_daily)) +
  geom_boxplot(aes(fill = Site), outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, size = 0.6, alpha = 0.3, colour = "black") +
  geom_text(data = year_stats$letters, aes(x = factor(Year), y = 33, label = Letter), inherit.aes = FALSE, size = 3.5) +
  geom_text(data = year_stats$kw, aes(x = -Inf, y = 1, label = kw_label), inherit.aes = FALSE, hjust = -0.05, size = 3) +
  facet_wrap(~Site) +
  ylim(0,35)+
  scale_fill_manual(values = site_colors) +
  labs(x = NULL, y = "Daily fDOM (QSU)") +
  theme_bw() +
  theme(legend.position = "none")

# panel b: daily fDOM distribution by season, one facet per reservoir
fdom_by_season_plot <- daily_fdom |>
  ggplot(aes(x = Season, y = fDOM_1_QSU_daily)) +
  geom_boxplot(aes(fill = Site), outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, size = 0.6, alpha = 0.3, colour = "black") +
  geom_text(data = season_stats$letters, aes(x = Season, y = 33, label = Letter), inherit.aes = FALSE, size = 3.5) +
  geom_text(data = season_stats$kw, aes(x = -Inf, y = 1, label = kw_label), inherit.aes = FALSE, hjust = -0.05, size = 3) +
  facet_wrap(~Site) +
  ylim(0,35)+
  scale_fill_manual(values = site_colors) +
  labs(x = NULL, y = "Daily fDOM (QSU)") +
  theme_bw() +
  theme(legend.position = "none")

wrap_plots(fdom_by_year_plot, fdom_by_season_plot, ncol = 1) +
  plot_layout(axis_titles = "collect", axes = "collect_x") +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 14, face = "bold"))


#### Summary table: Inter- vs Intra-annual variability by reservoir --------------------

# ---- Inter-annual: mean (and range) of each site's yearly means ----
inter_summary <- year_table |>
  group_by(Site) |>
  summarise(
    Inter_mean  = round(mean(Mean, na.rm = TRUE), 2),
    Inter_range = paste0(round(min(Mean, na.rm = TRUE), 2), " - ", round(max(Mean, na.rm = TRUE), 2)),
    .groups = "drop"
  )

# ---- Intra-annual: mean (and range) of each site's seasonal means ----
intra_summary <- season_table |>
  group_by(Site) |>
  summarise(
    Intra_mean  = round(mean(Mean, na.rm = TRUE), 2),
    Intra_range = paste0(round(min(Mean, na.rm = TRUE), 2), " - ", round(max(Mean, na.rm = TRUE), 2)),
    .groups = "drop"
  )

variability_summary_table <- inter_summary |>
  left_join(intra_summary, by = "Site") |>
  mutate(Site = factor(Site, levels = c("FCR", "BVR", "CCR"))) |>
  arrange(Site)

variability_summary_gt <- variability_summary_table |>
  gt() |>
  tab_header(title = "Inter- vs Intra-annual fDOM Variability by Reservoir") |>
  cols_label(
    Site = "Reservoir",
    Inter_mean = "Inter-annual Mean",
    Inter_range = "Inter-annual Range",
    Intra_mean = "Intra-annual Mean",
    Intra_range = "Intra-annual Range"
  )

variability_summary_gt











