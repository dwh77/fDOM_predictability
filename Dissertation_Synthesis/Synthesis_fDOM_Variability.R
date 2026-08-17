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
  select(-fDOM_9_QSU_daily ) |>
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



## summary stats by reservoir
daily_fdom |>
  group_by(Site) |>
  summarise(min = min(fDOM_1_QSU_daily, na.rm = T),
            mean = mean(fDOM_1_QSU_daily, na.rm = T),
            median = median(fDOM_1_QSU_daily, na.rm = T),
            max = max(fDOM_1_QSU_daily, na.rm = T),
            sd = sd(fDOM_1_QSU_daily, na.rm = T)) |>
  mutate(CV = round(sd / mean, 2) * 100 )






####################################################################################################
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




####################################################################################################
#### Intra vs Inter CV variability -------------------------------

# ---- Helper to compute CV ----
summarize_fdom <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(
      Mean = mean(fDOM_1_QSU_daily, na.rm = TRUE),
      SD   = sd(fDOM_1_QSU_daily, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(CV = round(SD / Mean, 2) * 100)
}

# ---- Table 1: by Site x Season (across all years) ----
season_table <- summarize_fdom(daily_fdom, Site, Season)

# ---- Table 2: by Site x Year ----
year_table <- summarize_fdom(daily_fdom, Site, Year)

site_colors <- c(BVR = "orange", CCR = "skyblue", FCR = "#009E73")


#### Intra vs Inter CV, faceted by reservoir (sensu Whitney space/time boxplots) -------

# ---- Intra: the 4 season CVs (season_table), Label = Season; Inter: the 4 year CVs
# (year_table), Label = Year. Combined into one 8-level shape scale (solid shapes for
# the 4 seasons, hollow shapes for the 4 years) so both point types share one legend ----
intra_inter_df <- bind_rows(
  season_table |> mutate(Group = "Intra", Label = as.character(Season)) |> select(Site, CV, Group, Label),
  year_table   |> mutate(Group = "Inter", Label = as.character(Year))   |> select(Site, CV, Group, Label)
) |>
  mutate(
    Group = factor(Group, levels = c("Intra", "Inter")),
    Label = factor(Label, levels = c("Winter", "Spring", "Summer", "Fall", "2022", "2023", "2024", "2025"))
  )

# ---- Values going into each box, and the mean CV per Site x Group (for reporting) ----
intra_inter_df |> arrange(Site, Group, Label)

intra_inter_means <- intra_inter_df |>
  group_by(Site, Group) |>
  summarise(Mean_CV = round(mean(CV), 2), n = n(), .groups = "drop")

intra_inter_means

# ---- t-test (Intra vs Inter) within each reservoir ----
ttest_by_site <- do.call(rbind, lapply(split(intra_inter_df, intra_inter_df$Site), function(sub) {
  p <- t.test(CV ~ Group, data = sub)$p.value
  data.frame(
    Site  = unique(sub$Site),
    p_val = p,
    label = paste0("t-test p ", if (p < 0.001) "< 0.001" else paste0("= ", round(p, 3)))
  )
}))

ttest_by_site

# ---- Plot ----
intra_inter_df |>
  ggplot(aes(x = Group, y = CV, fill = Site)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(
    aes(shape = Label),
    colour = "black",
    position = position_jitter(width = 0.15, height = 0),
    size = 5
  ) +
  stat_summary(fun = mean, geom = "point", shape = 4, colour = "red", size = 5, stroke = 1.5) +
  geom_text(data = ttest_by_site, aes(x = 1.5, y = -Inf, label = label),
            inherit.aes = FALSE, vjust = -0.5, size = 5) +
  facet_wrap(~Site, nrow = 1) +
  scale_shape_manual(values = c(15, 16, 17, 18, 0, 1, 2, 5)) +
  scale_fill_manual(values = site_colors) +
  guides(fill = "none") +
  labs(x = NULL, y = "CV (%)", shape = "Season / Year") +
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 24))
