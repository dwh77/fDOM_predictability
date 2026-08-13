#### fDOM to DOC

library(tidyverse)
library(ggpmisc)
library(patchwork)

#read in chem
## Carey lab read only key: ?key=yltMpS4UEIk12AvB9L7OL5uRiG0
chem <- read.csv( "https://pasta.lternet.edu/package/data/eml/edi/199/13/3f09a3d23b7b5dd32ed7d28e9bc1b081?key=yltMpS4UEIk12AvB9L7OL5uRiG0" )
# chem <- read.csv("C:/Users/dwh18/Downloads/chemistry_2013_2024 (2).csv")

fdom_df <- read_csv("./Dissertation_Synthesis/Daily_fDOM_data.csv")


#### fDOM vs DOC at each reservoir's shallow depth (CCR = 1.5m, FCR = 1.6m) --

## DOC at each reservoir's shallow depth
doc_shallow <- chem |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Reservoir %in% c("CCR", "FCR"), Site == 50) |>
  select(Reservoir, Date, Depth_m, DOC_mgL) |>
  group_by(Reservoir, Date, Depth_m) |>
  summarise(DOC = mean(DOC_mgL, na.rm = TRUE), .groups = "drop") |>
  filter((Reservoir == "CCR" & Depth_m == 1.5) |
         (Reservoir == "FCR" & Depth_m == 1.6)) |>
  filter(DOC > 0.1) |>
  select(-Depth_m)

## fDOM at each reservoir's shallow depth
fdom_shallow <- fdom_df |>
  filter(Site %in% c("CCR", "FCR")) |>
  select(Date, Reservoir = Site, fDOM = fDOM_1_QSU_daily) |>
  filter(!is.na(fDOM))

## combined df: Reservoir, Date, fDOM, DOC
fdom_doc_shallow <- inner_join(fdom_shallow, doc_shallow, by = c("Date", "Reservoir")) |>
  filter(!is.na(fDOM), !is.na(DOC))

## shared regression + plot builder -- prints the lm summary and correlation
## test to console, and plots with a linear fit + R^2/p annotation, plus a
## separate "Pearson's r = " text annotation just below it. Uses
## geom = "text_npc" (normalized panel coordinates, the same mechanism
## stat_poly_eq's label.x/label.y use internally) rather than the more usual
## annotate("text", x = -Inf, ...) trick -- confirmed x = -Inf silently drops
## the label under log_log's scale_x_log10()/scale_y_log10() (log(-Inf) isn't
## a valid position on a log-transformed scale), while text_npc positions by
## panel fraction and renders correctly on both linear and log axes. small.p = TRUE
## renders a lowercase p in the stat_poly_eq label (R^2 stays capitalized,
## matching standard notation). exclude_months drops rows whose Date falls in
## those calendar months *before* fitting, so excluded months are removed
## from both the regression and the plotted points -- e.g.
## exclude_months = c(9, 10) drops September and October. log_log = TRUE fits
## log(DOC) ~ log(fDOM) instead -- scale_x_log10()/scale_y_log10() make
## stat_poly_line()/stat_poly_eq() compute the fit in log-log space (verified:
## the R^2/p shown then match a manual lm(log(DOC) ~ log(fDOM)) fit exactly,
## not just relabeled linear-fit ticks), and the printed console summary plus
## the Pearson's r are computed on logged values to match what's plotted.
plot_fdom_doc <- function(df, title, exclude_months = NULL, log_log = FALSE) {

  if (!is.null(exclude_months)) {
    df <- df |> filter(!month(Date) %in% exclude_months)
  }

  if (log_log) {
    fit <- lm(log(DOC) ~ log(fDOM), data = df)
    print(summary(fit))
    cor_result <- cor.test(log(df$fDOM), log(df$DOC), method = "pearson")
    print(cor_result)
  } else {
    fit <- lm(DOC ~ fDOM, data = df)
    print(summary(fit))
    cor_result <- cor.test(df$fDOM, df$DOC, method = "pearson")
    print(cor_result)
  }

  pearson_label <- paste0("Pearson's r = ", round(cor_result$estimate, 2))

  # Single reservoir: plain black points, no legend (nothing to distinguish by
  # color). Multiple reservoirs: color by Reservoir with a legend, as before.
  one_reservoir <- n_distinct(df$Reservoir) == 1

  p <- df |> ggplot(aes(x = fDOM, y = DOC))

  if (one_reservoir) {
    p <- p + geom_point(color = "black")
  } else {
    p <- p + geom_point(aes(color = Reservoir))
  }

  p <- p +
    stat_poly_line(method = "lm", linewidth = 2, color = "black") +
    stat_poly_eq(small.p = TRUE, label.x = "left", label.y = "top", parse = TRUE,
                 aes(label = after_stat(paste(rr.label, p.value.label, sep = "~~~")))) +
    annotate(geom = "text_npc", npcx = 0.05, npcy = 0.83,
             label = pearson_label, hjust = 0) +
    labs(x = "fDOM (QSU)", y = "DOC (mg/L)", color = "Reservoir", title = title) +
    theme_bw()+ theme(legend.position = "top")

  if (one_reservoir) {
    p <- p + theme(legend.position = "none")
  }

  if (log_log) {
    p <- p +
      scale_x_log10() +
      scale_y_log10() +
      labs(x = "fDOM (QSU, log scale)", y = "DOC (mg/L, log scale)")
  }

  p
}

## 1) CCR only
plot_fdom_doc(fdom_doc_shallow |> filter(Reservoir == "CCR"), "CCR: fDOM vs DOC, 1.5m")

## 2) FCR only
plot_fdom_doc(fdom_doc_shallow |> filter(Reservoir == "FCR"), "FCR: fDOM vs DOC, 1.6m")

## 3) CCR and FCR combined, single regression fit across all the data
plot_fdom_doc(fdom_doc_shallow, "CCR and FCR combined: fDOM vs DOC, shallow depth")


# #check log log
# plot_fdom_doc(fdom_doc_shallow |> filter(Reservoir == "FCR"), "FCR: fDOM vs DOC, 1.6m", log_log = T)
# plot_fdom_doc(fdom_doc_shallow |> filter(Reservoir == "CCR"), "CCR: fDOM vs DOC, 1.5m",  log_log = T)


# example excluding specific months from the fit, e.g. Sept/Oct:
plot_fdom_doc(fdom_doc_shallow |> filter(Reservoir == "CCR"),
              "CCR: fDOM vs DOC, 1.5m (no Sep/Oct)", exclude_months = c(9, 10,11))

plot_fdom_doc(fdom_doc_shallow |> filter(Reservoir == "FCR"),
              "FCR: fDOM vs DOC, 1.5m (no Sep/Oct)", exclude_months = c(9, 10,11))


## Time series of fDOM (left axis) and DOC (right axis) for one reservoir at a
## time (CCR or FCR, not combined). ggplot2 has no independent secondary
## y-scale -- sec_axis() only relabels a linear transform of the primary
## scale. DOC is rescaled onto fDOM's numeric range for plotting, then
## sec_axis() back-transforms the right-axis labels to real DOC units.
plot_fdom_doc_ts <- function(df, reservoir, title = reservoir) {

  df_res <- df |> filter(Reservoir == reservoir)

  fdom_range <- range(df_res$fDOM, na.rm = TRUE)
  doc_range  <- range(df_res$DOC,  na.rm = TRUE)

  rescale_doc_to_fdom <- function(doc) {
    (doc - doc_range[1]) / diff(doc_range) * diff(fdom_range) + fdom_range[1]
  }
  rescale_fdom_to_doc <- function(y) {
    (y - fdom_range[1]) / diff(fdom_range) * diff(doc_range) + doc_range[1]
  }

  df_res |>
    mutate(DOC_scaled = rescale_doc_to_fdom(DOC)) |>
    ggplot(aes(x = Date)) +
    geom_point(aes(y = fDOM, color = "fDOM"), size = 1) +
    geom_point(aes(y = DOC_scaled, color = "DOC"), size = 1.8) +
    scale_color_manual(values = c("fDOM" = "black", "DOC" = "dodgerblue")) +
    scale_y_continuous(
      name     = "fDOM (QSU)",
      sec.axis = sec_axis(~ rescale_fdom_to_doc(.), name = "DOC (mg/L)")
    ) +
    labs(x = "Date", color = NULL, title = title) +
    theme_bw() +
    theme(legend.position = "top")
}

## CCR
plot_fdom_doc_ts(fdom_doc_shallow, "CCR")

## FCR
plot_fdom_doc_ts(fdom_doc_shallow, "FCR")













#### fDOM to EEMs ----
eems <- read_csv("https://raw.githubusercontent.com/dwh77/Carvins_Carbon/refs/heads/main/Data/chemistry_joined.csv")

eems_sensor <- eems |>
  filter(Site == 50, Depth_m == 1.5) |>
  select(Date, DOC_mgL, DON_mgL, HIX, BIX, SUVA254, a254_m, Peak_A, Peak_T)


ccrfdom1m <- fdom_doc_shallow |>
  filter(Reservoir == "CCR")


fdom_eems <- left_join(eems_sensor, ccrfdom1m, by = "Date") |>
  select(-DOC, -Reservoir) |>
  select(Date, fDOM, everything())

fdom_eems |>
  pivot_longer(-c(1:2)) |>
  ggplot(aes(x = fDOM, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y")+
  geom_smooth(method = "lm")





#### check fDOM ~ chla ####
fdomchla <- read_csv("./Predictions/Data/Daily_catwalk_RH_2021_2026.csv") |>
  filter(!is.na(fDOM_1_QSU_daily),
         !is.na(Chla_1_ugL_daily))


fdomchla |>
  ggplot(aes(x = fDOM_1_QSU_daily, y = Chla_1_ugL_daily))+
  geom_point()+
  geom_smooth(method = "lm")

cor(fdomchla$fDOM_1_QSU_daily, fdomchla$Chla_1_ugL_daily)


















############################# OLD #################################################
################ 1) CCR: fDOM vs DOC, choose depth(s) -- 1.5, 9, or both ------------------------

## DOC by depth, CCR only
ccr_doc_long <- chem |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Date > ymd("2021-04-01")) |>
  filter(Reservoir == "CCR", Site %in% c(50)) |>
  select(Date, Depth_m, DOC_mgL) |>
  group_by(Date, Depth_m) |>
  summarise(DOC = mean(DOC_mgL, na.rm = TRUE), .groups = "drop") |>
  filter(Depth_m %in% c(1.5, 9)) |>
  rename(Depth = Depth_m)

## fDOM by depth, CCR only
ccr_fdom_long <- fdom_df |>
  filter(Site == "CCR") |>
  select(Date, fDOM_1_QSU_daily, fDOM_9_QSU_daily) |>
  pivot_longer(cols = c(fDOM_1_QSU_daily, fDOM_9_QSU_daily),
               names_to = "Depth", values_to = "fDOM") |>
  mutate(Depth = case_when(
    Depth == "fDOM_1_QSU_daily" ~ 1.5,
    Depth == "fDOM_9_QSU_daily" ~ 9
  ))

## combined df: fDOM, Depth, DOC (CCR only)
ccr_fdom_doc <- inner_join(ccr_fdom_long, ccr_doc_long, by = c("Date", "Depth")) |>
  filter(!is.na(fDOM), !is.na(DOC))

## regression + plot for CCR at the chosen depth(s): 1.5, 9, or c(1.5, 9)
plot_ccr_fdom_doc <- function(depths = c(1.5, 9)) {

  df <- ccr_fdom_doc |>
    mutate(month = month(Date)) |>
    #filter(!month %in% c(10,11)) |>
    filter(Depth %in% depths)

  fit <- lm(DOC ~ fDOM, data = df)
  print(summary(fit))
  print(cor.test(df$fDOM, df$DOC, method = "pearson"))

  df |>
    ggplot(aes(x = fDOM, y = DOC)) +
    geom_point(aes(color = factor(Depth))) +
    stat_poly_line(method = "lm", linewidth = 2, color = "black") +
    stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
                 aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~"), size = 3)) +
    labs(x = "fDOM (QSU)", y = "DOC (mg/L)", color = "Depth (m)",
         title = paste0("CCR: fDOM vs DOC, depth = ", paste(depths, collapse = " & "))) +
    theme_bw()
}

plot_ccr_fdom_doc(1.5)        # CCR, 1.5m only
plot_ccr_fdom_doc(9)          # CCR, 9m only
plot_ccr_fdom_doc(c(1.5, 9))  # CCR, both depths pooled


################ 2) CCR (1.5m) and FCR (1.6m): fDOM vs DOC pooled across reservoirs ------------------------

## DOC at each reservoir's shallow depth
ccrfcr_doc <- chem |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Reservoir %in% c("CCR", "FCR"), Site %in% c(50)) |>
  select(Reservoir, Date, Depth_m, DOC_mgL) |>
  group_by(Reservoir, Date, Depth_m) |>
  summarise(DOC = mean(DOC_mgL, na.rm = TRUE), .groups = "drop") |>
  filter((Reservoir == "CCR" & Depth_m == 1.5) |
         (Reservoir == "FCR" & Depth_m == 1.6)) |>
  select(-Depth_m)

## fDOM at each reservoir's shallow depth
ccrfcr_fdom <- fdom_df |>
  filter(Site %in% c( "FCR")) |>
  select(Date, Reservoir = Site, fDOM = fDOM_1_QSU_daily) |>
  filter(!is.na(fDOM))

## combined df: fDOM, Reservoir, DOC
ccrfcr_fdom_doc <- inner_join(ccrfcr_fdom, ccrfcr_doc, by = c("Date", "Reservoir")) |>
  filter(!is.na(fDOM), !is.na(DOC))

## regression + plot, pooled across CCR and FCR
plot_ccrfcr_fdom_doc <- function(df = ccrfcr_fdom_doc) {

  fit <- lm(DOC ~ fDOM, data = df)
  print(summary(fit))
  print(cor.test(df$fDOM, df$DOC, method = "pearson"))

  df |>
    ggplot(aes(x = fDOM, y = DOC)) +
    geom_point(aes(color = Reservoir)) +
    stat_poly_line(method = "lm", linewidth = 2, color = "black") +
    stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
                 aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~"), size = 3)) +
    labs(x = "fDOM (QSU)", y = "DOC (mg/L)", color = "Reservoir",
         title = "CCR (1.5m) and FCR (1.6m): fDOM vs DOC pooled") +
    theme_bw()
}

plot_ccrfcr_fdom_doc()



#############################################################################################
########################### OLD ###########################################

# Filter chem
exo_chem <- chem |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Date > ymd("2021-04-08")) |>
  filter(Reservoir == "CCR",
         Site %in% c(50,51)) |>
  select(Reservoir, Site, Date, Depth_m, Rep, DOC_mgL) |>
  group_by(Reservoir, Site, Date, Depth_m) |>
  summarise(across(c(DOC_mgL), mean, na.rm = TRUE)) |>
  filter(Depth_m %in% c(1.5)) |> #c(1.5,9)
  pivot_wider(
    names_from  = Depth_m,
    values_from = DOC_mgL,
    names_prefix = "DOC_",
    names_glue  = "DOC_{Depth_m}m"
  )


## read in fDOM
# eval <- read_csv("./Predictions/Data/Daily_catwalk_RH_2021_2026.csv") |>
#   mutate(Date = as.Date(Date)) |> filter(Date <= ymd("2026-01-31")) |>
#   select(Date, fDOM_1_QSU_daily )

ccr_daily <- read_csv("./Dissertation_Synthesis/Daily_fDOM_data.csv") |>
  filter(Site == "CCR")



## join
fdom_doc <- full_join(exo_chem, eval, by = "Date") |>
  mutate(yday = yday(Date), month = month(Date)) |>
  #filter(!month %in% c(9, 10,11)) |>
  # filter(DOC_1.5m < 3.9, DOC_1.5m > 2.2) |>
  filter(!is.na(fDOM_1_QSU_daily),
         !is.na(DOC_1.5m))


################ CCR fDOM vs DOC across both depths (1.5m and 9m) ------------------------

## DOC in long format (Depth as its own column) for both depths, CCR only
chem_long <- chem |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Date > ymd("2021-04-01")) |>
  filter(Reservoir == "CCR",
         Site %in% c(50)) |>
  select(Reservoir, Date, Depth_m, Rep, DOC_mgL) |>
  group_by(Reservoir, Date, Depth_m) |>
  summarise(across(c(DOC_mgL), mean, na.rm = TRUE), .groups = "drop") |>
  filter(Depth_m %in% c(1.5, 9)) |>
  rename(Depth = Depth_m, DOC = DOC_mgL)

## fDOM in long format (Depth as its own column) for both depths
fdom_long <- ccr_daily |>
  select(Date, fDOM_1_QSU_daily, fDOM_9_QSU_daily) |>
  pivot_longer(cols = c(fDOM_1_QSU_daily, fDOM_9_QSU_daily),
               names_to = "Depth", values_to = "fDOM") |>
  mutate(Depth = case_when(
    Depth == "fDOM_1_QSU_daily" ~ 1.5,
    Depth == "fDOM_9_QSU_daily" ~ 9
  ))

## combined df: fDOM, Depth, DOC
fdom_doc_depth <- inner_join(fdom_long, chem_long, by = c("Date", "Depth")) |>
  select(Date, Depth, fDOM, DOC) |>
  filter(!is.na(fDOM), !is.na(DOC))

## regression pooled across both depths
doc_fdom_lm <- lm(DOC ~ fDOM, data = fdom_doc_depth)
summary(doc_fdom_lm)

cor.test(fdom_doc_depth$fDOM, fdom_doc_depth$DOC, method = "pearson")

## plot: points colored by depth, single pooled regression line
fdom_doc_depth |>
  filter(Depth == 9) |>
  ggplot(aes(x = fDOM, y = DOC)) +
  geom_point(aes(color = factor(Depth))) +
  #geom_smooth()+
  stat_poly_line(method = "lm", linewidth = 2, color = "black") +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~"), size = 3)) +
  labs(x = "fDOM (QSU)", y = "DOC (mg/L)", color = "Depth (m)",
       title = "CCR: fDOM vs DOC, 9") + #fDOM vs DOC, 1.5m and 9m pooled
  theme_bw()

















################ CCR and FCR fDOM vs DOC, shallow sensor depth ------------------------
## Shallow chem/sensor depth differs by reservoir: CCR = 1.5m, FCR = 1.6m

## DOC at the shallow depth for CCR and FCR
chem_1v5_ccrfcr <- chem |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Reservoir %in% c("CCR", "FCR"),
         Site %in% c(50, 51)) |>
  select(Reservoir, Date, Depth_m, Rep, DOC_mgL) |>
  group_by(Reservoir, Date, Depth_m) |>
  summarise(across(c(DOC_mgL), mean, na.rm = TRUE), .groups = "drop") |>
  filter((Reservoir == "CCR" & Depth_m == 1.5) |
         (Reservoir == "FCR" & Depth_m == 1.6)) |>
  rename(DOC = DOC_mgL)

## fDOM at 1.5m for CCR and FCR, from the same synthesis file used for ccr_daily
fdom_1v5_ccrfcr <- read_csv("./Dissertation_Synthesis/Daily_fDOM_data.csv") |>
  filter(Site %in% c("CCR", "FCR")) |>
  select(Date, Reservoir = Site, fDOM = fDOM_1_QSU_daily) |>
  filter(!is.na(fDOM))

## combined df: fDOM, Reservoir, DOC
fdom_doc_ccrfcr <- inner_join(fdom_1v5_ccrfcr, chem_1v5_ccrfcr, by = c("Date", "Reservoir")) |>
  select(Date, Reservoir, fDOM, DOC) |>
  filter(!is.na(fDOM), !is.na(DOC))

## regression pooled across both reservoirs
doc_fdom_lm_ccrfcr <- lm(DOC ~ fDOM, data = fdom_doc_ccrfcr)
summary(doc_fdom_lm_ccrfcr)

cor.test(fdom_doc_ccrfcr$fDOM, fdom_doc_ccrfcr$DOC, method = "pearson")

## plot: points colored by reservoir, single pooled regression line
fdom_doc_ccrfcr |>
  filter(DOC > 0.1) |>
  ggplot(aes(x = fDOM, y = DOC)) +
  geom_point(aes(color = Reservoir)) +
  stat_poly_line(method = "lm", linewidth = 2, color = "black") +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~"), size = 3)) +
  labs(x = "fDOM (QSU)", y = "DOC (mg/L)", color = "Reservoir",
       title = "CCR and FCR: fDOM vs DOC 1.5m") +
  theme_bw()


################ Time series: fDOM (left axis) and DOC (right axis) by reservoir ------------------------
## ggplot2 has no independent secondary y-scale -- sec_axis() only relabels a
## linear transform of the primary scale, and that single transform applies to
## the whole plot, not per facet. CCR and FCR have different fDOM/DOC ranges, so
## scaling is computed separately per reservoir and each gets its own ggplot
## (with its own correct sec_axis transform), then stacked with patchwork.

make_fdom_doc_ts_plot <- function(df, reservoir_name) {

  df_res <- df |> filter(Reservoir == reservoir_name)

  fdom_range <- range(df_res$fDOM, na.rm = TRUE)
  doc_range  <- range(df_res$DOC,  na.rm = TRUE)

  rescale_doc_to_fdom <- function(doc) {
    (doc - doc_range[1]) / diff(doc_range) * diff(fdom_range) + fdom_range[1]
  }
  rescale_fdom_to_doc <- function(y) {
    (y - fdom_range[1]) / diff(fdom_range) * diff(doc_range) + doc_range[1]
  }

  df_res |>
    mutate(DOC_scaled = rescale_doc_to_fdom(DOC)) |>
    ggplot(aes(x = Date)) +
    geom_point(aes(y = fDOM, color = "fDOM"), size = 1) +
    geom_point(aes(y = DOC_scaled, color = "DOC"), size = 1.8) +
    scale_color_manual(values = c("fDOM" = "black", "DOC" = "dodgerblue")) +
    scale_y_continuous(
      name     = "fDOM (QSU)",
      sec.axis = sec_axis(~ rescale_fdom_to_doc(.), name = "DOC (mg/L)")
    ) +
    labs(x = "Date", color = NULL, title = reservoir_name) +
    theme_bw() +
    theme(legend.position = "top")
}

ccr_fdom_doc_ts <- make_fdom_doc_ts_plot(fdom_doc_ccrfcr, "CCR")
fcr_fdom_doc_ts <- make_fdom_doc_ts_plot(fdom_doc_ccrfcr, "FCR")

ccr_fdom_doc_ts / fcr_fdom_doc_ts +
  plot_annotation(title = "CCR and FCR: fDOM and DOC time series, shallow depth")






######### Metals #########################
metals <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/455/9/9a072c4e4af39f96f60954fc4f7d8be5")

## TFe at shallow depth (CCR = 1.5m, FCR = 1.6m), Site 50 only
fe_shallow <- metals |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Reservoir %in% c("CCR", "FCR"),
         Site == 50) |>
  select(Reservoir, Date, Depth_m, TFe_mgL) |>
  group_by(Reservoir, Date, Depth_m) |>
  summarise(TFe_mgL = mean(TFe_mgL, na.rm = TRUE), .groups = "drop") |>
  filter((Reservoir == "CCR" & Depth_m == 1.5) |
         (Reservoir == "FCR" & Depth_m == 1.6)) |>
  select(-Depth_m)

fe_shallow |>
  ggplot(aes(x = Date, y = TFe_mgL, color = Reservoir, shape = Reservoir))+
  geom_point()


################ FCR fDOM vs TFe regression, shallow depth ------------------------

fcr_fe_shallow <- fe_shallow |>
  filter(Reservoir == "FCR") |>
  select(Date, TFe_mgL)

fcr_fdom_shallow <- fdom_1v5_ccrfcr |>
  filter(Reservoir == "FCR") |>
  select(Date, fDOM)

fcr_fdom_fe <- inner_join(fcr_fdom_shallow, fcr_fe_shallow, by = "Date") |>
  filter(!is.na(fDOM), !is.na(TFe_mgL))

fcr_fdom_fe_lm <- lm(TFe_mgL ~ fDOM, data = fcr_fdom_fe)
summary(fcr_fdom_fe_lm)

cor.test(fcr_fdom_fe$fDOM, fcr_fdom_fe$TFe_mgL, method = "pearson")

fcr_fdom_fe |>
  ggplot(aes(x = fDOM, y = TFe_mgL)) +
  geom_point() +
  stat_poly_line(method = "lm", linewidth = 2) +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~"), size = 3)) +
  labs(x = "fDOM (QSU)", y = "TFe (mg/L)",
       title = "FCR: fDOM vs TFe, 1.6m") +
  theme_bw()


################ FCR: correct fDOM for Fe interference, then vs DOC ------------------------
## No interpolation -- correction only applied on dates with both an Fe and a
## DOC measurement. fDOMcor = fDOMuncor * 0.9284 * exp(0.414 * Fe)

fcr_doc_shallow <- chem_1v5_ccrfcr |>
  filter(Reservoir == "FCR") |>
  select(Date, DOC)

fcr_fdom_fe_doc <- fcr_fdom_shallow |>
  inner_join(fcr_fe_shallow, by = "Date") |>
  inner_join(fcr_doc_shallow, by = "Date") |>
  filter(!is.na(fDOM), !is.na(TFe_mgL), !is.na(DOC)) |>
  mutate(fDOM_corrected = fDOM * 0.9284 * exp(0.414 * TFe_mgL))

## how many days actually have all three (fDOM, Fe, DOC)?
nrow(fcr_fdom_fe_doc)
range(fcr_fdom_fe_doc$Date)

## regression: Fe-corrected fDOM vs DOC
fdom_cor_doc_lm <- lm(DOC ~ fDOM_corrected, data = fcr_fdom_fe_doc)
summary(fdom_cor_doc_lm)

cor.test(fcr_fdom_fe_doc$fDOM_corrected, fcr_fdom_fe_doc$DOC, method = "pearson")

fcr_fdom_fe_doc |>
  ggplot(aes(x = fDOM_corrected, y = DOC)) +
  geom_point() +
  stat_poly_line(method = "lm", linewidth = 2) +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~"), size = 3)) +
  labs(x = "Fe-corrected fDOM (QSU)", y = "DOC (mg/L)",
       title = "FCR: Fe-corrected fDOM vs DOC, 1.6m (paired Fe+DOC days only)") +
  theme_bw()


fcr_fdom_fe_doc |>
  ggplot(aes(x = fDOM, y = DOC)) +
  geom_point() +
  stat_poly_line(method = "lm", linewidth = 2) +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~"), size = 3)) +
  labs(x = "fDOM (QSU)", y = "DOC (mg/L)",
       title = "fDOM vs DOC, 1.6m ") +
  theme_bw()






############# OLD #################################

##quick ggplot lm
fdom_doc |>
  #filter(!month %in% c(9, 10,11)) |>
  # filter(DOC_1.5m < 3.75, DOC_1.5m > 2.25) |>
  ggplot(aes(x = fDOM_1_QSU_daily, y = DOC_1.5m, color = month))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2) +
stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
             inherit.aes = FALSE, aes(x = fDOM_1_QSU_daily, y = DOC_1.5m,
                                      label = paste(..adj.rr.label.., ..p.value.label.., sep = "~~~"), size = 3)  ) +
theme_bw()


## get pearson cor
doc_lm <- lm(DOC_1.5m ~ fDOM_1_QSU_daily, data = fdom_doc)

lm_label <- paste0("y = ", round(coef(doc_lm)[2], 3), "x ", round(coef(doc_lm)[1], 3))

cor.test(fdom_doc$DOC_1.5m, fdom_doc$fDOM_1_QSU_daily, method = "pearson")
correlation <- cor(fdom_doc$DOC_1.5m, fdom_doc$fDOM_1_QSU_daily, method = "pearson")

cor_label <- paste0("Pearson's r = ", round(correlation, 2))


#plot
fdom_doc |>
  ggplot(aes(x = fDOM_1_QSU_daily, y = DOC_1.5m))+
  geom_point()+
  geom_abline(intercept = coef(doc_lm)[1], slope = coef(doc_lm)[2],
              linewidth = 1)+
  theme_bw()+
  xlim(5,20)+ ylim(2,4)+
  labs(x = "fDOM (QSU)", y = "DOC (mg/L)")+
  ggtitle("1.5 m: excluding fall")+
  annotate("text", x = 19, y = 2.5, label = lm_label, hjust = 1.1, vjust = -0.5, size = 3.5)+
  annotate("text", x = 19, y = 2.6, label = cor_label, hjust = 1.1, vjust = -0.5, size = 3.5)



################ CCR 9 m  check ------------------------
### check fcr
ccr9m_chem <- chem |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Date > ymd("2021-04-01")) |>
  filter(Reservoir == "CCR",
         Site %in% c(50, 51)) |>
  select(Reservoir, Site, Date, Depth_m, Rep, DOC_mgL) |>
  group_by(Reservoir, Site, Date, Depth_m) |>
  summarise(across(c(DOC_mgL), mean, na.rm = TRUE)) |>
  filter(Depth_m %in% c(9)) |>
  pivot_wider(
    names_from  = Depth_m,
    values_from = DOC_mgL,
    names_prefix = "DOC_",
    names_glue  = "DOC_{Depth_m}m"
  )


ccr9m_daily <- read_csv("./Dissertation_Synthesis/Daily_fDOM_data.csv") |>
  filter(Site == "CCR") |> select(-fDOM_1_QSU_daily)

CCR9m_fdom_doc <- full_join(ccr9m_chem, ccr9m_daily, by = "Date") |>
  mutate(yday = yday(Date), month = month(Date)) |>
  # filter(!month %in% c(9, 10,11)) |>
  # filter(DOC_1.6m < 10) |>
  filter(!is.na(fDOM_9_QSU_daily),
         !is.na(DOC_9m))

#cor and p value
cor_result <- cor.test(CCR9m_fdom_doc$DOC_9m, CCR9m_fdom_doc$fDOM_9_QSU_daily, method = "pearson")

r_val <- round(cor_result$estimate, 3)
p_val <- cor_result$p.value
p_label <- ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3)))

#plot
CCR9m_fdom_doc |>
  ggplot(aes(x = fDOM_9_QSU_daily, y = DOC_9m)) +
  geom_point() +
  ggtitle("CCR 9m")+
  stat_poly_line(method = "lm", linewidth = 2) +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
               inherit.aes = FALSE, aes(x = fDOM_9_QSU_daily, y = DOC_9m,
                                        label = paste(..adj.rr.label.., ..p.value.label.., sep = "~~~"), size = 3)  ) +
  theme_bw()





################ FCR check ------------------------

bvrchemcheck <- chem |>
  filter(Reservoir == "BVR",
         Depth_m > 0.1, Depth_m < 3)


### check fcr
fcr_chem <- chem |>
  mutate(Date = as.Date(DateTime)) |>
  filter(Date > ymd("2018-05-01")) |>
  filter(Reservoir == "FCR",
         Site %in% c(50)) |>
  select(Reservoir, Site, Date, Depth_m, Rep, DOC_mgL) |>
  group_by(Reservoir, Site, Date, Depth_m) |>
  summarise(across(c(DOC_mgL), mean, na.rm = TRUE)) |>
  filter(Depth_m %in% c(1.6)) |>
  pivot_wider(
    names_from  = Depth_m,
    values_from = DOC_mgL,
    names_prefix = "DOC_",
    names_glue  = "DOC_{Depth_m}m"
  )




FCR_fdom_doc <- full_join(fcr_chem, fcr_daily, by = "Date") |>
  mutate(yday = yday(Date), month = month(Date)) |>
  filter(!month %in% c(9, 10,11)) |>
  # filter(DOC_1.6m < 10) |>
  filter(!is.na(fDOM_1_QSU_daily),
         !is.na(DOC_1.6m))

#cor and p value
cor_result <- cor.test(FCR_fdom_doc$DOC_1.6m, FCR_fdom_doc$fDOM_1_QSU_daily, method = "pearson")

r_val <- round(cor_result$estimate, 3)
p_val <- cor_result$p.value
p_label <- ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3)))

#plot
FCR_fdom_doc |>
  ggplot(aes(x = fDOM_1_QSU_daily, y = DOC_1.6m)) +
  geom_point() +
  ggtitle("FCR 1.5m no fall")+
  stat_poly_line(method = "lm", linewidth = 2) +
  annotate("text", x = -Inf, y = 11.5,
           label = paste0("Pearson r = ", r_val, ",  ", p_label),
           hjust = -0.1, vjust = 1.5, size = 5) +
  theme_bw()

