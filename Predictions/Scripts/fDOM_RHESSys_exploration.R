### Look at fDOM to RHESSys processes

library(tidyverse)

## read in fDOM data ----
eval <- read_csv("./Predictions/Data/Daily_catwalk_RH_2021_2026.csv")


#fdom TS
#plotly::ggplotly(
  eval |> ggplot(aes(x = Date, y = fDOM_1_QSU_daily))+geom_point()
  #)





#### read in RHESSys processes ----

workpath <- "./Catchment_Modeling/Run_RHESSys/out"

output_grow <- read_delim(paste0(workpath, "/ccrTR/HarvestNone/TR1850_2026_NOharvest_run_grow_basin.daily"),
                          delim = " ", col_names = T)

output_h2o <- read_delim(paste0(workpath, "/ccrTR/HarvestNone/TR1850_2026_NOharvest_run_basin.daily"),
                         delim = " ", col_names = T)


# output_h2o_grow <- left_join(output_h2o, output_grow, by = c("day", "month", "year", "basinID")) |>
#   mutate(date = ymd(paste(year, month, day, sep = "-"))) |>
#   filter(date >= ymd("2021-04-01")) |>
#   select(date, #streamflow, streamflow_NO3, streamflow_DOC, streamflow_DON, lai.y,
#          gpsn.y, resp, plant_resp, soil_resp, nuptake, denitrif, nitrif, grazingC) |>
#   rename(#lai = lai.y,
#          gpsn = gpsn.y)


#try a lag
output_h2o_grow_lag <- left_join(output_h2o, output_grow, by = c("day", "month", "year", "basinID")) |>
  mutate(date = ymd(paste(year, month, day, sep = "-"))) |>
  filter(date >= ymd("2021-04-01")) |>
  select(date, gpsn.y, resp, plant_resp, soil_resp, gw.Qout, unsat_drain, streamflow) |>
  rename(gpsn = gpsn.y)

# define the original variable names to lag
vars <- c("gpsn", "resp", "plant_resp", "soil_resp", "streamflow", "gw.Qout", "unsat_drain")

output_h2o_grow_lag <- output_h2o_grow_lag |>
  mutate(
    # across(all_of(vars), ~ slider::slide_dbl(., mean, .before = 15, .after = 0, .complete = FALSE),
    #        .names = "{.col}_15day"),
    across(all_of(vars), ~ slider::slide_dbl(., mean, .before = 30, .after = 0, .complete = FALSE),
           .names = "{.col}_30day"),
    # across(all_of(vars), ~ slider::slide_dbl(., mean, .before = 60, .after = 0, .complete = FALSE),
    #        .names = "{.col}_60day")
  )



### join to fDOM

fdom_RH <- eval |> select(Date, fDOM_1_QSU_daily) |>
  filter(Date >= ymd("2024-01-01")) |>
  full_join(output_h2o_grow_lag, by = c("Date" = "date"))


# ##plot ts
# fdom_RH |>
#   select(Date, fDOM_1_QSU_daily, gpsn, resp, gw.Qout ) |>
#   pivot_longer(-1) |>
#   ggplot(aes(x = Date, y = value))+
#   geom_point()+
#   facet_wrap(~name, scales = "free_y", ncol = 1)
#
#
# ##plot
# fdom_RH |>
#   pivot_longer(-c(1:2)) |>
#   ggplot(aes(x = value, y = fDOM_1_QSU_daily))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   facet_wrap(~name, scales = "free_x")


## cor plot
library(corrplot)

df_corr1m <- fdom_RH |>
  select(1,2, 10:16) |>
  select(-Date)

cor_matrix <- cor(df_corr1m, use = "pairwise.complete.obs")

cor_matrix <- round(cor_matrix, 2)

# compute p-values
p_matrix <- cor.mtest(df_corr1m, conf.level = 0.95)$p

# create significance mask based on |r| > 0.5
sig_matrix <- ifelse(abs(cor_matrix) >= 0.5, 0.04, 1)  # trick: use p.mat cutoff logic

corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         tl.col      = "black",
         tl.srt      = 45,
         tl.cex      = 0.8,
         cl.cex      = 0.8,
         mar         = c(0, 0, 0, 0),
         p.mat       = sig_matrix,
         sig.level   = 0.05,
         pch         = "*",
         pch.cex     = 2,
         insig       = "label_sig",
         col         = colorRampPalette(c("red", "white", "blue"))(200))



