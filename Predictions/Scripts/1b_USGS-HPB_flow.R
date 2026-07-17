## Script to gap fill missing HPB flow with data from USGS
# also calculate flow percentiles for period of prediction generation



library(dataRetrieval)
library(ggpmisc)


#### Read in and look at USGS flow from Tinker creek ------------------------
#get tinker creek data from USGS
USGS <- read_waterdata_daily(
  monitoring_location_id = "USGS-02055100",  # c("USGS-02055100", "USGS-02018500") #Tinker, Catawaba
  parameter_code = "00060",
  time = c("2020-01-01", "2026-04-01")
)


#clean up daily data
USGS_daily <- USGS |>
  rename(Date = time) |>
  mutate(Flow_cms = value / 35.3) |>
  select(Date, Flow_cms) |>
  sf::st_drop_geometry()



#Monthly usgs plots
USGS |>
  mutate(Date = time, Flow = value) |>
  mutate(year = year(Date),
         month_year = floor_date(Date, "month")) |>
  filter(year > 2023) |>
  ggplot(aes(x = factor(month_year), y = (Flow/35.3))) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 1) +
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.5, color = "gray30") +
  scale_x_discrete(labels = function(x) format(as.Date(x), "%b %Y")) +
  scale_y_log10()+
  labs(x = NULL, y = "USGS Tinker Discharge (cms)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        text = element_text(size = 13))




#### Read in HPB flow ----------------------------
hpbQ <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/2333/1/630f42ffb3560c3a6afd592511756c1e")

#get daily mean stage
HPB_daily <- hpbQ |>
  mutate(Date = as.Date(DateTime)) |>
  group_by(Date) |>
  summarise(#Daily_Stage_cm = mean(stage_cm, na.rm = T),
            Daily_Q_cms = mean(Flow_cms, na.rm = T))

#look at data
HPB_daily |> ggplot(aes(x= Date, y = Daily_Q_cms))+geom_point()

#join to USGS
USGS_HPB_Q <- full_join(USGS_daily, HPB_daily, by = "Date") |>
  rename(USGS_Q_cms = Flow_cms,  HPB_Q_cms = Daily_Q_cms)


#### HPB regress to USGS ----
#look at regress
USGS_HPB_Q |>
  # filter(HPB_Q_cms < 500,
  #        USGS_Q_cms < 5000) |>
  ggplot(aes(x = USGS_Q_cms, y = HPB_Q_cms)) +
  geom_point() +
  xlim(0,5)+
  stat_poly_line(method = "lm", linewidth = 2) +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
               inherit.aes = FALSE, aes(x = USGS_Q_cms, y = HPB_Q_cms,
                                        label = paste(..adj.rr.label.., ..p.value.label.., sep = "~~~"), size = 3)  ) +
  labs(x = "USGS Flow (cms)", y = "HPB Q (cms)") +
  theme_bw()


## fit regression
Qcms_lm <- lm(HPB_Q_cms ~ USGS_Q_cms, data = USGS_HPB_Q)
summary(Qcms_lm)
intercept <- coef(Qcms_lm)[1]
slope <- coef(Qcms_lm)[2]

lm_label <- paste0("y = ", round(coef(Qcms_lm)[2], 3), "x ", round(coef(Qcms_lm)[1], 3))

#calc interp and fill missing
USGS_HPB_Q <- USGS_HPB_Q |>
  mutate(HPBinterp_Q_cms = (slope*USGS_Q_cms) + intercept
    ) |>
  mutate(HPB_Q_cms_filled = ifelse(is.na(HPB_Q_cms), HPBinterp_Q_cms, HPB_Q_cms))


## get pearson r
Q_noNA <- USGS_HPB_Q |>
  filter(!is.na(HPB_Q_cms))

cor.test(Q_noNA$HPB_Q_cms, Q_noNA$USGS_Q_cms, method = "pearson")
correlation <- cor(Q_noNA$HPB_Q_cms, Q_noNA$USGS_Q_cms, method = "pearson")
correlation

cor_label <- paste0("Pearson's r = ", round(correlation, 2))

#Plot
USGS_HPB_Q |>
  ggplot(aes(x = USGS_Q_cms, y = HPB_Q_cms))+
  geom_point()+
  geom_abline(intercept = coef(Qcms_lm)[1], slope = coef(Qcms_lm)[2],
              linewidth = 0.75, color = "red")+
  theme_bw()+
  xlim(0,4)+
  labs(x = "USGS Flow (cms)", y = "HPB Flow (cms)")+
  annotate("text", x = 1.5, y = 0.65, label = lm_label,
           hjust = 1.1, vjust = -0.5, size = 3.5)+
  annotate("text", x = 1.5, y = 0.75, label = cor_label,
           hjust = 1.1, vjust = -0.5, size = 3.5)


##save figure
# ggsave("./Predictions/Figures/HPB_USGS_SIfig.png", height = 3.5, width = 4.5, units = "in")


#timeseries
USGS_HPB_Q |>
  filter(Date > ymd("2024-04-15")) |>
  select(Date, HPB_Q_cms, HPBinterp_Q_cms) |>
  pivot_longer(-1) |>
  ggplot(aes(x= Date, y = value, color = name))+
  geom_point()+
  theme_bw()+ theme(legend.position = "top")






#### Get flow percentiles --------------------------------

#get percentiles for days based on USGS for forecast eval timeframe
head(USGS_HPB_Q)


# flow_percentiles_USGS <- USGS_HPB_Q |>
#   filter(Date >= ymd("2024-01-01"), Date <= ymd("2026-01-31")) |>
#   filter(!is.na(USGS_Q_cms )) |>
#   mutate(flow_percentile = percent_rank(USGS_Q_cms) * 100)



flow_flags <- USGS_HPB_Q |>
  filter(Date >= ymd("2024-01-01"), Date <= ymd("2026-01-31")) |>
  mutate(
    #USGS
    decile_usgs     = ntile(USGS_Q_cms , 10),
    flow_class_usgs = case_when(decile_usgs == 1  ~ "Low flow",  decile_usgs == 10 ~ "High flow",
      TRUE         ~ "Normal"),
    #HPB filled
    decile_hpb     = ntile(HPB_Q_cms_filled , 10),
    flow_class_hpb = case_when(decile_hpb == 1  ~ "Low flow",  decile_hpb == 10 ~ "High flow",
                                TRUE         ~ "Normal"),
  ) |>
  # select(Date, USGS_Q_Ls, HPB_Q_Ls, HPB_Q_Ls_filled,
  #        decile_usgs, flow_class_usgs, decile_hpb, flow_class_hpb) |>
  select(Date, decile_usgs, flow_class_usgs, decile_hpb, flow_class_hpb)

# Step 3: join to your other data frame and filter extreme days
USGS_HPB_Q_classes <- USGS_HPB_Q |>
  left_join(flow_flags, by = "Date")

write.csv(USGS_HPB_Q_classes, "./Predictions/Data/HPB_USGS_Flows.csv", row.names = F)




