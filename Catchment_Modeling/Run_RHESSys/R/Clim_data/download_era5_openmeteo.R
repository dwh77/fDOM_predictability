library(tidyverse)
library(httr2)

# ── Configuration ─────────────────────────────────────────────────────────────
site_lat    <- 37.31719    # Roanoke/Blacksburg, VA
site_lon    <- -79.97369

start_date  <- as.Date("1940-01-01")   # ERA5 available from 1940-01-01
end_date    <- as.Date("2026-05-01")

out_dir     <- "clim" #"ccr_rhessys/clim"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

clim_prefix <- "ccr_era5"   # output: ccr_era5.tmin, ccr_era5.tmax, etc.


# ── Open-Meteo variables ────────────────────────────────���──────────────────────
# Daily: temperature, precipitation, wind, humidity
# Hourly: radiation and VPD (not available as daily sums/means from the API)

OM_DAILY_VARS <- paste(c(
  "temperature_2m_max",           # tmax            °C
  "temperature_2m_min",           # tmin            °C
  "precipitation_sum",            # total precip    mm → m
  "wind_speed_10m_mean",          # wind @ 10m      m/s
  "relative_humidity_2m_mean"     # relative RH     % → 0–1
), collapse = ",")

# is_day is included to mask daytime hours for the VPD mean.
OM_HOURLY_VARS <- paste(c(
  "direct_radiation",             # W/m²  → summed to kJ/m²/day
  "diffuse_radiation",            # W/m²  → summed to kJ/m²/day
  "vapour_pressure_deficit",      # kPa   → daytime mean → Pa
  "is_day"                        # 0/1   → daytime mask for VPD
), collapse = ",")


# ── API helpers ───────────────────────────────────────────────────────────────

fetch_era5_daily_chunk <- function(start, end, lat, lon) {
  Sys.sleep(2)   # avoid 429 rate-limit on Open-Meteo free tier
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive",
    "?latitude=",   lat,
    "&longitude=",  lon,
    "&start_date=", format(start, "%Y-%m-%d"),
    "&end_date=",   format(end,   "%Y-%m-%d"),
    "&daily=",      OM_DAILY_VARS,
    "&timezone=UTC",
    "&wind_speed_unit=ms"
  )
  message("  [daily]  ", format(start, "%Y-%m-%d"), " – ", format(end, "%Y-%m-%d"))
  resp <- request(url) |>
    req_timeout(120) |>
    req_retry(max_tries = 6,
              is_transient = \(r) resp_status(r) %in% c(429, 500, 503),
              backoff = \(i) min(300, 30 * 2^(i - 1))) |>
    req_perform()
  result <- resp_body_json(resp, simplifyVector = TRUE)
  if (!is.null(result$error)) stop("Open-Meteo error: ", result$reason)
  df <- as_tibble(result$daily)
  df$DATE <- as.Date(df$time)
  select(df, -time)
}

# Fetch hourly data and aggregate to daily Kdown_direct, Kdown_diffuse, and vpd.
# Uses 2-year chunks — hourly data is 24× larger than daily.
fetch_era5_hourly_chunk <- function(start, end, lat, lon) {
  Sys.sleep(2)   # avoid 429 rate-limit on Open-Meteo free tier
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive",
    "?latitude=",   lat,
    "&longitude=",  lon,
    "&start_date=", format(start, "%Y-%m-%d"),
    "&end_date=",   format(end,   "%Y-%m-%d"),
    "&hourly=",     OM_HOURLY_VARS,
    "&timezone=UTC"
  )
  message("  [hourly] ", format(start, "%Y-%m-%d"), " – ", format(end, "%Y-%m-%d"))
  resp <- request(url) |>
    req_timeout(180) |>
    req_retry(max_tries = 6,
              is_transient = \(r) resp_status(r) %in% c(429, 500, 503),
              backoff = \(i) min(300, 30 * 2^(i - 1))) |>
    req_perform()
  result <- resp_body_json(resp, simplifyVector = TRUE)
  if (!is.null(result$error)) stop("Open-Meteo error: ", result$reason)

  hrly <- as_tibble(result$hourly) |>
    mutate(
      datetime          = as.POSIXct(time, tz = "UTC"),
      DATE              = as.Date(datetime),
      direct_radiation  = as.numeric(direct_radiation),
      diffuse_radiation = as.numeric(diffuse_radiation),
      vpd_kpa           = as.numeric(vapour_pressure_deficit),
      is_day            = as.integer(is_day)
    ) |>
    select(DATE, direct_radiation, diffuse_radiation, vpd_kpa, is_day)

  # Aggregate to daily:
  #   Radiation: sum(W/m²) × 3.6 = kJ/m²/day  (W/m² × 3600 s/h ÷ 1000)
  #   VPD: daytime mean (is_day == 1), kPa → Pa (× 1000)
  hrly |>
    group_by(DATE) |>
    summarise(
      Kdown_direct  = sum(direct_radiation,  na.rm = TRUE) * 3.6,
      Kdown_diffuse = sum(diffuse_radiation,  na.rm = TRUE) * 3.6,
      vpd           = mean(vpd_kpa[is_day == 1], na.rm = TRUE) * 1000,
      .groups = "drop"
    )
}


# ── Step 1: Download ERA5 daily variables ─────────────────────────────��───────

message("== Downloading ERA5 daily variables from Open-Meteo ==")

daily_starts <- seq(start_date, end_date, by = "10 years")
daily_ends   <- c(daily_starts[-1] - 1, end_date)

raw <- map2(daily_starts, daily_ends,
            \(s, e) fetch_era5_daily_chunk(s, e, site_lat, site_lon)) |>
  bind_rows()

message("  → ", nrow(raw), " days  (", format(min(raw$DATE)), " – ", format(max(raw$DATE)), ")")


# ── Step 1b: Download ERA5 hourly variables and aggregate to daily ─────────────

message("== Downloading ERA5 hourly variables from Open-Meteo ==")

hourly_starts <- seq(start_date, end_date, by = "2 years")
hourly_ends   <- c(hourly_starts[-1] - 1, end_date)

hourly_daily <- map2(hourly_starts, hourly_ends,
                     \(s, e) fetch_era5_hourly_chunk(s, e, site_lat, site_lon)) |>
  bind_rows()

message("  → ", nrow(hourly_daily), " days with hourly-derived radiation and VPD")


# ── Step 2: Unit conversions and derived variables ────────────────────────────

tmax <- as.numeric(raw$temperature_2m_max)    # °C
tmin <- as.numeric(raw$temperature_2m_min)    # °C
rain <- as.numeric(raw$precipitation_sum) / 1000   # mm → m
wind <- as.numeric(raw$wind_speed_10m_mean)   # m/s at 10m; screen_height = 10 in base file
rh   <- as.numeric(raw$relative_humidity_2m_mean) / 100   # % → 0–1

# atm_trans = Kdown_surface / Ra (Hargreaves FAO-56 Eq. 21)
# Kdown_surface = Kdown_direct + Kdown_diffuse from hourly sums (kJ/m²/day)
kdown_total_kj <- (hourly_daily$Kdown_direct + hourly_daily$Kdown_diffuse)

doy     <- as.integer(format(raw$DATE, "%j"))
lat_r   <- site_lat * pi / 180
d_r     <- 1 + 0.033 * cos(2 * pi * doy / 365)
dec     <- 0.409 * sin(2 * pi * doy / 365 - 1.39)
omega_s <- acos(pmin(1, pmax(-1, -tan(lat_r) * tan(dec))))
Ra_kj   <- (24 * 60 / pi) * 0.0820 * d_r *
           (omega_s * sin(lat_r) * sin(dec) +
            cos(lat_r) * cos(dec) * sin(omega_s)) * 1000

atm_trans <- pmin(1.0, pmax(0.0, ifelse(Ra_kj > 0, kdown_total_kj / Ra_kj, NA_real_)))


# ── Step 3: Assemble output data frame ────────────────────────────────────────

era5 <- tibble(
  DATE              = raw$DATE,
  tmax              = round(tmax,        4),
  tmin              = round(tmin,        4),
  rain              = round(rain,        6),
  wind              = round(wind,        4),
  relative_humidity = round(rh,          4),
  atm_trans         = round(atm_trans,   5)
) |>
  left_join(
    hourly_daily |>
      mutate(
        Kdown_direct  = round(Kdown_direct,  2),
        Kdown_diffuse = round(Kdown_diffuse, 2),
        vpd           = round(vpd,           2)
      ),
    by = "DATE"
  )

era5 <- mutate(era5, across(where(is.numeric), \(x) replace_na(x, -999.0)))


#fix rain issue at first observation
era5 <- era5 |> 
  mutate(rain = ifelse(rain == -999, 0, rain))

#write csv with era5 data
write.csv(era5, paste0(out_dir, "/era5_1jan1940_1may2026.csv"), row.names = F)

## can read this in if needing to edit data later after compiling
# era5 <- read_csv(paste0(out_dir, "/era5_1jan1940_1may2026.csv"))

message(sprintf("ERA5 range: tmin %.1f–%.1f °C  tmax %.1f–%.1f °C  rain %.4f–%.4f m/day",
  min(era5$tmin), max(era5$tmin),
  min(era5$tmax), max(era5$tmax),
  min(era5$rain), max(era5$rain)))
message(sprintf("            wind %.1f–%.1f m/s  RH %.2f–%.2f  atm_trans %.3f–%.3f",
  min(era5$wind), max(era5$wind),
  min(era5$relative_humidity), max(era5$relative_humidity),
  min(era5$atm_trans[era5$atm_trans != -999]),
  max(era5$atm_trans[era5$atm_trans != -999])))
message(sprintf("            Kdown_direct %.0f–%.0f kJ/m²/day  vpd %.0f–%.0f Pa",
  min(era5$Kdown_direct[era5$Kdown_direct != -999]),
  max(era5$Kdown_direct),
  min(era5$vpd[era5$vpd != -999]),
  max(era5$vpd)))


# ----- Format ERA5 for spinup file  ---------------------
era5_spinup <- era5 |> 
  filter(DATE < ymd("1960-01-01"))


# ----- Format ERA5 for for transient run ---------------------

backfill_clim_1850 <- function(era5,
                               cycle_start = as.Date("1940-01-01"),
                               cycle_end   = as.Date("1959-12-31"),
                               fill_start  = as.Date("1850-01-01")) {
  
  fill_end <- cycle_start - 1  # 1939-12-31
  
  # Extract the 20-year cycle block
  cycle_block <- era5 |> filter(DATE >= cycle_start, DATE <= cycle_end)
  n_cycle     <- nrow(cycle_block)
  
  # Generate all dates to backfill
  backfill_dates <- seq(fill_start, fill_end, by = "day")
  
  # Align from the TAIL: fill_end (1939-12-31) = cycle row n_cycle (1959-12-31)
  # days_from_end = 0 for fill_end, increases going backward in time
  days_from_end <- as.integer(fill_end - backfill_dates)
  cycle_row     <- n_cycle - (days_from_end %% n_cycle)
  
  backfill <- cycle_block[cycle_row, ] |>
    mutate(DATE = backfill_dates)
  
  bind_rows(backfill, era5) |> arrange(DATE)
}

era5_1850 <- backfill_clim_1850(era5)


# ── Step 4: Write RHESSys climate files ───────────────────────────────────────
#function to write files
write_rhessys_clim <- function(df, var_col, file_path) {
  vals <- df[[var_col]]
  #if (all(vals == -999)) { message("Skipping ", var_col, " — no data"); return(invisible(NULL)) }
  first  <- df$DATE[[1]]
  header <- paste(format(first, "%Y"), as.integer(format(first, "%m")),
                  as.integer(format(first, "%d")), "1")
  writeLines(c(header, as.character(vals)), con = file_path)
  message("Wrote ", nrow(df), " values → ", basename(file_path))
}

#name extension and variables
pf <- function(ext) file.path(out_dir, paste0(clim_prefix, ".", ext))

clim_vars <- c("tmax", "tmin", "rain", "wind", "relative_humidity",
               "atm_trans", "Kdown_direct", "Kdown_diffuse", "vpd")

#write all function 
write_all_clim <- function(df, prefix) {
  pf <- function(ext) file.path(out_dir, paste0(prefix, ".", ext))
  for (v in clim_vars) write_rhessys_clim(df, v, pf(v))
}

#write all files
write_all_clim(era5_spinup, "ccr_era5_spinup")
write_all_clim(era5_1850,   "ccr_era5_1850")

# ── Step 5: Write base station file ───────────────────────────────────────────

# non_critical_vars <- c("wind", "relative_humidity", "atm_trans",
#                        "Kdown_direct", "Kdown_diffuse", "vpd")
# 
# written_vars <- Filter(\(v) file.exists(pf(v)) && !all(era5[[v]] == -999),
#                        non_critical_vars)
# 
# base_lines <- c(
#   "101 base_station_id",
#   "100 x_coordinate",
#   "100 y_coordinate",
#   "346.7 z_coordinate",
#   "2.0 effective_lai",
#   "10 screen_height",   # matches ERA5 10m wind measurement height
#   "annual annual_climate_prefix",
#   "0 number_non_critical_annual_sequences",
#   "monthly monthly_climate_prefix",
#   "0 number_non_critical_monthly_sequences",
#   paste0("clim/", clim_prefix_extension, " daily_climate_prefix"),
#   paste(length(written_vars), "number_non_critical_daily_sequences"),
#   written_vars,
#   "hourly hourly_climate_prefix",
#   "0 number_non_critical_hourly_sequences"
# )
# 
# 
# ## spinup base
# clim_prefix_extension <- "ccr_era5_spinup"
# base_file_SPIN <- file.path(out_dir, paste0(clim_prefix_extension, "_base"))
# writeLines(base_lines, base_file_SPIN)


write_base_station <- function(prefix, clim_path = "clim") {
  base_lines <- c(
    "101 base_station_id",
    "100 x_coordinate",
    "100 y_coordinate",
    "346.7 z_coordinate",
    "2.0 effective_lai",
    "10 screen_height",
    "annual annual_climate_prefix",
    "0 number_non_critical_annual_sequences",
    "monthly monthly_climate_prefix",
    "0 number_non_critical_monthly_sequences",
    paste0(clim_path, "/", prefix, " daily_climate_prefix"),
    "8 number_non_critical_daily_sequences",
    "wind",
    "relative_humidity",
    "atm_trans",
    "Kdown_direct",
    "Kdown_diffuse",
    "vpd",
    "ndep_NO3",
    "ndep_NH4",
    "hourly hourly_climate_prefix",
    "0 number_non_critical_hourly_sequences"
  )
  writeLines(base_lines, file.path(out_dir, paste0(prefix, "_base")))
}

write_base_station("ccr_era5_1850")
write_base_station("ccr_era5_spinup")



# ── Step 6: Diagnostic summary ────────────────────────────────────────────────
# Unit conversion reference:
#
# Source  Variable                    RHESSys file       Conversion
# ──────────────────────────────────────────────────────────────────────────────
# daily   temperature_2m_max (°C)   → .tmax              none
# daily   temperature_2m_min (°C)   → .tmin              none
# daily   precipitation_sum  (mm)   → .rain              ÷ 1000 (m)
# daily   wind_speed_10m     (m/s)  → .wind              none (screen_height=10)
# daily   relative_humidity  (%)    → .relative_humidity ÷ 100
# hourly  direct_radiation   (W/m²) → .Kdown_direct      sum × 3.6 (kJ/m²/day)
# hourly  diffuse_radiation  (W/m²) → .Kdown_diffuse     sum × 3.6 (kJ/m²/day)
# hourly  vapour_pressure_deficit(kPa) → .vpd            daytime mean × 1000 (Pa)
# derived (Kdown_direct+diffuse)/Ra  → .atm_trans        Hargreaves FAO-56 Eq.21
