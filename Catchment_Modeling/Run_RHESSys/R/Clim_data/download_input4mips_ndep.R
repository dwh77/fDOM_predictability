library(tidyverse)
library(ncdf4)
library(httr2)

# ── Configuration ─────────────────────────────────────────────────────────────
site_lat   <- 37.31719    # Roanoke/Blacksburg, VA
site_lon   <- -79.97369

start_date <- as.Date("1901-01-01")
end_date     <- as.Date("2026-05-01")

data_dir   <- "clim/input4mips_raw"
output_no3 <- "clim/ccr_isimip3a.ndep_NO3"
output_nh4 <- "clim/ccr_isimip3a.ndep_NH4"

dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)


# ── Source: ISIMIP3a N-deposition forcing ─────────────────────────────────────
# Files: ndep-nhx (NH4, reduced N) and ndep-noy (NO3, oxidised N)
# Each combines dry + wet deposition; units kgN/m²/s; monthly global 0.5°×0.5°
# Coverage: 1901–2021.  Dates outside that range carry the boundary year forward.
# Source: https://data.isimip.org/protocol/ISIMIP3a/

ISIMIP_BASE <- "https://files.isimip.org/ISIMIP3a/InputData/socioeconomic/n-deposition/histsoc"

ISIMIP_FILES <- c(
  nhx = "ndep-nhx_histsoc_monthly_1901_2021.nc",
  noy = "ndep-noy_histsoc_monthly_1901_2021.nc"
)


# ── Step 1: Download ISIMIP3a N-deposition NetCDF files ───────────────────────

download_nc <- function(url, dest) {
  if (file.exists(dest)) {
    message("Using cached: ", basename(dest))
    return(invisible(dest))
  }
  message("Downloading (", round(as.numeric(
    resp_header(request(url) |> req_method("HEAD") |> req_timeout(10) |>
                  req_error(is_error=\(r)FALSE) |> req_perform(),
                "Content-Length")) / 1024^2, 0), " MB): ", basename(dest))
  tmp <- paste0(dest, ".tmp")
  tryCatch({
    download.file(url, tmp, mode = "wb", quiet = FALSE)
    file.rename(tmp, dest)
    message("Downloaded: ", basename(dest))
  }, error = function(e) {
    unlink(tmp)
    stop("Download failed for ", basename(dest), ": ", conditionMessage(e))
  })
  invisible(dest)
}

nc_files <- list()
for (spec in names(ISIMIP_FILES)) {
  url  <- paste0(ISIMIP_BASE, "/", ISIMIP_FILES[[spec]])
  dest <- file.path(data_dir, ISIMIP_FILES[[spec]])
  download_nc(url, dest)
  nc_files[[spec]] <- dest
}


# ── Step 2: Extract site time series from each NetCDF ─────────────────────────

read_isimip_ndep <- function(nc_file, site_lat, site_lon) {
  nc <- nc_open(nc_file)
  on.exit(nc_close(nc))

  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")

  # Normalise longitude to file convention
  lon_range  <- range(lon, na.rm = TRUE)
  site_lon_adj <- if (lon_range[1] >= 0 && site_lon < 0) site_lon + 360 else site_lon

  i_lat <- which.min(abs(lat - site_lat))
  i_lon <- which.min(abs(lon - site_lon_adj))
  message(sprintf("  Nearest cell: lat=%.3f (idx %d), lon=%.3f (idx %d)",
    lat[i_lat], i_lat, lon[i_lon], i_lon))

  # Variable name is derived from the filename prefix (e.g. "ndep-nhx")
  var_name  <- sub("_histsoc.*", "", basename(nc_file))
  # NetCDF variable name uses underscore not hyphen
  nc_var    <- gsub("-", "_", var_name)
  if (!(nc_var %in% names(nc$var))) {
    # Fall back: use first non-coordinate variable
    coord_vars <- c("lat", "lon", "time", "lat_bnds", "lon_bnds", "time_bnds")
    nc_var <- setdiff(names(nc$var), coord_vars)[1]
    message("  Using variable: ", nc_var)
  }

  var_info  <- nc$var[[nc_var]]
  dim_names <- sapply(var_info$dim, `[[`, "name")
  ndims     <- length(dim_names)
  start     <- rep(1L, ndims)
  count     <- rep(-1L, ndims)

  lat_dim <- which(dim_names %in% c("lat", "latitude", "y"))[1]
  lon_dim <- which(dim_names %in% c("lon", "longitude", "x"))[1]
  start[lat_dim] <- i_lat;  count[lat_dim] <- 1L
  start[lon_dim] <- i_lon;  count[lon_dim] <- 1L

  vals <- as.numeric(ncvar_get(nc, nc_var, start = start, count = count))

  # Apply scale/offset/fill if present
  fill   <- ncatt_get(nc, nc_var, "_FillValue")
  scale  <- ncatt_get(nc, nc_var, "scale_factor")
  offset <- ncatt_get(nc, nc_var, "add_offset")
  if (fill$hasatt)   vals[vals == fill$value] <- NA
  if (scale$hasatt)  vals <- vals * scale$value
  if (offset$hasatt) vals <- vals + offset$value

  # Time axis
  time_vals  <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  origin     <- as.Date(substr(gsub("^\\w+ since ", "", time_units), 1, 10))
  unit_type  <- gsub(" since.*", "", time_units)
  dates <- switch(unit_type,
    "days"   = origin + time_vals,
    "months" = origin + months(as.integer(time_vals)),
    "years"  = origin + lubridate::years(as.integer(time_vals)),
    stop("Unsupported time unit: ", unit_type)
  )

  # Unit conversion → kgN/m²/day (RHESSys expected units, per rhessys.h)
  nc_units <- ncatt_get(nc, nc_var, "units")$value
  message("  File units: ", nc_units)
  if (grepl("g.*N.*m.*-2.*mon|gN/m2/mon|g N m-2 mon", nc_units, ignore.case = TRUE)) {
    # ISIMIP3a: gN/m²/month → kgN/m²/day
    # Divide by 1000 (g→kg); divide by days in each month to get per-day rate.
    days_per_month <- lubridate::days_in_month(as.Date(dates))
    vals <- vals / 1000 / as.numeric(days_per_month)
    message("  Converted gN/m²/month → kgN/m²/day (÷1000 ÷ days_in_month)")
  } else if (grepl("/s$|s-1|per.second", nc_units, ignore.case = TRUE)) {
    vals <- vals * 86400
    message("  Converted kgN/m²/s → kgN/m²/day (×86400)")
  } else {
    message("  No unit conversion applied (assuming kgN/m²/day already)")
  }

  tibble(date = as.Date(dates), year = lubridate::year(dates),
         month = lubridate::month(dates), value = vals)
}

message("\n== Extracting NHx (NH4) ==")
nhx_monthly <- read_isimip_ndep(nc_files[["nhx"]], site_lat, site_lon)

message("== Extracting NOy (NO3) ==")
noy_monthly <- read_isimip_ndep(nc_files[["noy"]], site_lat, site_lon)

message(sprintf("Monthly N-dep spans %s – %s",
  format(min(nhx_monthly$date)), format(max(nhx_monthly$date))))
message(sprintf("  NHx range: %.3e – %.3e kgN/m²/day  (= %.1f kgN/ha/yr)",
  min(nhx_monthly$value, na.rm=TRUE), max(nhx_monthly$value, na.rm=TRUE),
  mean(nhx_monthly$value, na.rm=TRUE) * 365 * 1e4))
message(sprintf("  NOy range: %.3e – %.3e kgN/m²/day  (= %.1f kgN/ha/yr)",
  min(noy_monthly$value, na.rm=TRUE), max(noy_monthly$value, na.rm=TRUE),
  mean(noy_monthly$value, na.rm=TRUE) * 365 * 1e4))


# ── Step 3: Expand monthly → daily ────────────────────────────────────────────
# Each monthly value is already a per-day rate (kgN/m²/day after conversion).
# Assign it to every day in that calendar month.
# Dates before 1901 or after 2021 carry the boundary month's value forward.

daily_dates <- seq(start_date, end_date, by = "day")

monthly_combined <- nhx_monthly |>
  rename(ndep_NH4 = value) |>
  left_join(noy_monthly |> select(year, month, ndep_NO3 = value),
            by = c("year", "month")) |>
  select(year, month, ndep_NH4, ndep_NO3)

daily_frame <- tibble(DATE = daily_dates) |>
  mutate(year = lubridate::year(DATE), month = lubridate::month(DATE)) |>
  left_join(monthly_combined, by = c("year", "month")) |>
  tidyr::fill(ndep_NH4, ndep_NO3, .direction = "updown")

stopifnot(all(!is.na(daily_frame$ndep_NH4)), all(!is.na(daily_frame$ndep_NO3)))

daily_ndep <- daily_frame |>
  mutate(ndep_NH4 = signif(ndep_NH4, 6),
         ndep_NO3 = signif(ndep_NO3, 6))


# ── setup spinup and 1850 ───────────────────────────────────────────────────
#get value that starts data
val_NH4 <- daily_ndep$ndep_NH4[1]  # 8.71930e-07 (1901-01-01)
val_NO3 <- daily_ndep$ndep_NO3[1]  # 4.89669e-07

# 1) Extended back to 1850
pre_1901 <- tibble(
  DATE     = seq(as.Date("1850-01-01"), as.Date("1900-12-31"), by = "day"),
  ndep_NH4 = val_NH4,
  ndep_NO3 = val_NO3
) |> mutate(year = year(DATE), month = month(DATE))

ndep_1850 <- bind_rows(pre_1901, daily_ndep) |>
  select(DATE, year, month, ndep_NH4, ndep_NO3)

# 2) Spinup: 1940-1959 filled with 1901 constant
ndep_spinup <- tibble(
  DATE     = seq(as.Date("1940-01-01"), as.Date("1959-12-31"), by = "day"),
  ndep_NH4 = val_NH4,
  ndep_NO3 = val_NO3
) |> mutate(year = year(DATE), month = month(DATE)) |>
  select(DATE, year, month, ndep_NH4, ndep_NO3)



# ── Step 5: Diagnostic plot ───────────────────────────────────────────────────

daily_ndep |>
  select(DATE, ndep_NO3, ndep_NH4) |>
  pivot_longer(-DATE, names_to = "species", values_to = "kgN_m2_day") |>
  mutate(species = recode(species,
    ndep_NO3 = "NOy (NO3)",
    ndep_NH4 = "NHx (NH4)"
  )) |>
  ggplot(aes(DATE, kgN_m2_day, color = species)) +
  geom_line(linewidth = 0.3, alpha = 0.8) +
  scale_color_manual(values = c("NOy (NO3)" = "steelblue", "NHx (NH4)" = "darkorange")) +
  labs(
    title    = "Daily N deposition — RHESSys input",
    subtitle = "ISIMIP3a histsoc (1901-2021); values after 2021 held at 2021 level",
    caption  = paste0("Site: Roanoke/Blacksburg, VA (", site_lat, "N, ", abs(site_lon), "W)\n",
                      "ISIMIP3a ndep-nhx + ndep-noy (dry + wet combined). Units: kgN/m2/day"),
    x = NULL, y = expression(kgN~m^{-2}~day^{-1}), color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")


# ── Step 4: Write RHESSys .ndep_NO3 and .ndep_NH4 files ──────────────────────

write_rhessys_clim <- function(df, var_col, file_path) {
  first  <- df$DATE[[1]]
  header <- paste(format(first, "%Y"), as.integer(format(first, "%m")),
                  as.integer(format(first, "%d")), "1")
  writeLines(c(header, as.character(df[[var_col]])), con = file_path)
  message("Wrote ", nrow(df), " daily values → ", file_path)
}

write_rhessys_clim(ndep_1850,   "ndep_NH4", file.path("clim/ccr_era5_1850.ndep_NH4"))
write_rhessys_clim(ndep_1850,   "ndep_NO3", file.path("clim/ccr_era5_1850.ndep_NO3"))

write_rhessys_clim(ndep_spinup, "ndep_NH4", file.path("clim/ccr_era5_spinup.ndep_NH4"))
write_rhessys_clim(ndep_spinup, "ndep_NO3", file.path("clim/ccr_era5_spinup.ndep_NO3"))


# ── look at generated data ──────────────────────────────────────────────────────────
read_clim_file <- function(path) {
  lines  <- readLines(path)
  hdr    <- as.integer(strsplit(trimws(lines[1]), "\\s+")[[1]])
  start  <- as.Date(sprintf("%d-%02d-%02d", hdr[1], hdr[2], hdr[3]))
  values <- as.numeric(lines[-1])
  varname <- sub(".*\\.", "", basename(path))
  data.frame(
    date        = start + seq(0, length(values) - 1),
    placeholder = values,
    stringsAsFactors = FALSE
  ) |> setNames(c("date", varname))
}

no3dep <- read_clim_file("clim_copy/ccr_era5_1850.ndep_NO3")
plot(no3dep$date, no3dep$ndep_NO3)

no3depspin <- read_clim_file("clim_copy/ccr_era5_spinup.ndep_NO3")
plot(no3depspin$date, no3depspin$ndep_NO3)


nh4dep <- read_clim_file("clim_copy/ccr_era5_1850.ndep_NH4")
plot(nh4dep$date, nh4dep$ndep_NH4)

nh4depspin <- read_clim_file("clim_copy/ccr_era5_spinup.ndep_NH4")
plot(nh4depspin$date, nh4depspin$ndep_NH4)
