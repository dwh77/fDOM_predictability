#### Minimal example RHESSys run
#### Starts from the worldfile state produced by the no-harvest transient run
#### (see Spin_Run_CCR.R), runs 5 years, then reads the output and plots LAI
#### and streamflow. Meant to show the basic steps of running the model, not
#### to produce a real scenario run.
devtools::install_github("RHESSys/RHESSysIOinR")
library(RHESSysIOinR)
library(tidyverse)

setwd("/workspace/Catchment_Modeling")

output_folder <- "Run_model/out/ExampleRun"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

name <- "RHESSys_example_run"

# 5 calendar years, fully within the real ERA5 climate record (1850 to ~2026-05-01)
# has to end on hour 24 so tec output works
dates <- c("2021 1 1 1", "2026 1 1 24")

input_hdr <- IOin_hdr(
  basin      = "Run_model/defs/basin.def",
  hillslope  = "Run_model/defs/hill.def",
  zone       = "Run_model/defs/zone.def",
  soil       = c("Run_model/defs/soil_silt-loam.def", "Run_model/defs/soil_water.def"),
  landuse    = "Run_model/defs/landuse_undeveloped.def",
  stratum    = c("Run_model/defs/veg_deciduous.def", "Run_model/defs/veg_evergreen.def",
                 "Run_model/defs/veg_deciduous_ID5.def", "Run_model/defs/veg_nonveg.def"),
  basestations = "Run_model/clim/ccr_era5_1850_base"
)

# builds a tec file that just turns on daily output; no restart state needed here
input_tec_data <- IOin_tec_std(start = dates[1], end = dates[2], output_state = FALSE)

input_rhessys <- IOin_rhessys_input(
  version          = "RHESSys_exec/rhessys7.5",
  tec_file         = paste0("Run_model/tecfiles/", name, ".tec"),
  world_file       = "Run_model/worldfiles/transient/HarvestNone/ccr_mixed.world.Y2940M1D2H23.state.Y2026M4D2H23.state",
  world_hdr_prefix = "ccr",
  flowtable        = "Run_model/worldfiles/Generated/ccr_mixed.flow",
  start            = dates[1],
  end              = dates[2],
  output_folder    = output_folder,
  output_prefix    = name,
  commandline_options = "-b -g -str Run_model/worldfiles/Generated/ccr_mixed.stream -stro"
)

run_rhessys_single(
  input_rhessys = input_rhessys,
  hdr_files     = input_hdr,
  tec_data      = input_tec_data,
  output_filter = NULL,
  return_cmd    = FALSE   # set TRUE to just print the command instead of running it
)

############################## Read output and plot ##############################

basin_daily <- read_delim(
  paste0(output_folder, "/", name, "_basin.daily"),
  delim = " ", col_names = TRUE
) |>
  mutate(date = ymd(paste(year, month, day, sep = "-")))

basin_daily |>
  select(date, lai, streamflow) |>
  pivot_longer(-date) |>
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  labs(x = NULL, y = NULL, title = "Example run: LAI and streamflow")
