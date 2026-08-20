#devtools::install_github("RHESSys/RHESSysIOinR")
library(RHESSysIOinR)
library(tidyverse)

getwd()
setwd("/workspace/Catchment_Modeling")
getwd()

output_folder <- "Run_RHESSys/out/ccrSPIN"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)


################### SPINUP ######################################

## NOTE this will take over 8 hours to run, I reccomend not rerunning this and starting with the worldfile generated from this run; see transient run below

# dates <- c("1940 1 1 1", "2940 1 2 24") #has to end with a 24 hour so that the tec output will work
#
# name <- "spinup_run_1000"
#
# #header set up
# input_hdr = IOin_hdr(
#   basin      = "Run_RHESSys/defs/basin.def",
#   hillslope  = "Run_RHESSys/defs/hill.def",
#   zone       = "Run_RHESSys/defs/zone.def",
#   soil       = c("Run_RHESSys/defs/soil_silt-loam.def", "Run_RHESSys/defs/soil_water.def"),
#   landuse    = "Run_RHESSys/defs/landuse_undeveloped.def",
#   stratum    = c("Run_RHESSys/defs/veg_deciduous.def", "Run_RHESSys/defs/veg_evergreen.def",
#   "Run_RHESSys/defs/veg_deciduous_ID5.def", "Run_RHESSys/defs/veg_nonveg.def"),
#   #basestations = "Run_RHESSys/clim/ccr_base"
#   basestations = "Run_RHESSys/clim/ccr_era5_spinup_base"
# )
#
#
# input_rhessys = IOin_rhessys_input(
#   version          = "RHESSys_exec/rhessys7.5",
#   tec_file         =  paste0("Run_RHESSys/tecfiles/",name,".tec"), #for using tecfile built below
#   world_file       = "Run_RHESSys/worldfiles/Generated/ccr_mixed.world", #what was made in pre-processing
#   world_hdr_prefix = "ccr",
#   flowtable        = "Run_RHESSys/worldfiles/Generated/ccr_mixed.flow", #what was made in pre-processing
#   start            = dates[1],
#   end              = dates[2],
#   output_folder    = output_folder,
#   output_prefix    = name,
#   commandline_options = "-b -g -climrepeat"
#   )
#
# #make tec file locally
# input_tec_data = IOin_tec_std(start = dates[1], end = dates[2], output_state = T)
#
# #run model with inputs defined above
# run_rhessys_single(
#   input_rhessys = input_rhessys,
#   hdr_files     = input_hdr,
#   tec_data      = input_tec_data, #can be NULL if calling a local file and not using IOin_tec_std
#   #def_pars      = input_def_pars,
#   output_filter = NULL, #cant get any version working currently so using legacy output
#   return_cmd    = F   # flip to FALSE to run
# )
#
#
# # Move spin up state file to proper folder manually



###################################### TRANSIENT RUN WITH No harvest ######################################

output_folder <- "Run_RHESSys/out/ccrTR/HarvestNone"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)


#### 1850 to 2026 with no harvest (can change output above and names/scenarios below to run a harvest)

dates = c("1850 1 1 1", "2026 4 2 24") #has to end with a 24 hour so that the tec output will work

name = "TR1850_2026_NOharvest_run"
#name = "TR1850_2026_harvest1946_run"

#header directly copied from hpb.hdr
input_hdr = IOin_hdr(
  basin      = "Run_RHESSys/defs/basin.def",
  hillslope  = "Run_RHESSys/defs/hill.def",
  zone       = "Run_RHESSys/defs/zone.def",
  soil       = c("Run_RHESSys/defs/soil_silt-loam.def", "Run_RHESSys/defs/soil_water.def"),
  landuse    = "Run_RHESSys/defs/landuse_undeveloped.def",
  stratum    = c("Run_RHESSys/defs/veg_deciduous.def", "Run_RHESSys/defs/veg_evergreen.def",
  "Run_RHESSys/defs/veg_deciduous_ID5.def", "Run_RHESSys/defs/veg_nonveg.def"),
  basestations = "Run_RHESSys/clim/ccr_era5_1850_base"
)

#NOTE: dwh not fully sure why but the output current state needs to be 1 hour less than the end date to get worldfile to write
input_tec_data = IOin_tec_all_options(print_daily_on = dates[1],
                                      print_daily_growth_on = dates[1],
                                      output_current_state = "2026 4 2 23") #,
                                      #redefine_world_thin_harvest = "1946 7 1 1")

input_rhessys = IOin_rhessys_input(
  version          = "RHESSys_exec/rhessys7.5",
  tec_file         =  paste0("Run_RHESSys/tecfiles/",name,".tec"), #for using tecfile built below
  world_file       = "Run_RHESSys/worldfiles/spin/ccr_mixed.world.Y2940M1D2H23.state", #state written by spinup above
  world_hdr_prefix = "ccr",
  flowtable        = "Run_RHESSys/worldfiles/Generated/ccr_mixed.flow", #what was made in pre-processing
  start            = dates[1],
  end              = dates[2],
  output_folder    = output_folder,
  output_prefix    = name,
  #harvest cmnd line
  #commandline_options = "-b -g -str Run_RHESSys/worldfiles/Generated/ccr_mixed.stream -stro -redefn Run_RHESSys/worldfiles/Harvest/ccr_mixed_harvest"
  #no harvest cmnd line
  commandline_options = "-b -g -str Run_RHESSys/worldfiles/Generated/ccr_mixed.stream -stro"
)

run_rhessys_single(
  input_rhessys = input_rhessys,
  hdr_files     = input_hdr,
  tec_data      = input_tec_data, #can be NULL if calling a local file and not using IOin_tec_std
  #def_pars      = input_def_pars,
  output_filter = NULL, #cant get any version working currently so using legacy output
  return_cmd    = F   # flip to FALSE to run
)

# Move transient state file to proper folder manually
spin_state <- paste0(input_rhessys$world_file, ".Y2026M4D2H23.state")
file.rename(spin_state, "Run_RHESSys/worldfiles/transient/HarvestNone/ccr_mixed.world.Y2940M1D2H23.state.Y2026M4D2H23.state")



################################ 5 year patch run  #####################################

##NOTE: this code needs to be updated to match outputs generated in transient run above
#but this allows for run that has patch level outputs (for making nice maps), but takes significantly longer to run

# #### 5 year patch run off of 2025 state to test harvest and output; will evaluate in separate script
#
# dates = c("2021 1 1 1", "2026 4 2 24") #has to end with a 24 hour so that the tec output will work
#
#
# name = "five_year_runpatch"
#
# #header directly copied from hpb.hdr
# input_hdr = IOin_hdr(
#   basin      = "Run_RHESSys/defs/basin.def",
#   hillslope  = "Run_RHESSys/defs/hill.def",
#   zone       = "Run_RHESSys/defs/zone.def",
#   soil       = c("Run_RHESSys/defs/soil_silt-loam.def", "Run_RHESSys/defs/soil_water.def"),
#   landuse    = "Run_RHESSys/defs/landuse_undeveloped.def",
#   stratum    = c("Run_RHESSys/defs/veg_deciduous.def", "Run_RHESSys/defs/veg_evergreen.def",
#   "Run_RHESSys/defs/veg_deciduous_ID5.def", "Run_RHESSys/defs/veg_nonveg.def"),
#   basestations = "Run_RHESSys/clim/ccr_era5_1850_base"
# )
#
# #make tec file locally
# input_tec_data = IOin_tec_std(start = dates[1], end = dates[2], output_state = F)
#
# input_rhessys = IOin_rhessys_input(
#   version          = "RHESSys_exec/rhessys7.5",
#   tec_file         =  paste0("Run_RHESSys/tecfiles/",name,".tec"), #for using tecfile built below
#   world_file       = "Run_RHESSys/worldfiles/ccr_mixed.world.Y4000M1D2H23.state.Y2026M4D2H23.state", #what was made after harvest spinup run
#   world_hdr_prefix = "ccr",
#   flowtable        = "Run_RHESSys/worldfiles/ccr_mixed.flow", #what was made in pre-processing
#   start            = dates[1],
#   end              = dates[2],
#   output_folder    = output_folder,
#   output_prefix    = name,
#   commandline_options = "-b -g -str Run_RHESSys/worldfiles/ccr_mixed.stream -stro -p" #have to add -p to end to get patch output to work, can't be before -str
# )
#
# run_rhessys_single(
#   input_rhessys = input_rhessys,
#   hdr_files     = input_hdr,
#   tec_data      = input_tec_data, #can be NULL if calling a local file and not using IOin_tec_std
#   #def_pars      = input_def_pars,
#   output_filter = NULL, #cant get any version working currently so using legacy output
#   return_cmd    = F   # flip to FALSE to run
# )






