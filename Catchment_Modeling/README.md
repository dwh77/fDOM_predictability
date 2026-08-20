# Catchment Modeling — RHESSys for Carvins Cove Reservoir (CCR)

## Overview

This folder contains everything needed to run RHESSys for the CCR fDOM forecasting project:

```         
fDOM_predictability/                 <- top-level repo (clone this)
├── .devcontainer/                   <- dev container config (devcontainer.json + Dockerfile)
└── Catchment_Modeling/
    ├── RHESSys_exec/
    │   └── rhessys7.5                <- precompiled RHESSys executable, ready to use
    └── Run_model/                    <- RHESSys application code: defs, worldfiles, clim,
                                          tecfiles, out, and the R scripts that run/evaluate the model
```

A precompiled `rhessys7.5` executable is provided in `RHESSys_exec/`, so there's no need to clone or build the RHESSys source yourself — you can go straight from opening the dev container to running simulations.

`Run_model/` is organized by the input files needed for RHESSys (`clim`, `defs`, `tecfiles`, `worldfiles`, `out`). These inputs are largely compiled through scripts in the `R` folder (see descriptions below). The `defs` folder files are from the RHESSys parameter library and have been updated to better calibrate RHESSys to CCR.

#### Folders within `Run_model/R` are as follows:

- **Clim_data** — scripts for compiling ERA5 and ISIMIP data sets
- **Maps_Worldfiles** — contains scripts for compiling needed spatial data for the model (DEM and land cover) then for creating RHESSys worldfiles
- **Run_RHESSys** — contains scripts for running the RHESSys model in CCR for spinup and other use cases; see `worldfiles/README.md` for further details on the worldfile pipeline
- **Eval_RHESSys** — scripts for evaluating RHESSys outputs

See `Run_model/R/README.md` and `Run_model/worldfiles/README.md` for more detail on each folder.

------------------------------------------------------------------------

## Setup

This project runs inside a VS Code Dev Container so that everyone runs RHESSys with the same OS libraries and R packages.

### 1. Get the repo onto your computer

``` bash
git clone <fDOM_predictability repo URL>
```

### 2. Install Docker

Install [Docker Desktop](https://www.docker.com/products/docker-desktop/) (or another Docker engine) and make sure it's running. The dev container is built and run through Docker, so VS Code needs Docker open and running before it can open the workspace in a container.

### 3. Install VS Code

1.  Install [VS Code](https://code.visualstudio.com/download).
2.  In VS Code, install the **Dev Containers** extension (`ms-vscode-remote.remote-containers`):
    - Click the **Extensions** icon in the left sidebar (the square-blocks icon), or press `Ctrl+Shift+X` (Windows/Linux) / `Cmd+Shift+X` (Mac).
    - Type `Dev Containers` into the search box at the top of the Extensions panel.
    - Click on **Dev Containers** (published by Microsoft) in the results list.
    - Click the blue **Install** button.

### 4. Open the workspace as a container in VS Code

1.  Open the `fDOM_predictability` folder in VS Code (File \> Open Folder) — the top-level folder, so `Catchment_Modeling/` is visible alongside `.devcontainer/`.
2.  Run **Dev Containers: Reopen in Container** from the command palette (Cmd/Ctrl+Shift+P).

This builds the image from `.devcontainer/Dockerfile` that is located in this repo and mounts the repo into the container as `/workspace`. It also automatically installs the R and C/C++ extensions inside the container per `.devcontainer/devcontainer.json` — no manual extension setup needed once you're in.

**Note:** because `RHESSys_exec/rhessys7.5` is already provided and committed, there is no build step required — the executable is ready to use as soon as the container is open.

------------------------------------------------------------------------

## Quick start guide for RHESSys

This repo is configured so you can start running RHESSys simulations using worldfiles that have already been generated. Worldfiles for both the spinup period (representing pre-industrial conditions) and a transient run (representing current-day conditions) are provided in `Run_model/worldfiles/`.

- A example script for how RHESSys is run is provided in [`Run_model/R/Run_RHESSys/RHESSys_example_run.R`](Run_model/R/Run_RHESSys/RHESSys_example_run.R) that runs a 5-year simulation and plots simple outputs (LAI and streamflow). This is a good smoke test that your setup (container, R packages, executable, worldfiles) is working end to end.

- To recreate the spinup and transient run simulation, the script is provided in [`Run_model/R/Run_RHESSys/Spin_Run_CCR.R`](Run_model/R/Run_RHESSys/Spin_Run_CCR.R). NOTE: to rerun both of these scripts will take \~10 hours.

- If you're interested in remaking worldfiles or RHESSys input maps, see the readME within the [`Run_model/R/Maps_Worldfiles`](Run_model/R/Maps_Worldfiles) folder.

To run the example: open `RHESSys_example_run.R` in VS Code and run the script. It starts from the pre-built worldfile state, runs RHESSys for 5 years, reads the output back in, and plots LAI and streamflow.

If that plot renders without errors, your setup is working end to end.

------------------------------------------------------------------------

## Model assumptions

We note a few model assumptions associated with the current RHESSys configuration that motivate future work.

Notable assumptions include:

- Soil type is assumed to be the same across the entire catchment. To expand this would need to develop a soil cover map that would be read in with the PreProcessing workflow and would need to update def files associated with those new soil types.

- Mixed canopy regions are all assumed to be deciduous currently. Further work could develop mixed canopy conditions to represent a mix of deciduous and evergreen trees. This would additionally need to be incorporated into the PreProcessing workflow and would need updates to associated def files.

- Patch size could be further refined to either improve model run time efficiency, or to better represent spatial variation in land cover (i.e. mixed canopy, or include small regions of developed and grass land). Done in the PreProcessing script where the patch map is created. **NOTE** if any Preprocessing steps are changed and there are any changes to spatial resolution, the world file, flow table, and stream routing table will need to be remade in the `Maps_Worldfiles` folder.

- The CCR reservoir area is assumed to be a flat standing water area across the catchment (elevation is 356.7 meters across the entire reservoir). Future work could incorporate reservoir bathymetry to develop more accurate streamflow paths within the reservoir (possible interest for 3D modeling).

- If you are interested in not using GRASS GIS and want to convert the workflow to R, see the resources developed in the rhutils package (<https://github.com/wburke24/rhutils>), there are excellent resources here. Delineating subbasins across CCR with this workflow ran into issues, and the GRASS functions worked best for catchment-wide subbasin delineation.
