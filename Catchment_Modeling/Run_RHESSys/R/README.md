# R folder overview

- **Clim_data/** — Scripts that download and reformat climate driver data for RHESSys: ERA5 reanalysis (via Open-Meteo) and input4MIPs nitrogen deposition data.
- **Maps_Worldfiles/** — Scripts that build RHESSys spatial inputs, plus the `CCR_for_MS` subfolder with the DEM/land-cover preprocessing and stream routing work used to generate the base CCR worldfile, flowtable, and stream files.
- **Run_RHESSys/** — Scripts that actually execute RHESSys runs for CCR via `RHESSysIOinR` (spinup, transient, and a minimal example run).
- **Eval_RHESSys/** — Script to read RHESSys output files and evaluate/visualize results
