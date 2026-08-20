# worldfiles folder overview


- **Generated/** — The base worldfile, flowtable, and stream file (`ccr_mixed.world`/`.flow`/`.stream`) built by the preprocessing scripts in `R/Maps_Worldfiles/`. This is the starting point for the spinup run.
- **spin/** — The worldfile state written at the end of the 1000 year spinup run (`ccr_mixed.world.Y2940M1D2H23.state`), used as the initial condition for every transient scenario run. The `ccr/` subfolder holds a header file (`ccr.hdr`) that `RHESSysIOinR` auto-generates the first time this state is used as a run's input.
- **transient/** — One subfolder (`HarvestNone`), holding the worldfile state written at the end of that scenario's transient run (e.g. `ccr_mixed.world.Y2940M1D2H23.state.Y2026M4D2H23.state`). These are the states used as starting points for further runs.
