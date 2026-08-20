## Read me for RHESSys inputs maps for CCR

This folder builds the spatial inputs used to generate the CCR worldfile, flow table, and stream file. Note that the GRASS project used to make the slope, etc. maps is in folder RHESSys_Maps/CCR_files/CCR/GRASS_ccr_demfullpond.

### Pipeline order / what each script does

1. **1_GetDEM.R** — Downloads the DEM (via FedData) for the CCR area, reprojects it to UTM17N, rasterizes the watershed boundary from the StreamStats shapefile, and writes `ccr_ws.tif` / `ccr_DEM_fullpond.tif` to `Downloaded_Data/`.
2. **LandCover_NLCD_datacomp.R** — Downloads 2021 NLCD land cover, reprojects/masks it to the CCR watershed, and reclassifies NLCD classes into RHESSys vegetation stratum IDs (1 = non-veg, 2 = grass, 3 = deciduous, 4 = evergreen, 5 = mixed).
3. **CCR_preprocess_forMS.Rmd** — The main preprocessing notebook. Builds the hillslope/subbasin rasters and the k-means patch map from the GRASS-derived DEM products, finalizes the soil and NLCD rasters into `spatial_data/`, then calls `RHESSysPreprocess()` (using `template_ccr.txt`) to generate the worldfile (`ccr_mixed.world`) and flow table (`ccr_mixed.flow`).
4. **template_ccr.txt** — The RHESSysPreprocessing template that maps worldfile state variables (basin → hillslope → zone → patch → stratum) to the rasters in `spatial_data/`. Used by `CCR_preprocess_forMS.Rmd`, not run on its own.
5. **CCR_StreamRouting.R** — Builds the stream reach network/topology table (`ccr_mixed.stream`) from the GRASS-derived stream raster: traces reach adjacency and upstream/downstream connectivity out from a known outlet reach.
6. **Map_for_Paper.R** — Makes a publication-quality map of the CCR watershed and stream network for talks/papers. Not part of the worldfile-generation pipeline itself.

Other folders here: `Downloaded_Data/` holds the raw DEM/NLCD downloads from step 1–2, `spatial_data/` holds the finalized rasters produced by step 3 and consumed by the template.

### What the world, flow, and stream files are

- **ccr_mixed.world** — The RHESSys worldfile: the nested basin → hillslope → zone → patch → stratum hierarchy with each unit's static state variables and parameter IDs, built by `RHESSysPreprocess()` from `template_ccr.txt` and the rasters in `spatial_data/`. This is the initial worldfile carried into `worldfiles/Generated/` and used to start the spinup run.
- **ccr_mixed.flow** — The flow table: describes surface and subsurface routing (the "gamma" weights) between patches, generated alongside the worldfile by the same `RHESSysPreprocess()` call.
- **ccr_mixed.stream** — The stream network/routing table: reach-level topology (adjacency, upstream/downstream reach IDs, channel geometry) built separately by `CCR_StreamRouting.R`, used for RHESSys's stream routing (`-str` command-line flag).

These three files are also duplicated under `worldfiles/Generated/` — see [worldfiles/README.md](../../../worldfiles/README.md).
