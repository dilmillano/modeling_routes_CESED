# Modeling Cocaine Trafficking Routes in Colombia: An R-based Least-Cost Path Framework

**Paper:** *Advancing interdisciplinary science for modeling drug trafficking routes*

**Authors:** Sandra Aguilar-Gomez (Tecnológico de Monterrey) · Diana Millan-Orduz (Universidad de los Andes) · Lucas Marín-Llanes (Northwestern University) · Maria Alejandra Vélez (Universidad de los Andes)

---

## Overview

This repository provides the complete open-source implementation of the spatial modeling framework developed to estimate cocaine trafficking routes within Colombia for the period 2017–2022. The model combines satellite-derived coca cultivation data, georeferenced cocaine seizure records, and multi-source geospatial layers within a multimodal cost surface. A Least-Cost Path (LCP) algorithm is then applied to estimate optimal trafficking corridors between coca cultivation areas and major seizure hotspots. To quantify spatial uncertainty, the framework implements Ensemble Path Occurrence Forecasting (EPOF), which evaluates segment-level recurrence across 14 alternative resistance scenarios and assigns a confidence score to each route segment.

Key results: the model identifies an average of 36,359 km of trafficking routes per year, covering approximately 57% of Colombian territory within 20 km of a modeled corridor. Of these, 8,465 km correspond to persistent corridors active throughout the full six-year period.

---

## Methodology

The framework is organized into five sequential stages:

**1. Definition of origin points**
Coca cultivation data from UNODC–SIMCI (1 km × 1 km annual grid) are filtered to retain cells with more than 20 hectares of coca. Contiguous cells are dissolved into clusters and their centroids are used as route origins.

**2. Definition of trafficking nodes**
Cocaine seizures reported by the Ministry of Defense are normalized by local population using Thiessen polygons and Gridded Population of the World (GPW) rasters. Only seizures exceeding the 99th percentile of population-adjusted volume (land) or 1,000 kg (maritime) are retained as trafficking nodes — the domestic endpoints of modeled routes.

**3. Construction of the cost surface**
A categorical raster integrates infrastructure layers (rivers, primary/secondary/tertiary roads, tracks, ports) and restriction layers (water bodies, military battalions) following a fixed hierarchical priority. A continuous slope raster derived from NASADEM (30 m) is then combined multiplicatively with the categorical raster. The resulting composite resistance surface encodes the logistical friction of movement across the Colombian landscape.

**4. Route modeling via Least-Cost Path (LCP)**
Each origin point is linked to its 20 nearest trafficking nodes (selected via Euclidean distance elbow analysis). LCP routes are computed for each origin–node pair across 14 alternative resistance scenarios (generated through controlled parametric variation of the baseline cost surface). This yields an ensemble of spatially comparable route realizations per year.

**5. Ensemble Path Occurrence Forecasting (EPOF)**
All modeled routes are geometrically segmented (planarized) into elementary linear units. For each segment, an occurrence count is computed across the full ensemble. A quantile-based classification assigns a discrete confidence score *C_s* to each segment, reflecting the frequency with which it recurs across scenarios. Only segments with *C_s* ≥ 25 are retained for analysis. A density-weighted skeletonization procedure then merges parallel segments within 1 km into single centerlines to reduce spatial noise.

---

## Repository Structure

```
modeling_routes_CESED/
│
├── 00_area/                          # Study area boundary (extent shapefile — not shared)
│
├── 01_origin/
│   ├── code/
│   │   └── 01_generate_coca_dissolved_and_centroids.R
│   ├── input/                        # UNODC–SIMCI coca grid (not shared)
│   └── output/                       # Dissolved clusters and centroids (not shared)
│
├── 02_exit_points/
│   ├── 01_seizures/
│   │   ├── code/
│   │   │   ├── 01_prepare_seizures.R
│   │   │   ├── 02_thiessen.R
│   │   │   └── 03_reproject.R
│   │   ├── input/                    # Ministry of Defense seizure records (not shared)
│   │   └── output/                   # Yearly seizure shapefiles and Thiessen polygons (not shared)
│   │
│   ├── 02_population/
│   │   ├── code/
│   │   │   ├── 01_download_gpw_gee.js       # Google Earth Engine script — GPW download
│   │   │   ├── 02_download_worldpop_gee.js  # Google Earth Engine script — WorldPop download
│   │   │   └── 03_reproject_9377.R
│   │   └── output/                   # Annual population rasters (not shared)
│   │
│   ├── 03_aggregate_population/
│   │   ├── code/
│   │   │   ├── 01_aggregate_population.R
│   │   │   ├── 02_compare_population_sources.R
│   │   │   └── 03_compare_CENSO_2018.R
│   │   ├── input/                    # Municipal census data (not shared)
│   │   └── output/                   # Aggregated population tables (not shared)
│   │
│   └── 04_normalization/
│       ├── code/
│       │   ├── 01_assign_population_to_seizure_points.R
│       │   ├── 02_compute_per_capita_and_z.R
│       │   └── 03_select_thresholds.R
│       ├── input/                    # (not shared)
│       └── output/                   # Normalized seizure layers and trafficking nodes (not shared)
│
├── 03_cost_surface/
│   ├── code/
│   │   ├── 01_extraer_vias_OSM.R     # OSM road network extraction
│   │   └── 02_crear_superficie_raster.R  # Composite resistance raster construction
│   ├── input/                        # Rivers, roads, military bases, ports (not shared)
│   ├── intermedios_osm/              # Intermediate OSM files (not shared)
│   └── output/                       # cost_surface_1km.tif (not shared)
│
├── 04_LCP/
│   ├── code/
│   │   ├── 00_comprobacion_puntos_origen_destino_euclidiana.R  # k-node elbow analysis
│   │   ├── 01_LCP.R                                           # LCP baseline (2017, 10 scenarios)
│   │   ├── 02_LCP_2017_esc11_20.R                             # LCP scenarios 11–20 (2017)
│   │   ├── 03_metricas_convergencia.R                         # Scenario convergence metrics
│   │   ├── 03_metricas_test1.R
│   │   ├── 03_metricas_test2.R
│   │   ├── 03_metricas_test3.R
│   │   ├── 04_LCP_2018_2022_14ESC.R                           # LCP 2018–2022, 14 scenarios
│   │   └── 04_LCP_2018_2022_14ESC_REANUDACION.R               # Resumable version
│   ├── input/                        # (not shared)
│   └── output/                       # Annual LCP route shapefiles (not shared)
│
├── 05_post_processing/
│   ├── code/
│   │   ├── 01_EPOF_count.R            # Segment occurrence counting (EPOF)
│   │   ├── 02_sum_long.R              # Route length aggregation
│   │   ├── 03_spatiotemporal_category.R  # Temporal classification (persistent/regular/emerging/declining)
│   │   ├── 04_veredas_municipios.R    # Spatial join to veredas and municipalities
│   │   ├── 04_veredas_municipios_corr.R  # Corrected version
│   │   └── center_line.ipynb          # Centerline extraction (Python/GeoPandas)
│   └── output/                       # Processed corridors and spatiotemporal categories (not shared)
│
└── 06_results/                       # Summary figures and tables (not shared)
```

---

## Requirements

### R packages

Install all required packages with:

```r
install.packages(c(
  "sf",           # vector spatial data
  "terra",        # raster operations
  "raster",       # legacy raster (used in LCP scripts)
  "gdistance",    # least-cost path computation
  "dplyr",        # data manipulation
  "future.apply", # parallel processing (Windows-compatible)
  "tibble",       # data frames
  "ggplot2",      # visualization
  "readr",        # CSV import
  "openxlsx"      # Excel export
))
```

### Python (for centerline extraction)

The script `05_post_processing/code/center_line.ipynb` requires:

```
geopandas, shapely, scikit-image, jupyter
```

### Google Earth Engine

Scripts `02_exit_points/02_population/code/01_download_gpw_gee.js` and `02_download_worldpop_gee.js` are JavaScript files for the [Google Earth Engine Code Editor](https://code.earthengine.google.com/). A GEE account is required to run them.

---

## How to Run

The scripts follow a modular, sequential structure. Adapt the file paths at the top of each script to your local directory before running.

| Step | Folder | Script(s) | Description |
|------|--------|-----------|-------------|
| 1 | `01_origin/code/` | `01_generate_coca_dissolved_and_centroids.R` | Generate origin points from UNODC–SIMCI coca grid |
| 2a | `02_exit_points/01_seizures/code/` | `01_prepare_seizures.R` → `02_thiessen.R` → `03_reproject.R` | Filter and spatialize seizure records |
| 2b | `02_exit_points/02_population/code/` | GEE scripts → `03_reproject_9377.R` | Download and reproject population rasters |
| 2c | `02_exit_points/03_aggregate_population/code/` | `01` → `02` → `03` | Aggregate population to Thiessen polygons |
| 2d | `02_exit_points/04_normalization/code/` | `01` → `02` → `03` | Normalize seizures and select trafficking nodes |
| 3 | `03_cost_surface/code/` | `01_extraer_vias_OSM.R` → `02_crear_superficie_raster.R` | Build categorical and composite resistance raster |
| 4 | `04_LCP/code/` | `00` → `01` → `03_metricas_convergencia.R` → `04_LCP_2018_2022_14ESC.R` | Run LCP for all years and scenarios |
| 5 | `05_post_processing/code/` | `01` → `02` → `03` → `04` | EPOF counting, temporal classification, spatial joins |

> **Note on computational requirements:** LCP computation is the most intensive step. The script `04_LCP_2018_2022_14ESC.R` uses `future.apply` for Windows-compatible parallelization. Execution for 14 scenarios × 6 years may require several hours depending on available RAM and CPU cores. A resumable version (`04_LCP_2018_2022_14ESC_REANUDACION.R`) is provided to restart from checkpoints.

---

## Data Availability and Code Availability

The GitHub repository associated with the paper is openly available at [https://github.com/dilmillano/modeling_routes_CESED](https://github.com/dilmillano/modeling_routes_CESED). Its use requires proper citation of CESED and the researchers associated with this paper.

The repository includes the complete R-based framework developed for this study, including the code to construct the resistance raster, implement the least-cost path (LCP) models, and this README file that explains how to run and adapt the routines. The code is modular and can be customized for different regions, time periods, or spatial layers of interest (e.g., road networks, river systems, enforcement presence).

**For ethical and security reasons related to the sensitive nature of cocaine trafficking data, the original seizure datasets, the coca cultivation shapefiles (UNODC–SIMCI), and the resulting modeled routes cannot be publicly shared.** Only the modeling framework and its documentation are provided to ensure reproducibility of the methodology.

The following data types are therefore **not included** in this repository:

- Cocaine seizure records (Ministry of Defense, Colombia)
- Coca cultivation grids (UNODC–SIMCI)
- Modeled route shapefiles and rasters
- Cost surface rasters
- Population rasters (GPW, WorldPop)
- All intermediate and output geospatial files

Researchers seeking access to the underlying datasets should contact the corresponding author or the relevant data custodians directly.

---

## Citation

If you use this code, please cite:

> Aguilar-Gomez, S., Millan-Orduz, D., Marín-Llanes, L., & Vélez, M. A. (2025). *Advancing interdisciplinary science for modeling drug trafficking routes*. CESED — Centro de Estudios sobre Seguridad y Drogas, Universidad de los Andes.

---

## License

This code is shared for academic and research purposes. Its use requires proper citation of CESED and the associated researchers. See the paper for full methodological details.
