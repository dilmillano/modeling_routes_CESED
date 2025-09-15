# Modeling Routes CESED

## Citation

If you use this modeling framework in your research, please cite:

**Aguilar-Gomez, S., Millan-Orduz, D., Marín-Llanes, L., & Vélez, M. A. (Year). Advancing interdisciplinary science for modeling drug trafficking routes.** [Journal/Conference details]

## Ethical and Security Notice

For ethical and security reasons related to the sensitive nature of cocaine trafficking data, the original seizure datasets and the resulting modeled routes cannot be publicly shared. Only the modeling framework and its documentation are provided to ensure reproducibility of the methodology.

## Contact Information

**Corresponding Authors:**
- **Sandra Aguilar-Gomez**  
  E-mail: saguilargomez@tec.mx

- **Diana Millan-Orduz**  
  E-mail: d.millanorduz@uniandes.edu.co

## Overview

This repository contains R scripts for analyzing cocaine trafficking routes and seizure patterns in Colombia and neighboring countries. The analysis focuses on spatial modeling of drug trafficking routes using least-cost path analysis and resistance surface modeling.

The project analyzes cocaine cultivation clusters, seizure patterns, and models optimal trafficking routes using geographic information systems (GIS) and spatial analysis techniques. The analysis covers the period 2017-2022 and includes data from Colombia, Ecuador, Peru, Brazil, and Venezuela.

## Scripts Description

### 1. 1_coca_cultivation_clusters.R
**Purpose**: Processes coca cultivation clusters and generates centroids for route modeling.

**Functionality**:
- Loads coca cultivation shapefiles (COCA_SIMCI2.shp)
- Filters polygons with cultivation values > 20 for years 2017-2022
- Dissolves contiguous polygons and extracts centroids
- Exports dissolved polygons and centroids as separate shapefiles

**Output**: Dissolved cultivation areas and centroid points for each year.

### 2. 2_seizures_percentiles.R
**Purpose**: Calculates percentiles and quartiles for seizure data and gasoline prices.

**Functionality**:
- Processes seizure data by crop type (Cocaine base, Cocaine, Coca leaf, Coca paste)
- Calculates percentiles (25th, 50th, 75th, 90th, 95th, 99th) for Z-values
- Analyzes gasoline price data (2010-2023) with similar statistical measures
- Exports results to Excel files with separate sheets for each analysis

**Output**: Statistical summaries of seizure patterns and gasoline price distributions.

### 3. 3_seizures_zvalue_plots.R
**Purpose**: Creates visualizations of Z-values for different seizure types.

**Functionality**:
- Loads seizure data with Z-values for 2017-2022
- Transforms data using cube root for better visualization
- Creates histograms and boxplots for each seizure type
- Combines visualizations using patchwork library
- Calculates and displays 80th percentiles

**Output**: Comprehensive plots showing distribution patterns of seizure Z-values.

### 4. 4_gpw_seizures_percentiles.R
**Purpose**: Integrates Gridded Population of the World (GPW) data with seizure analysis.

**Functionality**:
- Extracts population data from GPW raster (2000-2023) for seizure polygons
- Calculates per capita seizure rates (IP_ columns)
- Computes standardized Z-scores for per capita rates
- Groups analysis by crop type and calculates percentiles
- Uses parallel processing for efficient raster extraction

**Output**: Population-adjusted seizure analysis with standardized scores.

### 5. 5_seizures_for_modeling.R
**Purpose**: Prepares seizure data for route modeling by country and crop type.

**Functionality**:
- Filters seizure data by country (Ecuador, Peru, Brazil, Venezuela)
- Separates data by crop type for targeted analysis
- Calculates quintiles and quartiles for Z-values (2017-2022)
- Removes zero values for accurate statistical analysis
- Prepares datasets for least-cost path modeling

**Output**: Country and crop-specific datasets ready for route modeling.

### 6. 6_create_resistance_raster.R
**Purpose**: Creates resistance surfaces for least-cost path analysis.

**Functionality**:
- Loads multiple spatial layers (rivers, roads, military bases, gasoline stations, seizures)
- Reprojects all layers to WGS84 (EPSG:4326)
- Creates a base raster with 250m resolution
- Rasterizes vector layers with appropriate resistance values
- Combines multiple resistance factors into a single raster

**Output**: Comprehensive resistance raster for route modeling.

### 7. 7_scenarios_validation.R
**Purpose**: Validates the optimal number of scenarios for route modeling.

**Functionality**:
- Analyzes route similarity using Jaccard index
- Calculates route overlap and buffer-based similarity
- Implements early stopping criteria for scenario selection
- Compares route lengths and spatial patterns
- Determines optimal number of scenarios (validates 10 scenarios)

**Output**: Validation metrics supporting the use of 10 modeling scenarios.

### 8. 9_model_direct_routes.R
**Purpose**: Models direct trafficking routes using least-cost path analysis.

**Functionality**:
- Loads resistance raster and applies reclassification matrices
- Creates 9 different scenarios (2-10) with varying resistance values
- Calculates conductance layers using gdistance package
- Applies geographic correction for distance calculations
- Uses parallel processing for efficient computation

**Output**: Optimal routes for each scenario based on resistance surfaces.

### 9. 10_unique_route_segments.R
**Purpose**: Identifies unique route segments across different scenarios.

**Functionality**:
- Processes route shapefiles for scenarios 2-10
- Creates unique geometry identifiers for route segments
- Counts frequency of each unique route segment
- Exports unique segments with occurrence counts
- Handles missing files gracefully

**Output**: Unique route segments with frequency analysis.

## Data Requirements

### Input Data
- **Coca cultivation data**: COCA_SIMCI2.shp
- **Seizure data**: Incautaciones_GPW.xlsx, Incautaciones_coca.shp
- **Population data**: Population_Count_2000_2023_V2.tif (GPW)
- **Infrastructure data**: Rivers, roads, military bases, gasoline stations
- **Geographic boundaries**: Country and administrative boundaries

### Required R Packages
`
library(sf)          # Spatial data handling
library(dplyr)       # Data manipulation
library(terra)       # Raster processing
library(raster)      # Raster analysis
library(gdistance)   # Least-cost path analysis
library(ggplot2)     # Visualization
library(patchwork)   # Plot combination
library(vegan)       # Ecological analysis
library(future.apply) # Parallel processing
library(parallel)    # Parallel computing
`

## Methodology

1. **Data Preparation**: Clean and standardize spatial and tabular data
2. **Statistical Analysis**: Calculate percentiles and Z-scores for seizure patterns
3. **Resistance Modeling**: Create resistance surfaces incorporating multiple factors
4. **Route Modeling**: Apply least-cost path analysis across multiple scenarios
5. **Validation**: Use similarity metrics to validate model results
6. **Visualization**: Create comprehensive plots and maps

## Output Files

- Dissolved cultivation areas and centroids
- Statistical summaries (Excel files)
- Resistance rasters (GeoTIFF)
- Optimal route shapefiles
- Validation metrics and plots
- Unique route segment analysis

## Usage

1. Ensure all required data files are in the specified paths
2. Install required R packages
3. Run scripts in numerical order for complete analysis
4. Adjust file paths as needed for your system
5. Modify parameters (years, countries, scenarios) as required

## License

This project is for academic and research purposes. Please cite appropriately if used in publications.
