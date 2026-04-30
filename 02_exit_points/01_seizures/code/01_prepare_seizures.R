# ------------------------------------------------------------
# R script: duplicate input with ID_SEIZURE, then filter by year/cultivo,
#           reproject to EPSG:9377, and export (full copy + per-year outputs)
# Author: (you)
# Date: 2025-10-29
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)         # vector geospatial IO
  library(dplyr)      # data manipulation
  library(stringi)    # accent handling
  library(readr)      # parsing helpers
})

# ----------------------------
# 1) Paths and constants
# ----------------------------
in_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/01_seizures/input/incautaciones.shp"
out_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/01_seizures/output"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Target CRS: MAGNA-SIRGAS 2018 / Origen-Nacional (national unique origin)
crs_target_epsg <- 9377

# Utility: remove existing ESRI Shapefile sidecar files (clean overwrite)
remove_existing_shp <- function(path_with_ext) {
  base_no_ext <- tools::file_path_sans_ext(path_with_ext)
  exts <- c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".qmd", ".qix", ".sbn", ".sbx")
  files <- paste0(base_no_ext, exts)
  files <- files[file.exists(files)]
  if (length(files)) invisible(file.remove(files))
}

# ----------------------------
# 2) Read input
# ----------------------------
seiz <- st_read(in_path, quiet = TRUE)

if (!all(c("year", "cultivo") %in% names(seiz))) {
  stop("The input shapefile must contain 'year' and 'cultivo' fields.")
}

if (is.na(st_crs(seiz))) {
  warning("Input has no CRS defined. Verify the source CRS before transforming.")
}

# ----------------------------
# 3) Create ID_SEIZURE on the full original dataset
#    and save a clean copy in EPSG:9377
# ----------------------------
# Note: ID_SEIZURE is a simple sequential integer stable with current order.
seiz_with_id <- seiz %>%
  mutate(ID_SEIZURE = dplyr::row_number())

# Reproject full copy to target CRS
seiz_with_id_9377 <- st_transform(seiz_with_id, crs_target_epsg)

# Write full copy (keeps original attribute names + ID_SEIZURE)
full_out <- file.path(out_dir, "incautaciones_with_id.shp")
remove_existing_shp(full_out)
st_write(seiz_with_id_9377, full_out, driver = "ESRI Shapefile", quiet = TRUE)
cat(sprintf("Exported FULL copy with ID_SEIZURE to: %s (CRS EPSG:%s)\n",
            full_out, st_crs(seiz_with_id_9377)$epsg))

# ----------------------------
# 4) Normalize attributes for robust filtering
# ----------------------------
normalize_ascii_lower <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    trimws()
}

seiz2 <- seiz_with_id %>%
  mutate(
    year_int     = suppressWarnings(readr::parse_integer(as.character(.data$year))),
    cultivo_norm = normalize_ascii_lower(.data$cultivo)
  )

years_keep <- 2017:2022
cultivos_keep <- c("hoja de coca", "pasta de coca", "base de coca", "cocaina")

# ----------------------------
# 5) Filter features (ID_SEIZURE is preserved)
# ----------------------------
seiz_f <- seiz2 %>%
  filter(
    !is.na(year_int),
    year_int %in% years_keep,
    cultivo_norm %in% cultivos_keep
  )

cat("Total features read: ", nrow(seiz), "\n")
cat("Total features after filter: ", nrow(seiz_f), "\n")
cat("Count by year after filter:\n")
print(seiz_f %>% as.data.frame() %>% count(year_int) %>% arrange(year_int))

if (nrow(seiz_f) == 0) stop("No features match the filters. Nothing to export.")

# ----------------------------
# 6) Reproject filtered set once to EPSG:9377
# ----------------------------
seiz_f_9377 <- st_transform(seiz_f, crs_target_epsg)

# ----------------------------
# 7) Export one shapefile per year (in EPSG:9377)
#    Keep ID_SEIZURE; drop helper fields
# ----------------------------
drop_helpers <- function(x) dplyr::select(x, -cultivo_norm, -year_int)

layer_name_for_year <- function(y) paste0("seiz_", y)
unique_years <- sort(unique(seiz_f_9377$year_int))

for (yy in unique_years) {
  sub_y <- dplyr::filter(seiz_f_9377, year_int == yy)
  if (nrow(sub_y) == 0) next
  
  lyr <- layer_name_for_year(yy)
  out_path <- file.path(out_dir, paste0(lyr, ".shp"))
  
  remove_existing_shp(out_path)
  st_write(
    obj    = drop_helpers(sub_y),  # keep ID_SEIZURE, remove temp cols
    dsn    = out_path,
    driver = "ESRI Shapefile",
    quiet  = TRUE
  )
  
  cat(sprintf("Exported %d features to: %s (CRS EPSG:%s)\n",
              nrow(sub_y), out_path, st_crs(sub_y)$epsg))
}

cat("Done. Full copy with ID_SEIZURE and yearly shapefiles saved in EPSG:9377.\n")
