# Converting shapefiles to geopackages! 

# Faster geoprocessing! 

library(sf)


lsoa_boundaries <- st_read("data/LSOA_boundaries/LSOA 2021/Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10).shp")


st_write(obj = lsoa_boundaries, driver = "GPKG", dsn = "data/LSOA_boundaries/LSOA 2021/Lower_layer_Super_Output_Areas_EW_BFC.gpkg")

