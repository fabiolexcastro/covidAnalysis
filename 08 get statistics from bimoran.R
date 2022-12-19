

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, hrbrthemes, ggnewscale, terra, rgeos, gtools, sf, fs, haven, glue, rgeoda, rnaturalearth, rnaturalearthdata, RColorBrewer, classInt)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
sh20 <- st_read('gpkg/bimoran/bimoran_total_count_2020_totalipm.gpkg')
sh21 <- st_read('gpkg/bimoran/bimoran_total_count_2021_totalipm.gpkg')

# ZOMAC / PDET
zomc <- read_csv('tble/zomac_pdet.csv')

# Mpios base
mpio <- st_read('SHP/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')

# Cleaning the shapefiles -------------------------------------------------
sh20 <- dplyr::select(sh20, MPIO_CCNCT, count, Totalipm, clase, geom)
sh21 <- dplyr::select(sh20, MPIO_CCNCT, count, Totalipm, clase, geom)


