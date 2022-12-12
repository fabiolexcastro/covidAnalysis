
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgeos, gtools, sf, fs, haven, glue, rgeoda, rnaturalearth, rnaturalearthdata, RColorBrewer, classInt)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
s_20 <- st_read('gpkg/total_mortalidad_2020.gpkg')
s_21 <- st_read('gpkg/total_mortalidad_2021.gpkg')

# To calculate the weigths ------------------------------------------------
q_20 <- queen_weights(s_20, order = 1)
q_21 <- queen_weights(s_21, order = 1)


