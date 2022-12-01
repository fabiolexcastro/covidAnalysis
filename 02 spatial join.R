
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rgeos, gtools, sf, fs, haven, rgeoda)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
tble <- read_csv('./tble/data_v1.csv')
head(tble)
shpf <- st_read('./SHP/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')
