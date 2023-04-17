
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rgeos, gtools, readxl, sf, fs, haven)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
mpio <- st_read('G:/D/data/IGAC/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')
ctgr <- read_excel('datos/tbl/categoria municipios.xlsx')
colnames(ctgr) <- c('cod', 'categoria')


