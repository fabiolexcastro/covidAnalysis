
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

# Join between the shapefile and the table --------------------------------
mpio_ctgr <- inner_join(mpio, ctgr, by = c('MPIO_CCNCT' = 'cod'))
mpio_ctgr
dir_create('datos/gpkg')
st_write(mpio_ctgr, 'datos/gpkg/mpios_categoria.gpkg')

# Tasa --------------------------------------------------------------------
excel_sheets('datos/tbl/TASA MORTALIDAD MUNICIPIO.xlsx')
rate <- read_excel('datos/tbl/TASA MORTALIDAD MUNICIPIO.xlsx', sheet = 'Hoja2')

View(rate)
rate <- mutate(rate, divi = as.numeric(DIVIPOLA), rate = as.numeric(TASAMORTALIDAD))
rate[!complete.cases(rate),]
