
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgeos, gtools, sf, fs, haven, glue, rgeoda, rnaturalearth, rnaturalearthdata, RColorBrewer, classInt)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
m_20 <- st_read('gpkg/moran_fallecidosGral_2020.gpkg')
m_21 <- st_read('gpkg/moran_fallecidosGral_2021.gpkg')
zmpd <- read_csv('tble/zomac_pdet.csv')
zmpd <- mutate(zmpd, id_espa = as.character(id_espa))

# To make the join --------------------------------------------------------
m_20 <- left_join(m_20, zmpd, by = c('MPIO_CCNCT' = 'id_espa')) 
m_21 <- left_join(m_21, zmpd, by = c('MPIO_CCNCT' = 'id_espa')) 


