

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgeos, gtools, sf, fs, haven, glue, rgeoda, rnaturalearth, rnaturalearthdata, RColorBrewer, classInt)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
m_20 <- st_read('gpkg/moran_fallecidosGral_2020.gpkg')
m_21 <- st_read('gpkg/moran_fallecidosGral_2021.gpkg')

# Make the count ----------------------------------------------------------
makeCount <- function(shpf){
  
  shpf <- m_20 # Proof
  freq <- as.data.frame(table(shpf$clase))
  colnames(freq) <- c('categoria', 'cantidad_municipios')
  freq <- freq %>% arrange(desc(cantidad_municipios))
  
  
}


