

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
# Make the count ----------------------------------------------------------
makeCount <- function(shpf, year){
  
  # shpf <- m_20 # Proof
  # year <- 2020
  
  freq <- as.data.frame(table(shpf$clase))
  colnames(freq) <- c('categoria', 'cantidad_municipios')
  freq <- freq %>% arrange(desc(cantidad_municipios))
  
  mpos <- shpf %>% 
    filter(!clase %in% c('Sin significancia', 'Aislados')) %>% 
    st_drop_geometry %>% 
    dplyr::select(nombredepartamento, nombremunicipio, count, clase) %>% 
    as_tibble() %>% 
    arrange(clase) %>% 
    mutate(anio = year)
  
  cat('Done!\n')
  return(mpos)
  
}



