

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
makeCount <- function(shpf, year){
  
  # shpf <- m_20 # Proof
  # year <- 2020
  
  freq <- as.data.frame(table(shpf$clase))
  colnames(freq) <- c('categoria', 'cantidad_municipios')
  freq <- freq %>% arrange(desc(cantidad_municipios))
  
  mpos <- shpf %>% 
    filter(!clase %in% c('Sin significancia', 'Aislados')) %>% 
    st_drop_geometry %>% 
    dplyr::select(MPIO_CCNCT, nombredepartamento, nombremunicipio, count, clase) %>% 
    as_tibble() %>% 
    arrange(clase) %>% 
    mutate(anio = year)
  
  cat('Done!\n')
  return(mpos)
  
}

count_2020 <- makeCount(shpf = m_20, year = 2020)
count_2021 <- makeCount(shpf = m_21, year = 2021)

library(xlsx)
dir_create('tble/results')
write.xlsx(as.data.frame(count_2020), file = 'tble/results/countMoran_gral_univariado.xlsx', sheetName = 'y2020', row.names = FALSE)
write.xlsx(as.data.frame(count_2021), file = 'tble/results/countMoran_gral_univariado.xlsx', sheetName = 'y2021', append = TRUE, row.names = FALSE)

# Read pdet and zomac -----------------------------------------------------
pdet <- st_read('SHP/pdet.shp')
zomc <- st_read('SHP/zomac.shp')

pdet_tble <- pdet %>% 
  st_drop_geometry() %>% 
  dplyr::select(ID_ESPA) %>%
  as_tibble() %>% 
  mutate(tipo_mpio = 'PDET')

zomc_tble <- zomc %>% 
  st_drop_geometry() %>% 
  dplyr::select(ID_ESPA) %>%
  as_tibble() %>% 
  mutate(tipo_mpio = 'ZOMAC')

pdet_zomc <- full_join(pdet_tble, zomc_tble, by = 'ID_ESPA') %>% setNames(c('id_espa', 'typo_x', 'typo_y'))
pdet_zomc <- mutate(pdet_zomc, id_espa = as.character(id_espa))
pdet_zomc <- mutate(pdet_zomc, zomac_pdet = paste0(typo_x, '-', typo_y))
head(pdet_zomc)
unique(pdet_zomc$zomac_pdet)
pdet_zomc <- mutate(pdet_zomc, zomac_pdet = gsub('PDET-NA', 'PDET', zomac_pdet))
pdet_zomc <- mutate(pdet_zomc, zomac_pdet = gsub('NA-ZOMAC', 'ZOMAC', zomac_pdet))
pdet_zomc <- dplyr::select(pdet_zomc, id_espa, zomac_pdet)

# Join with the main table ------------------------------------------------
count_2020 <- left_join(count_2020, pdet_zomc, by = c('MPIO_CCNCT' = 'id_espa'))
count_2020_zomac_pdet <- filter(count_2020, !is.na(zomac_pdet))
count_2020_zomac_pdet


