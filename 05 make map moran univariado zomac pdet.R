
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgeos, gtools, ggnewscale, sf, fs, haven, glue, rgeoda, rnaturalearth, rnaturalearthdata, RColorBrewer, classInt)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
m_20 <- st_read('gpkg/moran_fallecidosGral_2020.gpkg')
m_21 <- st_read('gpkg/moran_fallecidosGral_2021.gpkg')
zmpd <- read_csv('tble/zomac_pdet.csv')
zmpd <- mutate(zmpd, id_espa = as.character(id_espa))

clrs <- read_csv('tble/colors_moran.csv')

# Administrative data
wrld <- ne_countries(scale = 50, returnclass = 'sf')
dpts <- st_read('./SHP/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')

# To make the join --------------------------------------------------------
m_20 <- left_join(m_20, zmpd, by = c('MPIO_CCNCT' = 'id_espa')) 
m_21 <- left_join(m_21, zmpd, by = c('MPIO_CCNCT' = 'id_espa')) 

m_20_zm <- filter(m_20, !is.na(zomac_pdet))
m_20_zm <- filter(m_20_zm, clase != 'Sin significancia')
m_20_zm <- dplyr::rename(m_20_zm, Tipo = zomac_pdet)
m_20_zm <- mutate(m_20_zm, Tipo = factor(Tipo, levels = c('ZOMAC', 'PDET', 'PDET-ZOMAC')))

m_21_zm <- filter(m_21, !is.na(zomac_pdet))
m_21_zm <- filter(m_21_zm, clase != 'Sin significancia')
m_21_zm <- dplyr::rename(m_21_zm, Tipo = zomac_pdet)
m_21_zm <- mutate(m_21_zm, Tipo = factor(Tipo, levels = c('ZOMAC', 'PDET', 'PDET-ZOMAC')))


# -------------------------------------------------------------------------
# To make the map ---------------------------------------------------------
# -------------------------------------------------------------------------

# 2020 --------------------------------------------------------------------

mran_clrs <- clrs$color
names(mran_clrs) <- clrs$clase

g_mrn_2020 <- ggplot() + 
  geom_sf(data = m_20, aes(fill = clase, col = clase), lwd = 0.2)+
  scale_fill_manual(values = mran_clrs, name = 'Clase') +
  scale_color_manual(values = mran_clrs, guide = 'none') +
  geom_sf(data = dpts, fill = NA, col = 'grey60', lwd = 0.5) + 
  geom_sf(data = wrld, fill = NA, col = 'grey60', lwd = 0.2) + 
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = m_20_zm, aes(col = Tipo), fill = NA, lwd = 0.6) +
  scale_color_manual(values = c('#C9D3B4', '#70687A', '#CDDD57'), name = 'Tipo') +
  coord_sf(xlim = ext(dpts)[1:2], ylim = ext(dpts)[3:4]) + 
  ggtitle(label = 'Análisis LISA para fallecidos por COVID-19 en el año 2020') + 
  labs(x = 'Lon', y = 'Lat', caption = 'INS - 2020', fill = 'Categoria') +
  theme_minimal() + 
  theme(axis.text.x = element_text(family = 'serif'), 
        axis.text.y = element_text(family = 'serif'), 
        axis.title.x = element_text(family = 'serif'), 
        axis.title.y = element_text(family = 'serif'), 
        plot.title = element_text(family = 'serif', hjust = 0.5, face = 'bold'),
        legend.position = 'bottom', 
        legend.title = element_text(face = 'bold', family = 'serif'), 
        legend.text = element_text(family = 'serif')) +
  guides(color=guide_legend(nrow = 2, byrow = TRUE))

ggsave(plot = g_mrn_2020, filename = './png/maps/morn_2020_zomacpdet.png', units = 'in', width = 7, height = 9, dpi = 300)

# 2021 --------------------------------------------------------------------

g_mrn_2021 <- ggplot() + 
  geom_sf(data = m_21, aes(fill = clase, col = clase), lwd = 0.2)+
  scale_fill_manual(values = mran_clrs, name = 'Clase') +
  scale_color_manual(values = mran_clrs, guide = 'none') +
  geom_sf(data = dpts, fill = NA, col = 'grey60', lwd = 0.5) + 
  geom_sf(data = wrld, fill = NA, col = 'grey60', lwd = 0.2) + 
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = m_21_zm, aes(col = Tipo), fill = NA, lwd = 0.6) +
  scale_color_manual(values = c('#C9D3B4', '#70687A', '#CDDD57'), name = 'Tipo') +
  coord_sf(xlim = ext(dpts)[1:2], ylim = ext(dpts)[3:4]) + 
  ggtitle(label = 'Análisis LISA para fallecidos por COVID-19 en el año 2021') + 
  labs(x = 'Lon', y = 'Lat', caption = 'INS - 2021', fill = 'Categoria') +
  theme_minimal() + 
  theme(axis.text.x = element_text(family = 'serif'), 
        axis.text.y = element_text(family = 'serif'), 
        axis.title.x = element_text(family = 'serif'), 
        axis.title.y = element_text(family = 'serif'), 
        plot.title = element_text(family = 'serif', hjust = 0.5, face = 'bold'),
        legend.position = 'bottom', 
        legend.title = element_text(face = 'bold', family = 'serif'), 
        legend.text = element_text(family = 'serif')) +
  guides(color=guide_legend(nrow = 2, byrow = TRUE))

ggsave(plot = g_mrn_2021, filename = './png/maps/morn_2021_zomacpdet.png', units = 'in', width = 7, height = 9, dpi = 300)
