
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgeos, gtools, sf, fs, haven, glue, rgeoda, rnaturalearth, rnaturalearthdata, RColorBrewer, classInt)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
s_20 <- st_read('gpkg/total_mortalidad_2020.gpkg')
s_21 <- st_read('gpkg/total_mortalidad_2021.gpkg')

dpts <- st_read('./SHP/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')

# To calculate the weigths ------------------------------------------------
q_20 <- queen_weights(s_20, order = 1, precision_threshold = 0.10)
q_21 <- queen_weights(s_21, order = 1, precision_threshold = 0.10)

# Moran 2020 --------------------------------------------------------------
m_20 <- local_moran(q_20, st_drop_geometry(s_20['count']))
mran_lbls <- lisa_labels(m_20)
mran_clrs <- setNames(lisa_colors(m_20), mran_lbls)

s_20 <- mutate(s_20, cluster_num = lisa_clusters(m_20) + 1,
                     cluster = factor(mran_lbls[cluster_num], levels = mran_lbls))

ggplot(s_20, aes(fill = cluster)) +
  geom_sf(color = "white", size = 0) +
  scale_fill_manual(values = mran_clrs, na.value = "green") +
  theme_dark()

# Labels (spanish)
lbls <- s_20 %>% st_drop_geometry() %>% distinct(cluster_num, cluster) %>% arrange(cluster_num)
lbls <- mutate(lbls, clase = c('Sin significancia', 'Alto-Alto', 'Bajo-Bajo', 'Bajo-Alto', 'Alto-Bajo', 'Isolados'))

# Join labels
s_20 <- inner_join(s_20, lbls, by = 'cluster_num')
names(mran_clrs) <- c('Sin significancia', 'Alto-Alto', 'Bajo-Bajo', 'Bajo-Alto', 'Alto-Bajo', 'Indefinido', 'Aislados')

mrn_2020 <- mean(m_20$lisa_vals)

# To make the map 2020
wrld <- ne_countries(scale = 50, returnclass = 'sf')
g_mrn_2020 <- ggplot() + 
  geom_sf(data = s_20, aes(fill = clase, col = clase), lwd = 0.2)+
  scale_fill_manual(values = mran_clrs) +
  scale_color_manual(values = mran_clrs, guide = 'none') +
  geom_sf(data = dpts, fill = NA, col = 'grey60', lwd = 0.5) + 
  geom_sf(data = wrld, fill = NA, col = 'grey60', lwd = 0.2) + 
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
        legend.text = element_text(family = 'serif'))

ggsave(plot = g_mrn_2020, filename = './png/maps/morn_2020.png', units = 'in', width = 7, height = 9, dpi = 300)


