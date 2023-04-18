

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rgeoda, terra, rgeos, cowplot, ggpubr, gtools, readxl, rnaturalearthdata, rnaturalearth, sf, fs, haven, RColorBrewer, ggspatial, colourpicker)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Administrative data -----------------------------------------------------
dpts <- st_read('G:/D/data/IGAC/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
wrld <- ne_countries(scale = 50, returnclass = 'sf')

# Load data ---------------------------------------------------------------
shpf <- st_read('datos/gpkg/mpios_rate_ipm_v2.gpkg')

# Weighted ----------------------------------------------------------------
qnwg <- queen_weights(shpf, order = 1, precision_threshold = 0.10)

# Moran Rate --------------------------------------------------------------
mran_rate <- local_moran(qnwg, st_drop_geometry(shpf['rate']))
mean(mran_rate$lisa_vals) ## 0.30 
mean(mran_rate$p_vals) ## 0.19

# Lisa --------------------------------------------------------------------
mran_rate_lbls <- lisa_labels(mran_rate)
mran_rate_clrs <- setNames(lisa_colors(mran_rate), mran_rate_lbls)

shpf <- mutate(shpf, cluster_num = lisa_clusters(mran_rate) + 1,
                     cluster = factor(mran_rate_lbls[cluster_num], levels = mran_rate_lbls))
table(shpf$cluster)

# Labels in spanish
lbls <- shpf %>% st_drop_geometry() %>% distinct(cluster_num, cluster) %>% arrange(cluster_num) %>% mutate(clase = c('Sin significancia', 'Alto-Alto', 'Bajo-Bajo', 'Bajo-Alto', 'Alto-Bajo', 'Aislados'), color = c('#eeeeee', '#FF0000', '#0000FF', '#a7adf9', '#f4ada8', '#999999'))
write.csv(lbls, 'datos/tbl/labels_colors_lisa.csv', row.names = FALSE)

# Join labels
shpf <- inner_join(shpf, lbls, by = c('cluster_num'))

# To make the map ---------------------------------------------------------
names(mran_rate_clrs) <- lbls$clase

g_mran_rate <- ggplot() + 
  geom_sf(data = shpf, aes(fill = clase, col = clase)) + 
  scale_fill_manual(values = mran_rate_clrs) +
  scale_color_manual(values = mran_rate_clrs, guide = 'none') + 
  geom_sf(data = dpts, fill = NA, col = 'grey60') + 
  geom_sf(data = wrld, fill = NA, col = 'grey60') + 
  ggtitle(label = 'Análisis LISA para tasa de mortalidad por\n1 millón de habitantes') + 
  labs(x = 'Lon', y = 'Lat', fill = 'Clúster', caption = 'Adaptado de INS, 2023') +
  coord_sf(xlim = ext(dpts)[1:2], ylim = ext(dpts)[3:4]) + 
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5, face = 'bold'))

ggsave(plot = g_mran_rate, filename = 'png/mapa_unimoran_rate.png', units = 'in', width = 7, height = 10, dpi = 300)


