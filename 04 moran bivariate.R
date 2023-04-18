
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rgeoda, terra, rgeos, cowplot, ggpubr, gtools, readxl, rnaturalearthdata, rnaturalearth, sf, fs, haven, RColorBrewer, ggspatial, colourpicker)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Colors ------------------------------------------------------------------
clrs <- read.csv('datos/tbl/labels_colors_lisa.csv')[,c(3, 4)]
lbls.clrs <- clrs$color
names(lbls.clrs) <- clrs$clase
lbls.clrs

# Administrative data -----------------------------------------------------
dpts <- st_read('G:/D/data/IGAC/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
wrld <- ne_countries(scale = 50, returnclass = 'sf')

# Load data ---------------------------------------------------------------
shpf <- st_read('datos/gpkg/mpios_rate_ipm_v2.gpkg')

# Weighted ----------------------------------------------------------------
qnwg <- queen_weights(shpf, order = 1, precision_threshold = 0.10)

# Local bimoran -----------------------------------------------------------
bimr <- local_bimoran(w = qnwg, df = st_drop_geometry(shpf[c('rate', 'ipm')]))
lbls <- lisa_labels(bimr)

shpf <- mutate(shpf, cluster_num = lisa_clusters(bimr) + 1, cluster = factor(lbls[cluster_num], levels = lbls))
table(shpf$cluster) %>% as.data.frame() %>% filter(!Var1 %in% c('Not significant', 'Isolated', 'Undefined')) %>% pull(Freq) %>% sum()

nmes <- shpf %>% st_drop_geometry() %>% distinct(cluster) %>% mutate(clase = c('Sin significancia', 'Alto-Alto', 'Bajo-Bajo', 'Bajo-Alto', 'Alto-Bajo', 'Aislados'))
shpf <- inner_join(shpf, nmes, by = 'cluster')

g_bmran <- ggplot() + 
  geom_sf(data = shpf, aes(fill = clase, col = clase)) + 
  scale_fill_manual(values = lbls.clrs) +
  scale_color_manual(values = lbls.clrs, guide = 'none') + 
  geom_sf(data = dpts, fill = NA, col = 'grey60') + 
  geom_sf(data = wrld, fill = NA, col = 'grey60') + 
  ggtitle(label = 'Análisis LISA bivariado entre para tasa de mortalidad por\n1 millón de habitantes e Índice de Pobreza Multidimensional') + 
  labs(x = 'Lon', y = 'Lat', fill = 'Clúster', caption = '') +
  coord_sf(xlim = ext(dpts)[1:2], ylim = ext(dpts)[3:4]) + 
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5, face = 'bold'))

ggsave(plot = g_bmran, filename = 'png/mapa_bimoran_rate_ipm.png', units = 'in', width = 7, height = 10, dpi = 300)

