
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgeos, cowplot, ggpubr, gtools, readxl, rnaturalearthdata, rnaturalearth, sf, fs, haven, RColorBrewer, ggspatial, colourpicker)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
shpf <- st_read('datos/gpkg/mpios_rate_ipm_v1.gpkg')
shpf <- dplyr::select(shpf, MPIO_CCNCT, DPTO_CNMBR, MPIO_CNMBR)

tble <- read_csv('datos/tbl/clean_v1.csv')
tble <- mutate(tble, MPIO_CCNCT = as.character(MPIO_CCNCT))

# Administrative data -----------------------------------------------------
dpto <- st_read('G:/D/data/IGAC/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
wrld <- ne_countries(scale = 50, returnclass = 'sf')

# Join  -------------------------------------------------------------------
rslt <- inner_join(shpf, tble, by = c('MPIO_CCNCT', 'DPTO_CNMBR', 'MPIO_CNMBR'))

st_write(rslt, 'datos/gpkg/mpios_rate_ipm_v2.gpkg')

# Rate --------------------------------------------------------------------
hist(rslt$rate)
summary(rslt$rate)
boxplot(rslt$rate)

g_rate <- ggplot() +
  geom_sf(data = rslt, aes(fill = rate, col = rate)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) +
  scale_color_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd'), guide = 'none') +
  geom_sf(data = dpto, fill = NA, col = 'grey50') + 
  geom_sf(data = wrld, fill = NA, col = 'grey60', lwd = 0.2) + 
  labs(x = 'Lon', y = 'Lat', fill = 'Tasa de mortalidad por\n1 millón de hab', caption = 'INS, 2023') +
  ggtitle(label = 'Tasa de mortalidad - Hasta marzo 2023') +
  coord_sf(xlim = ext(dpto)[1:2], ylim = ext(dpto)[3:4]) + 
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(3, 'line'), 
        legend.key.height = unit(0.8, 'line'), 
        plot.title = element_text(hjust = 0.5, face = 'bold'))

ggsave(plot = g_rate, filename = 'png/mapa_rate.png', units = 'in', width = 7, height = 10, dpi = 300)

# IPM ---------------------------------------------------------------------

hist(rslt$ipm)
summary(rslt$rate)
boxplot(rslt$rate)

rslt %>% filter(ipm < 5)

g_ipm <- ggplot() +
  geom_sf(data = rslt, aes(fill = ipm, col = ipm)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) +
  scale_color_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd'), guide = 'none') +
  geom_sf(data = dpto, fill = NA, col = 'grey50') + 
  geom_sf(data = wrld, fill = NA, col = 'grey60', lwd = 0.2) + 
  labs(x = 'Lon', y = 'Lat', fill = 'Índice de Pobreza\nMultidimensional', caption = 'DANE, 2018') +
  ggtitle(label = 'Índice de Pobreza Multidimensional') +
  coord_sf(xlim = ext(dpto)[1:2], ylim = ext(dpto)[3:4]) + 
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(3, 'line'), 
        legend.key.height = unit(0.8, 'line'), 
        plot.title = element_text(hjust = 0.5, face = 'bold'))

ggsave(plot = g_ipm, filename = 'png/mapa_ipm.png', units = 'in', width = 7, height = 10, dpi = 300)

# Join both 
g_both <- ggarrange(g_rate, g_ipm, ncol = 2, nrow = 1)
ggsave(plot = g_both, filename = 'png/mapa_rate_ipm.png', units = 'in', width = 15, height = 9, dpi = 300)
