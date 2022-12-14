

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, hrbrthemes, terra, rgeos, gtools, sf, fs, haven, glue, rgeoda, rnaturalearth, rnaturalearthdata, RColorBrewer, classInt)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)


# Load data ---------------------------------------------------------------
tble <- read_csv('tble/data_v1.csv')
colnames(tble)
dplyr::select(tble, Totalipm)
hist(tble$Totalipm)

shpf <- st_read('SHP/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')

# Histogram ---------------------------------------------------------------
g_hs_tt <- ggplot(data = tble %>% filter(!is.na(year1)), aes(x = Totalipm)) + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~year1) +
  labs(x = 'Índice de pobreza multidimensional', y = 'Frecuencia', caption = 'INS') + 
  ggtitle(label = 'Índice de pobreza multidimensional para mortalidad por COVID-19') +
  theme_ipsum_ps() + 
  theme(axis.text.x = element_text(family = 'serif', size = 8), 
        axis.text.y = element_text(family = 'serif', size = 8), 
        plot.caption = element_text(family = 'serif', size = 7),
        axis.title.x = element_text(family = 'serif', size = 9), 
        strip.text = element_text(family = 'serif', size = 12, face = 'bold'),
        axis.title.y = element_text(family = 'serif', size= 9), 
        plot.title = element_text(family = 'serif', hjust = 0.5, size = 13, face = 'bold'),
        legend.position = 'bottom', 
        legend.title = element_text(face = 'bold', family = 'serif'), 
        legend.text = element_text(family = 'serif'))

dir_create('png/graphs')
ggsave(plot = g_hs_tt, filename = 'png/graphs/histogram_totalipm_year.png', units = 'in', width = 7, height = 5, dpi = 300)

# To make the group by (mpio) ---------------------------------------------
lipm <- dplyr::select(tble, codigo, Totalipm) %>% distinct()
lipm
lipm <- mutate(lipm, codigo = as.character(codigo))

lipm[duplicated(lipm$codigo),]
lipm %>% filter(codigo %in% c(17042, 17541, 68081))

lipm <- lipm[!is.na(lipm$Totalipm),]
nrow(shpf) - nrow(lipm)


gral <- tble %>% 
  group_by(codigo, nombredepartamento, nombremunicipio, year1) %>% 
  dplyr::summarise(count = sum(count)) %>% 
  ungroup()
gral <- mutate(gral, codigo = as.character(codigo))
gral <- inner_join(gral, lipm, by = 'codigo')
gral[which.min(gral$Totalipm),]
gral[which.max(gral$Totalipm),]

# Solve the problem
# Antioquia
shpf <- rbind(shpf %>% filter(DPTO_CNMBR != 'ANTIOQUIA'), 
              shpf %>% filter(DPTO_CNMBR == 'ANTIOQUIA') %>% mutate(MPIO_CCNCT = gsub('^0', '', MPIO_CCNCT)))

# Atlántico
shpf <- rbind(shpf %>% filter(DPTO_CNMBR != 'ATLÁNTICO'),
              shpf %>% filter(DPTO_CNMBR == 'ATLÁNTICO') %>% mutate(MPIO_CCNCT = gsub('^0', '', MPIO_CCNCT)))

# Join Totalipm with shapefile
shpf_lipm <- inner_join(shpf, lipm, by = c('MPIO_CCNCT' = 'codigo'))
anti_join(lipm, shpf_lipm, by = c('codigo' = 'MPIO_CCNCT')) %>% pull(codigo)
pull(lipm, codigo)

# Read shapes
wrld <- ne_countries(scale = 50, returnclass = 'sf')
dpts <- st_read('./SHP/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')

gm_tl <- ggplot() + 
  geom_sf(data = shpf_lipm, aes(fill = Totalipm, col = Totalipm)) +
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) +
  scale_color_gradientn(colors =  brewer.pal(n = 9, name = 'YlOrRd'), guide = 'none') +
  geom_sf(data = dpts, fill = NA, col = 'grey60', lwd = 0.5) + 
  geom_sf(data = wrld, fill = NA, col = 'grey60', lwd = 0.2) + 
  coord_sf(xlim = ext(dpts)[1:2], ylim = ext(dpts)[3:4]) + 
  ggtitle(label = 'Índice de pobreza Multidimensional') + 
  labs(x = 'Lon', y = 'Lat', caption = 'DANE', fill = 'Categoria') +
  theme_minimal() + 
  theme(axis.text.x = element_text(family = 'serif'), 
        axis.text.y = element_text(family = 'serif'), 
        axis.title.x = element_text(family = 'serif'), 
        axis.title.y = element_text(family = 'serif'), 
        plot.title = element_text(family = 'serif', hjust = 0.5, face = 'bold'),
        legend.position = 'bottom', 
        legend.title = element_text(face = 'bold', family = 'serif'), 
        legend.text = element_text(family = 'serif')) 

ggsave(plot = gm_tl, filename = './png/maps/totalipm.png', units = 'in', width = 7, height = 10, dpi = 300)

# To run bimoran ----------------------------------------------------------
shpf_lipm <- dplyr::select(shpf_lipm, MPIO_CCNCT, MPIO_CNMBR, Totalipm, geometry)

gral_20 <- filter(gral, year1 == 2020)
gral_21 <- filter(gral, year1 == 2021)

# 2020 --------------------------------------------------------------------
nrow(shpf_lipm)
nrow(gral_20)

shpf_lipm_20 <- inner_join(shpf_lipm, gral_20, by = c('MPIO_CCNCT' = 'codigo'))
nrow(shpf_lipm_20)

table(is.na(shpf_lipm_20$Totalipm))
table(is.na(shpf_lipm_20$count))

shpf_lipm_20 <- dplyr::select(shpf_lipm_20, MPIO_CCNCT, count, Totalipm)
qnwg_20 <- queen_weights(shpf_lipm_20, order = 1)
morn_20 <- local_bimoran(w = qnwg_20, df = st_drop_geometry(shpf_lipm_20[c('count', 'Totalipm')]))
lbls_20 <- lisa_labels(morn_20)
clrs_20 <- setNames(lisa_colors(morn_20), lbls_20)

shpf_lipm_20 <- mutate(shpf_lipm_20, cluster_num = lisa_clusters(morn_20) + 1, cluster = factor(lbls_20[cluster_num], levels = lbls_20))

lbls <- read_csv('tble/colors_moran.csv')
shpf_lipm_20 <- inner_join(shpf_lipm_20, lbls, byh = c('cluster_num'))
table(shpf_lipm_20$clase)

cor(shpf_lipm_20$count, shpf_lipm_20$Totalipm)

# To make the map ---------------------------------------------------------
shpf_lipm_20 <- mutate(shpf_lipm_20, clase = factor(clase, levels = lbls$clase))
shpf_lipm_20$clase

mran_clrs <- lbls$color
names(mran_clrs) <- lbls$clase

library(ggnewscale)

g_mrn_2020 <- ggplot() + 
  geom_sf(data = shpf_lipm_20, aes(fill = clase, col = clase), lwd = 0.2)+
  scale_fill_manual(values = mran_clrs, name = 'Clase') +
  scale_color_manual(values = mran_clrs, guide = 'none') +
  geom_sf(data = dpts, fill = NA, col = 'grey60', lwd = 0.5) + 
  geom_sf(data = wrld, fill = NA, col = 'grey60', lwd = 0.2) + 
  new_scale_fill() + 
  new_scale_color() +
  coord_sf(xlim = ext(dpts)[1:2], ylim = ext(dpts)[3:4]) + 
  ggtitle(label = 'Análisis LISA bivariado entre fallecidos por COVID - 19 (2020)\ne índice de pobreza multidimensional') + 
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

ggsave(plot = g_mrn_2020, filename = './png/maps/bimorn_2020_totalipm.png', units = 'in', width = 7, height = 9, dpi = 300)

# 2021 --------------------------------------------------------------------
shpf_lipm_21 <- inner_join(shpf_lipm, gral_21, by = c('MPIO_CCNCT' = 'codigo'))
nrow(shpf_lipm_21)

table(is.na(shpf_lipm_21$Totalipm))
table(is.na(shpf_lipm_21$count))

shpf_lipm_21 <- dplyr::select(shpf_lipm_21, MPIO_CCNCT, count, Totalipm)
qnwg_21 <- queen_weights(shpf_lipm_21, order = 1)
morn_21 <- local_bimoran(w = qnwg_21, df = st_drop_geometry(shpf_lipm_21[c('count', 'Totalipm')]))
lbls_21 <- lisa_labels(morn_21)
clrs_21 <- setNames(lisa_colors(morn_21), lbls_21)

shpf_lipm_21 <- mutate(shpf_lipm_21, cluster_num = lisa_clusters(morn_21) + 1, cluster = factor(lbls_21[cluster_num], levels = lbls_21))
shpf_lipm_21 <- inner_join(shpf_lipm_21, lbls, by = c('cluster_num'))
table(shpf_lipm_21$clase)
cor(shpf_lipm_21$count, shpf_lipm_21$Totalipm)

shpf_lipm_21 <- mutate(shpf_lipm_21, clase = factor(clase, levels = lbls$clase))
shpf_lipm_21$clase

g_mrn_2021 <- ggplot() + 
  geom_sf(data = shpf_lipm_21, aes(fill = clase, col = clase), lwd = 0.2)+
  scale_fill_manual(values = mran_clrs, name = 'Clase') +
  scale_color_manual(values = mran_clrs, guide = 'none') +
  geom_sf(data = dpts, fill = NA, col = 'grey60', lwd = 0.5) + 
  geom_sf(data = wrld, fill = NA, col = 'grey60', lwd = 0.2) + 
  new_scale_fill() + 
  new_scale_color() +
  coord_sf(xlim = ext(dpts)[1:2], ylim = ext(dpts)[3:4]) + 
  ggtitle(label = 'Análisis LISA bivariado entre fallecidos por COVID - 19 (2021)\ne índice de pobreza multidimensional') + 
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

ggsave(plot = g_mrn_2021, filename = './png/maps/bimorn_2021_totalipm.png', units = 'in', width = 7, height = 9, dpi = 300)

# To write ----------------------------------------------------------------
dir_create('gpkg/bimoran')
st_write(shpf_lipm_20, 'gpkg/bimoran/bimoran_total_count_2020_totalipm.gpkg')
st_write(shpf_lipm_21, 'gpkg/bimoran/bimoran_total_count_2021_totalipm.gpkg')

