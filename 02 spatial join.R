
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgeos, gtools, sf, fs, haven, glue, rgeoda, rnaturalearth, rnaturalearthdata, RColorBrewer, classInt)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
tble <- read_csv('./tble/data_v1.csv')
head(tble)
shpf <- st_read('./SHP/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')
dpts <- st_read('./SHP/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')

plot(st_geometry(dpts))
plot(st_geometry(shpf))
colnames(shpf)
colnames(tble)

# To make the join --------------------------------------------------------

# 2020 / 2021
tble_2020 <- filter(tble, year1 == 2020)
tble_2021 <- filter(tble, year1 == 2021)

# Categories 
tble_2020 %>% distinct(pertenenciaEtn, edad_class)
tble_2021 %>% distinct(pertenenciaEtn, edad_class)

# Total fallecidos --------------------------------------------------------
ttal_2020 <- tble_2020 %>% group_by(codigo, nombredepartamento, nombremunicipio) %>% summarise(count = sum(count)) %>% ungroup()
ttal_2021 <- tble_2021 %>% group_by(codigo, nombredepartamento, nombremunicipio) %>% summarise(count = sum(count)) %>% ungroup()
ttal_2020 <- ttal_2020 %>% mutate(codigo = as.character(codigo))
ttal_2021 <- ttal_2021 %>% mutate(codigo = as.character(codigo))

# Tidy shapefile ----------------------------------------------------------

# Antioquia
shpf <- rbind(shpf %>% filter(DPTO_CNMBR != 'ANTIOQUIA'), 
              shpf %>% filter(DPTO_CNMBR == 'ANTIOQUIA') %>% mutate(MPIO_CCNCT = gsub('^0', '', MPIO_CCNCT)))

# Atlántico
shpf <- rbind(shpf %>% filter(DPTO_CNMBR != 'ATLÁNTICO'),
              shpf %>% filter(DPTO_CNMBR == 'ATLÁNTICO') %>% mutate(MPIO_CCNCT = gsub('^0', '', MPIO_CCNCT)))


# To make the map ---------------------------------------------------------
shpf_2020 <- inner_join(shpf, ttal_2020, by = c('MPIO_CCNCT' = 'codigo'))
anti_join(ttal_2020, shpf_2020, by = c('codigo' = 'MPIO_CCNCT')) %>% pull(nombredepartamento)

shpf_2021 <- inner_join(shpf, ttal_2021, by = c('MPIO_CCNCT' = 'codigo'))

dir_create('gpkg')
st_write(shpf_2020, 'gpkg/total_mortalidad_2020.gpkg')
st_write(shpf_2021, 'gpkg/total_mortalidad_2021.gpkg')

# Class intervals 
clss_2020 <- classInt::classIntervals(var = pull(shpf_2020, count), n = 6, style = 'jenks')
clss_2020 <- clss_2020$brks
clss_2020 <- c(clss_2020[1], round(clss_2020[2:7], -1))

shpf_2020 <- mutate(shpf_2020, count_class = findInterval(x = count, vec = clss_2020, all.inside = T)) 
clss_2020 <- tibble(value = 1:6, inf = clss_2020[1:6], sup = clss_2020[2:7], interval = glue('{inf}-{sup}'))
shpf_2020 <- inner_join(shpf_2020, clss_2020, by = c('count_class' = 'value'))
shpf_2020 <- mutate(shpf_2020, interval = factor(interval, levels = clss_2020$interval))
glimpse(shpf_2020)

# Making the map
wrld <- ne_countries(scale = 50, returnclass = 'sf')

g_mrt_2020 <- ggplot() + 
  geom_sf(data = shpf_2020, aes(fill = interval, col = interval), lwd = 0.2)+
  scale_fill_manual(values = brewer.pal(n = 6, name = 'YlOrRd')) +
  scale_color_manual(values = brewer.pal(n = 6, name = 'YlOrRd'), guide = 'none') +
  geom_sf(data = dpts, fill = NA, col = 'grey60', lwd = 0.5) + 
  geom_sf(data = wrld, fill = NA, col = 'grey60', lwd = 0.2) + 
  coord_sf(xlim = ext(dpts)[1:2], ylim = ext(dpts)[3:4]) + 
  ggtitle(label = 'Mortalidad por COVID-19 en Colombia - Año 2020') + 
  labs(x = 'Lon', y = 'Lat', caption = 'INS - 2020', fill = 'Cantidad fallecidos') +
  theme_minimal() + 
  theme(axis.text.x = element_text(family = 'serif'), 
        axis.text.y = element_text(family = 'serif'), 
        axis.title.x = element_text(family = 'serif'), 
        axis.title.y = element_text(family = 'serif'), 
        plot.title = element_text(family = 'serif', hjust = 0.5, face = 'bold'),
        legend.position = 'bottom', 
        legend.title = element_text(face = 'bold', family = 'serif'), 
        legend.text = element_text(family = 'serif'))

dir_create('./png/maps')
ggsave(plot = g_mrt_2020, filename = './png/maps/mort_2020.png', units = 'in', width = 7, height = 9, dpi = 300)

# 2021 -------------------------------------------------
# Class intervals
clss_2021 <- clss_2020
summary(shpf_2021$count)




summary(shpf_2021$count)
