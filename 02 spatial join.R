
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rgeos, gtools, sf, fs, haven, rgeoda)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
tble <- read_csv('./tble/data_v1.csv')
head(tble)
shpf <- st_read('./SHP/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')
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


# To make the map ---------------------------------------------------------
shpf_2020 <- inner_join(shpf, ttal_2020, by = c('MPIO_CCNCT' = 'codigo'))
anti_join(ttal_2020, shpf_2020, by = c('codigo' = 'MPIO_CCNCT')) %>% pull(nombredepartamento)

ttal_2020 %>% filter(nombredepartamento == 'ANTIOQUIA')
shpf %>% filter(DPTO_CNMBR == 'ANTIOQUIA')

