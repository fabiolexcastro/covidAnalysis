
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
