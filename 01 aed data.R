
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rgeos, gtools, sf, fs, haven)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
tble <- read_dta('./dta/bdunidapdet.dta')
rawt <- tble

# Filtramos eliminando 2022 -----------------------------------------------
tble <- filter(tble, year1 != '2022')
unique(tble$MPIOPDET); unique(tble$pdetcat)

# Selecting the main variables --------------------------------------------
vars <- c('key', 'mortalidad', 'edad', 'Totalipm', 'year1', 'Analfabetismo', 'Bajologroeducativo', 'Barrerasaserviciosparacuidad', 'Barrerasdeaccesoaserviciosd', 'Desempleodelargaduración', 'Hacinamientocrítico',  'Inadecuadaeliminacióndeexcret', 'Inasistenciaescolar', 'Materialinadecuadodeparedese', 'Materialinadecuadodepisos', 'Rezagoescolar', 'Sinaccesoafuentedeaguamejo', 'Sinaseguramientoensalud', 'Trabajoinfantil', 'Trabajoinformal', 'pertenenciaétnica', 'sexo', 'pdetcat', 'nombredepartamento', 'nombremunicipio') 
sort(colnames(tble))

tble <- dplyr::select(tble, vars) # Selecionamos variables de interés
tble <- relocate(tble, key, nombredepartamento, nombremunicipio, year1)

# Pertenencia etnica ------------------------------------------------------
etnc <- tibble(pertenenciaetnica = c(1, 2, 3, 4, 5, 6), pertenenciaEtn = c('Indigena', 'ROM', 'Raizal', 'Palenquero', 'Negro', 'Otro'))

# Edades ------------------------------------------------------------------
edad <- tibble(edad = c(0:18, 19:64, 65:120), edad_class = c(rep('Niñez', length(0:18)), rep('Adultez', length(19:64)), rep('Vejez', length(65:120)) ))
tble <- inner_join(tble, edad, by = c('edad' = 'edad'))
tble <- relocate(tble, edad_class, .after = edad)

# Agrupacion de los datos -------------------------------------------------
indr <- tble %>% filter(year1 == '2020') %>% distinct(key, nombredepartamento, nombremunicipio, Totalipm, Analfabetismo, Bajologroeducativo, Barrerasaserviciosparacuidad, Barrerasdeaccesoaserviciosd, Desempleodelargaduración, Hacinamientocrítico, Inadecuadaeliminacióndeexcret, Inasistenciaescolar, Materialinadecuadodeparedese, Materialinadecuadodepisos, Rezagoescolar, Sinaccesoafuentedeaguamejo, Sinaseguramientoensalud, Trabajoinfantil, Trabajoinformal)
indr[duplicated(indr$key),]

# Pertenencia etnica ------------------------------------------------------
tble <- inner_join(tble, etnc, by = c('pertenenciaétnica' = 'pertenenciaetnica'))
tble <- relocate(tble, pertenenciaEtn, .after = pertenenciaétnica)

smmr <- tble %>%
  dplyr::relocate(key, nombredepartamento, nombremunicipio, year1, pertenenciaEtn, edad, sexo, pdetcat) %>% 
  rename(codigo = key) %>% 
  group_by(codigo, nombredepartamento, nombremunicipio, year1, pertenenciaEtn, sexo, edad_class, pdetcat) %>% 
  dplyr::summarise(count = n()) %>% 
  ungroup() 

# To join both tables into only one ---------------------------------------
fnal <- full_join(smmr, indr, by = c('codigo' = 'key', 'nombredepartamento' = 'nombredepartamento', 'nombremunicipio' = 'nombremunicipio'))

dir_create('./tble')
write.csv(fnal, './tble/data_v1.csv', row.names = FALSE)
