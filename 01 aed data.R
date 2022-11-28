
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

# Agrupacion de los datos -------------------------------------------------
indr <- tble %>% filter(year1 == '2020') %>% distinct(key, nombredepartamento, nombremunicipio, Totalipm, Analfabetismo, Bajologroeducativo, Barrerasaserviciosparacuidad, Barrerasdeaccesoaserviciosd, Desempleodelargaduración, Hacinamientocrítico, Inadecuadaeliminacióndeexcret, Inasistenciaescolar, Materialinadecuadodeparedese, Materialinadecuadodepisos, Rezagoescolar, Sinaccesoafuentedeaguamejo, Sinaseguramientoensalud, Trabajoinfantil, Trabajoinformal)
indr[duplicated(indr$key),]

tble %>%
  dplyr::relocate(key, nombredepartamento, nombremunicipio, year1, pertenenciaétnica, edad, sexo, pdetcat) %>% 
  rename(codigo = key) %>% 
  group_by(codigo, nombredepartamento, nombremunicipio, year1, pertenenciaétnica, sexo, pdetcat) %>% 
  dplyr::summarise(Totalipm = mean(Totalipm), Analfabetismo = mean(Analfabetismo), Bajologroeducativo = mean(Bajologroeducativo),
                   Barrerasaserviciosparacuidad = mean(Barrerasaserviciosparacuidad), Barrerasdeaccesoaserviciosd = mean(Barrerasdeaccesoaserviciosd),
                   )


