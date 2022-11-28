
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rgeos, gtools, sf, fs, haven)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
tble <- read_dta('./dta/bdunidapdet.dta')

head(tble)
dim(tble)
View(tble)

unique(tble$MPIOPDET)
unique(tble$pdetcat)

# Selecting the main variables --------------------------------------------
vars <- c('key', 'mortalidad', 'Totalipm', 'year1', 'Analfabetismo', 'Bajologroeducativo', 'Barrerasaserviciosparacuidad', 'Barrerasdeaccesoaserviciosd', 'Desempleodelargaduración', 'Hacinamientocrítico', 'Materialinadecuadodeparedese', 'Materialinadecuadodepisos', 'Rezagoescolar', 'Sinaccesoafuentedeaguamejo', 'Sinaseguramientoensalud', 'Trabajoinfantil', 'Trabajoinformal', 'pertenenciaétnica', 'sexo', 'pdetcat', 'nombredepartamento', 'nombremunicipio') 
sort(colnames(tble))

tble <- dplyr::select(tble, all_of(vars)) # Selecionamos variables de interés
tble <- relocate(tble, key, nombredepartamento, nombremunicipio, year1)
head(tble, 4)

# Filtramos eliminando 2022 -----------------------------------------------
tble <- filter(tble, year1 != '2021')

# Agrupacion de los datos -------------------------------------------------
tble



