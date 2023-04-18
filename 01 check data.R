
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rgeos, gtools, readxl, sf, fs, haven)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
mpio <- st_read('G:/D/data/IGAC/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')
ctgr <- read_excel('datos/tbl/categoria municipios.xlsx')
colnames(ctgr) <- c('cod', 'categoria')

# Join between the shapefile and the table --------------------------------
mpio_ctgr <- inner_join(mpio, ctgr, by = c('MPIO_CCNCT' = 'cod'))
mpio_ctgr
dir_create('datos/gpkg')
st_write(mpio_ctgr, 'datos/gpkg/mpios_categoria.gpkg')

# Tasa --------------------------------------------------------------------
excel_sheets('datos/tbl/TASA MORTALIDAD MUNICIPIO.xlsx')
rate <- read_excel('datos/tbl/TASA MORTALIDAD MUNICIPIO.xlsx', sheet = 'Hoja2')

View(rate)
rate <- mutate(rate, divi = as.numeric(DIVIPOLA), rate = as.numeric(TASAMORTALIDAD))
rate[!complete.cases(rate),]
rate <- dplyr::select(rate, -NOMBREMPIO)
rate <- drop_na(rate)
write.csv(rate, 'datos/tbl/rate.csv', row.names = FALSE)

rate <- mutate(rate, DIVIPOLA = as.character(DIVIPOLA))
rate <- dplyr::select(rate, DIVIPOLA, rate)

# Tidy Antioquia y Atlántico ----------------------------------------------
rate %>% pull(DIVIPOLA) %>% as.numeric() %>% sort()
mpio %>% pull(MPIO_CCNCT) %>% as.numeric() %>% sort()

rate_ant <- rate[grep('^5', rate$DIVIPOLA, value = FALSE),] %>% mutate(nchar = nchar(DIVIPOLA)) %>% filter(nchar == 4)
mpio_ant <- mpio %>% filter(DPTO_CNMBR == 'ANTIOQUIA')

aqui <- filter(mpio, DPTO_CNMBR == 'ANTIOQUIA') %>% mutate(MPIO_CCNCT = gsub('^0', '', MPIO_CCNCT))
atln <- filter(mpio, DPTO_CNMBR == 'ATLÁNTICO') %>% mutate(MPIO_CCNCT = gsub('^0', '', MPIO_CCNCT))
mpio <- rbind(filter(mpio, !DPTO_CNMBR %in% c('ANTIOQUIA', 'ATLÁNTICO')), aqui, atln)

# Join  -------------------------------------------------------------------
mpio
nrow(mpio_rate) - nrow(rate)
mpio_rate <- inner_join(mpio, rate, by = c('MPIO_CCNCT' = 'DIVIPOLA'))
nrow(mpio_rate) - nrow(rate)
miss <- anti_join(rate, as_tibble(st_drop_geometry(mpio_rate)), by = c('DIVIPOLA' = 'MPIO_CCNCT'))
miss %>% pull(DIVIPOLA)
st_write(mpio_rate, 'datos/gpkg/mpios_rate_v1.gpkg')

mpio_rate

# To add the IPM ----------------------------------------------------------
ipm <- read_excel('datos/tbl/IPM2018TOTAL.xlsx')
colnames(ipm) <- c('dptoo', 'MPIO_CCNCT', 'mpioo', 'ipm')
ipm <- mutate(ipm, MPIO_CCNCT = as.character(MPIO_CCNCT))

# Join IPM with mpio rate  ------------------------------------------------
mpio_rate
mpio_rate_2 <- inner_join(mpio_rate, ipm, by = 'MPIO_CCNCT')

miss <- anti_join(ipm, mpio_rate, by = 'MPIO_CCNCT')
mpio_rate %>% filter(MPIO_CCNCT %in% pull(miss, MPIO_CCNCT))

st_write(mpio_rate_2, 'datos/gpkg/mpios_rate_ipm_v1.gpkg')





