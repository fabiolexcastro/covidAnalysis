
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, terra, rgeos, gtools, sf, fs, haven, glue, rgeoda, rnaturalearth, rnaturalearthdata, RColorBrewer, classInt)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
s_20 <- st_read('gpkg/total_mortalidad_2020.gpkg')
s_21 <- st_read('gpkg/total_mortalidad_2021.gpkg')

# To calculate the weigths ------------------------------------------------
q_20 <- queen_weights(s_20, order = 1)
q_21 <- queen_weights(s_21, order = 1)

# Moran 2020 --------------------------------------------------------------
m_20 <- local_moran(q_20, st_drop_geometry(s_20['count']))
mran_lbls <- lisa_labels(m_20)
mran_clrs <- setNames(lisa_colors(m_20), mran_lbls)

s_20 <- mutate(s_20, cluster_num = lisa_clusters(m_20) + 1,
                     cluster = factor(mran_lbls[cluster_num], levels = mran_lbls))

ggplot(s_20, aes(fill = cluster)) +
  geom_sf(color = "white", size = 0) +
  scale_fill_manual(values = mran_clrs, na.value = "green") +
  theme_dark()
