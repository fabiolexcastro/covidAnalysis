

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
