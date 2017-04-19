library(dplyr)
library(rgdal)
library(ggmap)
library(rgeos)
library(viridis)
# library(svglite)

# Importar as bases
dens_sp <- readOGR(dsn = "dados", layer = "SIRGAS_SHP_densidade_demografica_2010") # densidade demográfica por setor censitário

# Computar variável de faixas de densidade habitacional em dens_sp
quantile(dens_sp@data$habit_hect, prob = seq(0,1, length = 9), na.rm = TRUE)
dens_sp@data$fx.habit_hect <- factor(cut(dens_sp@data$habit_hect,
                                         breaks=c(0,65.255560,106.287859,139.392596,173.381772,217.368452,294.968068,517.648063,30345.556016),
                                         labels = c('0-65',
                                                    '65-106',
                                                    '106-139',
                                                    '139-173',
                                                    '173-217',
                                                    '217-295',
                                                    '295-518',
                                                    '518-30345')))

# Preparar polígonos dens_sp para ggplot2 / ggmap (com fortify)
dens_sp@data <- rename(dens_sp@data, ID = id)
dens_sp$id <- row.names(dens_sp)
dens_sp.f <- fortify(dens_sp)
dens_sp.f <- left_join(dens_sp.f, dens_sp@data) # Unir atributos não geográficos
dens_sp.f$fx.habit_hect <- factor(dens_sp.f$fx.habit_hect)

ggplot(dens_sp.f, aes(long, lat, group = group, fill = fx.habit_hect), na.value = "grey") +
  geom_polygon() + coord_equal() + theme_classic() +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1)) +
  scale_fill_viridis(option="inferno", discrete = TRUE, na.value = "grey")
# ggsave("densindade_map_inferno.png", width = 10, height = 11 , units = "cm" )

ggplot(dens_sp.f, aes(long, lat, group = group, fill = fx.habit_hect), na.value = "grey") +
  geom_polygon() + coord_equal() + theme_classic() +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1)) +
  scale_fill_viridis(option="plasma", discrete = TRUE, na.value = "grey")
# ggsave("densindade_map_plasma.png", width = 10, height = 11 , units = "cm" )

ggplot(dens_sp.f, aes(long, lat, group = group, fill = fx.habit_hect), na.value = "grey") +
  geom_polygon() + coord_equal() + theme_classic() +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1)) +
  scale_fill_viridis(option="magma", discrete = TRUE, na.value = "grey")
# ggsave("densindade_map_magma.png", width = 10, height = 11 , units = "cm" )

ggplot(dens_sp.f, aes(long, lat, group = group, fill = fx.habit_hect), na.value = "grey") +
  geom_polygon() + coord_equal() + theme_classic() +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1)) +
  scale_fill_viridis(option="viridis", discrete = TRUE, na.value = "grey")
# ggsave("densindade_map_viridis.svg", width = 10, height = 11 , units = "cm")