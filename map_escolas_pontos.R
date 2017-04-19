library(dplyr)
library(rgdal)
library(ggmap)
library(rgeos)
library(viridis)
# library(svglite)

# Importar as bases
dens_sp <- readOGR(dsn = "dados", layer = "SIRGAS_SHP_densidade_demografica_2010") # densidade demográfica por setor censitário
CEU <-  readOGR(dsn = "dados", layer = "SIRGAS_SHP_TEMA_EDUCACAO_CEU") # Localização dos CEUs
EMEFM <- readOGR(dsn = "dados", layer = "SIRGAS_SHP_TEMA_EDUCACAO_REDE_PUBLICA_ENSINO_FUNDAMENTAL-MEDIO") # Localização de EMEFMs
pontos <- readOGR(dsn = "dados", layer = "DEINFO_PONTO_ONIBUS")


# Preparar pontos CEU para ggplot2 / ggmap (converter em data frame)
CEU$id <- row.names(CEU)
CEU2 <- data.frame(CEU)
ggplot() +
  geom_point(data=CEU2, aes(coords.x1, coords.x2), color="blue") +
  coord_equal() + theme_classic()

# Plotar mapa de densidade com localização dos CEUs
ggplot() +
  geom_polygon(data=dens_sp.f,
               aes(long, lat, group = group, fill=fx.habit_hect),
               na.value = "black") +
  geom_point(data=CEU2,
             aes(coords.x1, coords.x2),
             color="red",
             size = 1,
             alpha = 1/2) +
  coord_equal() + theme_classic() +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1)) +
  scale_fill_viridis(option="viridis", discrete = TRUE, na.value = "darkgrey")

# Preparar pontos EMEFMs para ggplot2 / ggmap (converter em data frame)
EMEFM2 <- data.frame(EMEFM)
# Calcular buffer
buffer <- gBuffer(spgeom = EMEFM, width = 1000)
buffer_emefm <- fortify(buffer)

# Plotar mapa de densidade com localização das EMEFMs e buffer
ggplot() +
  geom_polygon(data=dens_sp.f,
               aes(long, lat, group = group, fill=fx.habit_hect),
               na.value = "black") +
  geom_point(data=EMEFM2,
             aes(coords.x1, coords.x2),
             color="red",
             size = .03,
             alpha = 1/1.5) +
  geom_polygon(data=buffer_emefm, aes(long, lat, group = group), colour="red", fill=NA) +
  scale_fill_viridis(option="viridis", discrete = TRUE, na.value = "darkgrey") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1)) +
  coord_equal()
ggsave("map_densidade_sp_emefm_buffer.png", width = 12, height = 14, units = "cm")



# Preparar pontos "pontos" para ggplot2 / ggmap (converter em data frame)
pontos2 <- data.frame(pontos)

# Criar variáveis para legenda
pontos2$pontos <- rep(c('pontos'))
EMEFM2$emefm <- rep(c('emefm'))

# Plotar mapa com localização de "pontos" e EMEFMs
ggplot() +
  theme_classic() +
  geom_point(data=pontos2,
             aes(coords.x1, coords.x2, color = pontos),
             color="green",
             size = .2,
             alpha = 1/2,
             show.legend = TRUE) +
  geom_point(data=EMEFM2,
             aes(coords.x1, coords.x2),
             color="red",
             size = .6,
             alpha = 1/2,
             show.legend = TRUE) +
  coord_equal() +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1))
ggsave("pontos_emefs.png", width = 12, height = 10)
  