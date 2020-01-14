##############################################################################################################
   ###                            Verificar ocorrências em Unidades de Conservação                      ###        
##############################################################################################################

# Ler pacotes
library(rgdal)
library(dplyr)
library(sf)

# Ler shape com as Unidades de Conservação
uc <- rgdal::readOGR(dsn = "./data/shape/UCs_WGS84", layer = "UCs_wgs84", 
              use_iconv = TRUE, encoding = "UTF-8")

st_crs(uc)
proj4string(uc)
uc$NOME_UC1

# Ler a planilha com as espécies de plantas do recorte
treespp <- read.csv("./results/aoo_veg_cites.csv", row.names = 1, 
                    fileEncoding = "UTF-8")

especies <- treespp$nome_especie
familias <- treespp$final_family

# Plotando mapa do Brasil com Unidades de Conservação
estados <- rgdal::readOGR(dsn="./data/shape/Limites_v2017", 
                          layer="lim_unidade_federacao_a")

st_crs(estados)
plot(estados)

plot(uc, add = T, col = "cyan") 
