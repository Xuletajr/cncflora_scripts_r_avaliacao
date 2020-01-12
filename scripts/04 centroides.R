##############################################################################################################
   ###        Limpezas espaciais e adicionar centróides dos municípios nas coordenadas                  ###        
##############################################################################################################

# Ler pacotes
library(dplyr)
library(stringr)
library(rgbif)
library(readxl)
library(textclean)
library(flora)
library(lubridate)
library(rgdal)
# Função "filt_andrea.R"
source("./functions/filt_andrea.R")

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")
familias <- treespp$final_family
especies <- treespp$nome_especie

# Ler a planilha com as coordenadas dos centróides dos municípios
tabela_centroides <- read.delim(file = "./data/centroide_municipio.csv",
                                header = TRUE, sep = ";",
                                stringsAsFactors = FALSE,
                                fileEncoding = "ISO-8859-9")

tabela_centroides$municipality
tabela_centroides$stateProvince

# Ler a planilha com as coordenadas dos centróides das UCs
tabela_centroides_ucs <- read.delim(file = "./data/centroide_uc.csv",
                                    header = TRUE, sep = ";",
                                    stringsAsFactors = FALSE,
                                    fileEncoding = "ISO-8859-9")

# Deixar todos os nomes das UCs em minúsculo
tabela_centroides_ucs <- tabela_centroides_ucs %>%
   dplyr::mutate(uc = replace_non_ascii(tolower(NOME_UC1)))

tabela_centroides_ucs$uc

# Ler shape dos estados
estados <- rgdal::readOGR(dsn = "./data/shape/Limites_v2017", layer = "lim_unidade_federacao_a",
                          encoding = "UTF-8", use_iconv = TRUE)

estados$nome

# Tabela com a sigla pois uma limpeza é substituir a sigla ("rj") pelo nome completo
sigla_estados <- estados@data[,c("nome", "sigla")] %>%
   data.frame() %>%
   mutate(stateProvince = replace_non_ascii(tolower(nome))) %>%
   mutate(sigla = replace_non_ascii(tolower(sigla)))


# Centroides estados---- já deixa em minusculo sem acento tudo
centroides_estados <- rgeos::gCentroid(estados, byid = T, id = estados$nome) %>%
   data.frame %>%
   tibble::rownames_to_column(var = "estados_shp") %>%
   mutate(stateProvince = replace_non_ascii(tolower(estados_shp))) %>%
   rename(x_estado = x, y_estado = y) %>%
   left_join(sigla_estados)

centroides_estados

# Comparar o nome dos estados e dos municipios porque há municipios com o mesmo nome de alguns estados, importante para a limpeza----
sort(setdiff(centroides_estados$stateProvince, tabela_centroides$municipality))

# Mesmo nome estado e municipio
tabela_centroides[which(tabela_centroides$municipality %in%
                           centroides_estados$stateProvince),]
