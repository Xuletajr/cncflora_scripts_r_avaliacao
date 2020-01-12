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
