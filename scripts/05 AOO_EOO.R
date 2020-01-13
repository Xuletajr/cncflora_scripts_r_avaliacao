##############################################################################################################
   ###                                        Estimando EOO e AOO                                       ###        
##############################################################################################################

# Ler pacotes
library(readxl)
library(textclean)
library(dplyr)
library(stringr)
library(flora)
library(red)
library(ConR)
library(rgdal)

# Plotando mapa do Brasil e Américas.
a <- maps::map(,c("Brazil","Mexico"))
a$range
b <- maps::map(add = T)
b$range

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")

especies <- treespp$nome_especie
familias <- treespp$final_family
