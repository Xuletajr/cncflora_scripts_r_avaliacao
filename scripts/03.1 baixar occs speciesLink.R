##############################################################################################################
   ###                               Baixar e organizar dados do speciesLink                            ###        
##############################################################################################################

# Ler pacotes
library(dplyr)
library(stringr)
library(readxl)
library(readr)
library(rgbif)

# Adaptei a função datagbif.R disponível no GitHub de Pablo Hendrigo Alves de Melo:
# Original disponível em: https://github.com/pablopains/MergeSpeciesOccurrence/blob/master/funcoesgbif.R
# Adaptada disponível em: https://github.com/Xuletajr/MergeSpeciesOccurrence/edit/master/funcoesgbif.R
source("./functions/funcoesgbif.R") 