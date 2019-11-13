##############################################################################################################
      ###          Cruzar informações do recorte com o Projeto Flora do Brasil 2020 (IPT)          ###        
##############################################################################################################

# Ler pacotes
library(readr)
library(readxl)
library(dplyr)
library(flora)

# Ler dados FB2020 gerado pelos script 01----
flora <- read_csv("./ipt/all_flora.csv", locale = locale(encoding = "UTF-8")) %>% 
   dplyr::select(-1)

head(flora)
names(flora)
