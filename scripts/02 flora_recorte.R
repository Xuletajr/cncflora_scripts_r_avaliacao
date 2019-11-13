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

# Ler os dados com as espécies que será trabalhadas no recorte----
# A planilha precisa ter ao menos uma coluna denominada "scientificName" que contenha o nome científico + autoria das espécies
treespp <- read_csv("./results/plantas_selecionadas_2019_bloco3.csv") %>%
   dplyr::select(-1) %>%
   # Função do pacote "flora" que retira autoria do nome das espécies. Para manter certa padronização seria importante
   # sempre que entrar uma planilha a coluna com o binômio das espécies seja gerada com essa função. 
   mutate(nome_especie = purrr::map(scientificName, ~remove.authors(.)) %>%
             simplify2array()) 
