##############################################################################################################
###                               Baixar e organizar dados do speciesLink                            ###        
##############################################################################################################

# Ler pacotes
library(readxl)
library(dplyr)
library(tidyr)
library(biogeo)
library(readr)
library(stringr)

# Os dados de ocorrência foram baixados diratamente do site do speciesLink buscando 40 spp por vez utilizando
# --- fonte: www.splink.org.br
# Próximo passa seria automatizar o script para baixar diretamente as ocorrências do speciesLink, uma possibilidade
# é seria a função 'rspeciesLink' da Sara Mortara (https://github.com/saramortara/rspeciesLink) --- Precisa testar. 

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")

# Colocar as famílias e binômio das espécies em vetores.  
familias <- treespp$final_family
especies <- treespp$nome_especie
