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

# Ler a planilha com as espécies de plantas
treespp <- readr::read_csv("./results/names_flora.csv") #%>% dplyr::select(-1)

# Checando o número de espécies
unique(treespp$nome_especie)

# Colocar as famílias, binômio das espécies e nome científico em vetores.  
familias <- treespp$final_family
especies <- treespp$nome_especie
scientificName <- treespp$scientificName

# Conferindo no GBIF quais espécies espécies que apresentam 'usageKey' e 'speciesKey' diferentes, são os casos
# que o 'speciesKey' baixou ocorrências do sistema e não das espécies de interesse. 
gbif_name_backbone <- matrix(data = NA, nrow = 508, ncol = 7)
colnames(gbif_name_backbone) <- c("scientificName", "scientificName_gbif", "species_gbif",
                                  "usageKey", "sepciesKey", "rank", "status")

for(i in 1:length(especies)) {
   
   print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
   
   speciesKey <- rgbif::name_backbone(name=treespp$scientificName[i])
   
   gbif_name_backbone[i,1] <- scientificName[i]
   gbif_name_backbone[i,2] <- speciesKey$scientificName
   gbif_name_backbone[i,3] <- speciesKey$species
   gbif_name_backbone[i,4] <- speciesKey$usageKey
   gbif_name_backbone[i,5] <- speciesKey$speciesKey
   gbif_name_backbone[i,6] <- speciesKey$rank
   gbif_name_backbone[i,7] <- speciesKey$status
}

write.csv(gbif_name_backbone, "./results/gbif_name_backbone.csv", na="",  fileEncoding = "UTF-8")
