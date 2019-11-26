##############################################################################################################
   ###                               Baixar e organizar dados do speciesLink                            ###        
##############################################################################################################

# Ler pacotes
library(dplyr)
library(stringr)
library(readr)
library(rgbif)

# Adaptei a função datagbif.R disponível no GitHub de Pablo Hendrigo Alves de Melo:
# Original disponível em: https://github.com/pablopains/MergeSpeciesOccurrence/blob/master/funcoesgbif.R
# Adaptada disponível em: https://github.com/Xuletajr/MergeSpeciesOccurrence/edit/master/funcoesgbif.R
source("./functions/funcoesgbif.R") 

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")

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

write.csv(gbif_name_backbone, "./results/gbif_name_backbone.csv", na = "",  fileEncoding = "UTF-8")

# Cria uma pasta para colocar os dados de ocorrência que serão baixados 
dir.create("output_final")

# Buscar registros no gbif---- Usando o script Andrea juntamente com do Pablo Hendrigo
for (i in 1:length(especies)) {
   dir.create(paste0("./output_final/",familias[i]), showWarnings = F)
   nome_arquivo <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "gbif raw.csv")
   print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
   
   if (!file.exists(nome_arquivo)) {
      # Era utilizado 'speciesKey', mas por muitas vezes retornava os sinônimos. Mudamos para 'usageKey' 
      # que pareceu mais consistente com a espécie que temos interesse. Parece que o Bachman também utiliza 'usageKey'.
      key <- name_backbone(name = treespp$scientificName[i])$usageKey
      if (!is.null(key)) {
         occs <- list()
         
         # Função do Pablo. 
         occs[[i]] <- datagbif(sp_search = treespp$scientificName[i], 
                               remove.badissues = FALSE, limite = 0) %>%
            # A data de identificação vem com mais informações que utilizado pelo CNCFlora, só precisa ano.
            dplyr::mutate(dateIdentified = lubridate::year(dateIdentified)) %>%
            # Peguei o nome aceito e autoria da planilha com informações cruzadas com FB2020.
            dplyr::mutate(acceptedNameUsage = treespp$scientificName[i]) %>%
            dplyr::mutate(scientificNameAuthorship = word(acceptedNameUsage, 3, -1, sep = " ")) %>%
            # Utilizei o gênero e epíteto vindo da planilha com informações cruzadas com FB2020.
            dplyr::mutate(genus = word(especies[i], 1)) %>%
            dplyr::mutate(specificEpithet = word(especies[i], 2)) %>%
            # Adicionar uma coluna especificando de onde veio os dados da ocorrência. 
            dplyr::mutate(bibliographicCitation = "GBIF") %>% 
            dplyr::mutate(scientificName = especies[i]) %>%
            # O CNCFlora utiliza essas variáveis em branco para entrar no sistema
            dplyr::mutate(modified = "", identificationQualifier = "", infraspecificEpithet = "", 
                          typeStatus = "", fieldNumber = "", occurrenceID = "")
         
         print(lapply(occs,dim))
         if (any(!is.null(lapply(occs,dim)))) {
            dim.null <- lapply(occs, function(x) {!is.null(dim(x))})
            occs.f <- subset(occs, dim.null == T)
            occs.f <- dplyr::bind_rows(occs.f)
            print(dim(occs.f))
            write.csv(occs.f, nome_arquivo, na = "", row.names = F, fileEncoding = "UTF-8")
         }
      } else {
         
         warning(paste("No key found for", especies[i], "\n"))
      
      }
   } 
}

######   end----