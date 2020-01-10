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
# o binômio das espécies  ---- fonte: www.splink.org.br
# Próximo passa seria automatizar o script para baixar diretamente as ocorrências do speciesLink, uma possibilidade
# é seria a função 'rspeciesLink' da Sara Mortara (https://github.com/saramortara/rspeciesLink) --- Precisa testar. 

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")

# Colocar as famílias e binômio das espécies em vetores.  
familias <- treespp$final_family
especies <- treespp$nome_especie

# Ler a tabela com todas as ocorrências provenientes do speciesLink
# Estas ocorrências foram baixadas em planilhas separadas e juntadas em uma planilha só previamente
speciesLink <- read_csv("./results/speciesLink_geral.csv",
                        locale = locale(encoding = "UTF-8"))

# Filtra somente os nomes das espécies iguais aos da nossa lista (alguns nomes vêm desconfigurados)
speciesLink2 <- speciesLink %>% filter(scientificname %in% especies) %>%
   dplyr::mutate(nome_especie = scientificname) %>% 
   dplyr::rename(notas  = notes) %>% 
   dplyr::left_join(treespp) %>%
   dplyr::mutate(acceptedNameUsage = scientificName) %>%
   dplyr::mutate(scientificNameAuthorship = word(acceptedNameUsage, 3, -1, sep = " "))

# Conferindo algumas informações da planilha
names(speciesLink2) # variáveis disponíveis na planilha
speciesLink2$scientificNameAuthorship # Se os nomes vieram escritos corretamente, dependendo do "enconding" pode desconfigurar
speciesLink2 %>% count(is.na(scientificNameAuthorship)) # Quantos nomes vieram sem autor (i.e., NA's)

# Deixando as planilhas de ocorrência no formato utilizado pelo CNCFlora
speciesLink3 <- speciesLink2 %>%
   dplyr::mutate(modified = "", identificationQualifier = "", infraspecificEpithet = "", 
          typeStatus = "", fieldNumber = "", occurrenceID = "", occurrenceRemarks = "",
          bibliographicCitation = "speciesLink",
          comments = "") %>%
   dplyr::select(modified, institutioncode, collectioncode, catalognumber,
          nome_especie, identificationQualifier, final_family, genus, species,
          infraspecificEpithet, scientificNameAuthorship, identifiedby, yearidentified,
          typeStatus, collectornumber, fieldNumber, collector, yearcollected,
          monthcollected, daycollected, country, stateprovince, county,
          locality, latitude, longitude, notas, acceptedNameUsage,
          occurrenceID, comments, bibliographicCitation)  %>%
   dplyr::rename(institutionCode = institutioncode, collectionCode = collectioncode, 
          catalogNumber = catalognumber, scientificName = nome_especie, family = final_family, 
          specificEpithet = species, identifiedBy = identifiedby, dateIdentified = yearidentified, 
          recordNumber = collectornumber, recordedBy = collector, year = yearcollected, 
          month = monthcollected, day = daycollected, stateProvince = stateprovince,
          municipality = county, decimalLatitude = latitude, decimalLongitude = longitude,
          occurrenceRemarks = notas)

# Exportar uma planilha de ocorrências do speciesLink geral formatada 
write.csv(speciesLink3, "./results/speciesLink_geral_formatada.csv")

# Exportar as ocorrências de cada espécie em uma planilha separada
for (i in 1:length(especies)){
   nome_arquivo <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "speciesLink raw.csv")
   print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
   
   if (!file.exists(nome_arquivo)){
      occs <- list()
      occs[[i]] <- speciesLink3 %>% filter(scientificName %in% especies[i])
      
      print(lapply(occs,dim))
      if (any(!is.null(lapply(occs,dim)))){
         dim.null <- lapply(occs, function(x) {!is.null(dim(x))})
         occs.f <- subset(occs, dim.null == T)
         occs.f <- dplyr::bind_rows(occs.f)
         print(dim(occs.f))
         write.csv(occs.f, nome_arquivo, na = "", row.names = F, fileEncoding = "UTF-8")
      }
      
   } else {
      warning(paste("Species was not found in the speciesLink dataset", especies[i], "\n"))
   
      }
}

######   end----