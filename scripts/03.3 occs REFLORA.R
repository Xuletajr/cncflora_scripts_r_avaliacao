##############################################################################################################
   ###                                Baixar e organizar dados do REFLORA                               ###        
##############################################################################################################

# Ler pacotes
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(biogeo)
library(readr)
library(flora)

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")

# Colocar as famílias e binômio das espécies em vetores.  
familias <- treespp$final_family
especies <- treespp$nome_especie

# Ler as planilhas com dados reflora - Os dados foram baixados com script do Vicente no NODE (disponível no Bitbucket dele)
# Próximo passa seria automatizar o script para baixar diretamente as ocorrências do REFLORA
reflora_1 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo1.xlsx", sheet = 1)
reflora_2 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo2.xlsx", sheet = 1)
reflora_3 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo3.xlsx", sheet = 1)
reflora_4 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo4.xlsx", sheet = 1)
reflora_5 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo5.xlsx", sheet = 1)
reflora_6 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo6.xlsx", sheet = 1)
reflora_7 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo7.xlsx", sheet = 1)
reflora_8 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo8.xlsx", sheet = 1)
reflora_9 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo9.xlsx", sheet = 1)

# Juntando as planilhas
reflora_all <- dplyr::bind_rows(reflora_1, reflora_2, reflora_3, reflora_4,
                         reflora_5, reflora_6, reflora_7, reflora_8, reflora_9)

# Exportar uma planilha csv com todos os dados do reflora juntos
write.csv(reflora_all, "./results/reflora_all.csv", fileEncoding = "UTF-8")

# Juntando o gênero e epíteto específico em uma coluna
# O código que fiz abaixo para separar as datas só funciona se ler a planilha toda novamente. 
reflora_all <- read.csv("./results/reflora_all.csv", fileEncoding = "UTF-8") %>% 
   select(-1) %>% 
   dplyr::mutate(Gênero = str_to_title(Gênero)) %>%
   dplyr::mutate(Espécie = str_to_lower(Espécie)) %>%
   unite("nome_especie", Gênero, Espécie, remove = FALSE, sep = " ") 

# Juntando a planilha de ocorrências REFLORA com a planilha com informações do FLORA
reflora_all2 <- left_join(reflora_all, treespp)

# Conferindo algumas informações da planilha
names(reflora_all2) # variáveis disponíveis na planilha
unique(reflora_all2$occurrenceRemarks)
unique(reflora_all2$scientificName)
reflora_all2 %>% count(is.na(scientificName)) # Quantos nomes vieram sem autor (i.e., NA's)

# Ajustar algumas informações da planilha para ficar no formato usado pelo CNCFlora
# 
reflora_all3 <- reflora_all2 %>%  
   dplyr::mutate(year =  lubridate::year(dmy(De.)))  %>%   # Formatando data coleta - ano
   dplyr::mutate(month = lubridate::month(dmy(De.))) %>%   # Formatando data coleta - mês
   dplyr::mutate(day =   lubridate::day(dmy(De.)))   %>%   # Formatando data coleta - dia
   dplyr::mutate(bibliographicCitation = "REFLORA")    %>%
   dplyr::mutate(dateIdentified = lubridate::year(dmy(Data.da.Determinação))) %>%
   tidyr::unite("occurrenceRemarks", Descrição.da.Planta, Observações, remove = FALSE, sep = "|") %>%
   dplyr::mutate(modified = "", institutionCode = "", identificationQualifier = "", 
                 occurrenceID = "", infraspecificEpithet = "", typeStatus = "",
                 fieldNumber = "",  acceptedNameUsage = "", comments = "") 

unique(reflora_all3$year)
unique(reflora_all3$occurrenceRemarks)

# Mudando a latitude e longitude de GMS para decimal
dd_Lat <- mm_Lat <- ss_Lat <- ns_Lat <- NA

for(i in 1:nrow(reflora_all3)){
   dd_Lat[i] <- parse_number(str_split_fixed(reflora_all3$'Latitude.Mínima'[i], " ", 3))[1]
   mm_Lat[i] <- parse_number(str_split_fixed(reflora_all3$'Latitude.Mínima'[i], " ", 3))[2]
   ss_Lat[i] <- parse_number(str_split_fixed(reflora_all3$'Latitude.Mínima'[i], " ", 3))[3]
   ns_Lat[i] <- str_extract(reflora_all3$'Latitude.Mínima'[i], "[a-zA-Z]")
}

unique(ns_Lat)

dd_Long <- mm_Long <- ss_Long <- ns_Long <- NA

for(i in 1:nrow(reflora_all3)){
   dd_Long[i] <- parse_number(str_split_fixed(reflora_all3$'Longitude.Mínima'[i], " ", 3))[1]
   mm_Long[i] <- parse_number(str_split_fixed(reflora_all3$'Longitude.Mínima'[i], " ", 3))[2]
   ss_Long[i] <- parse_number(str_split_fixed(reflora_all3$'Longitude.Mínima'[i], " ", 3))[3]
   ns_Long[i] <- str_extract(reflora_all3$'Longitude.Mínima'[i], "[a-zA-Z]")
}

unique(ns_Long)

reflora_all4 <- reflora_all3 %>% 
   mutate(decimalLatitude = round(dms2dd(dd_Lat, mm_Lat, ss_Lat, ns_Lat), digits = 6)) %>%
   mutate(decimalLongitude = round(dms2dd(dd_Long, mm_Long, ss_Long, ns_Long), digits = 6))

reflora_all4$decimalLatitude
reflora_all4 %>% select(scientificName, Latitude.Mínima, Longitude.Mínima, decimalLatitude, decimalLongitude)  %>%
   write.csv("./results/reflora_all4.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

# Preparando os dados do reflora com a mesma estrutura da planilha modelo do sistema do CNCFlora
names(reflora_all4)
unique(reflora_all4$final_family)

reflora_all5 <- reflora_all4 %>% 
   dplyr::mutate(acceptedNameUsage = scientificName) %>%
   dplyr::mutate(scientificNameAuthorship = word(acceptedNameUsage, 3, -1, sep = " ")) %>%
   dplyr::mutate(scientificName = nome_especie) %>%
   dplyr::select(modified, institutionCode, 'Herbário.de.Origem', 'Código.de.Barra',
                 scientificName, identificationQualifier, final_family, Gênero, Espécie,
                 infraspecificEpithet, scientificNameAuthorship, Determinador, dateIdentified,
                 typeStatus, 'Número.da.Coleta', fieldNumber, Coletor, year, 
                 month, day, País, Estado, Município, 'Descrição.da.Localidade',
                 decimalLatitude, decimalLongitude, occurrenceRemarks, acceptedNameUsage,
                 occurrenceID, comments, bibliographicCitation) %>%
   dplyr::rename(collectionCode = 'Herbário.de.Origem',
                 catalogNumber= 'Código.de.Barra',
                 family = final_family,
                 genus = Gênero, 
                 specificEpithet = Espécie,
                 identifiedBy = Determinador, 
                 recordNumber = 'Número.da.Coleta', 
                 recordedBy = Coletor,
                 country = País, 
                 stateProvince = Estado, 
                 municipality = Município,
                 locality = 'Descrição.da.Localidade')

# Exportar a ocorrência geral com as colunas já formatadas
write.csv(reflora_all5, "./results/reflora_all5.csv", fileEncoding = "UTF-8",
          na = "", row.names = FALSE)

# Exportar as ocorrencias por espécie
for (i in 1:length(especies)){
   nome_arquivo <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "reflora raw.csv")
   print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
   
   if (!file.exists(nome_arquivo)){
      occs <- list()
      occs[[i]] <- reflora_all5 %>% filter(scientificName %in% especies[i])
      
      print(lapply(occs,dim))
      if (any(!is.null(lapply(occs,dim)))){
         dim.null <- lapply(occs, function(x) {!is.null(dim(x))})
         occs.f <- subset(occs, dim.null == T)
         occs.f <- dplyr::bind_rows(occs.f)
         print(dim(occs.f))
         write.csv(occs.f, nome_arquivo, na = "", row.names = F, fileEncoding = "UTF-8")
      }
      
   } else {
      warning(paste("Species was not found in the REFLORA dataset", especies[i], "\n"))
   }
}

######   end----
