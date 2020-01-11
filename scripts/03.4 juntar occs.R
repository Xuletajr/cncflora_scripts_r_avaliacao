##############################################################################################################
   ###                    Juntar as ocorrências REFLORA, speciesLink e GBIF                             ###        
##############################################################################################################

# Ler pacotes
library(dplyr)
library(stringr)
library(rgbif)
library(readxl)
library(readr)
library(plyr)
# Função change_NA_to_df.R disponível em: https://stevenmortimer.com/the-unfinished-duplicated-function/
source("./functions/new_duplicated.R") 

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")

# Colocar as famílias e binômio das espécies em vetores.  
familias <- treespp$final_family
especies <- treespp$nome_especie

###
# Ajustar a coluna de coletor do inpa provenientes do GBIF (só vem o nome no curador como coletor)
# Ler planilha contendo apnenas ocorrências do INPA compilada com dados do speciesLink
tabela_inpa_splink <- read.csv(file = "./results/inpa_filtrado.csv", header = TRUE,
                               encoding = "UTF-8") 

# Checar que a única instituição desta planilha é o INPA
unique(tabela_inpa_splink$institutioncode)

tabela_inpa_splink <- tabela_inpa_splink %>%
   dplyr::mutate(institutioncode = "Instituto Nacional de Pesquisas da Amazônia (INPA)")

# Todos os coletores do inpa
colectores_inpa <- tabela_inpa_splink %>% 
   dplyr::select(institutioncode, catalognumber, collector) %>% 
   dplyr::mutate(collector = as.character(collector), catalognumber = as.character(catalognumber)) %>% 
   dplyr::rename(institutionCode = institutioncode, catalogNumber = catalognumber)#catalogNumber = catalognumber, 

# Loop----
for (i in 1:length(especies)) {
   # Nomes
   nome_arquivo <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "gbif raw.csv")
   nome_out <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "inpa.csv")
   print(paste("Inpa", especies[i], i, "de", length(especies), sep = " "))
   if(!file.exists(nome_out)) {
      # Lê a tabela
      tabela_especie <- read.csv(nome_arquivo, stringsAsFactors = F, encoding = "UTF-8") %>% 
         mutate(catalogNumber = factor(catalogNumber))
      
      # Junta a tabela com os coletores
      if ("Instituto Nacional de Pesquisas da Amaz?nia (INPA)" %in% tabela_especie$institutionCode) {
         proof_name <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "inpa_proof.csv")
         tabela_especie <- left_join(tabela_especie, colectores_inpa)
         # Tudo o que for do inpa bota o coletor e tira a coluna collector
         tabela_especie <- tabela_especie %>%
            mutate(recordedBy = if_else(tabela_especie$institutionCode %in%
                                           "Instituto Nacional de Pesquisas da Amaz?nia (INPA)", collector, recordedBy)) %>%
            dplyr::select(-collector)
         
         # Exportar as planilhas
         write.csv(tabela_especie, file = nome_out, row.names = F, fileEncoding = "UTF-8", na = "")
         proof <- tabela_especie %>%
            filter(institutionCode == "Instituto Nacional de Pesquisas da Amaz?nia (INPA)") %>%
            dplyr::select(institutionCode, catalogNumber, recordedBy) %>%
            distinct()
         write.csv(proof, file = proof_name, row.names = F, fileEncoding = "UTF-8",  na = "")
      
         } else {
         
         write.csv(tabela_especie, file = nome_out, row.names = F, fileEncoding = "UTF-8", na = "")
      }
   }
}

###
# Cria um diretório para juntar as três fontes de dados (GBIF, REFLORA E speciesLINK)
dir.create("output_final2")

# Loop para juntar as três fontes de dados
for (i in 1:length(especies)) {
   print(paste("Juntando dados --- ", especies[i], i, "de", length(especies), sep = " "))
   
   nome_reflora <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "reflora raw.csv")
   nome_gbif <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "inpa.csv")
   nome_speciesLink <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "speciesLink raw.csv") # com dois underlines ? o correto.
   
   nome_out <- paste0("./output_final2/", familias[i],"/",familias[i],"_", especies[i],"_", "juntas.csv")
   
   dir.create(paste0("./output_final2/",familias[i]), showWarnings = F)
   
   if(!file.exists(nome_out)) {
      
      #file_list <- list.files(path = paste0("./output_final/", familias[i]), 
      #                       pattern = paste0(familias[i],"_", especies[i],"_ _"))
      
      reflora <- read.csv(nome_reflora, stringsAsFactors = F, encoding = "UTF-8")    
      gbif <- read.csv(nome_gbif, stringsAsFactors = F, encoding = "UTF-8")
      speciesLink <- read.csv(nome_speciesLink, stringsAsFactors = F, encoding = "UTF-8")
      
      #tabela_juntas <- lapply(paste0("./output_final/", familias[i],"/", file_list),function(x){
      #   read.csv(x, stringsAsFactors = F, encoding = "UTF-8" ) 
      #}) %>%
      tabela_juntas <- rbind.fill(reflora, gbif, speciesLink) # bind_rows
      
      write.csv(tabela_juntas, file = nome_out, row.names = FALSE, 
                na = "", fileEncoding = "UTF-8")
   }
}