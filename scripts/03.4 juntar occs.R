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
# Função "change_NA_to_df.R disponível" em: https://stevenmortimer.com/the-unfinished-duplicated-function/
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
# Criar um diretório para juntar as três fontes de dados (GBIF, REFLORA E speciesLINK)
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

### Limpeza de registros com função "new_duplicated.R"
# Criar um diretório para colocar as ocorrências com limpeza de registros
dir.create("output_final3")

for (i in 1:length(especies)) {
   print(paste("Limpando", especies[i], i, "de", length(especies), sep = " "))
   
   dir.create(paste0("./output_final3/",familias[i]), showWarnings = F)
   
   ### O arquivo com o inpa
   nome_juntas <- paste0("./output_final2/", familias[i],"/",familias[i],"_", especies[i],"_", "juntas.csv")
   
   ### O nome do arquivo que será criado com registros excluídos
   nome_excluded <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                           "excluded.csv")
   
   ### O nome do arquivo que será criado com registros duplicados (coletor, ano, numero de coleta). nao incluo nos excluded porque ele não vai ser excluido, vai ter uma cópia dele em clean e o duplicado não pode aparecer no excluded, vai ser confuso. mas eu quero checar que os s.n. não foram tomados como duplicados:
   nome_duplicata <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                            "duplicata.csv")
   
   ### O nome do arquivo limpo que será criado no final
   nome_clean <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                        "clean.csv")
   
   ### Agora a "tabela" é tabela_especie
   tabela_especie <- read_csv(nome_juntas, locale = locale(encoding = "UTF-8"),
                              na = c("", "NA")) # 
   
   # Remover registros não informativos, sem coletor, numero de coleta, ano e informações de localidade----
   # Seleciona os registros que devem sair
   tabela_exclude1 <- tabela_especie %>% dplyr::filter(is.na(municipality) & is.na(locality))
   
   # A tabela de espécies menos esses registros (antijoin)
   tabela_especie2 <- dplyr::anti_join(tabela_especie, tabela_exclude1)
   
   # Remover registros fora do Brasil ou que não tem informação de país----
   # seleciona os registros que devem sair
   tabela_exclude2 <- tabela_especie2 %>% dplyr::filter(is.na(country) | !country %in% 
                                                           c("Brazil", "Brasil","BRAZIL", "BRASIL", "Brésil", "brésil"))
  
   # A tabela de especie menos esses registros (antijoin)
   tabela_especie3 <- dplyr::anti_join(tabela_especie2, tabela_exclude2)
   
   # Registros que não tem nome de coletor nem numero de coleta
   # Seleciona os registros que devem sair
   tabela_exclude3 <- tabela_especie3 %>%
      dplyr::filter(is.na(recordedBy) & is.na(recordNumber))
   
   # A tabela de especie menos esses registros (antijoin)
   tabela_especie4 <- dplyr::anti_join(tabela_especie3, tabela_exclude3)
   
   # Juntar todas as tabelas do que seria exclu????do (cada passo gerou tabelas diferentes)
   tabela_exclude_final <- rbind.fill(tabela_exclude1, tabela_exclude2, tabela_exclude3)#dplyr::bind_rows(tabela_exclude1, tabela_exclude2, tabela_exclude3)
   write.csv(tabela_exclude_final, file = nome_excluded, fileEncoding = "UTF-8", 
             na = "", row.names = FALSE)
   
   # Remover registros com número de coleta, estado e ano iguais (coletas colaborativas duplicadas)
   # gera um vetor TrueFalse dizendo quem é duplicado, omitindo os s.n e NA
   vetor_duplicata <- tabela_especie4 %>%
      dplyr::select(year, recordNumber, stateProvince) %>%
      new_duplicated(., incomparables = c("NA", "s.n", "s/n"))
   
   # Cria a tabela de duplicados e salva
   tabela_duplicata <- tabela_especie4[vetor_duplicata,]
   
   # Tirar os duplicados da tabela especie
   tabela_especie4 <- tabela_especie4[!vetor_duplicata,]
   
   ####################################################
   # Registros com mesmo coletor e número de coleta
   # Precisa incluir escape para s.n. e NA {next}
   vetor_duplicata <- tabela_especie4 %>% dplyr::select(recordedBy, recordNumber) %>%
      new_duplicated(., incomparables = c("s.n", "s/n"))
   
   # Criar a tabela de duplicados e salvar
   tabela_duplicata <- bind_rows(tabela_duplicata, tabela_especie4[vetor_duplicata,])
   
   write.csv(tabela_duplicata, file = nome_duplicata, fileEncoding = "UTF-8", 
             na = "", row.names = FALSE)
   
   # Tirar os duplicados da tabela especie
   tabela_especie4 <- tabela_especie4[!vetor_duplicata,]
   write.csv(tabela_especie4, file = nome_clean, fileEncoding = "UTF-8", na = "", row.names=FALSE)
   
}

######   end----