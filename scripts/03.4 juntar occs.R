##############################################################################################################
   ###                    Juntar as ocorrências REFLORA, speciesLink e GBIF                             ###        
##############################################################################################################

# Ler pacotes

###
# Ajustar a coluna de coletor do inpa provenientes do GBIF (só vem o nome no curador como coletor)

tabela_inpa_splink <- read.csv(file = "./results/inpa_filtrado.csv", header = TRUE,
                               encoding = "UTF-8") 

# a coluna acceptedNameUsage não existe e a coluna scientifiName nãõ tem autor
unique(tabela_inpa_splink$institutioncode)

tabela_inpa_splink <- tabela_inpa_splink %>%
   mutate(institutioncode = "Instituto Nacional de Pesquisas da Amaz?nia (INPA)")

#todos os coletores do inpa
colectores_inpa <- tabela_inpa_splink %>% 
   dplyr::select(institutioncode, catalognumber, collector) %>% 
   mutate(collector = as.character(collector), catalognumber = as.character(catalognumber)) %>% 
   rename(institutionCode = institutioncode, catalogNumber = catalognumber)#catalogNumber = catalognumber, 

#loop----
for (i in 1:length(especies)) {
   #nomes
   nome_arquivo <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "gbif raw.csv")
   nome_out <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "inpa.csv")
   print(paste("Inpa", especies[i], i, "de", length(especies), sep = " "))
   if(!file.exists(nome_out)) {
      #le a tabela
      tabela_especie <- read.csv(nome_arquivo, stringsAsFactors = F, encoding = "UTF-8") %>% # row.names = 1,
         mutate(catalogNumber = factor(catalogNumber))
      
      #junta a tabela com os coletores
      if ("Instituto Nacional de Pesquisas da Amaz?nia (INPA)" %in% tabela_especie$institutionCode) {
         proof_name <- paste0("./output_final/", familias[i],"/",familias[i],"_", especies[i],"_", "inpa_proof.csv")
         tabela_especie <- left_join(tabela_especie, colectores_inpa)
         #tudo o que for do inpa bota o coletor e tira a coluna collector
         tabela_especie <- tabela_especie %>%
            mutate(recordedBy = if_else(tabela_especie$institutionCode %in%
                                           "Instituto Nacional de Pesquisas da Amaz?nia (INPA)", collector, recordedBy)) %>%
            dplyr::select(-collector)
         #escreve
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
