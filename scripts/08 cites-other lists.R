##############################################################################################################
   ### Apêndice CITES (Convention on International Trade in Endangered Species of Wild Fauna and Flora) ###        
##############################################################################################################

# Ler pacotes
library(readxl)
library(flora)

# Ler a planilha com as espécies de plantas do recorte
aoo <- read.csv("./results/aoo_veg.csv", row.names = 1)

# Ler planilha CITES---- 
# Planilha foi baixada do site: http://checklist.cites.org/#/en
cites <- read.csv("./data/Index_of_CITES_Species_2019-10-08 16_14.csv")

names(cites)

cites1 <- cites %>% 
   dplyr::filter(Kingdom == "Plantae") %>% 
   dplyr::filter(RankName %in% c("SPECIES", "SUBSPECIES", "VARIETY"))

# Conferir quais espécies estão com nomes listados nos apêndides da CITES
which(aoo$nome_especie %in% cites1$FullName)
aoo$nome_especie[c(105, 106,107)]

# Adicionar uma coluna com informações se a espécies ocorre nos apêndices CITES
# Neste caso só tinha spp no Apêndice II, tem que melhorar a função para listar corretamente
# caso tenha spp em outros apêndices
aoo <- aoo %>% 
   mutate(CITES = ifelse(nome_especie %in% cites$FullName, "yes (Appendix II)", ""))
aoo[aoo$CITES == "yes (Appendix II)",]

# Exportar planilha
write.csv(aoo, "./results/aoo_veg_cites.csv")

######   end----