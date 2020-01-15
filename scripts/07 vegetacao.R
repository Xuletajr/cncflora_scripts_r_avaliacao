##############################################################################################################
   ###                            Cruzar o tipo de vegetação com código IUCN                            ###        
##############################################################################################################

# Ler pacotes
library(tidyr)
library(dplyr)
library(readxl)

# Ler a planilha com as espécies de plantas do recorte com as estimativas de AOO e EOO
finalaoo <- read.csv("./results/tree_final_with_aooeoo.csv", row.names = 1)

unique(finalaoo$vegetationType)

# Ler a planilha com tipos de vegetação IUCN e Flora do Brasi
veg_comp <- readxl::read_excel("./data/Tabela comparativa - Tipos de vegetação IUCN e Flora do Brasil-11-12-2018.xlsx", 
                               sheet=1) %>% data.frame()

names(veg_comp) <- c("IUCN", "CNCFlora", "FB")
head(veg_comp)

veg_comp %>% count(FB,CNCFlora) %>% View()
unique(veg_comp$FB) # # A função grep tem problemas com caracteres especiais,e então tipos de vegetação como 
# Caatinga (stricto sensu) e Cerrado (lato sensu) acabam tendo problemas e não foram reconhecidas

veg_comp[25,3] <- "Cerrado"  # Deixar somente a palavra Cerrado - importante a palavra não se repetir  
veg_comp[35,3] <- "Caatinga" # mesma coisa para caatinga

# Verificando novamente
unique(veg_comp$FB)
unique(veg_comp$IUCN)

w <- grepl(pattern = veg_comp$FB[1], x = finalaoo$vegetationType)
finalaoo$vegetationType[w]

aoo_veg <- finalaoo
for (i in seq_along(unique(veg_comp$FB))) { # o último item do loop é de NA's
   vegname <- unique(veg_comp$FB)[i]
   data <- veg_comp %>% filter(FB == vegname)
   print(data)
   gr <- grep(pattern = vegname, x = finalaoo$vegetationType, ignore.case = TRUE)
   aoo_veg[[vegname]]  <- "" # Essa parte também parece ok!
   aoo_veg[[vegname]][gr]  <- data$CNCFlora
}

write.csv(aoo_veg, "./results/aoo_veg.csv", fileEncoding = "UTF-8")

# Ver como ficou a tabela - criou uma coluna para cada categoria do CNCFlora
names(aoo_veg)
View(aoo_veg)

# Juntando todos os códigos IUCN em apenas uma coluna denominada IUCN
aoo_veg2 <- aoo_veg %>% tidyr::unite(IUCN, 26:73, sep =" ")
View(aoo_veg2)

write.csv(aoo_veg2, "./results/aoo_veg.csv", fileEncoding = "UTF-8")

######   end----
