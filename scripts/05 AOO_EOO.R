##############################################################################################################
   ###                                        Estimando EOO e AOO                                       ###        
##############################################################################################################

# Ler pacotes
library(readxl)
library(textclean)
library(dplyr)
library(stringr)
library(purrr)
library(flora)
library(red)
library(ConR)
library(rgdal)
library(redlistr)

# Plotando mapa do Brasil e Américas.
a <- maps::map(,c("Brazil","Mexico"))
a$range
b <- maps::map(add = T)
b$range

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")

especies <- treespp$nome_especie
familias <- treespp$final_family

# Criar uma pasta para colocar as planilhas com estimativas de EOO e AOO
dir.create("./_aaoeoo/")

# Loop para estimar EOO e AOO para cada espécie.
# Conforme tem acontecido com os loops anteriores, este também tem para em algumas circustâncias
for (i in 1:length(especies)) { 
   print(paste(especies[i], familias[i], "- calculating AOO and EOO", i, "of", length(especies)))
   nome_final <- paste0("./output_final5/",familias[i],"/",familias[i], "_",
                        especies[i],"_", "final.csv")
   
   data_csv1 <- paste0("./_aaoeoo/", familias[i], "_", especies[i],"_", "aoo_eoo.csv")
   
   tabela.final <- read.csv(nome_final) 
   
   tabela <- tabela.final[complete.cases(tabela.final[,c("decimalLongitude", "decimalLatitude")]),]
   coord <- tabela[,c("decimalLongitude", "decimalLatitude")]
   pts <- SpatialPoints(coord)
   proj4string(pts) <- CRS("+proj=longlat +datum=WGS84")
   pts.utm <- spTransform(pts, CRS("+init=epsg:29193"))
   
   aoo2 <- getAOO(pts.utm, 2000)
   red_aoo2 <- round(red::aoo(pts.utm@coords), digits = 2)
   
   # Só estimar EOO se a planilha de ocorrências tiver mais de 3 ocorrências
   if (aoo2 < 4) {
      data <- data.frame(nome_especie = especies[i],
                         final_family = familias[i],
                         nusado = aoo2,
                         eoo = NA,
                         eoo_conr = NA,
                         aoo2 = aoo2,
                         red_aoo2 = red_aoo2,
                         aoo10 = NA)
      warning(paste(especies[i], "has less than 4 coordinates", "\n"))
      
   }  else { 
      eoo <- makeEOO(pts.utm)
      eoo_area <- getAreaEOO(eoo)
      eoo_conr <- ConR::EOO.computing(coord, Name_Sp = especies[i], write_shp = T)
      points(coord)
      aoo10 <- getAOO(pts.utm, 10000)
      
      
      data <- data.frame(nome_especie = especies[i],
                         final_family = familias[i],
                         nusado = nrow(tabela),
                         eoo = round(eoo_area, digits = 2),
                         eoo_conr = round(eoo_conr$EOO, digits = 2),
                         aoo2 = aoo2,
                         red_aoo2 = red_aoo2,
                         aoo10 = aoo10)
   }
   
   # Exportar as planilhas com estimativas de EOO e AOO
   write.csv(data, file = data_csv1, fileEncoding = "UTF-8", na="")
   
}