####################################################################################################
   ###                Baixar dados do Projeto Flora do Brasil 2020 (IPT)                      ###
   ###              e formatar a distribuição e forma de vida das plantas                     ###               
####################################################################################################

# Ler pacotes
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)
library(downloader)

# Baixar dados do Flora do Projeto Flora do Brasil 2020 (IPT)---
pag <- "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
download(url = pag, destfile = "iptflora", mode = "wb") # Para sistema operacional windows só funciona com mode = "wb"
unzip("iptflora", exdir = "./ipt") # salvar em pasta "ipt" dentro da pasta de trabalho

