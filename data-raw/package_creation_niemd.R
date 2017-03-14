######################
### Project: niemd ###
######################

##inicia o pacote
#install.packages("devtools")
devtools::setup(rstudio = FALSE)

##preencher o DESCRIPTION

## cria pasta para dados brutos
devtools::use_data_raw()

##salve este arquivo em data-raw

## Ignora data-raw
devtools::use_build_ignore("data-raw")

## importa os dados brutos e salva
rm(list = ls()) # limpa workspace
ipca95 <- read.csv("data-raw/ipca95.csv", dec=",", na.strings="-")
ipca95 <- ts(ipca95[,-1], start = c(1995,1), frequency = 12)
devtools::use_data(ipca95, overwrite = TRUE)


## Escrevas as funcoes e salve em R

## documenta as funcoes
devtools::document()

## testa o pacote, provavelmente recebera um erro de 
# dependencia
devtools::check()

## coloca as dependencias no pacote
devtools::use_package("ggplot2")
devtools::use_package("zoo")
devtools::use_package("tsDyn")
devtools::use_package("forecast")
devtools::use_package("vars")
devtools::use_package("Rlibeemd")
devtools::use_package("utils")
# devtools::use_package("utils")

## teste o pacote novamente
devtools::document()
devtools::check()

# corriga os possiveis erros ou adivertencias apontados 
## corrige os acentos
tools::showNonASCII(readLines("R/tabelas.R"))
tools::showNonASCII(readLines("R/ucmodel.R"))

# teste o pacote novamente
devtools::check()

# dados de maneira externa no pacote
devtools::use_data_raw()

#Adding `data-raw` to `.Rbuildignore`
devtools::use_build_ignore("data-raw")
# salve os dados brutos nesta pasta juntamente com
# o codigo para obter os dados transformados


# cria um vignette que reproduz os resultados
devtools::use_vignette("pimfc")
devtools::clean_vignettes("vignettes/niemd.Rmd")

## instala o pacote 
# vrifica por erros
devtools::document()
devtools::check()
# instala
devtools::install()


# apenas carrega o pacote
devtools::load_all()


# Teste do pacote

