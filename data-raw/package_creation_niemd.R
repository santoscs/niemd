
#inicia o pacote
#install.packages("devtools")
devtools::setup(rstudio = FALSE)

#preencher o DESCRIPTION

# cria pasta para dados brutos
devtools::use_data_raw()

#salve este arquivo em data-raw

# Ignora data-raw
devtools::use_build_ignore("data-raw")

devtools::install_github("santoscs/prevendo.inflacao.fatores.comuns")

#Escrevas as funcoes e salve em R

# documenta as funcoes
devtools::document()

# testa o pacote, provavelmente recebera um erro de 
# dependencia
devtools::check()

# coloca as dependencias no pacote
devtools::use_package("stats")
devtools::use_package("KFAS")
devtools::use_package("dynlm")
devtools::use_package("lmtest")
devtools::use_package("urca")
devtools::use_package("ggplot2")
devtools::use_package("zoo")
devtools::use_package("utils")

# teste o pacote novamente
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
devtools::use_vignette("nimcno-brasil")

## instala o pacote 
# vrifica por erros
devtools::document()
devtools::check()
# instala
devtools::install()


# apenas carrega o pacote
devtools::load_all()


# Teste do pacote

