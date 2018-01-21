#' Series IPCA e nucleos do Brasil
#' 
#' Dados sao extraidos do Banco Central do Brasil para as 
#' series mensais de janeiro de 1995 ate maio de 2016
#' 
#' @usage ipca95
#' 
#' @format Uma serie temporal mutivariada (mts) com 6 variaveis
#' 
#' \itemize{
#' \item{\code{ipca}}{Indice nacional de precos ao consumidor-amplo (\% mensal)}
#' \item{\code{ipca.mas}}{Nucleo medias aparadas com suavizacao (\% mensal)}
#' \item{\code{ipca.ma}}{Nucleo medias aparadas sem suavizacao (\% mensal)}
#' \item{\code{ipca.ex}}{Nucleo por exclusao - sem monitorados e alimentos no domicilio (\% mensal)}
#' \item{\code{ipca.ex2}}{Nucleo por exclusao - ex2 (\% mensal)}
#' \item{\code{ipca.dp}}{Nucleo de dupla ponderacao (\% mensal)}
#' }
#'
#' @source Banco Central do Brasil. site www.bcb.gov.br

"ipca95"