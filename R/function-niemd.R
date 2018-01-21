#' Transforma numero e texto formatado
#' 
#' Transforma numero e texto formatado para serem mostrados na
#' tabela, o numero arredondado para 3 casas decimais e separado
#' por virgula
#' 
#' @param x um objeto com valores numericos
#' @param dig the minimum number of digits to the right 
#' of the decimal point in formatting real numbers 
#' in non-scientific formats
#' 
#' @return o mesmo objeto que foi fornecido com numeros 
#' transformados em texto formatado
#' 
#' @export
#' 

n2tab <- function(x, dig = 3){
  format(round(x, dig), digits = dig, nsmall = dig, decimal.mark=",") 
}

#' Plota series temporais com ggplot2
#' 
#' @param x objeto ts ou mts com as series temporais
#' @param y (opicional) objeto ts ou mts com dimensao de x para ser
#' plotado junto com x no mesmo grafico 
#' @param escala Are scales shared across all facets
#'  ("fixed"), or do they vary across 
#'  rows ("free_x"), columns (the default, "free_y"), or both 
#'  rows and columns ("free")
#' @param facet as series sao plotadas em graficos diferente (facet = TRUE, the default),
#' ou no mesmo grafico (facet = FALSE)
#' @param name optional name for ts univariate
#' 
#' @return ggplot das series
#' 
#' @import ggplot2 zoo
#' 
#' @export
#' 

tsplot <- function(x, y = NULL, escala = 'free_y', facet = TRUE, name = NULL){
  nseries <- NCOL(x)
  ntime <- NROW(x)
  x <- zoo::as.zoo(x)
  df.x <- zoo::fortify.zoo(x, melt = TRUE)
  if(nseries==1 & !is.null(name)){
    df.x[,"Series"] <- rep(name, ntime)
  }
  if(!is.null(y)){
    y <- zoo::as.zoo(y)
    df.y <- zoo::fortify.zoo(y, melt = TRUE)
    if(facet){
      df <- ggplot2::fortify(cbind(df.x, Value2=df.y[,3]), index.name = "Index")
      p <- ggplot2::ggplot(data = df, ggplot2::aes(x = Index, y = Value))
      p <- p + ggplot2::geom_line(data = df, ggplot2::aes(x = Index, y = Value2),
                                  linetype=2, colour="red", size = 1/2, alpha = 1)
      p <- p + ggplot2::geom_line(size = 1/2, alpha = 1, colour="blue")  
      p <- p + ggplot2::facet_grid(Series ~ ., scales = "free_y") 
      #p <- p + ggplot2::facet_wrap(~ Series, scales = "free_y")
    }else{
      p <- ggplot(df.x, aes(x = Index, y = Value))
      p <- p + geom_line(data = df.y, aes(x = Index, y = Value, group = Series, size = Series), size = 1/2, alpha = 1, colour="blue")
      p <- p + geom_line(linetype=2, size = 1/2, alpha = 1, colour="blue")  # Drawing the "overlayer"
    }
    p <- p + ggplot2::labs(y="", x="")
    p <- p + ggplot2::theme_bw(base_size=14)
    return(p)  
  }
  if(!facet){
    p <- ggplot2::ggplot(data = df.x, ggplot2::aes(x = Index, y = Value, color=Series, linetype=Series))
    p <- p + ggplot2::geom_line(size = 3/4)  
    p <- p + ggplot2::labs(y="", x="")
    p <- p + ggplot2::theme_bw(base_size=14)
    return(p)  
  }else{
    p <-ggplot2::ggplot(df.x, ggplot2::aes(x=Index, y=Value, group_by())) +
      ggplot2::geom_line(size = 1/2, alpha = 1, colour="blue") +
      ggplot2::facet_grid(Series ~ ., scales = escala) +
      ggplot2::labs(y="", x="") +
      ggplot2::theme_bw(base_size=14)
  }
  return(p)
}


#' Significancia de um teste
#' 
#' Fornece o valor da estatistica de um teste junto com a indicacao
#' da significancia para valores 10\%, 5\% e 1\%.
#' 
#' @param stat valor da estatistica do teste
#' @param cval vetor com os valores criticos do teste para 10\%, 5\% e 1\%
#' @param comp logical. If TRUE (the default) the statistics is print too
#' 
#' @return um objeto "character" com valor da estatistica do teste formatado
#' junto com "***" para 1\%, "**" para 5\%, "*" para 10\% e " " nao significativo
#' 
#' @export
#'

sig <- function(stat, cval, comp = TRUE){
  x <- sum(abs(stat)>abs(cval))
  if(x==3) sig <- "***"
  if(x==2) sig <- "** "
  if(x==1) sig <- "*  "
  if(x==0) sig <- "   "
  if(comp){
    return(c(paste(format(round(stat,3), digits = 3, nsmall = 3, decimal.mark=","), sig, sep = "")))
  }else{
    return(sig)
  }
}

#' Tabela para o teste de estacionaridade ADF e KPSS
#' 
#' Efetua o teste ADF e KPSS para series em nivel e 
#' diferenciadas e fornece uma tabela com as estatisticas
#' 
#' @param x um objeto mts com series temporais multivariadas
#' @param d logico, TRUE (padrao) indica que teste deve ser 
#' efetuado tambem para series estacionarias
#' 
#' @return tabela tabela com as estatisticas dos testes
#' 
#' @export
#' 


tab.stationary <- function(x, d = TRUE){
  n <- dim(x)[2]
  tab <- data.frame(Variavel=colnames(x), tendencia="sim", ADF.lag=NA, ADF=NA, KPSS=NA)
  for(i in 1:n){
    tmp <- try(stats::na.fail(x[,i]), silent = TRUE)
    if(inherits(tmp, "try-error")){
      y <- stats::na.omit(x[,i])
      warning("NAs omited")
    }else{
      y <- x[,i]
    }
    k <- adf.lag(y, type = "trend", lag.max = 15, selectlags = "AIC")$lag
    adf <- urca::ur.df(y, type = "trend", lags=k)
    kpss <- urca::ur.kpss(y, type = "tau")
    tab[i,'ADF.lag'] <- k
    tab[i,'ADF'] <- sig(stat = adf@teststat[1], cval = adf@cval[1,])
    tab[i,'KPSS'] <- sig(stat = kpss@teststat, cval = kpss@cval[-3])
  }
  if(d){
    tab2 <- data.frame(Variavel=paste0("diff.",colnames(x)), tendencia="nao", ADF.lag=NA, ADF=NA, KPSS=NA)
    for(i in 1:n){
      y <- na.omit(diff(x[,i]))
      k <- adf.lag(y, type = "drift", lag.max = 15, selectlags = "AIC")$lag
      adf <- urca::ur.df(y, type = "drift", lags=k)
      kpss <- urca::ur.kpss(y, type = "mu")
      tab2[i,'ADF.lag'] <- k
      tab2[i,'ADF'] <- sig(stat = adf@teststat[1], cval = adf@cval[1,])
      tab2[i,'KPSS'] <- sig(stat = kpss@teststat, cval = kpss@cval[-3])
    }
    tabela <- rbind(tab,tab2)
    return(tabela)
  }
  tabela <- tab
  return(tabela)
}

#' Testa os criterios de Marques et al (2003)
#'
#' Testa os criterios de Marques et al (2003) que uma 
#' medida de nucleo da inflacao deve atender
#'
#' @param x serie do nucleo
#' @param y serie da inflacao 
#' 
#' @return  um vetor com os teste "ADF", "t alpha", "t gamma", 't lambda', 'F thetas'
#'
#' @import zoo dynlm urca lmtest
#' 
#' @export
#' 


marques <- function(x,y){
  requireNamespace("zoo")
  requireNamespace("dynlm")
  
  if(sum(round(stats::tsp(x),  3)!=round(stats::tsp(y), 3))==1){
    warning("series com inicio, fim ou frequencia diferentes, 
            usando somente a cobertura temporal comum")
    ini <- max(stats::tsp(x)[1], stats::tsp(y)[1])
    fim <- min(stats::tsp(x)[2], stats::tsp(y)[2])
    x <- stats::window(x, start = ini, end = fim)
    y <- stats::window(y, start = ini, end = fim)
  }
  y <- zoo::as.zoo(y)
  x <- zoo::as.zoo(x)
  
  ## Teste adf 
  z <- y - x
  fit.adf <- adf.lag(z, type = "drift", lag.max = 15, selectlags = "AIC")
  k <- fit.adf$lag
  adf <- urca::ur.df(z, lags=k, type= "drift")
  t.adf <- sig(stat = adf@teststat[1], cval = adf@cval[1,])
  t.alpha <- lmtest::coeftest(fit.adf$fit)[1,4]
  
  ## estima os mecanismos de correcao de erro para a inflacao y e
  ## e o nucleo x
  
  # escolha as defasagens k por AIC
  result.y <- result.x <- vector()
  for(i in 1:15){
    fity <- dynlm::dynlm(d(y) ~ L(I(y-x), 1) + L(d(y), 1:i) + L(d(x), 1:i))
    fitx <- dynlm::dynlm(d(x) ~ L(I(y-x), 1) + L(d(y), 1:i) + L(d(x), 1:i))
    result.y[i] <- stats::AIC(fity)
    result.x[i] <- stats::AIC(fitx)
  }
  k <- which.min(result.y)
  fity <- dynlm::dynlm(d(y) ~ L(I(y-x), 1) + L(d(y), 1:k) + L(d(x), 1:k))
  k <- which.min(result.x)
  fitx <- dynlm::dynlm(d(x) ~ L(I(y-x), 1) + L(d(y), 1:k) + L(d(x), 1:k))
  
  # exogeneidade fraca
  # p valor do teste t sobre gamma (y) e lambda (x)
  #t.gamma.y <- lmtest::coeftest(fity, vcov. = sandwich::NeweyWest(fity))[2,4]
  t.gamma.y <- summary(fity)$coefficients[2,4]
  #t.lambda.x <- lmtest::coeftest(fitx, vcov. = sandwich::NeweyWest(fitx))[2,4]
  t.lambda.x <- summary(fitx)$coefficients[2,4]
  
  # exogeneidade forte
  # p valor teste F sobre os thethas
  k <- which.min(result.x)
  fit1 <- dynlm::dynlm(d(x) ~ L(d(y), 1:k) + L(d(x), 1:k))
  fit2 <- dynlm::dynlm(d(x) ~ L(d(x), 1:k))
  #test.f <- lmtest::waldtest(fit1, fit2, test = "F", vcov = sandwich::NeweyWest(fit1))
  test.f <- lmtest::waldtest(fit1, fit2, test = "F")
  f.theta <- test.f$`Pr(>F)`[2]
  
  pvalue <- c(t.alpha, t.gamma.y, t.lambda.x, f.theta)
  result <- c(t.adf, n2tab(pvalue))
  names(result) <- c("ADF", "t alpha", "t gamma y", 't lambda x', 'F thetas')
  return(result)
}

#' Tabela para as condicoes Marques et. al. (2003)
#' 
#' Efetua o teste para as condicoes Marques et. al. (2003) para o nucleo e inflacao
#'  e fornece uma tabela com as estatisticas 
#' 
#' @param y um objeto ts com a inflacao observada
#' @param x um objeto mts com series dos nucleos a serem testados
#' 
#' @return tabela com as estatisticas dos testes
#' 
#' @export
#' 

tab.marques <- function(y, x){
  n <- dim(x)[2]
  tab <- data.frame(nucleos=colnames(x), ADF=NA, t.alpha=NA, t.gamma=NA,
                    t.lambda=NA, F.thetas=NA)
  for(i in 1:n){
    tab[i,2:6] <- marques(y=y,x=stats::na.omit(x[,i]))
  }
  return(tab)
}


#' Seleciona a defasagem para o teste ADF
#' 
#' Seleciona a defasagem para o teste ADF segundo o criterio
#' AIC ou BIC para um maximo de defasagens
#' 
#' @param y The vector tested for a unit root
#' @param type Test type, either "none", "drift" or "trend".
#' @param lag.max The maximum number of lags considered
#' @param selectlags Lag selection can be achieved according to the Akaike "AIC" or the Bayes "BIC" information criteria. 
#' 
#' @return A defasagem selecionda
#' 
#' @import stats
#' 
#' @export

adf.lag <- function (y, type = c("none", "drift", "trend"), lag.max = 15, selectlags = c("AIC", "BIC")) 
{
  selectlags <- match.arg(selectlags)
  type <- match.arg(type)
  if (ncol(as.matrix(y)) > 1) 
    stop("\ny is not a vector or univariate time series.\n")
  if (any(is.na(y))) 
    stop("\nNAs in y.\n")
  y <- as.vector(y)
  lag <- as.integer(lag.max)
  if (lag < 0) 
    stop("\nLags must be set to an non negative integer value.\n")
  CALL <- match.call()
  DNAME <- deparse(substitute(y))
  x.name <- deparse(substitute(y))
  z <- diff(y)
  n <- length(z)
  x <- stats::embed(z, lag.max)
  z.diff <- x[, 1]
  z.lag.1 <- y[lag.max:n]
  tt <- lag.max:n
  if (lag.max > 1) {
    critRes <- rep(NA, lag.max)
    for (i in 2:(lag.max)) {
      z.diff.lag = x[, 2:i]
      if (type == "none") 
        result <- stats::lm(z.diff ~ z.lag.1 - 1 + z.diff.lag)
      if (type == "drift") 
        result <- stats::lm(z.diff ~ z.lag.1 + 1 + z.diff.lag)
      if (type == "trend") 
        result <- stats::lm(z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
      critRes[i] <- stats::AIC(result, k = switch(selectlags, 
                                                  AIC = 2, BIC = log(length(z.diff))))
    }
    lag <- which.min(critRes)
  }else{
    lag <- lag.max
  }
  z.diff.lag = x[, 2:lag]
  if (type == "none") 
    result <- stats::lm(z.diff ~ z.lag.1 - 1 + z.diff.lag)
  if (type == "drift") 
    result <- stats::lm(z.diff ~ z.lag.1 + 1 + z.diff.lag)
  if (type == "trend") 
    result <- stats::lm(z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
  return(list(lag = lag, fit = result))
}


#' Acumula series em percentual ao mes em m meses
#' 
#' Transforma uma serie mensal dada em percentual ao mes 
#' em uma serie mensal com percentual nos ultimos m meses
#'
#' @param x A time series univariate
#' @param m number of monthes
#' 
#' @return A time series univariate 
#' 
#' @import zoo stats
#' 
#' @export

acum<-function(x, m=12){
  # input:
  # x(ts): serie a ser acumulada
  #output: 
  # x12(ts): serie acumulada
  
  x <- zoo::as.zoo(x)
  x12 <- zoo::rollapplyr(x, width=m, function(x) (prod(1+x/100)-1)*100)
  x12 <- stats::as.ts(x12)
  return(x12)
}


#' @title Previsao fora da amostra com modelo de defasagem distribuida
#' 
#' @description Realiza previsoes fora da amostra para yh com base no 
#' modelo de defasagem distribuida selecionado por BIC 
#' 
#' @param yh serie a ser prevista \eqn{y_{t+h}^h} 
#' @param yt (ts) preditor \eqn{y_{t-j+1}}
#' @param x (ts) preditor
#' @param m numero maximo de defasagens de x
#' @param p numero maximo de defasagens de yt
#' @param h horizonte de previsao
#' @param n numero de previsoes fora da amostra
#'   
#' @return lista contendo \code{fcast} (ts) valores previstos; \code{model} 
#' (dyn) modelo estimado no fim da amostra
#' 
#' @import stats zoo
#' 
#' @export

outsample.mdd <-function(yh, yt, x, m=3, p=3, n, h=12){
  # verifica se as series temporais estao em concordancia
  if(!identical(round(tsp(yh)), round(tsp(yt))))
    stop("series com inicio, fim ou frequencia diferente")
  if(!is.null(x))
    if(!identical(round(tsp(yh)), round(tsp(x))))
      stop("series com inicio, fim ou frequencia diferente")
  
  # tamanho, data e atributos das series
  Tn <- dim(as.matrix(yh))[1]
  timeline <- time(yh)
  atr <- tsp(yh)
  fcast <- vector()
  outdate <- timeline[(Tn-n+1):Tn]
  if(is.null(x)){
    for(i in 1:n){
      # restringe os dados
      yh.ajuste <- window(yh, end=timeline[Tn-n-h+i])
      yt.ajuste <- window(yt, end=timeline[Tn-n-h+i])
      # estima o modelo
      model <- mdd.selec(yh=yh.ajuste, yt=yt.ajuste, x=x, h=h, m=m, p=p)
      # get prediction for ith value
      fcast[i] <- model$fcast
    } 
  }else{
    for(i in 1:n){
      # restringe os dados
      yh.ajuste <- window(yh, end=timeline[Tn-n-h+i])
      yt.ajuste <- window(yt, end=timeline[Tn-n-h+i])
      x.ajuste <- window(x, end=timeline[Tn-n-h+i])
      # estima o modelo
      model <- mdd.selec(yh=yh.ajuste, yt=yt.ajuste, x=x.ajuste, h=h, m=m, p=p)
      # get prediction for ith value
      fcast[i] <- model$fcast
    }
  }
  fcast <- ts(fcast, end=index(model$fcast), frequency = atr[3])
  return(list(fcast=fcast, outdate=outdate, model=model))
}



#' @title Seleciona o modelo defasagem distribuida
#'
#' @description Seleciona o modelo defasagem distribuida com menor BIC
#'
#' @param yh serie a ser prevista \eqn{y_{t+h}^h} 
#' @param yt preditor \eqn{y_{t-j+1}}
#' @param x preditor
#' @param m numero maximo de defasagens de x
#' @param p numero maximo de defasagens de yt
#' @param h horizonte de previsao
#' 
#' @return best.fit o modelo selecionado dyn
#' 
#' @import stats
#' @export

mdd.selec <- function(yh, yt, x, h, m, p){
  bic.best <- Inf
  for(i in 1:m){
    for(j in 1:p){
      model <- mdd(yh=yh, yt=yt, x=x, h=h,m=i,p=j)
      bic <- BIC(model$fit)
      if(bic<bic.best){
        bic.best <- bic
        model.best <-model
      }
    }
  }
  return(model.best)
}

#' @title Estima e Prever com modelo de defadsagem distribuida (mdd)
#' 
#' @description Estima uma regressao de yh sobre x e seus m lags e 
#' sobre yt e seus p lags
#' 
#' @details yh igual a yt ou a alguma transformacao de yt, esse modelo
#' de previsao se baseia em Stock e Watson 1998
#'
#' @param yh (ts) serie a ser prevista \eqn{y_{t+h}^h} 
#' @param yt (ts) preditor \eqn{y_{t-j+1}}
#' @param x (ts) preditor
#' @param m numero de defasagens de x
#' @param p numero de defasagens de yt
#' @param h horizonte de previsao
#' 
#' @return uma lista com: fit(dyn): o modelo estimado e 
#' fcast(ts): valor previsto para \eqn{yh_{t+h}}
#' 
#' @import dyn zoo utils
#' @export

mdd <- function(yh, yt, x, h, m, p){
  requireNamespace("zoo")
  requireNamespace("dyn")
  
  # verifica se as series temporais estao em concordancia
  if(!identical(round(tsp(yh),3), round(tsp(yt), 3)))
    stop("series yh e yt com inicio, fim ou frequencia diferente")
  if (!is.null(x))
    if(!identical(round(tsp(yh),3), round(tsp(x), 3)))
      stop("series yh e x com inicio, fim ou frequencia diferente")
  # transforma para o formato zoo
  if (is.null(x)) {
    dados <- zoo::as.zoo(ts.intersect(yh, yt))
  } else {
    dados <- zoo::as.zoo(ts.intersect(yh, yt, x))
    # nomes dos xf
    v <- colnames(dados)[-(1:2)]
  } 
  
  # funcao lag
  L <- function(x, k = 1) stats::lag(x, -k)
  if(is.null(x)){
    form <- as.formula("yh ~ L(yt, h:(h+p-1))")
  }else{
    form <- as.formula(paste("yh ~ L(yt, h:(h+p-1)) + ",
                             paste("L(", v,", h:(h+m-1))",
                                   collapse=" + ", sep="")))
  }
  fit <- dyn::dyn$lm(form, data=dados)
  fcast <- tail(na.omit(predict(fit, dados)), 1)
  return(list(fit=fit, fcast=fcast))
}

#' Tabela compara REQM dos modelos
#' 
#' A partir de uma matrix x com as previsoes dos modelos e 
#' a series observada, retorna uma tabela com REQM, REQM relativo
#' e teste dm
#' 
#' @param x matriz com previsoes nas colunas
#' @param obs nome da serie observada
#' @param ref nome da previsao de referencia
#'
#' @return Uma matriz com REQM, EQMR e teste DM 
#' 
#' @importFrom forecast dm.test
#' 
#' @export 
#' 
tab.reqm <- function(x, obs="ipca", ref="ipca.focus"){
  y <- x[,obs]
  nomes <- c(ref ,colnames(x[, colnames(x)!=obs & colnames(x)!=ref]))
  # calculca reqm e reqm relativo
  eqm <- reqm <- NULL
  for(i in 1:length(nomes)){
    reqm <- cbind(reqm,sqrt(mean((x[,nomes[i]]-y)^2)))
  }
  for(i in 1:length(nomes)){
    eqm <- cbind(eqm,mean((x[,nomes[i]]-y)^2))
  }
  colnames(reqm) <- colnames(eqm) <- nomes
  eqmr <- eqm/eqm[,ref]
  #dm test
  dm<-NULL
  e <- apply(x[,nomes], 2, function(x) x-y)
  sele <- colnames(x[, colnames(x)!=obs & colnames(x)!=ref])
  for(i in sele){
    dm <- cbind(dm, forecast::dm.test(e1=e[,i], e2=e[,ref], h=12)$p.value)
  }
  colnames(dm) <- sele
  # formatando numeros
  dm <- format(round(dm, digits = 2), decimal.mark = ",")
  reqm <- format(round(reqm, digits = 2), decimal.mark = ",")
  eqmr <- format(round(eqmr, digits = 2), decimal.mark = ",")
  tab <- rbind(reqm, eqmr)
  tab <- rbind(tab, c("",dm))
  tab <- t(tab)
  colnames(tab) <- c("reqm", "eqmr", "dm test")
  return(tab)
}

#' Tabela com teste de previsao incorporada
#' 
#' A partir de uma matrix x com as previsoes dos modelos e 
#' a series observada, retorna uma tabela com o teste de previsao
#' incorporada de Harvey et. al. 1998
#' 
#' @inheritParams tab.reqm
#' 
#' @return Uma matriz com a lambda estimado e o p valor do teste
#' de previsao incorporada
#' 
#' @export
#' 
tab.enctest <- function(x, obs="ipca12", ref="ipca12.focus"){
  n <- length(x[1,])-2
  tabela1 <- matrix(NA, n, 2)
  nomes <- c(ref ,colnames(x[, colnames(x)!=obs & colnames(x)!=ref]))
  colnames(tabela1) <- c("Lambda", "(valor p)")
  nomeB <- nomes[nomes!=ref]
  for(j in 1:length(nomeB)){
    aux <- enc.test(y=x[,obs], fA=x[,ref], fB=x[,nomeB[j]])
    tabela1[j,"Lambda"] <- aux["I(fB - fA)","Estimate"]
    tabela1[j,"(valor p)"] <- aux["I(fB - fA)","Pr(>|t|)"]
  }
  # formatando numeros
  tab <- format(round(tabela1, digits = 2), decimal.mark = ",")
  # junta as tabelas
  tabela <- matrix(paste(tab[,1], sub(" ", "", paste("(", tab[,2], ")", sep=""))), nrow = n)
  tabela <- cbind(nomeB, tabela)
  tabela <- cbind("ipca12.focus", tabela)
  colnames(tabela) <- c("Modelo A", "Modelo B", "Lambda (valor p)")
  return(tabela)
}


#' Tabela com teste de previsao incorporada
#' 
#' A partir de uma matrix x com as previsoes dos modelos e 
#' a series observada, retorna uma tabela com o teste de previsao
#' incorporada de Harvey et. al. 1998
#' 
#' @inheritParams tab.reqm
#' @param h Length ahead of the forecast.
#' 
#' @return Uma matriz com a lambda estimado e o p valor do teste
#' de previsao incorporada
#' 
#' @export
#' 
tab.comptest <- function(x, obs="ipca12", ref=NULL, h){
  n <- length(x[1,])-2
  tabela1 <- matrix(NA, n, 2)
  nomes <- c(ref ,colnames(x[, colnames(x)!=obs & colnames(x)!=ref]))
  colnames(tabela1) <- c("Lambda", "(valor p)")
  nomeB <- nomes[nomes!=ref]
  for(j in 1:length(nomeB)){
    aux <- comp.test(y=x[,obs], fA=x[,ref], fB=x[,nomeB[j]], h = h)
    tabela1[j,"Lambda"] <- aux["fB","Estimate"]
    tabela1[j,"(valor p)"] <- aux["fB","Pr(>|t|)"]
  }
  # formatando numeros
  tab <- format(round(tabela1, digits = 2), decimal.mark = ",")
  # junta as tabelas
  tabela <- matrix(paste(tab[,1], sub(" ", "", paste("(", tab[,2], ")", sep=""))), nrow = n)
  tabela <- cbind(nomeB, tabela)
  tabela <- cbind("ipca12.focus", tabela)
  colnames(tabela) <- c("Modelo A", "Modelo B", "Lambda (valor p)")
  return(tabela)
}



#' Test for Forecast Encompassing
#'
#' The Test for Forecast Encompassing compares the forecast of two forecast methods. 
#' 
#' The null hypothesis is that the method A forecast encompasses
#' of method B, i.e., method B have the same information
#' of method A. The alternative hypothesis is that method B 
#' have additional information.
#' 
#' @param fA Forecast from method A.
#' @param fB Forecast from method B.
#' @param y The observed time series.
#' @param h Length ahead of the forecast.
#' 
#' @return An object of class "coeftest" which is 
#' essentially a coefficient matrix with columns 
#' containing the estimates, associated standard 
#' errors, test statistics and p values.
#' 
#' 
#' @import lmtest sandwich stats
#' @export

comp.test <- function(y, fA, fB, h){
  yh <- y - glag(y, -h)
  fA <- fA - glag(y, -h)
  fB <- fB - glag(y, -h)
  fit <- stats::lm(yh ~ fA + fB) 
  return(lmtest::coeftest(fit, vcov. = sandwich::NeweyWest(fit)))
}

#' Generalized Lag 
#' 
#' Compute a lagged version of a vector like time series
#' 
#' @param x A vector or univariate time series
#' @param k	The number of lags
#' 
#' @return A vector object with the same length of x.
#' 
#' @import stats
#' 
#' @export
#' 
glag <- function(x, k = -1){
  h <- abs(k)
  if(k>0){
    xh <- c(x, rep(NA, h))
    ans <- embed(xh, h+1)[,1]
  }
  
  if(k<0){
    xh <- c(rep(NA, h), x) 
    ans <- embed(xh, h+1)[,(h+1)]
  }
  if(k==0){
    ans <- x
  }
  return(ans)
}


#' Test for Forecast Encompassing
#'
#' The Test for Forecast Encompassing compares the forecast of two forecast methods. 
#' 
#' The null hypothesis is that the method A forecast encompasses
#' of method B, i.e., method B have the same information
#' of method A. The alternative hypothesis is that method B 
#' have additional information.
#' 
#' @param fA Forecast from method A.
#' @param fB Forecast from method B.
#' @param y The observed time series.
#' 
#' @return An object of class "coeftest" which is 
#' essentially a coefficient matrix with columns 
#' containing the estimates, associated standard 
#' errors, test statistics and p values.
#' 
#' 
#' @import lmtest sandwich zoo
#' @export

enc.test <- function(y, fA, fB){
  requireNamespace("zoo")
  fit <- dyn::dyn$lm(y ~ offset(fA) + I(fB-fA)) 
  return(lmtest::coeftest(fit, vcov. = sandwich::NeweyWest(fit)))
}


