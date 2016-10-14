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
    df <- ggplot2::fortify(cbind(df.x, Value2=df.y[,3]), index.name = "Index")
    p <- ggplot2::ggplot(data = df, ggplot2::aes(x = Index, y = Value))
    p <- p + ggplot2::geom_line(data = df, ggplot2::aes(x = Index, y = Value2),
                                linetype="dotted", size = 1/2)
    p <- p + ggplot2::geom_line(size = 1/2, alpha = 3/4)  
    p <- p + ggplot2::facet_grid(Series ~ ., scales = "free_y") 
    #p <- p + ggplot2::facet_wrap(~ Series, scales = "free_y")
    p <- p + ggplot2::labs(y="", x="")
    p <- p + ggplot2::theme_bw()
    return(p)  
  }
  if(!facet){
    p <- ggplot2::ggplot(data = df.x, ggplot2::aes(x = Index, y = Value, color=Series))
    p <- p + ggplot2::geom_line(size = 1/2, alpha = 3/4)  
    p <- p + ggplot2::theme_bw()
    return(p)  
    
  }else{
    p <-ggplot2::ggplot(df.x, ggplot2::aes(x=Index, y=Value, group_by())) +
      ggplot2::geom_line(size = 1/2, alpha = 3/4) +
      ggplot2::facet_grid(Series ~ ., scales = escala) +
      ggplot2::labs(y="", x="") +
      ggplot2::theme_bw()
  }
  return(p)
}




