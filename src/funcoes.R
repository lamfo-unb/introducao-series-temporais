library(ggplot2)
library(magrittr)
library(strucchange)


media_movel <- function(x, periodos = 7){
  x_mm <- stats::filter(x, rep(1 / periodos, periodos), sides = 1)
  as.numeric(x_mm)
}


plot_line <- function(data, x, y, title = NULL){
  x <- ggplot2::sym(x)
  y <- ggplot2::sym(y)
  
  data %>% 
    ggplot2::ggplot(ggplot2::aes(x = !!x, y = !!y)) +
    ggplot2::geom_line(size = .9) +
    ggplot2::labs(
      x = NULL, y = 'Número de óbitos', title = title
    )
}


plot_breaks <- function(x, obj_brk, nbreaks){
  plot(x)
  lines(fitted(obj_brk, breaks = nbreaks), col = 4)
  lines(confint(obj_brk, breaks = nbreaks))
}

calcular_quebras <- function(x){
  breakpoints(`~`(x, 1), h = .1)
}


posicao_ultima_quebra <- function(obj_brk, nbreaks){
  .breakpoints <- summary(obj_brk)$breakpoints
  
  x <- .breakpoints[as.character(nbreaks),]
  x <- na.omit(x)
  
  return(x[nbreaks])
}


obter_bic_min <- function(obj_brk){
  
  rss <- summary(obj_brk)$RSS
  bic <- rss["BIC", ]
  brk_min <- bic[bic == min(bic)]
  brk_min <- list('quebras' = as.integer(names(brk_min)), 'bic' = as.double(brk_min))
  
  return(brk_min)
}


obter_rss_min <- function(obj_brk){
  
  rss <- summary(obj_brk)$RSS
  rss <- rss["RSS", ]
  brk_min <- rss[rss == min(rss)]
  brk_min <- list('quebras' = as.integer(names(brk_min)), 'rss' = as.double(brk_min))
  
  return(brk_min)
}


calc_intervalo_prev <- function(posicao_quebra, lenx){
  (lenx - posicao_quebra) + 1
}


periodo_prev <- function(x, criterio = 'bic'){
  ## criterio: c('rss', 'bic')
  
  obj_break <- calcular_quebras(x)
  
  if (criterio == 'rss') {
    nbreaks <- obter_rss_min(obj_break)$quebras
  } else if (criterio == 'bic') {
    nbreaks <- obter_bic_min(obj_break)$quebras
  } else {
    stop("Critério de seleção incorreto. O critério deve ser 'rss' ou 'bic'.")
  }
  
  puq <- posicao_ultima_quebra(obj_break, nbreaks)
  intervalo <- calc_intervalo_prev(puq, length(x))
  names(intervalo) <- NULL
  
  return(intervalo)
}
