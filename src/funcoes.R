library(ggplot2)
library(magrittr)


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