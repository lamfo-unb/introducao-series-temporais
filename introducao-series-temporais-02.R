
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(strucchange)

funcs <- new.env()
source("src/funcoes.R", local = funcs, encoding = 'UTF-8')

# Carregando dados -----------------------------------------------------------------------

srag_mg <- read.csv2(file = "data/srag_mg.csv")
srag_mg <- srag_mg %>% 
  mutate(dt_evoluca = ymd(dt_evoluca))

srag_mg <- srag_mg %>% 
  mutate(obitos = funcs$media_movel(obitos)) %>% 
  filter(!is.na(obitos))

srag_mg <- srag_mg %>% 
  filter(dt_evoluca <= as.Date("2020-08-14"))

obitos_mg <- ts(srag_mg$obitos, frequency = 7)

# Séries estacionárias -------------------------------------------------------------------

set.seed(327)
ruido_branco <- ts(rnorm(n = 300))
autoplot(ruido_branco) +
  labs(x = NULL, y = 'yt', title = 'Ruído Branco')

set.seed(327)
passeio_aleatorio <- ts(cumsum(rnorm(n = 300)))
autoplot(passeio_aleatorio) +
  labs(x = NULL, y = 'yt', title = 'Passeio aleatório')


# Processos autoregressivos---------------------------------------------------------------

set.seed(327)
ar1 <- arima.sim(model = list(ar = .8), n = 300)
autoplot(ar1) +
  labs(
    y = expression(paste(y[t])), title = 'Processo AR(1)'
  )

# Processo de médias móveis---------------------------------------------------------------

set.seed(327)
ma1 <- arima.sim(model = list(ma = .8), n = 300)
autoplot(ma1) +
  labs(y = expression(paste(y[t])), title = 'Processo MA(1)')

# Processos ARMA--------------------------------------------------------------------------

set.seed(327)
arma1 <- arima.sim(model = list(ar = .7, ma = .8), n = 300)
autoplot(arma1) +
  labs(y = expression(paste(y[t])), title = 'Processo ARMA(1, 1)')

# Processos ARIMA-------------------------------------------------------------------------

# Simulação
set.seed(327)
arima1 <- arima.sim(model = list(order = c(1, 1, 1), ar = .7, ma = .8), n = 300)
autoplot(arima1) +
  labs(y = expression(paste(y[t])), title = 'Processo ARIMA(1, 1, 1)')


# Série de número de óbitos MG
autoplot(obitos_mg) +
  labs(y = "Número de óbitos diários", title = 'Óbtios por SRAG - MG')


# Estimando modelo Arima  

model_arima <- auto.arima(y = obitos_mg, seasonal = FALSE)
model_arima


# Fazendo previsões

autoplot(forecast(model_arima), size = .9) +
  autolayer(fitted(model_arima), series = 'Ajustado', size = .9) +
  labs(
    x = NULL, y = 'N. óbitos', color = NULL,
    title = 'Previsão do numero de óbitos - MG'
  )


# Comparando modelos
erros <- read.csv2('data/erros-modelos.csv', stringsAsFactors = FALSE)
erro_arima <- accuracy(model_arima)
erros <- rbind(erros, c(modelo = 'Arima', erro_arima)) %>% 
  as.data.frame()

modelos <- erros$modelo
erros$modelo <- NULL
erros <- apply(erros, 2, as.numeric)
row.names(erros) <- modelos

knitr::kable(x = erros, digits = 2, row.names = TRUE)

# Quebras estruturais---------------------------------------------------------------------

# Calculando obtendo as quebras
quebras <- funcs$calcular_quebras(x = obitos_mg)
summary(quebras)$RSS %>% 
  as.data.frame() %>% 
  knitr::kable(digits = 0)


# Plotando as quebras
funcs$plot_breaks(x = obitos_mg, obj_brk = quebras, nbreaks = 5)
