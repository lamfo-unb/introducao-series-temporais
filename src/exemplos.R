
library(forecast)
library(dplyr)
library(lubridate)

set.seed(527)
ar1 <- arima.sim(n = 100, list(ar = .6))
model_ses <- forecast::ses(ar1)



autoplot(model_ses, size = .9) + 
  autolayer(fitted(model_ses), series = 'Ajustado', size =.9) +
  labs(
    x = 'Periodo',
    y = 'y',
    title = 'Previsão por Suavização Exponencial Simples',
    color = NULL
  )


dados <- read.csv2(
  file = 'apresentacoes/data/tab_srag_tratada_20200915.csv', stringsAsFactors = FALSE
)

dados_mg <- dados %>% 
  filter(sg_uf == 'MG') %>% 
  filter(dt_evoluca != '', sg_uf != '')

dados_mg <- dados_mg %>% 
  mutate(dt_evoluca = ymd(dt_evoluca)) %>% 
  arrange(dt_evoluca)

write.csv2(
  x = dados_mg, file = 'apresentacoes/data/srag_mg.csv', row.names = FALSE
)



media_movel(dados_mg$obitos)


