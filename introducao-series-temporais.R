
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

funcs <- new.env()
source("src/funcoes.R", local = funcs, encoding = 'UTF-8')

# Séries temporais------------------------------------------------------------------------
# Carregando e visualizando índice de produção industrial
path_data <- "data/producao-industrial.csv"
dados <- read.csv2(file = path_data, stringsAsFactors = FALSE)
head(dados) %>% 
  knitr::kable()


produto <- ts(
  data = dados$indice, start = c(2002, 1), frequency = 12
)
autoplot(produto, size = .9) +
  labs(
    x = NULL, y = 'Índice', title = 'Índice de produção industrial'
  )

# Decompondo índide de produção industrial
dec <- decompose(produto)
plot(dec)

# SES-------------------------------------------------------------------------------------

# Carregando e visualizando os dados de inflação mensal
path_data <- "data/variacao-mensal-ipca2.csv"
inf_mensal <- read.csv2(path_data, stringsAsFactors = FALSE)
head(inf_mensal) %>% knitr::kable()

inf_mensal <- ts(
  data = inf_mensal$var_mensal, start = c(2016, 06), frequency = 12
)

autoplot(inf_mensal, size = .9) + 
  labs(x = 'Periodo', y = 'Inflação mensal (%)', size = .9)

# Fazendo previsões usando modelo SES
model_ses <- ses(inf_mensal)

autoplot(model_ses, size = .9) +
  autolayer(fitted(model_ses), series = 'Ajustado', size = .9) +
  labs(y = 'Inflação mensal (%)', color = NULL)

# Holt------------------------------------------------------------------------------------

# Carregando e visualizando número de óbitos por
# SRAG em Minas Gerais
srag_mg <- read.csv2(file = "data/srag_mg.csv")
srag_mg <- srag_mg %>% 
  mutate(dt_evoluca = ymd(dt_evoluca))

tail(srag_mg) %>% knitr::kable()


funcs$plot_line(
  data = srag_mg, x = 'dt_evoluca', y = 'obitos',
  title = 'Óbitos por SRAG - MG'
)

# Calculando a média móvel dos dados
srag_mg <- srag_mg %>% 
  mutate(obitos = funcs$media_movel(obitos)) %>% 
  filter(!is.na(obitos))

funcs$plot_line(
  data = srag_mg, x = 'dt_evoluca', y = 'obitos',
  title = 'Óbitos por SRAG - MG'
)

# Selecionando dados até 2020-08-14
srag_mg <- srag_mg %>% 
  filter(dt_evoluca <= as.Date("2020-08-14"))

funcs$plot_line(
  data = srag_mg, x = 'dt_evoluca', y = 'obitos',
  title = 'Óbitos por SRAG - MG'
)

# Realizando previsões com o modelo Holt
obitos_mg <- ts(srag_mg$obitos, frequency = 7)
model_holt <- holt(y = obitos_mg)

autoplot(model_holt, size = .9) +
  autolayer(fitted(model_holt), series = 'Ajustado', size = .9) +
  labs(x = NULL, y = 'Número de óbitos', color = NULL)

accuracy(model_holt) %>% 
  knitr::kable()

forecast(model_holt) %>% 
  as.data.frame() %>% 
  head() %>% 
  knitr::kable(row.names = FALSE, digits = 2)


# Holt-Winters----------------------------------------------------------------------------

model_hw_add <- hw(y = obitos_mg, h = 10, seasonal = 'additive')
autoplot(model_hw_add, size = .9) +
  autolayer(fitted(model_hw_add), series = 'Ajustado', size = .9) +
  labs(y = 'Número de óbitos', color = NULL)


obitos_mg_mod <- obitos_mg[obitos_mg > 0]
obitos_mg_mod <- ts(obitos_mg_mod, frequency = 7)

model_hw_mult <- hw(y = obitos_mg_mod, h = 10, seasonal = 'multiplicative')
autoplot(model_hw_mult, size = .9) +
  autolayer(fitted(model_hw_add), series = 'Ajustado', size = .9) +
  labs(y = 'Número de óbitos', color = NULL)


erros <- rbind(
  accuracy(model_holt), accuracy(model_hw_add),
  accuracy(model_hw_mult)
) 
erros <- data.frame(erros)
row.names(erros) <- c('Holt', 'HW-Aditivo', 'HW-Multiplicativo')

knitr::kable(erros, digits = 3)

