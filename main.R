library(dplyr)
library(lubridate)

funcs <- new.env()
source("src/funcoes.R", local = funcs, encoding = 'UTF-8')


srag_mg <- read.csv2(
  file = 'apresentacoes/data/srag_mg.csv', stringsAsFactors = FALSE
)

srag_mg <- srag_mg %>% 
  mutate(dt_evoluca = ymd(dt_evoluca))


funcs$plot_line(
  data = srag_mg, x = 'dt_evoluca', y = 'obitos',
  title = 'Óbitos por SRAG - MG'
)

srag_mg2 <- srag_mg %>% 
  mutate(
    obitos = funcs$media_movel(obitos)
  ) %>%
  filter(!is.na(obitos))


funcs$plot_line(
  data = srag_mg2, x = 'dt_evoluca', y = 'obitos',
  title = 'Óbitos por SRAG - MG'
)
