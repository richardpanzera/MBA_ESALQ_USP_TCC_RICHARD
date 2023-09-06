# Instalação e Carregamento de Todos os Pacotes ---------------------------
library(fpp3)
library(tscount)
library(lmtest)
library(forecast)
library(tools)

#Garantindo replicabilidade
set.seed(0)

#Escolhendo aleatoriamente quais rotas serão modeladas
amostras <- sample.int(23,8)


## Importando dados de Estudo
library(readxl)
Base <- read_excel("G:/Meu Drive/Pessoal/TCC/Padrao_Viagens.xlsx", 
                   sheet = "Base_Final", col_types = c("date", 
                                                     "text", 
                                                     "text", 
                                                     "numeric", 
                                                     "numeric",
                                                     "numeric",
                                                     "numeric",
                                                     "numeric",
                                                     "numeric"))


#Mostra as Rotas escolhidas
Base %>% 
  filter(Rota_N %in% amostras) %>% 
  select(Rota, Carroceria, Rota_N) %>% 
  distinct()


## Testando o Modelo


##Altere o valor da amostra para gerar o estudo
rota <-  amostras[1]

#Filtra oa dados da base total
base_modelo <- Base  %>%  
  filter(Rota_N==rota)

#Prepara a base para criar o primeiro gráfico de TS, ACF e PACF
base_ts <- base_modelo %>% 
  rename(Semana = Indice) %>%  
  as_tsibble(key=c(Rota, Carroceria), index = Data, regular = F)

#gráfico de TS, ACF e PACF
base_ts %>% 
  gg_tsdisplay(viagem,
               plot_type = 'partial', lag=25) +
  labs(title =  paste("Série Temporal:",tools::toTitleCase(tolower(base_ts$Rota[1])),'|', tools::toTitleCase(tolower(base_ts$Carroceria[1]))))

#Escolhendo os indices para gerar a time series
inicio <- min(base_modelo[,9])
fim <- max(base_modelo[,9])
corte <- max(base_modelo[nrow(base_modelo[,9])-14,9]) #separação entre treino e teste

#Isolando a variável dependente (target)
ts_input <- ts(base_modelo$viagem, frequency=7, start = inicio, end= fim)

#Isolando as variáveis explicativas (features)
x_reg <- ts(base_modelo[,c(5:6,9)], frequency=7,start = inicio, end= fim)

#Criando as bases de treio e teste
ts_input_train <- window(ts_input, start=inicio, end=corte)
ts_input_test <- window(ts_input, start=corte+1/7)
x_reg_train <- window(x_reg, start=inicio, end=corte)
x_reg_test <- window(x_reg, start=corte+1/7)

#Modelo tscount
#Atenção neste ponto: usar os valores do trabalho para verificação nas
#no parâmetros past_obs (betas) e past_mean (alphas)
rota_pois <- 
  tsglm(ts_input_train, 
    model = list(past_obs=c(1,7), past_mean = c(7)),
    xreg = x_reg_train,
    link = "log",
    distr = "poisson")

#Resultados
rota_pois %>% 
  summary()  

#Teste dos resíduos
ljung_box(rota_pois$residuals)

#Gráfico análise visual dos erros
#Função criada pelo autor, não se esqueça se rodar o script funcao_plot
plotar(rota_pois$residuals)

#Ajusta as configurações de plotagem
dev.off()

#Armazena os valores preditos da amostra que ficaram de fora
predict <- predict(rota_pois,n.ahead = 14, level = 0.95, newxreg = x_reg_test)$pred

#Compara os erros da amostra treino e teste
round(rbind(forecast::accuracy(rota_pois$ts, rota_pois$fitted.values), forecast::accuracy(ts_input_test, predict)),4)

#plota o gráfico reais vs preditos,
plot_pred(ts_input, rota_pois, x_reg_test)

