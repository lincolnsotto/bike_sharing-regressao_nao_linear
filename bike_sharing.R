##################################################################################
#               REGRESSÃO NÃO LINEAR COM TRANSFORMAÇÃO DE BOX COX                #
#                          PREVENDO O ALUGUEL DE BIKES                           #
##################################################################################

# Link para download e maiores informações sobre o dataset:
# https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

##################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################

pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando o dataset
base_bikes <- read_csv("day.csv")
View(base_bikes)

# Verificando os formatos das variáveis
glimpse(base_bikes)

# Para modelos de regressão devemos realizar um processo de transformação das 
# variáveis do tipo categórica, esse processo consiste em modificá-las para uma 
# matriz de 0 e 1, onde 0 representa a ausência do evento e 1 a presença

# Modificando o tipo da variável
base_bikes$season <- as.factor(base_bikes$season)
base_bikes$yr <- as.factor(base_bikes$yr)
base_bikes$mnth <- as.factor(base_bikes$mnth)
base_bikes$holiday <- as.factor(base_bikes$holiday)
base_bikes$weekday <- as.factor(base_bikes$weekday)
base_bikes$workingday <- as.factor(base_bikes$workingday)
base_bikes$weathersit <- as.factor(base_bikes$weathersit)

# Removendo variáveis que não iremos utilizar inicialmente no nosso modelo
#base_bikes$instant <- NULL
#base_bikes$dteday <- NULL

#Verificando a correlação de Pearson entre as variáveis numéricas
chart.Correlation(
  base_bikes[10:16],
  method = c("pearson"),
)

# Verificando os formatos das variáveis
glimpse(base_bikes)

# Aplicando função para "dummizar" as variáveis
base_bikes_dummies <- dummy_columns(.data = base_bikes, 
                             select_columns = c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit"),
                             remove_selected_columns = T,
                             remove_most_frequent_dummy = T)

# Agora que temos a base adequada, vamos iniciar as etapas para treinar o modelo

# Como estamos diante de uma distribuição Não Linear, devemos aplicar a função canônica 
# de Box Cox na variviável Y (dependente)
lambda_cnt <- powerTransform(base_bikes_dummies$cnt)
base_bikes_dummies$cnt_bc <- (((base_bikes_dummies$cnt ^ lambda_cnt$lambda) - 1) / 
                 lambda_cnt$lambda)

# Treinando o modelo por OLS
modelo_bikes <- lm(formula = cnt_bc ~ . 
                       -instant
                       -dteday
                       -casual
                       -registered
                       -cnt,
                       data = base_bikes_dummies)

# Vamos avaliar alguns retornos estatísticos para entender o modelo
summary(modelo_bikes)

# Removendo variváveis estatísticamente insignificantes para o modelo
modelo_bikes <- step(modelo_bikes, K = 3.841459)

# Vamos avaliar alguns retornos estatísticos para entender o modelo
summary(modelo_bikes)

# Inserindo a nova variável com os valores previstos na base original
base_bikes$yhat <- (((modelo_bikes$fitted.values*(lambda_cnt$lambda))+
                     1))^(1/(lambda_cnt$lambda))

# Visualizando a base dados com a nova variávek yhat contendo os dados previstos
View(base_bikes)

# Plotando o valor previsto Vs o valor original
ggplot(data = base_bikes)+
  geom_point(mapping = aes(x = instant, y = cnt, color = "Real"), size = 2)+
  geom_point(mapping = aes(x = instant, y = yhat, color = "Previsto"), size = 1)+
  labs(x = "ID_Registro", y = "Qntde de Bikes")+  
  scale_color_discrete(name = "Legenda", labels = c("Previsto", "Real"))

# Aparentemente o modelo tem um fit alto para previsão da nossa demanda de bikes, 
# antes de proceguir vamos realizar alguns testes estatísticos para eliminar qualquer viés

# Shapiro Francia - teste de normalidade dos resíduos
# Antes de executar o teste, vamos aumentar a capacidade de analise (necessário para dataset > 5k de registros)
# Pacote nortest, função sf.test
sf.test2 <- function (x) 
{
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if ((n < 5 || n > 50000)) 
    stop("sample size must be between 5 and 5000")
  y <- qnorm(ppoints(n, a = 3/8))
  W <- cor(x, y)^2
  u <- log(n)
  v <- log(u)
  mu <- -1.2725 + 1.0521 * (v - u)
  sig <- 1.0308 - 0.26758 * (v + 2/u)
  z <- (log(1 - W) - mu)/sig
  pval <- pnorm(z, lower.tail = FALSE)
  RVAL <- list(statistic = c(W = W), p.value = pval, method = "Shapiro-Francia normality test", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}

# Agora que aumentamos a capacidade de análise do teste, pode executá-lo
sf.test2(modelo_bikes$residuals)
# Não passou

# Plotando os resíduos do nosso modelo
base_bikes %>%
  mutate(residuos = modelo_bikes$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

# Resíduos do modelo comparados com uma distribuição normal
base_bikes %>%
  mutate(residuos = modelo_bikes$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_bikes$residuals),
                            sd = sd(modelo_bikes$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()


# Heterocedasticidade (Breausch - Pagan)
ols_test_breusch_pagan(modelo_bikes)
# Não passou 

# Multicolinearidade
ols_vif_tol(modelo_bikes)
# Não passou