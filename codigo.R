#Bibliotecas:
library(tidyverse) 
library(caret)
library(glmnet)
library(knitr)
library(kableExtra)
library(gridExtra)
library(mlbench)
library(plotmo)

#Funções auxiliares:
box_plot = function(x, y, data) {
  ggplot(data, aes(x = as.factor(.data[[x]]), y = .data[[y]],
                   fill = as.factor(.data[[x]]))) +
    geom_boxplot(outlier.size = 0.7) +
    labs(x = x,y = y,fill = x) +
    theme(legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.line = element_line(colour = "black"),
          panel.background = element_rect(fill = "white", size = 2),
          panel.grid.major = element_line(colour = "gray",size = 0.5,
                                          linetype = "solid"))
}

hist_plot = function(x, data){
  ggplot(data, aes(x = .data[[x]])) +
    geom_histogram(size = 1, alpha = 1, fill = 'dodgerblue', col = 'black',
                   bins = 30) +
    labs(x = x, y = "Frequência") +
    theme(axis.line = element_line(colour = "black"),
          panel.background = element_rect(fill = "white", size = 2),
          panel.grid.major = element_line(colour = "gray",size = 0.5,
                                          linetype = "solid"))
}

pontos_plot = function(x, y, data) {
  ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(size = 1, alpha = 1, color = 'dodgerblue') +
    labs(x = x, y = y) +
    theme(axis.line = element_line(colour = "black"),
          panel.background = element_rect(fill = "white", size = 2),
          panel.grid.major = element_line(colour = "gray",size = 0.5,
                                          linetype = "solid"))
}

barras_plot = function(x, y, data) {
  ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[x]])) +
    geom_col(position = "dodge") +
    labs(x = x, y = y) +
    theme(axis.line = element_line(colour = "black"),
          panel.background = element_rect(fill = "white", size = 2),
          panel.grid.major = element_line(colour = "gray",size = 0.5,
                                          linetype = "solid"))
}

#Importação dos dados
dados <- read_csv("framingham.csv")

#Exploratória

## Estrutura dos dados:
glimpse(dados)

## Análise e eliminação de variável faltante:
sum(is.na(dados))
dados <- dados %>% drop_na() %>%
  mutate(male = as.integer(male),
         currentSmoker = as.integer(currentSmoker), 
         BPMeds = as.integer(BPMeds),
         prevalentStroke = as.integer(prevalentStroke), 
         prevalentHyp = as.integer(prevalentHyp),
         TenYearCHD = as.integer(TenYearCHD))

## Sumário dos dados
summary(dados)

## Tabelas de contingência e testes de associação
testes=data.frame()
k=0
for (i in c(1,4,6,7,8,9)) {
  tab = table(dados[[16]],dados[[i]])
  test.chisq = chisq.test(tab)
  k= k + 1
  testes[k,1] = names(dados)[16]
  testes[k,2] = names(dados)[i]
  testes[k,3] = test.chisq$p.value %>% round(4)
}
names(testes) = c("Var1","Var2","P-valor")
(testes)

testes2=data.frame()
k2 = 0
for (i in 2:9) {
  test2.chisq = chisq.test(tabs[,i])
  k2= k2 + 1
  testes2[k2,1] = names(dados)[16]
  testes2[k2,2] = colnames(tabs[,i])
  testes2[k2,3] = test.chisq$p.value %>% round(4)
}
(testes2)

## Mais tabelas das colunas contínuas (agrupadas pela média)
tabs = dados %>% group_by(TenYearCHD) %>% 
  summarise(age = mean(age) %>% round(2),
            education = mean(education) %>% round(2),
            cigsPerDay = mean(cigsPerDay) %>% round(2),
            sysBP = mean(sysBP) %>% round(2),
            diaBP = mean(diaBP) %>% round(2),
            BMI = mean(BMI) %>% round(2),
            heartRate = mean(heartRate) %>% round(2),
            glucose = mean(glucose)) 
(tabs)

## Criação dos gráficos com as distribuições dos dados:

g1 <- barras_plot("male", "Quantidade",
                  dados %>% dplyr::group_by(male) %>% 
                    mutate(male = factor(male)) %>%
                    summarise(Quantidade = n()))
g2 <- hist_plot("age", dados)
g3 <- barras_plot("education", "Quantidade",
                  dados %>% dplyr::group_by(education) %>% 
                    mutate(education = factor(education)) %>%
                    summarise(Quantidade = n()))
g4 <- barras_plot("currentSmoker",
                  "Quantidade",
                  dados %>% dplyr::group_by(currentSmoker) %>% 
                    mutate(currentSmoker = factor(currentSmoker)) %>%
                    summarise(Quantidade = n()))
g5 <- hist_plot("cigsPerDay", dados)
g6 <- barras_plot("BPMeds", "Quantidade",
                  dados %>% dplyr::group_by(BPMeds) %>% 
                    mutate(BPMeds = factor(BPMeds)) %>%
                    summarise(Quantidade = n()))
g7 <- barras_plot("prevalentStroke", "Quantidade",
                  dados %>%
                    dplyr::group_by(prevalentStroke) %>% 
                    mutate(prevalentStroke = factor(prevalentStroke)) %>%
                    summarise(Quantidade = n()))
g8 <- barras_plot("prevalentHyp", "Quantidade", 
                  dados %>%
                    dplyr::group_by(prevalentHyp) %>%
                    mutate(prevalentHyp = factor(prevalentHyp)) %>%
                    summarise(Quantidade = n()))
g9 <- barras_plot("diabetes", "Quantidade",
                  dados %>% dplyr::group_by(diabetes) %>%
                    mutate(diabetes = factor(diabetes)) %>%
                    summarise(Quantidade = n()))
g10 <- hist_plot("totChol", dados)
g11 <- hist_plot("sysBP", dados)
g12 <- hist_plot("diaBP", dados)
g13 <- hist_plot("BMI", dados)
g14 <- hist_plot("heartRate", dados)
g15 <- hist_plot("glucose", dados)
g16 <- barras_plot("TenYearCHD", "Quantidade",
                   dados %>% dplyr::group_by(TenYearCHD) %>% 
                     mutate(TenYearCHD = factor(TenYearCHD)) %>%
                     summarise(Quantidade = n()))
                   
#male vs TenYearCHD
TenYearCHD_male <-  dados %>% dplyr::group_by(TenYearCHD, male) %>% 
  mutate(TenYearCHD = factor(TenYearCHD),
         male = factor(male)) %>% 
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = male, y = Quantidade, fill = TenYearCHD)) +  
  geom_col(position = "dodge") +
  theme_bw()

#age vs TenYearCHD
TenYearCHD_age <-  dados %>% dplyr::group_by(TenYearCHD, age) %>% 
  mutate(TenYearCHD = factor(TenYearCHD)) %>%
  ggplot(aes(y = TenYearCHD, x = age, fill = TenYearCHD)) +  
  geom_boxplot() +
  theme_bw()

#education vs TenYearCHD
TenYearCHD_education <-  dados %>% 
  dplyr::group_by(TenYearCHD, education) %>% 
  mutate(TenYearCHD = factor(TenYearCHD),
         age = factor(education)) %>%
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = education, y = Quantidade, fill = TenYearCHD)) +  
  geom_col(position = "dodge") +
  theme_bw()

#currentSmoker vs TenYearCHD
TenYearCHD_currentSmoker <-  dados %>%
  dplyr::group_by(TenYearCHD, currentSmoker) %>% 
  mutate(TenYearCHD = factor(TenYearCHD),
         currentSmoker = factor(currentSmoker)) %>% 
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = currentSmoker, y = Quantidade, fill = TenYearCHD)) +  
  geom_col(position = "dodge") +
  theme_bw()

#cigsPerDay vs TenYearCHD
TenYearCHD_cigsPerDay <- dados %>%
  dplyr::group_by(TenYearCHD, cigsPerDay) %>% 
  mutate(TenYearCHD = factor(TenYearCHD)) %>%
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = cigsPerDay, y = Quantidade, col = TenYearCHD)) +  
  geom_point(size =2, alpha = 1, 
             position = position_jitter(width =  0.2),
             stat = "identity")  +
  theme_bw()

#BPMeds vs TenYearCHD
TenYearCHD_BPMeds <- dados %>% dplyr::group_by(TenYearCHD, BPMeds) %>% 
  mutate(TenYearCHD = factor(TenYearCHD),
         currentSmoker = factor(BPMeds)) %>% 
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = BPMeds, y = Quantidade, fill = TenYearCHD)) +  
  geom_col(position = "dodge") +
  theme_bw()

#prevalentStroke vs TenYearCHD
TenYearCHD_prevalentStroke <- dados %>%
  dplyr::group_by(TenYearCHD, prevalentStroke) %>% 
  mutate(TenYearCHD = factor(TenYearCHD),
         prevalentStroke = factor(prevalentStroke)) %>% 
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = prevalentStroke, y = Quantidade, fill = TenYearCHD)) +  
  geom_col(position = "dodge") +
  theme_bw()

#prevalentHyp vs TenYearCHD
TenYearCHD_prevalentHyp <- dados %>%
  dplyr::group_by(TenYearCHD, prevalentHyp) %>% 
  mutate(TenYearCHD = factor(TenYearCHD),
         prevalentHyp = factor(prevalentHyp)) %>% 
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = prevalentHyp, y = Quantidade, fill = TenYearCHD)) +  
  geom_col(position = "dodge") +
  theme_bw()

#diabetes vs TenYearCHD
TenYearCHD_diabetes <- dados %>%
  dplyr::group_by(TenYearCHD, diabetes) %>% 
  mutate(TenYearCHD = factor(TenYearCHD),
         diabetes = factor(prevalentHyp)) %>% 
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = diabetes, y = Quantidade, fill = TenYearCHD)) +  
  geom_col(position = "dodge") +
  theme_bw()

#totChol vs TenYearCHD
TenYearCHD_totChol <-  dados %>%
  dplyr::group_by(TenYearCHD, totChol) %>% 
  mutate(TenYearCHD = factor(TenYearCHD)) %>%
  ggplot(aes(y = TenYearCHD, x = totChol, fill = TenYearCHD)) +  
  geom_boxplot() +
  theme_bw()

#sysBP vs TenYearCHD
TenYearCHD_sysBP <-  dados %>% dplyr::group_by(TenYearCHD, sysBP) %>% 
  mutate(TenYearCHD = factor(TenYearCHD)) %>%
  ggplot(aes(y = TenYearCHD, x = sysBP, fill = TenYearCHD)) +  
  geom_boxplot() +
  theme_bw()

#diaBP vs TenYearCHD
TenYearCHD_diaBP <-  dados %>%
  dplyr::group_by(TenYearCHD, diaBP) %>% 
  mutate(TenYearCHD = factor(TenYearCHD)) %>%
  ggplot(aes(y = TenYearCHD, x = diaBP, fill = TenYearCHD)) +  
  geom_boxplot() +
  theme_bw()

#BMI vs TenYearCHD
TenYearCHD_BMI <-  dados %>%
  dplyr::group_by(TenYearCHD, diaBP) %>% 
  mutate(TenYearCHD = factor(TenYearCHD)) %>%
  ggplot(aes(y = TenYearCHD, x = BMI, fill = TenYearCHD)) +  
  geom_boxplot() +
  theme_bw()

#heartRate vs TenYearCHD
TenYearCHD_heartRate <-  dados %>%
  dplyr::group_by(TenYearCHD, heartRate) %>% 
  mutate(TenYearCHD = factor(TenYearCHD)) %>%
  ggplot(aes(y = TenYearCHD, x = heartRate, fill = TenYearCHD)) +  
  geom_boxplot() +
  theme_bw()

#glucose vs TenYearCHD
TenYearCHD_glucose <-  dados %>%
  dplyr::group_by(TenYearCHD, glucose) %>% 
  mutate(TenYearCHD = factor(TenYearCHD)) %>%
  ggplot(aes(y = TenYearCHD, x = glucose, fill = TenYearCHD)) +  
  geom_boxplot() +
  theme_bw()

## Visualização dos gráficos acima:
g1
g2
g3
g4
g5
g6
g7
g8
g9
g10
g11
g12
g13
g14
g15
g16
TenYearCHD_male
TenYearCHD_age
TenYearCHD_education
TenYearCHD_currentSmoker
TenYearCHD_cigsPerDay
TenYearCHD_BPMeds
TenYearCHD_prevalentStroke
TenYearCHD_prevalentHyp
TenYearCHD_diabetes
TenYearCHD_totChol
TenYearCHD_sysBP
TenYearCHD_diaBP
TenYearCHD_BMI
TenYearCHD_heartRate
TenYearCHD_glucose

# Modelagem dos dados:

## Separação em dados de teste e treino:
set.seed(123)
train.samp <- dados$TenYearCHD %>% 
  createDataPartition(p=0.8, list = FALSE)
train.data <- dados[train.samp,]
test.data <- dados[-train.samp,]

## Matrizes X e Y
x <- as.matrix(train.data[,-1])
y <- train.data$TenYearCHD %>% matrix(ncol = 1)
x.test <- as.matrix(TenYearCHD ~., test.data[,-c(16)])
y.test <- test.data$TenYearCHD %>% matrix(ncol = 1)

## Validação cruzada para lasso:
set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1,
                      family = "binomial", lambda = NULL)
(cv.lasso)
plot(cv.lasso)

## Modelo lasso usando menor valor de lambda:
model_min <- glmnet(x, y, alpha = 1, family = "binomial",
                    lambda = cv.lasso$lambda.min)

predicted.classes <- 
  ifelse(predict(model_min, newx = x.test) > 0.5,  1, 0) %>%
  matrix(ncol = 1)
class(predicted.classes)
class(y.test)
table(predicted.classes,  as.factor(y.test))

##  Modelo lasso usando uma desvio padrão acima do valor mínimo de lambda:
model_1se <- glmnet(x, y, alpha = 1, family = "binomial",
                    lambda = cv.lasso$lambda.1se)
predicted.classes <-
  ifelse(predict(model_1se, newx = x.test) > 0.5,  1, 0) %>%
  matrix(ncol = 1)
class(predicted.classes)
class(y.test)
table(predicted.classes,  as.factor(y.test))

## Validação cruzada para ridge:
set.seed(123)
cv.ridge <- cv.glmnet(x, y, alpha = 0, family = "binomial", lambda = NULL)
(cv.ridge)
plot(cv.ridge)

## Modelo ridge usando menor valor de lambda:
modell_min <- glmnet(x, y, alpha = 0, family = "binomial",
                     lambda = cv.ridge$lambda.min)
predicted.classes <- 
  ifelse(predict(modell_min, newx = x.test) > 0.5,  1, 0) %>%
  matrix(ncol = 1)
class(predicted.classes)
class(y.test)
table(predicted.classes,  as.factor(y.test))

##  Modelo ridge usando uma desvio padrão acima do valor mínimo de lambda:
modell_1se <- glmnet(x, y, alpha = 0, family = "binomial",
                     lambda = cv.ridge$lambda.1se)
predicted.classes <- 
  ifelse(predict(modell_1se, newx = x.test) > 0.5,  1, 0) %>%
  matrix(ncol = 1)
class(predicted.classes)
class(y.test)
table(predicted.classes,  as.factor(y.test))

# Simulação:

## Função de criação dos dados 
simulacao = function(sample_size = 10000, beta_0 = -4, beta_1 = 7, 
                     beta_2 = 5, beta_3 = -4, beta_4 = 2, beta_5 = 3, 
                     beta_6 = 0, beta_7 = 0, beta_8 = 0,
                     beta_9 = 0, beta_10 = 0) {
  
  #binaria
  variavel_1 <- sample(c(0,1), sample_size, replace = TRUE,
                       prob = c(0.45, 0.55))
  variavel_2 <- rnorm(sample_size, 5, 10)
  variavel_3 <- rnorm(sample_size, 20, 25)
  #ordinária
  variavel_4 <- sample(c(0,1,2,3), sample_size, replace = TRUE)
  #valores proximos de 0
  variavel_5 <- rnorm(sample_size, 0, 0.75)
  #binaria
  variavel_6 <- sample(c(0,1), sample_size, replace = TRUE,
                       prob = c(0.25, 0.75))
  variavel_7 <- rnorm(sample_size, 10, 5)
  variavel_8 <- rnorm(sample_size, 2, 2)
  #valores proximos de 0
  variavel_9 <- rnorm(sample_size, 0, 0.25)
  #ordinária
  variavel_10 <- sample(c(0,1,2,3), sample_size, replace = TRUE,
                        prob = c(0.15, 0.30, 0.35, 0.20))
  
  xb <- beta_0 + beta_1 * variavel_1 + beta_2 * variavel_2 +
    beta_3 * variavel_3 + beta_4 * variavel_4 + 
    beta_5 * variavel_5 + beta_6 * variavel_6 + 
    beta_7 * variavel_7 + beta_8 * variavel_8 +
    beta_8 * variavel_8 + beta_9 * variavel_9 +
    beta_10 * variavel_10
  
  y <- ifelse((1 - plogis(xb)) >= 0.5, 1, 0)
  data.frame(variavel_1, variavel_2, variavel_3, variavel_4, 
             variavel_5, variavel_6, variavel_7, variavel_8,
             variavel_9, variavel_10, y)
}

## Dados simulados
set.seed(123)
dados_sim = simulacao()
head(dados_sim)

X_sim <- dados_sim %>% select(-y) %>% as.matrix()
Y_sim <- dados_sim %>% select(y) %>% as.matrix()

mod_ridge_sim <- glmnet(X_sim, Y_sim, alpha = 0, 
                        family = "binomial")
plot_glmnet(mod_ridge_sim, label = 5)

mod_lasso_sim <- glmnet(X_sim, Y_sim, alpha = 1, 
                        family = "binomial")
plot_glmnet(mod_lasso_sim, label = 5)