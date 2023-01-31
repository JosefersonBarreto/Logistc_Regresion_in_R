
library(tidyverse)

#dados[1:6]<-as.character(dados[1:6])

dados<-dados[-1:-7]
#dados[1:13]<-dados|>
#mutate_if(factor)

cols=colnames(dados)


dados$anaemia<-as.factor(dados$anaemia)

dados$diabetes<-as.factor(dados$diabetes)

dados$high_blood_pressure<-as.factor(dados$high_blood_pressure)


dados$sex<-as.factor(dados$sex)


dados$smoking<-as.factor(dados$smoking)

dados$DEATH_EVENT<-as.factor(dados$DEATH_EVENT)

dados$FX_age<-as.factor(dados$FX_age)


dados$FX_creat<-as.factor(dados$FX_creat)


dados$FX_ejec<-as.factor(dados$FX_ejec)


dados$FX_plat<-as.factor(dados$FX_plat)

dados$FX_serum<-as.factor(dados$FX_serum)


dados$FX_serum_so<-as.factor(dados$FX_serum_so)


dados$FX_time<-as.factor(dados$FX_time)

glimpse(dados)



table(dados$anaemia,dados$DEATH_EVENT)


round(prop.table(table(dados$anaemia,dados$DEATH_EVENT)),2)








#vamos fazer o modelo sem as variaveis categoricas 


dados<-dados[-1:-5]



#transformando as variaveis 






install.packages("fastDummies")

library(fastDummies)
### variaveis que vamos criar as dummies

x=dados[2:8]

alvo<-dados[1]
## Aplicando a funcao que vai gerar todas as dummies
dum2 <- dummy_cols(x, remove_selected_columns = T, remove_most_frequent_dummy = F) # dummies mod1

### Novo dataset com as dummies
bd_dum = data.frame( alvo,  dum2  )





indice_divide_dados <- sample(x = nrow(bd_dum),
                              size = 0.7 * nrow(bd_dum),
                              replace = FALSE)
View(indice_divide_dados)

# Aplicando o índice
dados_treino <- bd_dum[indice_divide_dados,]
dados_teste <- bd_dum[-indice_divide_dados,]





# como temos poucas observações vamos usar o metodo oversampling metodo smoote




# Seed
set.seed(301)

# Pacote
install.packages("DMwR")
#install.packages( "C:/Users/joseferson/Documents/joseferson barreto/DMwR_0.4.1.tar.gz", repos=NULL, type="source" )
library(DMwR)

# SMOTE - Synthetic Minority Oversampling Technique
?SMOTE
dados_treino_balanceados <- SMOTE(DEATH_EVENT ~ ., bd_dum, perc.over = 400, perc.under = 100)

teste<-round(SMOTE(DEATH_EVENT ~ ., dados_treino, perc.over = 1, perc.under = 1),0)
# Checando o balanceamento de classe da variável target
prop.table(table(dados_treino_balanceados$DEATH_EVENT)) * 100

SMOTe

teste<-dados_treino_balanceados |> arrange(DEATH_EVENT)


teste<-dados_treino_balanceados |> arrange(dados_treino_balanceados$FX_age_50_60)


set.seed(100)
train <- sample(nrow(dados_treino_balanceados), 0.70*nrow(dados_treino_balanceados), replace = FALSE)
TrainSet <- dados_treino_balanceados[train,]
TestSet <- dados_treino_balanceados[-train,]


mod1<- glm(DEATH_EVENT ~ .
           ,family = binomial(link='logit') ,na.action = na.fail,
           data = TrainSet 
)


summary(mod1)


step(mod1)



mod2<-glm(formula = DEATH_EVENT ~ FX_age_60_70 + FX_age_70_80 + FX_age_80mais + 
      FX_creat_100_200 + FX_creat_200_400 + FX_creat_400_800 + 
      FX_creat_800mais + FX_ejec_30_40 + FX_ejec_40mais + FX_plat_200000_250000 + 
      FX_plat_250000_289000 + FX_plat_289000mais + FX_serum_0.90_1.10 + 
      FX_serum_1.10_1.83 + FX_serum_1.83mais + FX_serum_so_134_137 + 
      FX_serum_so_137mais + FX_time_103_190 + FX_time_190mais + 
      FX_time_55_103, family = binomial(link = "logit"), data = TrainSet, 
    na.action = na.fail)


summary(mod2)




# analisando a curva rock e acuracia


probs_logistica <- predict(mod2, TestSet ,type='response' )
pred_log_test <-ifelse( probs_logistica > 0.5, 1 , 0)
tab_test = table(pred_log_test,TestSet$DEATH_EVENT)
confusionMatrix( tab_test )

library(ROCR)
perf <- performance(probs_logistica,"tpr","fpr")
plot(perf,colorize=TRUE)


library(ggplot2)
curvas = data.frame( probs = probs_logistica,  grupo = as.factor(TestSet$DEATH_EVENT) )
cols <- c("darkred", "darkblue")


PRROC_obj <- roc.curve(scores.class0 = pro, weights.class0=df$labels,
                       curve=TRUE)
plot(PRROC_obj)

ggplot(curvas, aes(x = probs, color = grupo)) +
  geom_density(alpha = 0.7, aes(x=probs, group=grupo, fill=grupo), adjust=2 ) + 
  scale_fill_manual(values = cols)



### ANALISE DE RESIDUOS 

plot(predict(mod2),residuals(mod2),col=c("blue","red")[1+TrainSet$DEATH_EVENT])
abline(h=0,lty=2,col="grey")





#### Cruva ROC

install.packages("PRROC")
library(ROCit)
PRROC_obj <- PRROC::roc.curve(scores.class0 = probs_logistica, weights.class0=as.factor(TrainSet$DEATH_EVENT),
                              curve=TRUE)


X11()
plot(PRROC_obj)




roc_reg_log <- pROC::roc(pred_roc_reg_log$survived , pred_roc_reg_log$pred_reg_log, percent = TRUE)




pred_roc_reg_log <- 
  dplyr::tibble(
    probs_logistica,
    "survived" = as.factor(as.numeric(TestSet$DEATH_EVENT)-1)
  ) %>% arrange(desc(probs_logistica))



roc_reg_log <- pROC::roc(pred_roc_reg_log$survived , pred_roc_reg_log$probs_logistica, percent = TRUE)


library(pROC)


par(pty = "s")
plot.roc(
  roc_reg_log,
  print.auc = TRUE,
  legacy.axes = TRUE,
  xlab = "Taxa de Falso Positivo (100 - Especificidade)",
  ylab = "Taxa de Verdadeiro Positivo (Sensibilidade)"
)





write.csv(dados_treino_balanceados,"dados_balanc.csv")
