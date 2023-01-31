```{r}
dados4<-dados |>
  dplyr::select(where(is.numeric)) 

# as variaveis que serão convertidas são 


matrix(colnames(dados),ncol = 1)

dados5<-dados 

dados5<-dados |> arrange(dados$age)
FX_age <- cut(dados5$age,
              breaks=c(-Inf,50, 60, 70,80, Inf),
              labels=c("ate50","50_60","60_70","70_80", "80mais"))


dados5$FX_age<-FX_age

# faazendo o mesmo para variável creatinine_phosphokinase

dados5<-dados5 |> arrange(dados5$creatinine_phosphokinase)

table(dados5$creatinine_phosphokinase)

FX_creat <- cut(dados5$creatinine_phosphokinase,
                breaks=c(-Inf,100,200,400, 800, Inf),
                labels=c("ate100","100_200","200_400","400_800","800mais"))

dados5$FX_creat<-FX_creat

table(FX_creat)

prop.table(
  table(FX_creat))


#faazendo para variavel ejection_fraction

dados5<-dados5 |> arrange(dados5$ejection_fraction)

prop.table(
  table(FX_ejec))

FX_ejec <- cut(dados5$ejection_fraction,
               breaks=c(-Inf,30,40, Inf),
               labels=c("ate30","30_40","40mais"))


dados5$FX_ejec<-FX_ejec



# faazendo para variavel platelets

dados$platelets
dados5<-dados5 |> arrange(dados5$platelets)


FX_plat <- cut(dados5$platelets,
               breaks=c(-Inf,200000,250000,289000, Inf),
               labels=c("ate200000","200000_250000", "250000_289000","289000mais"))


prop.table(
  table(FX_plat))
dados5$FX_plat<-FX_plat


# faazendo para variavel serum_creatinine

dados$serum_creatinine

dados5<-dados5 |> arrange(dados5$serum_creatinine)

FX_serum <- cut(dados5$serum_creatinine,
                breaks=c(-Inf,0.90,1.10,1.83, Inf),
                labels=c("ate0.90","0.90_1.10", "1.10_1.83","1.83mais"))


prop.table(
  table(FX_serum))

dados5$FX_serum<-FX_serum
# faazendo para variavel serum_sodium

dados5<-dados5 |> arrange(dados5$serum_sodium) 


FX_serum_so <- cut(dados5$serum_sodium,
                   breaks=c(-Inf,134,137, Inf),
                   labels=c("ate134","134_137","137mais"))



dados5$FX_serum_so<-FX_serum_so

# fazendo para variavel time


dados5<-dados5 |> arrange(dados5$time) 


FX_time <- cut(dados5$time,
               breaks=c(-Inf, 55,103,190, Inf),
               labels=c("ate55","55_103","103_190","190mais"))


prop.table(
  table(FX_time))

dados5$FX_time<-FX_time


dados5<-dados5[-14]


alvo= dados5[6]


write.csv(dados5,"dados_p_dammies.csv",row.names = F,sep = ";")

```

o proximo passo é transformar as variaveis em dammy



```{r}
dados<-read.csv("dados_p_dammies.csv")

library(tidyverse)

#dados[1:6]<-as.character(dados[1:6])


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