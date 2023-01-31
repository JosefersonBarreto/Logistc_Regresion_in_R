# ==============================================================================
#           análise de sobrevivencia - análise até a ocorrência de mortes
#===============================================================================


library(tidyverse)
library(plotly)
library(survival)
library(survMisc)
library(survminer)


dados<-read.csv("dados1_edit.csv",header = T)

library(survival)
# as analises descritivas estão no script anterior 

 # Estimador de Kaplan-Meier


ek <- survfit(Surv(dados$time,dados$DEATH_EVENT)~dados$sex)


plot(ek, lty=1, xlab="Tempo (semana)",
     #     # Symbol color
     #     col.main = "white",     # Title color
     #     col.sub = "white",       # Subtitle color
     #     col.lab = "white",    # X and Y-axis labels color
     #     col.axis = "white",   # Tick labels color
     #     fg = "white",
     ylab="S(t) Estimada", col = 1, 
     lwd = 2,main = "Curva de Sobrevivência ",conf.int=F)
