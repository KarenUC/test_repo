##### -------- Limpiar Ambiente ------- ###### 
rm(list = ls())


###### ----- Carga de librerias ------ ####

librerias <- c("readxl","ggplot2","scales","timeSeries","forecast","fOptions", "xlsx", "dplyr", "tidyverse", "skimr", "haven", "lubridate",
               "data.table", "matrixStats","tidyverse")
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))


db<-readRDS("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Intersemestral 2022/Week 1/leverage_dta.Rds")

require("tidyverse")
ggplot(db) +
  geom_point(aes(x=x,y=y))

reg1<-lm(y~x,data=db)
summary(reg1)

require("stargazer")
stargazer(reg1,type="text")

db<- db %>% mutate(ej=c(rep(0,30),1))
head(db)

reg2<-lm(y~x+ej,db)
stargazer(reg1,reg2,type="text")

db<-db %>% mutate(res_y_e=lm(y~ej,db)$residuals,
                  res_x_e=lm(x~ej,db)$residuals,
)
reg3<-lm(res_y_e~res_x_e,db)
stargazer(reg1,reg2,reg3,type="text")

db<-db %>% mutate(res_y_x=lm(y~x,db)$residuals,
                  res_e_x=lm(ej~x,db)$residuals,
)
reg4<-lm(res_y_x~res_e_x,db)
stargazer(reg1,reg2,reg3,reg4,type="text")

u<-lm(y~x,data=db)$residual[31]
u

h<-lm.influence(reg1)$hat[31]
h

alpha<-u/(1-h)
alpha

### EJEMPLO OPTIMIZACIÓN GRADIENT 

