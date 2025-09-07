library(olsrr)
library(MASS)

datos <- read.csv("Liga_nacional_de_futbol.csv")
datos<-subset(datos, select = -Equipo)

M1 <- lm(y~.,data=datos,x=TRUE,y=TRUE)
summary(M1)

#1
ols_step_forward_aic(M1,details = TRUE)

#2
ols_step_backward_aic(M1,details = TRUE)

#3
paso_a_paso <- stepAIC(M1,direction = "both",trace = TRUE)
summary(paso_a_paso)