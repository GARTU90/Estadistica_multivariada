library(ggplot2)
library(olsrr)
library(leaps)
library(GGally)

datos<-read.csv("Rendimiento_de_gasolina.csv",header=T)

## inspeccionando el csv vemos que faltan un par de datos en la variable x3, para no afectar el modelo,
## borraremos ambas ovservaciones ya que considero que usar el promedio o cualquier otra transformacion
## seria demasiado

datos$Automóvil<-NULL
datos<-na.omit(datos)

ggpairs(datos)

M_completo<- lm(y~ .,datos,x=TRUE,y=TRUE)
summary(M_completo)

## AIC ,CIS, C_p
outs<- leaps(M_completo$x,datos$y,int=FALSE)

plot(outs$size,outs$Cp,log = "y",cex=0.3)
lines(outs$size,outs$size)
text(outs$size,outs$Cp,labels = row(outs$which),cex = 0.5,pos=4)
#ahi nuestro cp = p en este caso con 11 por lo que se puede apreciar en la grafica

idx_best_rule<- which.min(abs(outs$Cp - outs$size))
noms_x <- colnames(M_completo$x)
vars_rule<- noms_x[outs$which[idx_best_rule,]]
vars_rule

## 

ejemplo <- ols_step_all_possible(M_completo)
plot(ejemplo)
ejemplo

### seleccion paso a paso 

modelo_completo<- lm(y ~ . , datos)

paso_a_paso <- stepAIC(modelo_completo,direction = "both",trace = TRUE)
summary(paso_a_paso)
###   variables encontradas: x5 x8 y x10

ols_step_forward_aic(modelo_completo,details = TRUE)
###   variables encontradas: x1 y x4

ols_step_backward_aic(modelo_completo,details = TRUE)
##    variables encontradas: x5, x8 y x10

## aqui creamos los modelos que nos arrojo cada funcion
modelo_both<-lm(y~x5+x8+x10,datos)
modelo_forward<-lm(y~x1+x4,datos)
modelo_backward<-lm(y~x5+x8+x10,datos)

AIC(modelo_both,modelo_forward,modelo_backward)
### podemos ver que el aic mas bajo fueron en este caso backward y both, que dieron con el mismo modelo

summary(modelo_both)
### analizando el anova del modelo con menor AIC
### vemos que ahora si todos sus parametros son significativos, un error resifual menor al completo
### unos r cuadrados mas parecidos e igual de altos  y ya 


### que significa esto apra nosotros en la vida real?
### pues que hay una relacion fuerte entre la relacion del eje tracero con la longitud del vehiculo total y su peso
### el otro modelo implicaba una relacion entre la cilindrada y la relacion de comprecion
### lo cual tiene tambien muchisimo sentido!
### ya que en la vida real es inveitable que un motor con mas cilindros y mas inyeccion de gasolina gaste mass 
### y los otros modelos siguieren una relacion de peso y dimenciones con el desempeño de gasolina

### conclusion!
### ambos modelos tienen bastante sentido en la vida real, debido a eso
### yo en lo personal suguiero el modelo con menos variables ya que es mas sencillo y se acerca bastante al modelo y ademas 
### tiene sentido en la vida real, ya despues los calculos y eso se peuden pasar aocn el otro modelo apra seguir considerando el
### peso y las idmensiones como para hacer un doble analisis





