#normalidad yay
install.packages("datarium")
library(datarium)
data("marketing")
modeloCompleto<-lm(sales~youtube+facebook+newspaper,data = marketing)
# pruebda de normalidad
#
# h_0 la distribucion es normal
# H_1 no son normales
residuales<- modeloCompleto$residuals
shapiro.test(residuales)
qqnorm(residuales)
qqline(residuales)

#el modelo completo no es normal debido a que el p_valor es mucho menor a nuestro alpha(0.05)
modeloSinNewspaper<-lm(sales~youtube+facebook,data = marketing)
residuales2<- modeloSinNewspaper$residuals
shapiro.test(residuales2)
qqnorm(residuales2)
qqline(residuales2)

#el modelo sin NewsPaper no es normal debido a que el p_valor es mucho menor a nuestro alpha(0.05)