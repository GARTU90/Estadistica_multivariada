###############################################################################################
#Ajustar un modelo de regresión lineal múltiple que relacione la cantidad de juegos ganados 
#con las yardas por aire del equipo (x2), el porcentaje de jugadas por tierra (x7) y las yardas 
#por tierra del contrario (x8).
###############################################################################################
datos=read.csv("Liga_nacional_de_futbol.csv")
datos
M1 <- lm(y~x2+x7+x8,data=datos)
M1

############################################################################################
###     b) Formar la tabla de análisis de varianza y probar la significancia de la regresión
############################################################################################
nrow(datos)

idv <- rep(1,nrow(datos))
idv

X<- matrix(c(idv,datos$x2,datos$x7,datos$x8),nrow=28,ncol=4)
Y<- matrix(datos$y,nrow=28,ncol=1)

beta<- solve(t(X)%*%X) %*% t(X) %*% Y
beta 

#pues para poder hacer la tabla necesimatos calcular lo siguiente 

#Fuente de variacion  | Suma de cuadrados | Grados de libertad  | Cuadrados medios  | F0 
#regrecion            | SCE               | p                   | CMR               | CMR/CMRES 
#residuales           | SSE               | n-p-1               | CMRES 
#total                | SCT               | n-1


#p  numero de columnas a usar
p <- nrow(beta)-1
p

#n  numero de filas
n<- nrow(datos)
n

#n-p-1
n-p-1

#n-1
n-1

#SCE
SCE<- t(beta)%*% t(X)%*%Y-sum(Y)**2/n
SCE

#SSE
SSE<-t(Y)%*% Y- t(beta) %*% t(X) %*% Y
#SCT
SCT <- t(Y) %*% Y-sum(Y)**2/n
SCT

#SSE
SSE<-SCT-SCE
SSE

SCT.m <- sum((datos$y-mean(datos$y))**2)
SCT.m

SCE.m<- sum((M1$fitted - mean(datos$y))**2)
SCE.m

SSE.m<-sum(M1$residuals**2)
SSE.m

GLT<-n-1
GLT

GLRes<-df.residual(M1)
GLRes

GLR<- GLT-GLRes
GLR

#CMR
CMR<-SCE/GLR
CMR

#CMRES
CMRes<- SSE/GLRes
CMRes

#F0
F0<-CMR/CMRes
F0

##################################################################################################### 
### c) Calcular el estadístico t para probar las hipótesis H0:β2=0, H0:β7=0 y H0:β8=0. 
### ¿Qué conclusiones se pueden sacar acerca del papel de las variables x2, x7 y x8 en el modelo? 
#####################################################################################################

varest <- SSE /(nrow(Y)-nrow(beta))
varest

# Matriz inversa
XtX_inv <- solve(t(X) %*% X)

# Inicialización de vector para guardar t0
t0_vals <- numeric(ncol(X))

# Bucle para recorrer la diagonal
for (j in 1:ncol(X)) {
  cjj <- XtX_inv[j, j]
  t0 <- beta[j] / sqrt(varest * cjj)
  t0_vals[j] <- t0
}
  
t0_vals

################################################# 
### d) Calcular R2 y R2adj para este modelo. 
#################################################

# R2
R2 <-SCE/SCT
R2

# R2ADJ
R2ADJ <- 1-(1-R2)*((n-1)/(n-p-1))
R2ADJ

#################################################################### 
# e) Trazar una gráfica de probabilidad normal de los residuales. 
# ¿Parece haber algún problema con la hipótesis de normalidad? 
####################################################################

residuales<- M1$residuals

qqnorm(residuales)
qqline(residuales)

#################################################################################################
#     f)  Trazar e interpretar una gráfica de los residuales en función de la respuesta predicha.
#################################################################################################

ajustados <- M1$fitted.values   

plot(ajustados, residuales, xlab = "Valores ajustados",ylab = "Residuales",main = "Residuales vs Valores Ajustados")
abline(h = 0, col = "red")  

################################################################################################# 
# g) Trazar las gráficas de los residuales en función de cada una de las variables regresoras. 
# ¿Implican esas gráficas que se especificó en forma correcta el regresor? 
#################################################################################################

plot(M1$model$x2, residuales,xlab = "x2",ylab = "Residuales",main = "Residuales vs x2")
abline(h = 0, col = "red")

plot(M1$model$x7, residuales,xlab = "x7",ylab = "Residuales",main = "Residuales vs x7")
abline(h = 0, col = "red")

plot(M1$model$x8, residuales,xlab = "x8",ylab = "Residuales",main = "Residuales vs x8")
abline(h = 0, col = "red")

########################################################### 
# h) Calcular un intervalo de confianza de 95% para β7 
###########################################################

beta7_hat <- beta[3, ]  # estimador de beta7

cjj <- XtX_inv[3, 3]    # elemento diagonal correspondiente

se_beta7 <- sqrt(varest * cjj)  # error estándar

# Valor crítico t
tcrit <- qt(0.975, df = n - p - 1)

# Intervalo de confianza
IC_beta7 <- c(LI = beta7_hat - tcrit * se_beta7,  LS = beta7_hat + tcrit * se_beta7)
beta7_hat
IC_beta7
x_0 <- c(1, 2300, 56, 2100)

# Prediccion puntual
y0_hat <- as.numeric(t(x_0) %*% beta)

# Error estandar de la media predicha
se_y0 <- sqrt(varest * (t(x_0) %*% XtX_inv %*% x_0))

# Valor critico t
tcrit <- qt(0.975, df = n - p - 1)
IC_media <- c(LI = y0_hat - tcrit * se_y0, LS = y0_hat + tcrit * se_y0)
y0_hat
IC_media

M2 <- lm(y ~ x7 + x8, data = datos)
summary(M2)
n <- nrow(datos)
idv <- rep(1, n)
X2 <- matrix(c(idv, datos$x7, datos$x8), nrow=n, ncol=3)
Y  <- matrix(datos$y, nrow=n, ncol=1)

beta2 <- solve(t(X2) %*% X2) %*% t(X2) %*% Y

# sumas de cuadrados
SCT2 <- t(Y) %*% Y - (sum(Y)^2)/n
SCE2 <- t(beta2) %*% t(X2) %*% Y - (sum(Y)^2)/n
SSE2 <- SCT2 - SCE2

# grados de libertad
p2 <- nrow(beta2) - 1
GLR2  <- p2
GLRes2 <- n - p2 - 1

# cuadrados medios
CMR2   <- SCE2 / GLR2
CMRES2 <- SSE2 / GLRes2

# estadistico F
F0_2 <- CMR2 / CMRES2
F0_2
R2_M2 <- SCE2 / SCT2

# R^2 ajustado
R2adj_M2 <- 1 - (1 - R2_M2) * ((n - 1) / (n - p2 - 1))
R2_M2
R2adj_M2
R2
R2ADJ
XtX_inv2 <- solve(t(X2) %*% X2)
resid2   <- Y - X2 %*% beta2
df2      <- n - p2 - 1
varest2  <- SSE2 / df2
beta7_hat_M2 <- beta2[2, ]
cjj2<- XtX_inv2[2,2]
se_beta7_M2  <- sqrt(varest2 * cjj2)
tcrit2   <- qt(0.975, df = n - p2 - 1)
IC_beta7_M2  <- c(LI = beta7_hat_M2 - tcrit2*se_beta7_M2, LS = beta7_hat_M2 + tcrit2*se_beta7_M2)
beta7_hat_M2
IC_beta7_M2

## --- (2) IC95% para la MEDIA en (x7=56, x8=2100) ---
x0_M2   <- c(1, 56, 2100)
y0_hat2 <- as.numeric(t(x0_M2) %*% beta2)
se_y0_2 <- sqrt(varest2 * (t(x0_M2) %*% XtX_inv2 %*% x0_M2))
IC_media_M2 <- c(LI = y0_hat2 - tcrit2*se_y0_2,LS = y0_hat2 + tcrit2*se_y0_2)
len_media_M2 <- diff(IC_media_M2)
