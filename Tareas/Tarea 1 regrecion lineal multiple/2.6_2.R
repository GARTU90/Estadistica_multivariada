datos <- read.csv("Rendimiento_de_gasolina.csv")
datos
############################################################################################################# 
#     a)  Ajustar un modelo de regresión lineal múltiple que relacione el rendimiento de la gasolina 
#         y, en millas por galón,la cilindrada del motor (x1) y la cantidad de gargantas del carburador (x6).
#############################################################################################################
modelo_16 <- lm(y ~ x1 + x6, data = datos)
modelo_16

##########################################################################################
#     b)  Formar la tabla de análisis de varianza y probar la significancia de la regresión.
##########################################################################################
# Matrices del modelo multiple
Y  <- as.matrix(datos$y)
X  <- cbind(1, datos$x1, datos$x6)
n  <- nrow(X)
p  <- ncol(X) - 1

XtX     <- t(X) %*% X
XtX_inv <- solve(XtX)
beta    <- XtX_inv %*% t(X) %*% Y
Yhat    <- X %*% beta
res     <- Y - Yhat

# Sumas de cuadrados (ANOVA)
SCE <- t(beta)%*% t(X)%*%Y-sum(Y)**2/n 

SCT <- t(Y) %*% Y-sum(Y)**2/n        
SSE <- SCT-SCE         

# Grados de libertad
GLT   <- n - 1
GLR   <- p
GLRes <- n - p - 1

# Cuadrados medios y prueba F global
CMR   <- SCE / GLR
CMRes <- SSE / GLRes
F0    <- CMR / CMRes
pval  <- pf(F0, df1 = GLR, df2 = GLRes, lower.tail = FALSE)

 summary(modelo_16)

#########################################################################################################
#     c)  Calcular R2 y R2adj para este modelo. Compararlas con las R2 y R2adj 
#         Ajustado para el modelo de regresión lineal simple, que relaciona las millas con la cilindrada.
#########################################################################################################
 
# para el modelo multiple:
 
# R^2 y R^2 ajustado (múltiple)
R2    <- SCE / SCT
R2adj <- 1 - (SSE/GLRes) / (SCT/GLT)
 
# para el modelo lineal
 
modelo_1 <- lm(y ~ x1, data = datos)

Y2  <- as.matrix(datos$y)
X2  <- cbind(1, datos$x1)             
n2  <- nrow(X2)
p2  <- ncol(X2) - 1

XtX2     <- t(X2) %*% X2
XtX2_inv <- solve(XtX2)
beta2    <- XtX2_inv %*% t(X2) %*% Y2
Yhat2    <- X2 %*% beta2
res2     <- Y2 - Yhat2

SCE2 <- t(beta2)%*% t(X2)%*%Y2-sum(Y2)**2/n2 
SCT2 <- t(Y2) %*% Y2-sum(Y2)**2/n2        
SSE2 <- SCT2-SCE2 

GLT2   <- n2 - 1
GLR2   <- p2
GLRes2 <- n2 - p2 - 1

MSR2 <- SCE2 / GLR2
MSE2 <- SSE2 / GLRes2
F02  <- MSR2 / MSE2
pvalF2 <- pf(F02, df1 = GLR2, df2 = GLRes2, lower.tail = FALSE)

R2_S2    <- SCE2 / SCT2
R2adj_S2 <- 1 - (SSE2/GLRes2) / (SCT2/GLT2)

# (verificación opcional)
# summary(modelo_1)

#######################################################
#     d)  Determinar un intervalo de confianza para β1.
#######################################################
j <- 2  
varest2 <- MSE2
tcrit2  <- qt(0.975, df = GLRes2)

beta1_hat_S <- beta2[j, 1]
se_beta1_S  <- sqrt(varest2 * XtX2_inv[j, j])

IC_beta1_S <- c(LI = beta1_hat_S - tcrit2 * se_beta1_S,
                LS = beta1_hat_S + tcrit2 * se_beta1_S)

##################################################################################################
#     e)  Determinar un intervalo de confianza de 95% para el rendimiento promedio de la gasolina, 
#         cuando x1=225pulg 3 y x6=2 gargantas.
##################################################################################################
X0 <- matrix(c(1, 225, 2), nrow = 1)   # 1 x 3
y0_hat <- as.numeric(X0 %*% beta)

varest <- CMRes                  # = SSE / GLRes
tcrit  <- qt(0.975, df = GLRes)

se_mean <- sqrt( varest * (X0 %*% XtX_inv %*% t(X0)) )
IC_media <- c(LI = y0_hat - tcrit * se_mean,
              LS = y0_hat + tcrit * se_mean)

se_pred <- sqrt( varest * (1 + X0 %*% XtX_inv %*% t(X0)) )
IC_pred <- c(LI = y0_hat - tcrit * se_pred,
             LS = y0_hat + tcrit * se_pred)

list(
  anova_multiple = list(SCE = SCE, SSE = SSE, SCT = SCT,
                        GLR = GLR, GLRes = GLRes, GLT = GLT,
                        CMR = CMR, CMRes = CMRes, F0 = F0, pval = pval),
  r2_multiple    = list(R2 = R2, R2adj = R2adj),
  r2_simple      = list(R2 = R2_S2, R2adj = R2adj_S2, F = F02, pval = pvalF2),
  IC_beta1_simple = list(beta1_hat = beta1_hat_S, IC95 = IC_beta1_S),
  prediccion_punto = list(y0_hat = y0_hat, IC_media_95 = IC_media, IC_pred_95 = IC_pred)
)

##############################################################################################################
#     f)  Determinar un intervalo de predicción de 95% para una nueva observación de rendimiento de gasolina, 
#         cuando x1=225pulg 3 y x6=2 gargantas.
#############################################################################################################

X0_f <- matrix(c(1, 225, 2), nrow = 1)                 # 1 x 3
y0_hat_f <- as.numeric(X0_f %*% beta)

varest_f <- CMRes                                      # = SSE / GLRes
tcrit_f  <- qt(0.975, df = GLRes)

se_pred_f <- sqrt( varest_f * (1 + X0_f %*% XtX_inv %*% t(X0_f)) )
IC_pred_f <- c(LI = y0_hat_f - tcrit_f * se_pred_f,
               LS = y0_hat_f + tcrit_f * se_pred_f)

y0_hat_f
IC_pred_f

###############################################################################################################
#     g)  Considerar el modelo de regresión lineal simple, que relaciona las millas con la cilindrada. 
#         Construir un intervalo de confianza de 95% para el rendimiento promedio de la gasolina y un intervalo 
#         de predicción para el rendimiento, cuando x1=225pulg 3. Comparar las longitudes de estos intervalos 
#         con los intervalos obtenidos en los dos incisos anteriores. ¿Tiene ventajas agregar x6al modelo?
###############################################################################################################

# Punto nuevo para el simple
X0_g <- matrix(c(1, 225), nrow = 1)                    # 1 x 2
y0_hat_g <- as.numeric(X0_g %*% beta2)

varest_g <- MSE2
tcrit_g  <- qt(0.975, df = GLRes2)

# IC de la media (simple)
se_mean_g <- sqrt( varest_g * (X0_g %*% XtX2_inv %*% t(X0_g)) )
IC_media_g <- c(LI = y0_hat_g - tcrit_g * se_mean_g,
                LS = y0_hat_g + tcrit_g * se_mean_g)

# Intervalo de predicción (simple)
se_pred_g <- sqrt( varest_g * (1 + X0_g %*% XtX2_inv %*% t(X0_g)) )
IC_pred_g <- c(LI = y0_hat_g - tcrit_g * se_pred_g,
               LS = y0_hat_g + tcrit_g * se_pred_g)

# Longitudes (simple vs múltiple)
len_media_g <- IC_media_g[2] - IC_media_g[1]
len_pred_g  <- IC_pred_g[2]  - IC_pred_g[1]

# Para comparar la predicción de f) (múltiple) con la de g) (simple):
len_pred_f  <- IC_pred_f[2]  - IC_pred_f[1]

list(
  simple = list(y0_hat = y0_hat_g,
                IC_media_95 = IC_media_g,
                IC_pred_95  = IC_pred_g,
                longitudes  = list(media = len_media_g, pred = len_pred_g)),
  multiple = list(y0_hat = y0_hat_f,
                  IC_pred_95 = IC_pred_f,
                  longitud_pred = len_pred_f)
)
#######################################################################################################
#     h)  Trazar una gráfica de probabilidad normal de los residuales. ¿Parece haber algún problema con 
#         la hipótesis de normalidad?
#######################################################################################################
res_mult <- residuals(modelo_16)

qqnorm(res_mult, main = "QQ-plot residuales (modelo múltiple)")
qqline(res_mult, col = "red", lwd = 2)
## parece haber normalidad

res_simp <- residuals(modelo_1)

qqnorm(res_simp, main = "QQ-plot residuales (modelo simple)")
qqline(res_simp, col = "blue", lwd = 2)
## tambien parece haber normalidad

#################################################################################################
#     i)  Trazar e interpretar una gráfica de los residuales en función de la respuesta predicha.
#################################################################################################
res_mult <- residuals(modelo_16)
yhat_mult <- fitted(modelo_16)

plot(yhat_mult, res_mult,
     main = "Residuales vs valores ajustados (múltiple)",
     xlab = "Valores ajustados",
     ylab = "Residuales",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

res_simp <- residuals(modelo_1)
yhat_simp <- fitted(modelo_1)

plot(yhat_simp, res_simp,
     main = "Residuales vs valores ajustados (simple)",
     xlab = "Valores ajustados",
     ylab = "Residuales",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

###################################################################################################
#     j)  Trazar las gráficas de los residuales en función de cada una de las variables regresoras. 
#         ¿Implican esas gráficas que se especificó en forma correcta el regresor?
###################################################################################################

# Residuales del modelo múltiple
res_mult <- residuals(modelo_16)

# Residuales vs x1 (cilindrada)
plot(datos$x1, res_mult,
     main = "Residuales vs x1 (cilindrada)",
     xlab = "x1 (cilindrada)", ylab = "Residuales",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Residuales vs x6 (gargantas)
plot(datos$x6, res_mult,
     main = "Residuales vs x6 (gargantas)",
     xlab = "x6 (gargantas)", ylab = "Residuales",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

# Residuales del modelo simple
res_simp <- residuals(modelo_1)

plot(datos$x1, res_simp,
     main = "Residuales vs x1 (modelo simple)",
     xlab = "x1 (cilindrada)", ylab = "Residuales",
     pch = 19, col = "purple")
abline(h = 0, col = "red", lwd = 2)
