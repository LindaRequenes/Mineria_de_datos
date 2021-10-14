########## SERIE DE TIEMPO - ACCIDENTES LABORALES ##########

# Abrir archivo
setwd("C:/Users/Lety/Downloads/Minería de Datos")
x<-read.csv("SerieDeTiempo_AccidentesLaborales.csv",header=T)
dim(x)
serie
serie<-ts(x$Accidentes,frequency=721,start=c(2015,1,1),end=c(2016,12,31))
plot(serie,main='Accidentes Laborales',xlab='Días',ylab='Número de accidentes')


  ### 1. Clasificación ###

y<-lm(serie~time(serie))
summary(y) #p-valor=0.7038 No rechazo H0
#No tiene tendencia lineal
#Serie estacionaria en media
library(tseries)
adf.test(serie,alternative='stationary')
#p-valor=0.01 Rechazamos H0
#Serie estacionaria en varianza
    
  #SERIE ESTACIONARIA


  ### 2. Modelar ### 

aic.arma<-Inf
orden.arma<-c(0,0,0)
for (j in 0:10)
{
  for (i in 0:10)
  {
    aic.xx<-AIC(arima(serie,order=c(j,0,i),method="ML"))
    if(aic.arma>aic.xx)
    {
      aic.arma<-aic.xx
      orden.arma<-c(j,0,i)
    }
  }
}
aic.arma #AIC = 5586.853
orden.arma #Mejor ARMA(8,9)


    ### 3. Ruido Blanco ###

# Media
res<-residuals(arima(serie,order = c(8,0,9),method = 'ML'))
mean(res) #Media=0.1706533, esta cerca del cero

# Varianza
plot(res,type='p',main='Dispersión de residuales') #No hay un patrón en los datos

# Independencia

#Prueba gráfica
acf(res,main='Autocorrelación',ci=0.99)
pacf(res,main='Autocorrelación Parcial',ci=0.99)
#Ambas no estan dentro del intervalo de confianza asi que utilizamos la prueba analítica

#Prueba analítica
Box.test(res)
#p-valor=0.9614 No rechazo H0
#Residuales independientes (incorrelaccionados)
##Hasta aquí se cumple ruido blanco##

# Normalidad
shapiro.test(res)
#p-valor=0.4697 No rechazo H0
#Residuales cumplen normalidad
#Por lo que sería un ruido blanco gaussiano

# Estandarizar y revisar los puntos atípicos fuera de 3 y -3
w<-res/sd(res)
plot(w,main='Datos Estandarizados',type = 'p',ylim=c(5,-5))
abline(h=c(3,-3),col='red') #La h son para las lineas horizontales
#No existen puntos atípicos


    ### 4. Predicciones ###

# Modelo original
pd<-predict(arima(serie,order = c(8,0,9),method = 'ML'),n.ahead=60)$pred
plot(serie,main='Accidentes Laborales',
     xlim=c(2015,2016.3),ylim=c(0,60),
     xlab='Días',ylab='Número de accidentes')
lines(pd,col='magenta') #Modelo original ARIMA(1,2,1)
legend(x=2016.1, y=55, legend=c('Serie Original', 'Predicción'), 
       col=c('black','magenta'), pch='-',  cex= 0.7,  xpd = T )
