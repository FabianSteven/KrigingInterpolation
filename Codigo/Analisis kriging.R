library(gridExtra)
require(devtools)
require(gstat)
require(sp)
require(sf)
require(ggplot2)
require(rgeos)
require(readxl)
library(nortest)
library(sp)
library(raster)
library(dismo)
library(ggvoronoi)
library(dplyr)
library(scatterplot3d)
library(rgl)

#Visualizar la tabla
View(tabla1_mod)

#Tipo de variable
class(tabla1_mod)
#Se realiza un backup
Adf=tabla1_mod
ndf=tabla1_mod
ndf2=tabla1_mod
#Tipo de variable

class(ndf)
#Visualizar el nombre de la columnas

names(ndf)
#Visualizar las 6 primeras filas de la tabla

head(ndf)
#Visualizar algunas estadisticas con als varibles del dataset
summary(ndf)
#Distancias entre lso puntos
ndf <- filter(ndf, ndf$IDCIUDAD == "11001")
quantile(dist(ndf[,11,9]))
quantile(dist(tabla1[,22,24]))


#Calcular el variograma
apply(ndf,2,var)


#Graficos de histogramas y frecuencia

ggplot(ndf, aes(COSTO)) + 
  geom_histogram(aes(), bins = 19, col=1, fill=8, alpha=.5) +
  labs(x="COSTO [n]",y="Count", title = "Histograma", 
       subtitle="Datos sin procesar") + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5,
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold")) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))

ggplot(ndf, aes(COSTO)) + 
  geom_vline(aes(xintercept = mean(COSTO), color="Mean"), linetype="dashed",
             size=1) + 
  geom_vline(aes(xintercept = median(COSTO), color="Median"), linetype="dashed",
             size=1)+
  geom_density(col="#FF6666", alpha=.2, fill="#FF6666") +
  labs(x= 'COSTO', y='Densidad')+
  scale_color_manual(name = "Estadisticas", values = c(Median = "green", 
                                                     Mean = "blue"))+
  labs(x="COSTO[n]",y="Densidad", title = "Curva de densidad", 
       subtitle="Datos sin procesar, Media, Mediana") + 
  theme(plot.title = element_text(face = "bold", size = 20,hjust =0.5, 
                                  color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))



#Q-Q Grafico

ggplot(data=ndf, aes(sample=COSTO))+ stat_qq_line(col="red", size=1.2)+ stat_qq()+
  labs(x="Teorico",y="Muesta", title = "Grafico Q-Q", 
       subtitle="Datos sin procesar") + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))

#prueba de normalidad
#prueba Spahiro-Wilk

shapiro.test(ndf$COSTO) 

#prueba de kolmogorov-smirnov
lillie.test(ndf$COSTO)

#como el COSTO p es menor que 0.05 entonces podemos asumir que no hay una distribución normal, F :c

#mapa de burbujas

ggplot(ndf,aes(LONGITUD,LATITUD)) + geom_point(aes(size=LOCALIDAD), color= "blue" ,alpha=.8) +
  labs(x="Este",y="Norte", title = "Servicios aceptados por dia [m]") +
  theme(plot.title = element_text(face = "bold", size = 20,hjust =0.5,
                                  color = "black")) +
  theme (axis.text = element_text(colour = "black",size =10, face = "bold"))


# Mapa de Voronoi o polígonos de Thiessen

coordinates(ndf2) = ~LONGITUD + LATITUD
class(ndf2)
voronoi_map=voronoi(ndf2)
voronoi_map
plot(voronoi_map)
points(ndf2, col="red", pch=19, bg= 21, cex=1, lwd=2)


voranoi_map_ggplot = ggplot(ndf,aes(LONGITUD,LATITUD)) + 
  scale_fill_gradientn("COSTO", colors=c("seagreen","darkgreen","green1","yellow",
                                      "gold4", "sienna"), values=scales::rescale(c(91,92,93,94,95,96))) + 
  scale_color_gradientn("COSTO", colors=c("seagreen","darkgreen","green1","yellow",
                                       "gold4", "sienna"), values=scales::rescale(c(91,92,93,94,95,96))) + 
  labs(x="Este",y="Norte", title = "Mapa Voronoi")+ 
  labs(x="Este",y="Norte", title = "Mapa Voronoi") + 
  theme(plot.title = element_text( face = "bold", size = 20,hjust =0.5,  
                                   color = "black")) + 
  theme (axis.text = element_text(colour = "black", size =10, face = "bold"))


voranoi_map_ggplot +
  geom_voronoi(aes(fill=COSTO)) + geom_point(col="red")+ stat_voronoi(geom="path")


# Análisis de la tendencia x Orientación, lineal, de segundo y tercer orden

ggplot(ndf,aes(LONGITUD,COSTO)) + 
  geom_smooth(method = "lm", formula= LATITUD~LONGITUD,se=F,size=1.2, aes(colour="Linear")) + 
  geom_smooth(method = "lm", formula = LATITUD ~ poly(LONGITUD, 2), se = F, size=1.2,
              aes(colour="Second_Order")) + 
  geom_smooth(method = "lm", formula = LATITUD ~ poly(LONGITUD, 3), se = F, size=1.2,
              aes(colour="Third_Order")) +
  scale_color_manual(name = "Regresion Type", values = c(Linear = "red", 
                                                         Second_Order = "blue", Third_Order = "green4"))+
  labs(x="Este [m]",y="COSTO [m]", title = "Gráfico de dispersión", 
       subtitle="COSTO vs LONGITUD") + geom_point() +
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold")) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black")) 

# Análisis de la tendencia y Northing, lineal, de segundo y tercer orden

ggplot(ndf,aes(LATITUD,COSTO)) + 
  geom_smooth(method = "lm", formula= LATITUD~LONGITUD,se=F,size=1.2, aes(colour="Linear")) + 
  geom_smooth(method = "lm", formula = LATITUD ~ poly(LONGITUD, 2), se = F, size=1.2,
              aes(colour="Second_Order")) + 
  geom_smooth(method = "lm", formula = LATITUD ~ poly(LONGITUD, 3), se = F, size=1.2,
              aes(colour="Third_Order")) +
  scale_color_manual(name = "Regresion Type", values = c(Linear = "red", 
                                                         Second_Order = "blue", Third_Order = "green4"))+
  labs(x="Norte [m]",y="COSTO [m]", title = "Grafico de dispersión", 
       subtitle="COSTO vs LATITUD") + geom_point() +
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,hjust =0.5, color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold")) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black")) 


#Modelo 3d

scatterplot3d(ndf$LONGITUD,ndf$LATITUD,ndf$COSTO, xlab="Este",
              ylab="Norte", zlab="COSTO")

plot3d(ndf$LONGITUD,ndf$LATITUD,ndf$COSTO, xlab="Este",
       ylab="Norte", zlab="COSTO", size=5, col="blue")


vgm_cloud = (variogram(COSTO~1,data=ndf2,cutoff=30, cloud=T))
?variogram
plot(vgm_cloud)
View(vgm_cloud)
class(vgm_cloud)
write.table(vgm_cloud, 
            file="C:/Users/fagud/Downloads/kriging/cloud_points.xls", sep=",")
?write.table

#con ggplot


ggplot(vgm_cloud,aes(x=dist,gamma)) + geom_point(colour = "blue", size = 1) +
  labs(x="Distancia [m]",y="Gamma", title = "Semivariograma", 
       subtitle="Datos sin procesar - nube semivariograma ") + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))


sel = plot(variogram(COSTO~1, ndf2,cutoff=500, cloud = T),digitize = T)
plot(sel, ndf2)
 

g = gstat(id='COSTO', formula=COSTO~1,data = ndf2) 
vgm1 = variogram(g) 
head(vgm1) 
vgm1       
plot(vgm1) 
#vgm1 = variogram(g, cutoff=30, width=15)
#vgm1 
#plot(vgm1) 
#View (vgm1)

pair_count = ggplot(data = vgm1) +
  geom_col(mapping = aes(x = dist, y = np), width = 0.01, color = "blue") +
  labs(x="Distancia [m]",y="Numero de puntos") + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black")) +
  geom_hline(aes(yintercept = var(ndf$COSTO), color="Variance"), linetype="dashed",
             size=1) +
  scale_color_manual(name = "Statistics", values = c(Variance = "red"))
plot(pair_count) 


# la línea de varianza con ggplot 

Semivario1= ggplot(vgm1,aes(x=dist,gamma)) + geom_point(colour = "blue", size = 1) +
  labs(x="Distancia [m]",y="Gamma", title = "Semivariograma", 
       subtitle="Datos sin procesar - Semivariograma omnidireccional") + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black")) +
  geom_hline(aes(yintercept = var(ndf$COSTO), color="Variance"), linetype="dashed",
             size=1) +
  scale_color_manual(name = "Statistics", values = c(Variance = "red"))
plot(Semivario1)




grid.arrange(Semivario1, pair_count, ncol = 1, heights = c(3, 1))


#Semivariograma direccional

# Semivariograma direccional para la nube de semivariogramas

plot(variogram(COSTO~1,ndf2,cutoff=30,alpha=0, tol.hor=22.5, width=15, cloud=T))
?variogram
# Semivariograma direccional para el semivariograma experimental

vgm2 = variogram(g,alpha=c(0,45,90,135),tol.hor=22.5,cutoff=30,width=15)
plot(vgm2)

# con ggplot

ggplot(vgm2,aes(x=dist,y=gamma,col=factor(dir.hor),shape=factor(dir.hor))) +
  geom_point(size=2) +
  geom_line() +
  labs(x="Distancia [m]",y="Gamma", title = "Semivariograma", 
       subtitle="Datos sin procesar - Semivariograma direccional",col="atzimut",shape='') +
  geom_hline(aes(yintercept = var(ndf2$COSTO), color="Variance"), linetype="dashed",
             size=1) + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))

#Variografía

# El mapa del variograma nos ayuda a identificar la anisotropía 

map.vgm = variogram(g, width=15, cutoff=30,map=TRUE)
plot(map.vgm)

# Mapa de variogramas con ggplot

ggplot(data.frame(map.vgm),aes(x=map.dx,y=map.dy,fill=map.COSTO)) +
  geom_raster() +
  scale_fill_gradientn(colours= topo.colors(10)) +
  labs(x="Este-Oeste",y="Norte-Sur", title = "Mapa de Varigramas",
       subtitle="Datos sin procesar - Semivariograma omnidireccional", fill="semivariance")+ 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))


#Eliminación de la tendencia de los datos

# Nube de residuos para una tendencia lineal ~x+y (polinomio de primer orden)
vgm_residual_cloud = (variogram(COSTO~LONGITUD+LATITUD,ndf2,cutoff=30, cloud=T)) 
plot(vgm_residual_cloud)

# Nube de residuos con ggplot

ggplot(vgm_residual_cloud,aes(x=dist,gamma)) + 
  geom_point(colour = "blue", size = 1) +
  labs(x="Distancia [m]",y="Gamma", title = "Semivariograma", 
       subtitle="semivariograma Nube residual") + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))

#Varianza en los residuos para añadir al gráfico

model_1 = lm(COSTO~LONGITUD+LATITUD, data=ndf)
ndf$predicted_1 = predict(model_1)
ndf$residuals_1 = residuals(model_1)

var(ndf$residuals_1)

# Semivariograma experimental omnidireccional de los residuos

g_trend = gstat(id='COSTO', formula=COSTO~LONGITUD+LATITUD,data = ndf2) #objeto gstat 

vgm_Residuals = variogram(g_trend, cutoff=30, width=5)
plot(vgm_Residuals) 

#linea de varianza

ggplot(vgm_Residuals,aes(x=dist,gamma)) + geom_point(colour = "blue", size = 1) +
  labs(x="Distancia [m]",y="Gamma", title = "Semivariograma", 
       subtitle=" Semivariograma omnidireccional de residuos") + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black")) +
  geom_hline(aes(yintercept = var(ndf$residuals_1), color="Variance"), linetype="dashed",
             size=1) +
  scale_color_manual(name = "Statistics", values = c(Variance = "red")) 

# Semivariograma residual 4 direcciones diferentes

vgm_4dir_resid = variogram(g_trend,alpha=c(0,45,90,135),tol.hor=22.5,
                           cutoff=30, width=15)
plot(vgm_4dir_resid)


ggplot(vgm_4dir_resid,aes(x=dist,y=gamma,col=factor(dir.hor),
                          shape=factor(dir.hor))) + geom_point(size=2) + geom_line() +
  labs(x="Distancia [m]",y="Gamma", title = "Semivariograma", 
       subtitle="Semivariograma direccional residual",
       col="atzimut",shape='') +
  geom_hline(aes(yintercept = var(ndf$residuals_1), color="Variance"), 
             linetype="dashed",size=1) + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))

#Mapa de variograma residual - Nos ayuda a identificar la anisotropía 

map.vgm.resid = variogram(g_trend, width=15, cutoff=30,map=TRUE)
plot(map.vgm.resid)


ggplot(data.frame(map.vgm.resid),aes(x=map.dx,y=map.dy,fill=map.COSTO)) +
  geom_raster() +
  scale_fill_gradientn(colours= topo.colors(10)) +
  labs(x="Este-Oeste",y="Norte-Sur", title = "Mapa de Variograma",
       subtitle="Semivariograma residual omnidireccional", 
       fill="semivariancia")+ 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))

#Modelo Semivariograma

#"Exp" (Un modelo exponencial)
#"Sph" (Un modelo esférico)
#"Gau" (Un modelo gaussiano)
#"Bes" (Un modelo de Bessel)

# Orden de los parámetros para la función fit.variogram: sill, model, range y nugget

# Esférico
vgm_residual.fit_sph = fit.variogram(vgm_Residuals, 
                                     model = vgm(0.1,"Sph",100,0.5)) 
vgm_residual.fit_sph
plot(vgm_Residuals, vgm_residual.fit_sph)

# Exponencial
vgm_residual.fit_exp = fit.variogram(vgm_Residuals, 
                                     model = vgm(0.1,"Exp",100,0.5)) 
vgm_residual.fit_exp
plot(vgm_Residuals, vgm_residual.fit_exp)

# Gaussiano
vgm_residual.fit_gau = fit.variogram(vgm_Residuals, 
                                     model = vgm(0.1,"Gau",100,0.5)) 

vgm_residual.fit_gau2 = vgm(psill=0.17,model="Gau",range=40,nugget=0)
plot(vgm_Residuals, vgm_residual.fit_gau2)

# Bessel
vgm_residual.fit_bes = fit.variogram(vgm_Residuals, 
                                     model = vgm(0.1,"Bes",100,0.5)) 
vgm_residual.fit_bes
plot(vgm_Residuals, vgm_residual.fit_bes)

fit.variogram(vgm_Residuals, vgm(c("Exp", "Bes", "Sph")))


model_shp = variogramLine(vgm_residual.fit_sph, maxdist = max(vgm_Residuals$dist))
head(model_shp)

model_exp = variogramLine(vgm_residual.fit_exp, maxdist = max(vgm_Residuals$dist))
head(model_exp)

model_gau2 = variogramLine(vgm_residual.fit_gau2, maxdist = max(vgm_Residuals$dist))
head(model_gau2)

#ggplot

ggplot(vgm_Residuals, aes(x = dist, y = gamma)) +
  geom_point(size=2) +
  geom_line(data = model_shp, linetype="solid", aes(color= "Spherical"), size=0.8)+
  geom_line(data = model_exp, linetype="dashed", aes(color= "Exponential"), size=0.8)+
  geom_line(data = model_gau2, linetype="twodash", aes(color= "Gaussian"), size=0.8)+
  labs(x="Distancia [m]",y="Gamma", title = "Modelo Semivariograma ", 
       subtitle="Modelo esférico, exponencial y gaussiano ajustado en el semivariograma residual omnidireccional
       ",
       col="Model",shape='')+
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black") + scale_color_manual(name = "Theoretical Model", 
                                                                       values = c(Spherical= "blue", Exponential= "Red",Gaussian="green" )))

#Modelo direccional Semivariograma

# Direccional 135
vgm_135dir_resid = variogram(g_trend,alpha=c(135),tol.hor=22.5,
                             cutoff=30, width=15)
plot(vgm_135dir_resid)


#Esferico 135
vgm_residual.fit_sph_135 = fit.variogram(vgm_135dir_resid, 
                                         model = vgm(0.1,"Sph",100,0.5)) 
vgm_residual.fit_sph_135
plot(vgm_135dir_resid, vgm_residual.fit_sph_135, col="red")


#Validación cruzada

# validación cruzada esférica omnidireccional

cross_validation_sph = 
  krige.cv(COSTO ~ LONGITUD+LATITUD, locations = ndf2, model = vgm_residual.fit_sph)
summary(cross_validation_sph)
summary(cross_validation_sph$residual)
class(cross_validation_sph)
View(cross_validation_sph)


# validación cruzada exponencial omnidireccional
cross_validation_exp = 
  krige.cv(COSTO ~ LONGITUD+LATITUD, locations = ndf2, model = vgm_residual.fit_exp)
summary(cross_validation_exp$residual)
class(cross_validation_exp)

# validación cruzada gaussiana omnidireccional
cross_validation_gau = 
  krige.cv(COSTO ~ LONGITUD+LATITUD, locations = ndf2, model = vgm_residual.fit_gau2)
summary(cross_validation_gau$residual)
class(cross_validation_gau)

# validación cruzada esférica  grados 
cross_validation_sph_135 = 
  krige.cv(COSTO ~ LONGITUD+LATITUD, locations = ndf2, model = vgm_residual.fit_sph_135)
summary(cross_validation_sph_135)
summary(cross_validation_sph_135$residual)
class(cross_validation_sph)

cross_validation_exp_2 = krige.cv(COSTO ~ LONGITUD+LATITUD, locations = ndf2,
                                  model = vgm_residual.fit_exp)%>%st_as_sf()
class(cross_validation_exp_2)
View(cross_validation_exp_2)

#graficas

ggplot(as.data.frame(cross_validation_exp), aes(observed,var1.pred)) +
  coord_fixed() + geom_abline(slope=1, col="red",size=1.2) +
  geom_smooth(method = "lm", formula= y~x,size=1.2, col="black", se=F)+
  geom_point(col = "blue") +
  labs(x="observado", y="predecido", title = "Validacion cruzada") +
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5,
                                   color = "black")) +
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic",
                                   color="black"))

ggplot(as.data.frame(cross_validation_exp), aes(residual)) +
  geom_histogram(bins = 10, col=1, fill="grey") +
  labs( x="residual", y= "frecuencia", title = "Histograma",
        subtitle = "COSTOes residuales") + geom_vline(xintercept=0, col=2)+
  theme(plot.title = element_text(face = "bold", size = 20,hjust =0.5,
                                  color = "black")) +
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic",
                                   color="black"))

ggplot(as.data.frame(cross_validation_exp), aes(observed,residual)) +
  coord_fixed() + geom_abline(slope=1, col="red",size=1.2) +
  geom_smooth(method = "lm", formula= y~x,size=1.2, col="black", se=F)+
  geom_point(col = "blue") +
  labs(x="observado", y="residual", title = "Residual Vs Observado") +
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5,
                                   color = "black")) +
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic",
                                   color="black"))

ggplot(data=cross_validation_exp_2, aes(sample=residual))+ stat_qq_line(col="red", size=1.2)+ stat_qq()+
  labs(x="Teorico",y="muestra", title = "Grafico Q-Q ", 
       subtitle="") + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))

# error cuadrático medio (RMSE)

(UK.RMSE_shp=
    sqrt(sum(cross_validation_sph$residual^2)/
           (length(cross_validation_sph$residual))))

(UK.RMSE_exp=
    sqrt(sum(cross_validation_exp$residual^2)/
           (length(cross_validation_exp$residual))))

(UK.RMSE_gau=
    sqrt(sum(cross_validation_gau$residual^2)/
           (length(cross_validation_gau$residual))))

(UK.RMSE_exp_2=
    sqrt(sum(cross_validation_sph_135$residual^2)/
           (length(cross_validation_sph_135$residual))))

# diferencia entre lo observado y lo previsto
bubble(cross_validation_exp, "residual", 
       main = "Validación cruzada Residuales modelo exponecial")

#creacion del grid
library(rgdal)

library(maptools)

coordinates(ndf2)= ~ LONGITUD+LATITUD

x.range_1<- as.numeric(c(-74.22278, -74.00357))

y.range_1<- as.numeric(c(4.462181, 4.80202))


grd = expand.grid(x = seq(from = x.range_1[1], to =x.range_1[2],by = 0.0078),
                  y = seq(from = y.range_1[1], to =y.range_1[2],by = 0.008))

coordinates(grd)= ~ x + y


griddf=as.data.frame(grd)

gridded(grd) = TRUE

attach(ndf)

library(rgdal)

first_order = as.formula(COSTO ~ LONGITUD+ LATITUD )

model1 = lm(first_order, data=ndf)

fullgrid(grd) <- TRUE

# kriging Universal

# COSTOes predichos y varianza (error de interpolación)
UK = krige(COSTO~LONGITUD+LATITUD, ndf2, grd, model=vgm_residual.fit_sph) 
summary(UK)
