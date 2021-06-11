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
View(Taxis)
#Tipo de variable
class(Taxis)
#Se realiza un backup
Adf=Taxis
ndf=NTaxis
ndf2=NTaxis
#Tipo de variable
class(Adf)
class(ndf)
#Visualizar el nombre de la columnas
names(Adf)
names(ndf)
#Visualizar las 6 primeras filas de la tabla
head(Adf)
head(ndf)
#Visualizar algunas estadisticas con als varibles del dataset
summary(Adf)
summary(ndf)
#Distancias entre lso puntos
quantile(dist(ndf[,1:2]))
#Calcular el variograma
apply(ndf,2,var)


#Graficos de histogramas y frecuencia

ggplot(ndf, aes(Localidad)) + 
  geom_histogram(aes(), bins = 19, col=1, fill=8, alpha=.5) +
  labs(x="Localidades [n]",y="Count", title = "Histograma", 
       subtitle="Datos sin procesar") + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5,
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold")) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))

ggplot(ndf, aes(Localidad)) + 
  geom_vline(aes(xintercept = mean(Localidad), color="Mean"), linetype="dashed",
             size=1) + 
  geom_vline(aes(xintercept = median(Localidad), color="Median"), linetype="dashed",
             size=1)+
  geom_density(col="#FF6666", alpha=.2, fill="#FF6666") +
  labs(x= 'Localidades', y='Densidad')+
  scale_color_manual(name = "Estadisticas", values = c(Median = "green", 
                                                     Mean = "blue"))+
  labs(x="Localidades[n]",y="Densidad", title = "Curva de densidad", 
       subtitle="Datos sin procesar, Media, Mediana") + 
  theme(plot.title = element_text(face = "bold", size = 20,hjust =0.5, 
                                  color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))



#Q-Q Grafico

ggplot(data=ndf, aes(sample=Localidad))+ stat_qq_line(col="red", size=1.2)+ stat_qq()+
  labs(x="Teorico",y="Muesta", title = "Grafico Q-Q", 
       subtitle="Datos sin procesar") + 
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold"))+
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black"))

#prueba de normalidad
#prueba Spahiro-Wilk

shapiro.test(ndf$Localidad) 

#prueba de kolmogorov-smirnov
lillie.test(ndf$Localidad)

#como el valor p es menor que 0.05 entonces podemos asumir que no hay una distribución normal, F :c

#mapa de burbujas

ggplot(Adf,aes(Longitud,Latitud)) + geom_point(aes(size=Día), color= "blue" ,alpha=.8) +
  labs(x="Este",y="Norte", title = "Servicios aceptados por dia [m]") +
  theme(plot.title = element_text(face = "bold", size = 20,hjust =0.5,
                                  color = "black")) +
  theme (axis.text = element_text(colour = "black",size =10, face = "bold"))


# Mapa de Voronoi o polígonos de Thiessen

coordinates(ndf2) = ~Longitud + Latitud
class(ndf2)
voronoi_map=voronoi(ndf2)
plot(voronoi_map)
points(ndf2, col="red", pch=19, bg= 21, cex=1, lwd=2)


voranoi_map_ggplot = ggplot(ndf,aes(Longitud,Latitud)) + 
  scale_fill_gradientn("Localidad", colors=c("seagreen","darkgreen","green1","yellow",
                                      "gold4", "sienna"), values=scales::rescale(c(91,92,93,94,95,96))) + 
  scale_color_gradientn("Localidad", colors=c("seagreen","darkgreen","green1","yellow",
                                       "gold4", "sienna"), values=scales::rescale(c(91,92,93,94,95,96))) + 
  labs(x="Este",y="Norte", title = "Mapa Voronoi")+ 
  labs(x="Este",y="Norte", title = "Mapa Voronoi") + 
  theme(plot.title = element_text( face = "bold", size = 20,hjust =0.5,  
                                   color = "black")) + 
  theme (axis.text = element_text(colour = "black", size =10, face = "bold"))


voranoi_map_ggplot +
  geom_voronoi(aes(fill=Localidad)) + geom_point(col="red")+ stat_voronoi(geom="path")


# Análisis de la tendencia x Orientación, lineal, de segundo y tercer orden

ggplot(ndf,aes(Longitud,Localidad)) + 
  geom_smooth(method = "lm", formula= Latitud~Longitud,se=F,size=1.2, aes(colour="Linear")) + 
  geom_smooth(method = "lm", formula = Latitud ~ poly(Longitud, 2), se = F, size=1.2,
              aes(colour="Second_Order")) + 
  geom_smooth(method = "lm", formula = Latitud ~ poly(Longitud, 3), se = F, size=1.2,
              aes(colour="Third_Order")) +
  scale_color_manual(name = "Regresion Type", values = c(Linear = "red", 
                                                         Second_Order = "blue", Third_Order = "green4"))+
  labs(x="Este [m]",y="Localidad [m]", title = "Gráfico de dispersión", 
       subtitle="Localidad vs Longitud") + geom_point() +
  theme(plot.title = element_text( face = "bold",size = 20,hjust =0.5, 
                                   color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold")) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black")) 

# Análisis de la tendencia y Northing, lineal, de segundo y tercer orden

ggplot(ndf,aes(Latitud,Localidad)) + 
  geom_smooth(method = "lm", formula= Latitud~Longitud,se=F,size=1.2, aes(colour="Linear")) + 
  geom_smooth(method = "lm", formula = Latitud ~ poly(Longitud, 2), se = F, size=1.2,
              aes(colour="Second_Order")) + 
  geom_smooth(method = "lm", formula = Latitud ~ poly(Longitud, 3), se = F, size=1.2,
              aes(colour="Third_Order")) +
  scale_color_manual(name = "Regresion Type", values = c(Linear = "red", 
                                                         Second_Order = "blue", Third_Order = "green4"))+
  labs(x="Norte [m]",y="Localidad [m]", title = "Grafico de dispersión", 
       subtitle="Localidad vs Latitud") + geom_point() +
  theme(plot.title = element_text(face = "bold", 
                                  size = 20,hjust =0.5, color = "black")) + 
  theme(axis.text = element_text(colour = "black", size =10, face = "bold")) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="italic", 
                                   color="black")) 


#Modelo 3d

scatterplot3d(ndf$Longitud,ndf$Latitud,ndf$Localidad, xlab="Este",
              ylab="Norte", zlab="Localidad")

plot3d(ndf$Longitud,ndf$Latitud,ndf$Localidad, xlab="Este",
       ylab="Norte", zlab="Localidad", size=5, col="blue")


vgm_cloud = (variogram(Localidad~1,data=ndf2,cutoff=30, cloud=T))
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


sel = plot(variogram(Localidad~1, ndf2,cutoff=500, cloud = T),digitize = T)
plot(sel, ndf2)
 

g = gstat(id='Valor', formula=Localidad~1,data = ndf2) 
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
  geom_hline(aes(yintercept = var(ndf$Localidad), color="Variance"), linetype="dashed",
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
  geom_hline(aes(yintercept = var(ndf$Localidad), color="Variance"), linetype="dashed",
             size=1) +
  scale_color_manual(name = "Statistics", values = c(Variance = "red"))
plot(Semivario1)




grid.arrange(Semivario1, pair_count, ncol = 1, heights = c(3, 1))


#Semivariograma direccional

# Semivariograma direccional para la nube de semivariogramas

plot(variogram(Valor~1,ndf2,cutoff=30,alpha=0, tol.hor=22.5, width=15, cloud=T))
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
  geom_hline(aes(yintercept = var(ndf2$Valor), color="Variance"), linetype="dashed",
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

ggplot(data.frame(map.vgm),aes(x=map.dx,y=map.dy,fill=map.Localidad)) +
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
vgm_residual_cloud = (variogram(Valor~Longitud+Latitud,ndf2,cutoff=30, cloud=T)) 
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

model_1 = lm(Valor~Longitud+Latitud, data=ndf)
ndf$predicted_1 = predict(model_1)
ndf$residuals_1 = residuals(model_1)

var(ndf$residuals_1)

# Semivariograma experimental omnidireccional de los residuos

g_trend = gstat(id='Valor', formula=Valor~Longitud+Latitud,data = ndf2) #objeto gstat 

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


ggplot(data.frame(map.vgm.resid),aes(x=map.dx,y=map.dy,fill=map.Valor)) +
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
  krige.cv(Valor ~ Longitud+Latitud, locations = ndf2, model = vgm_residual.fit_sph)
summary(cross_validation_sph)
summary(cross_validation_sph$residual)
class(cross_validation_sph)
View(cross_validation_sph)


# validación cruzada exponencial omnidireccional
cross_validation_exp = 
  krige.cv(Valor ~ Longitud+Latitud, locations = ndf2, model = vgm_residual.fit_exp)
summary(cross_validation_exp$residual)
class(cross_validation_exp)

# validación cruzada gaussiana omnidireccional
cross_validation_gau = 
  krige.cv(Valor ~ Longitud+Latitud, locations = ndf2, model = vgm_residual.fit_gau2)
summary(cross_validation_gau$residual)
class(cross_validation_gau)

# validación cruzada esférica  grados 
cross_validation_sph_135 = 
  krige.cv(Valor ~ Longitud+Latitud, locations = ndf2, model = vgm_residual.fit_sph_135)
summary(cross_validation_sph_135)
summary(cross_validation_sph_135$residual)
class(cross_validation_sph)
