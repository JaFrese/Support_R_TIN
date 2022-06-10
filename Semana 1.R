############################################################################
# Clase Semana 1 de TIN Agosto 2020
############################################################################
rm(list=ls()) # Para limpiar todas las variables
graphics.off() # Borra todos los gráficos
#setwd("~/Dropbox/AAA - TIN/TIN 2020 II SEM")
#getwd()
#dir()
#install.packages('tidyverse')
library(tidyverse)
#head(mpg)
plot(mpg$displ,mpg$hwy,col='red',xlab='displ',ylab='hwy',main='Autos',cex=1)
#help('plot')
# Usando ggplot para visualizar
#help('mpg')
#names(mpg)
#mpg$class
mpg$label <- c(rep("v",102), rep("f",132)) #agregar columna en la base de datos

library(ggplot2)
ggplot(data = mpg, aes(x=label, y=hwy, fill=label))+geom_boxplot()
ggplot(data = mpg, aes(x=cty, y=hwy, color=cty))+geom_point()+geom_smooth(method = "lm")
#ggplot(data=mpg)+geom_boxplot(aes(x=class,y=displ,fill=drv))
p=ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,color=drv)) +
  geom_smooth(mapping=aes(x=displ,y=hwy,color=drv),se=T,method = 'loess') # podemos guardar el resultado en la varaible p
#p

p+scale_y_continuous(breaks = seq(0, 50, 5)) # para cambiar la escala del eje y

# Se puede usar theme para cambiar muchas opciones de la gráfica.
#help('theme')

ggplot(data=mpg,mapping=aes(x=displ,y=hwy,color=class))+geom_point() +
  geom_smooth(data=filter(mpg,class=='subcompact'),se=F)

ggplot(data=mpg,mapping=aes(x=displ,y=hwy,color=class))+geom_point()+facet_wrap(~class,nrow=2)

ggplot(mpg, aes(displ, hwy)) + geom_point(colour = "blue")

ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span = 0.2)

#install.packages('directlabels')
# Para colocar etiquetas sobre los puntos:
ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point(show.legend = FALSE) +
  directlabels::geom_dl(aes(label = class), method = "smart.grid")
ggplot(mpg, aes(class)) + geom_bar()
ggplot(mpg, aes(class, fill = drv)) + geom_bar()+xlab('Clases de autos')
#View(mpg)
mod <- lm(mpg$hwy~mpg$cty)
summary(mod)
library(plotly)
plot_ly(mpg, x=~label,y=~hwy,type = "box",boxpoints="all",jitter=0.5,pointpos=-2,color = ~label)
#quantile(mpg$hwy,0.25,0.5,0.75) #expresar los cuantiles de forma numérica
#mean(mpg$hwy)
#mean(mpg$hwy[1:102])
#mean(mpg$hwy[103:234])

#####################################################################################################
# Usamos otro dataset
#Billetes Falsos:
bank2=read.table('C:/Users/pc/Desktop/bases de datos csv y excel/bank2.dat')
#head(bank2)
names(bank2)=c('X1','X2','X3','X4','X5','X6')
#head(bank2) # nombres y 6 filas del data.frame
bank.df<-data.frame(bank2,label=c(rep('v',100),rep('f',100))) # Agrgemos una columna adicional
#head(bank.df)

#write.csv(bank.df,'bank.csv',row.names=F) # para guardar el documento en formato csv
#row.names(bank.df)

#####################################################################################################

#Boxplot (Básico)
#m1 = mean(bank.df[1:100,6])
#m2 = mean(bank.df[101:200,6])
#boxplot(bank.df[1:100,6], bank.df[101:200,6], axes=FALSE, frame=TRUE)
#axis(side=1, at=seq(1,2),label=c('VERDADERO','FALSO') )
#axis(side=2, at=seq(130,150,.5), label=seq(130,150,.5))
#title('Billetes')
#lines(c(0.6, 1.4),c(m1, m1),lty="dotted",lwd=1.2,col="red")
#lines(c(1.6, 2.4),c(m2, m2),lty="dotted",lwd=1.2,col='blue')

# Usando ggplot para graficar boxplots

ggplot(data=bank.df) + geom_boxplot(mapping=aes(x=label,y=X6,fill=label))
# usando plotly
library('plotly')
datos=bank.df

plot_ly(datos,x=~label, y=~X6,type='box',boxpoints='all',jitter=0.5,pointpos=-2,color=~label, colors = c('#BF382A', '#0C4B8E'))

# Diagrama de dispeprsión 3D

#plot_ly(datos, x = ~datos$X6, y = ~datos$X1, z = ~datos$X2, color = ~datos$label, colors = c('#BF382A', '#0C4B8E')) %>%
#  add_markers() %>%
#  layout(scene = list(xaxis = list(title = 'Diagonal'),
#                      yaxis = list(title = 'Longitud'),
#                      zaxis = list(title = 'Altura')))
# Boxplot

plot_ly(datos, y = ~X6, color = ~label, type = "box")

#Histograma
x = bank.df[101:200,6];# datos de la diagonal para los billetes falsos
#min(x)
#max(x)
origin = 137.75;
quantile(x,c(0.25))
quantile(x)

# Histograma básico
#y = seq(137.75,141.05,0.1)
#hist(x,y, ylab="Diagonal", xlab="h = 0.1", xlim=c(137.5,141), ylim=c(0,10.5), main="Billetes", axes=FALSE)
#axis(side=1, at=seq(138,141), labels=seq(138,141))
#axis(side=2, at=seq(0,10,2),labels=seq(0,10,2))

x=seq(-3.14,3.14,0.1)
plot(x,sin(x),col='red',xlab='x',ylab='f(x)=sen(x)',main='Función Seno',type='o')

# Gráfico de funciones en 3D
x <- seq(-pi, pi, by = 0.2)
y <- seq(-pi, pi, by = 0.3)
parabola=function(x,y){x^2+y^2}
z=outer(x,y,parabola)

persp(x, y, z, phi = 45, theta = 45,
      xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
      main = "Surface elevation data"
)
library(plotly)
#p <- plot_ly(z = ~z) %>% add_surface()
#p

#p <- plot_ly(z = ~z) %>% add_surface(contours = list(
#  z = list(
#    show=TRUE,
#    usecolormap=TRUE,
#    highlightcolor="#ff0000",
#    project=list(z=TRUE)
#  )
#)
#) %>%
#  layout(
#    scene = list(
#      camera=list(
#        eye = list(x=1.87, y=0.88, z=-0.64)
#      )
#    ))
#p
