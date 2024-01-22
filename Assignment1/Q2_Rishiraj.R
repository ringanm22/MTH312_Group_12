rm(list = ls())

#dataset loading ......
d <- read.csv("diabetes.csv")
d = na.omit(d)
data <- data.frame(x = d$Glucose , y = d$BloodPressure)
status = d$Outcome
#==============================================================================


#kernel density estimate
#install.packages("ks")
library(ks)
kd <- kde(data)
attach(kd)
#==============================================================================


#density estimate at the given data points
k =(predict(kd , x = as.matrix(data)))
#==============================================================================

#Contours
contour_90 <- data.frame(contourLines(eval.points[[1]], eval.points[[2]],estimate, levels = cont["10%"])[[1]])
contour_80 <- data.frame(contourLines(eval.points[[1]], eval.points[[2]],estimate, levels =cont["20%"])[[1]])
contour_70 <- data.frame(contourLines(eval.points[[1]], eval.points[[2]],estimate, levels =cont["30%"])[[1]])
contour_60 <- data.frame(contourLines(eval.points[[1]], eval.points[[2]],estimate, levels =cont["40%"])[[1]])
contour_50 <- data.frame(contourLines(eval.points[[1]], eval.points[[2]],estimate, levels =cont["50%"])[[1]])
contour_40 <- data.frame(contourLines(eval.points[[1]], eval.points[[2]],estimate, levels =cont["60%"])[[1]])
contour_30 <- data.frame(contourLines(eval.points[[1]], eval.points[[2]],estimate, levels =cont["70%"])[[1]])
contour_20 <- data.frame(contourLines(eval.points[[1]], eval.points[[2]],estimate, levels =cont["80%"])[[1]])
contour_10 <- data.frame(contourLines(eval.points[[1]], eval.points[[2]],estimate, levels =cont["90%"])[[1]])
#=============================================================================


#2d plot of the quantiles contours
library(ggplot2)
ggplot(data, aes(x, y)) +
  geom_point() +
  geom_path(data = contour_90, aes(x, y), lwd = 0.8, color = "red") +
  geom_path(data = contour_80, aes(x, y), lwd = 0.8, color = "red") +
  geom_path(data = contour_70, aes(x, y), lwd = 0.8, color = "red") +
  geom_path(data = contour_60, aes(x, y), lwd = 0.8, color = "red") +
  geom_path(data = contour_50, aes(x, y), lwd = 0.8, color = "red") +
  geom_path(data = contour_40, aes(x, y), lwd = 0.8, color = "red") +
  geom_path(data = contour_30, aes(x, y), lwd = 0.8, color = "red") +
  geom_path(data = contour_20, aes(x, y), lwd = 0.8, color = "red") +
  geom_path(data = contour_10, aes(x, y), lwd = 0.8, color = "red") +
  
  labs(x = "Glucose Conc.", y = "Blood Pressure") +
  theme_minimal()

#==============================================================================



#3D plot of the bivariate density with quantile contours....looks cool....:) 
#install.packages("plotly")
library(plotly)
density = data.frame(x = data$x , y = data$y , z = k)
plot1 = plot_ly(density, x = ~x , y = ~y , z = ~z , type = "scatter3d" , color = k , opacity = 0.2 , marker = list(size = 6))
plot2 = plot_ly(contour_90 , x =~x, y = ~y, z = ~level ,type = "scatter3d", mode = "lines", line = list(color = "red" , width = 3))
plot3 = plot_ly(contour_80 , x =~x, y = ~y, z = ~level ,type = "scatter3d", mode = "lines", line = list(color = "red", width = 3) )
plot4 = plot_ly(contour_70 , x =~x, y = ~y, z = ~level ,type = "scatter3d", mode = "lines", line = list(color = "red", width = 3) )
plot5 = plot_ly(contour_60 , x =~x, y = ~y, z = ~level ,type = "scatter3d", mode = "lines", line = list(color = "red", width = 3) )
plot6 = plot_ly(contour_50 , x =~x, y = ~y, z = ~level ,type = "scatter3d", mode = "lines", line = list(color = "red", width = 3) )
plot7 = plot_ly(contour_40 , x =~x, y = ~y, z = ~level ,type = "scatter3d", mode = "lines", line = list(color = "red", width = 3) )
plot8 = plot_ly(contour_30 , x =~x, y = ~y, z = ~level ,type = "scatter3d", mode = "lines", line = list(color = "red", width = 3) )
plot9 = plot_ly(contour_20 , x =~x, y = ~y, z = ~level ,type = "scatter3d", mode = "lines", line = list(color = "red", width = 3) )
plot10= plot_ly(contour_10 , x =~x, y = ~y, z = ~level ,type = "scatter3d", mode = "lines", line = list(color = "red", width = 3) )
subplot_plot = subplot(plot1 , plot2, plot3 , plot4, plot5, plot6, plot7, plot8, plot9, plot10)
subplot_plot


#===============================================================================
# 2D Density plot (Heat Map)
#install.packages("viridis")
library(viridis)
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "tile", contour = FALSE) +
  theme(
    legend.position='right'
  )+
  scale_fill_viridis()+
  labs(title = "2D Density Plot", x = "Glucose Conc.", y = "Blood Pressure")

#==============================================================================

#Sunburst plot for bivariate data

#install.packages("aplpack")
library(aplpack)
bagplot(data,create.plot=TRUE,
        show.outlier=TRUE,show.looppoints=TRUE,
        show.bagpoints=TRUE,dkmethod=2,
        show.whiskers=TRUE,show.loophull=FALSE,
        show.baghull=TRUE,verbose=FALSE, xlab = "glucose conc.", ylab = "Blood Pressure" , main= "Sunburst plot")



