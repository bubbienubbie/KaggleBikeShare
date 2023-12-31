

library(tidyverse)
library(vroom)
library(DataExplorer)
library(GGally)

bike <- vroom("C:/Users/isaac/KaggleBikeShare/train.csv")

dplyr::glimpse(bike)
DataExplorer::plot_intro(bike)
DataExplorer::plot_correlation(bike)
DataExplorer::plot_bar(bike)
DataExplorer::plot_histrograms(bike)
DataExplorer::plot_missing(bike) 
GGally::ggpairs(bike)

plot1 <- ggplot(data=bike, aes(x=temp, y=count)) + geom_point() + geom_smooth(se=FALSE)
plot2 <- ggplot(data=bike, aes(x=atemp, y=count)) + geom_point() + geom_smooth(se=FALSE)
plot3 <- ggplot(data=bike, aes(x=windspeed, y=count)) + geom_point() + geom_smooth(se=FALSE)
plot4 <- ggplot(data=bike, aes(x=humidity, y=count)) + geom_point() + geom_smooth(se=FALSE)
(plot1 + plot2) / (plot3 + plot4)


install.packages("tidymodels")
library(tidymodels)






