#setwd("C:/Users/Duncan/Documents/Academics/UCLA_Academics/Classes/Stats_202B/CyclingDataAnalysis")
library('ProjectTemplate')
load.project()

#--------------------------------------------------------------
#This script carries out exploratory graphical analysis for each fit file in the data directory
#
setwd("graphs")
for(i in files){
  if(is.na(match(i,list.files())) == TRUE){
    dir.create(i)
  }
}

graphs_create <- function(dat,files){
  setwd(paste(files))
  pdf("plots.pdf")
  
  # plot <- ggplot(dat$record, aes(x = position_lat, y= position_long,  colour = power))+
  #           geom_jitter(width =0.002)+
  #           scale_colour_gradient(low = "yellow",high="red")
  # print(plot)
  
  plot <- ggplot(dat$record, aes(x = position_lat, y= position_long,  colour = power_5))+
    geom_jitter(width =0.002)+
    scale_colour_gradient(low = "yellow",high="red")
  print(plot)
  # 
  # plot <- ggplot(dat$record, aes(x = position_lat, y= position_long,  colour = altitude))+
  #   geom_jitter(width =0.002)+
  #   scale_colour_gradient(low = "yellow",high="red")
  # print(plot)
  
  plot <- ggplot(dat$record, aes(x = position_lat, y= position_long,  colour = speed))+
    geom_jitter(width =0.002)+
    scale_colour_gradient(low = "yellow",high="red")
  print(plot)
  
  plot <- ggplot(dat$record, aes(x = position_lat, y= position_long,  colour = heart_rate))+
    geom_jitter(width =0.002)+
    scale_colour_gradient(low = "yellow",high="red")
  print(plot)
# 
#   plot <- ggplot(dat$record, aes(x=heart_rate,y=power))+
#     geom_point(size=0.1,colour = "Dark Orange")
#   print(plot)
#   
#   plot <- ggplot(dat$record, aes(x=heart_rate,y=power_5))+
#     geom_point(size=0.1,colour = "Dark Orange")
#   print(plot)
#   
#   plot <- ggplot(dat$record, aes(x=heart_rate,y=cadence))+
#     geom_point(size=0.1,colour = "Dark Orange")
#   print(plot)
#   
#   plot <- ggplot(dat$record, aes(x=cadence,y=power))+
#     geom_point(size=0.1,colour = "Dark Orange")
#   print(plot)
#   
#   plot <- ggplot(dat$record, aes(x=cadence,y=power_5))+
#     geom_point(size=0.1,colour = "Dark Orange")
#   print(plot)
  dev.off()
  setwd("..")
  }

lapply(dat,graphs_create,files = files)

