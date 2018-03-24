#This script carries out PCA and looks at biplots to come up with a substantive interpretation
#of the characteristics of a ride.
#Look into whether we can classify rides as certain types e.g. races, intervals, group rides, solo rides.

require(ProjectTemplate)
load.project()

names(profile_session)
drop <- c("event","event_type","first_lap_index","sport","sub_sport","timestamp","avg_fractional_cadence","message_index","max_fractional_cadence",
          "time_in_power_zone_1","total_fat_calories","trigger")
profile_session_dropped <- profile_session[,-match(drop,names(profile_session))]

PCA_1 <- prcomp(profile_session[,-1],center= TRUE,scale = TRUE)

cache(variable = "PCA_1")
cache(variable = "profile_session_dropped")

PC1 <-PCA_1$rotation[,1]
PC2 <- PCA_1$rotation[,2]
PC3 <- PCA_1$rotation[,3]
PC4 <- PCA_1$rotation[,4]

tmp<- cbind(seq(1,43,1),summary(PCA_1)[[6]][2,])
tmp <- as.data.frame(tmp)
names(tmp) <- c("PC_Index","Var")
plot_var <- ggplot(data = tmp, aes(x= PC_Index,y = Var ))+
                      geom_col(fill = "Deep Sky Blue",color = "Dark Orange")+
                      xlab("PC Index")+
                      ylab("Proportion of Variance")+
                      ggtitle("Bar chart of importance of principle components")

print(plot_var)      

#returns projection of an observation onto specified PCs
PC_project <- function(obs,PC1,PC2){
  return(c(sum(obs*PC1),sum(obs*PC2)))
}



#returns line for specified variable in specified PCs
PC_line <- function(var,PC1,PC2,names,start,end){
  match <- match(var,names)
  tmp1 <- seq(start,end,0.01)
  tmp2 <- (1- PC1[match]*tmp1)/PC2[match]
  line <- data.frame(PC1 = tmp1,PC2 = tmp2)
  return(line)
}

profile_std <- scale(profile_session_dropped[,-1],center = TRUE,scale = TRUE)

project_1_2 <- apply(profile_std,1,PC_project,PC1 = PC1,PC2 = PC2)
project_1_2 <- data.frame("PC1" =project_1_2[1,],"PC2" =project_1_2[2,])

project_2_3 <- apply(profile_std,1,PC_project,PC1 = PC2,PC2 = PC3)
project_2_3 <- data.frame("PC2" =project_2_3[1,],"PC3" =project_2_3[2,])

PC_line("normalized_power",PC1,PC2,names(profile_session_dropped[,-1]))

lines_1_2 <- lapply(names(profile_session_dropped[,-1]),PC_line,PC1=PC1,PC2=PC2,names = names(profile_session_dropped[,-1]),start = -10, end = 10)
names(lines_1_2) <- names(profile_session_dropped[,-1])
lines_2_3 <- lapply(names(profile_session_dropped[,-1]),PC_line,PC1=PC2,PC2=PC3,names = names(profile_session_dropped[,-1]),start = -5,end = 2.5)
names(lines_2_3) <- names(profile_session_dropped[,-1])

#Make a  biplot

biplot1 <- ggplot(data = project_1_2, aes(x = PC1,y= PC2))+
              geom_point(color = "Dark Orange",position=position_jitter(width=1,height=.1))+
  
              geom_line(data = lines_1_2$avg_power, aes(x = PC1,y= PC2), color = "Deep Sky Blue")+
              annotate("text",label="Average Power",x = 2.5,y=10,size = 3 )+
  
              geom_line(data = lines_1_2$training_stress_score, aes(x = PC1,y= PC2), color = "Deep Sky Blue")+
              annotate("text",label="TSS",x = 0,y=30,size = 3 )+

              geom_line(data = lines_1_2$total_distance, aes(x = PC1,y= PC2), color = "Deep Sky Blue")+
              annotate("text",label="Distance",x = 0,y=-10,size = 3 )+
  
              geom_line(data = lines_1_2$total_elapsed_time, aes(x = PC1,y= PC2), color = "Deep Sky Blue")+
              annotate("text",label="Time",x = 5,y=-5,size = 3 )+
  
              ggtitle("Biplot on first and second principle components")

print(biplot1)

tmp <- cbind(seq(1,length(project_2_3[,1])),project_2_3,(project_1_2[,1]>0)*1)
tmp <- as.data.frame(tmp)
names(tmp) <- c("id","PC2","PC3","PC1")
tmp$PC1[tmp$PC1==1] <- "large PC1"
tmp$PC1[tmp$PC1==0] <- "small PC1"
tmp <- melt(tmp,id.vars =c("id","PC2","PC3"))

biplot2 <- ggplot(data = tmp, aes(x = PC2,y=PC3, color = value))+
                   scale_color_manual(values = c("Deep Sky Blue","Dark Orange"))+
                    geom_point(position=position_jitter(width=1,height=.1))+
                    
                   geom_line(data = lines_2_3$total_elapsed_time, aes(x = PC1,y= PC2),color = "Lawn Green")+
                   annotate("text",label="Time",x = 2.5,y=80,size = 3 )+
  
                    geom_line(data = lines_2_3$time_in_power_zone_9, aes(x = PC1,y= PC2),color = "Lawn Green")+
                    annotate("text",label="Power Zone 9",x = 2.5,y=3,size = 3 )+
  
                  geom_line(data = lines_2_3$max_heart_rate, aes(x = PC1,y= PC2),color = "Lawn Green")+
                  annotate("text",label="max heart rate",x = 2.5,y=15,size = 3 )+
  
                geom_line(data = lines_2_3$training_stress_score, aes(x = PC1,y= PC2),color = "Lawn Green")+
                 annotate("text",label="TSS",x = 2.5,y=-5,size = 3 )+
  
                  geom_line(data = lines_2_3$time_in_power_zone_2, aes(x = PC1,y= PC2),color = "Lawn Green")+
                  annotate("text",label="power zone 2",x = 2.5,y=-15,size = 3 )+
  
  
  
                    
                  ggtitle("Biplot on second and third principle components")
                   
  print(biplot2)
  
cache(variable = "biplot1")
cache(variable = "biplot2")


