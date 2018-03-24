library('ProjectTemplate')
load.project()

#####Exploratory Models#####

#Formulate our problem as follows firstly: predict heartrate based on other
#variables

lm <- lm(data=dat[[1]]$record, heart_rate ~ power + power_5 + cadence +
           speed + altitude)

lm2 <- lm(data=dat[[1]]$record, heart_rate ~ power + power_5 + cadence +
            speed + altitude + gradient_5)

lm3 <- lm(data=dat[[2]]$record, heart_rate ~ power + power_5 + cadence +
                   speed + altitude + gradient_5)

predict.lm(lm,dat[[1]]$record[150,])

tmp <- data.frame(fitted = lm2$fitted.values,actual = dat[[1]]$record$heart_rate)
colnames(tmp) <- c("fitted","actual")

plot <- ggplot(data = tmp,aes(x=fitted, y= actual))+
  geom_jitter(colour = "Dark Orange",size =0.01)+
  geom_line(data = data.frame(x=seq(125,200),y=seq(125,200)), aes(x=x,y=y))+
  geom_segment(aes(x=125,y=125,xend=180,yend=180))

print(plot)

#Seems to be some sort of fit - but probably not that interesting or useful

#Lets add it all the interaction terms!
vars <- c("heart_rate","power","power_5","cadence","speed","altitude","gradient_5")
data <- dat[[1]]$record[,match(vars,names(dat[[1]]$record))]

lm4 <- lm(data=data,heart_rate ~ .^2.) 
summary(lm4)

#Now try to predict power from other variables

lm5 <- lm(data=dat[[1]]$record, power_5 ~ heart_rate + cadence +
           speed + altitude + gradient_5)

#These linear models are basically useless for prediction, and don't really tell us anything we don't 
#already know from basic cycling knowledge.




