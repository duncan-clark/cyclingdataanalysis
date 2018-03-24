setwd("C:/Users/Duncan/Documents/Academics/UCLA_Academics/Classes/Stats_202B/CyclingDataAnalysis")
library('ProjectTemplate')
load.project()


#--------------------------------------------------------------
#This script carries calculates the usual Coggan metric i.e. TSB = CTL - ATL
#and hopes to use this to predict performance in tests

dates <- seq(ymd(substring(files[1],1,10)) ,ymd(substring(files[length(files)],1,10)) +1,1)
profile <- as.data.frame(matrix(0,nrow=length(dates),ncol = 6))
names(profile) <- c("date","FTP","Day_TSS","ATL","CTL","TSB")
profile$date <- dates

FTP_init <- 0.95*309
profile$FTP <- FTP_init
## lookup FTP based on most recent FTP test, test dates

FTP_update <- function(data,profile,date){
  FTP <- profile$FTP[profile$date == (date-1)]
  data <- data[as.vector(lapply(data,function(x){x$date})) == date]
  if(length(data) == 0){return(FTP)}
  if(is.null(lapply(data,function(x){x$intervals$interval_1200_1}))){return(FTP)}
  interval_20 <- max(unlist(lapply(data,function(x){x$intervals$interval_1200_1})))
  if(interval_20*0.95 > FTP){FTP = interval_20*0.95}
  return(FTP)
}

for(i in 2:length(profile[,1])){
  profile$FTP[i] <- FTP_update(dat,profile,as.numeric(profile$date[i]))
}

#08/01/2017 338W - not useable - no data from France trip
#05/08/2017 328W
#03/07/2017 327W
#07/17/2017 311W
#01/22/2017 311W
#02/25/2017 310W

#5 minute interval session date
#05/25/2017 381W
#05/27/2017 379W
#04/22/2017 369W
#03/01/2017 366W
#01/28/2017 365W
#03/06/2017 360W
#....... actually quite a lot of these observations this might be the one to consider - can assume I was 
#going pretty much all out in most cases, since never do a five minute effort in race - so was during interval
#training, and always went full gas for first interval.

#-----------------------------------------------------------------
#add extra element to each ride detaling Coggan metrics for the ride
#i.e. Normalised Power, IF, TSS, VI  
#$record is the data frame with the second by second data.
Coggan_add <- function(data){
  names <- c("FTP","Normalised_Power","IF","TSS","VI")
  Coggan <- matrix(0,ncol = length(names))
  colnames(Coggan) <- names
  data <- c(data,Coggan)
  names(data) <- c(names(data)[-length(data)],"Coggan")
  data$Coggan <- as.data.frame(Coggan)
  return(data)
}
dat <- lapply(dat,Coggan_add)

FTP_add <- function(data,profile){
  data$Coggan$FTP[1] <- max(profile$FTP[profile$date == as.Date(data$date,origin = "1970-01-01")])
  return(data)
}
dat <- lapply(dat,FTP_add,profile = profile)

Normalised_Power_add <- function(data){
 data$Coggan$Normalised_Power[1] <- mean((data$record$power_30)**4)**(1/4) 
  return(data)
 }
dat <- lapply(dat,Normalised_Power_add)

IF_add <- function(data){
  data$Coggan$IF[1] <- data$Coggan$Normalised/data$Coggan$FTP
  return(data)
}  
dat <- lapply(dat,IF_add)

TSS_add <- function(data){
  data$Coggan$TSS[1] <- length(data$record$timestamp)*(data$Coggan$IF**2)/36
  return(data)
}
dat <- lapply(dat,TSS_add)

VI_add <- function(data){
  data$Coggan$VI[1] <- data$Coggan$Normalised_Power/mean(data$record$power)
  return(data)
}
dat <- lapply(dat,VI_add)

#Add TSS, ATL,CTL and TSB for profile

  
sum_TSS <- function(data,date){
  data <- data[lapply(data,function(x){x$date}) == as.numeric(date)]
  TSS <- sum(unlist(lapply(data,function(x){x$Coggan$TSS})))
  return(TSS)
}
profile$Day_TSS <- sapply(profile$date,sum_TSS,data= dat)
  
add_ATL_CTL_TSB <- function(profile){
  profile$CTL <- c(rep(0,41),roll_mean(profile$Day_TSS,n= 42, align="left"))
  profile$ATL <- c(rep(0,6),roll_mean(profile$Day_TSS,n= 7, align="left"))
  profile$TSB <- profile$CTL - profile$ATL
  profile$TSB[1:41] <- 0
  return(profile)}
profile <- add_ATL_CTL_TSB(profile)

#Add our outcome to profile - 5 minute efforts wattages

intervals_300 <- data.frame(matrix(0,nrow = length(profile$date),ncol=2))
names(intervals_300) <- c("date","intervals_300")  
intervals_300$date <- profile$date
tmp_int <- lapply(dat,function(x){
  if(is.null(x$intervals$interval_300_1)){return(0)}
  return(x$intervals$interval_300_1)})
tmp_date <- lapply(dat,function(x){
  return(as.Date(x$date,origin = "1970-01-01"))})


intervals_300$intervals_300 <- lapply(intervals_300$date,function(x)
{if(is.null(unlist(tmp_int[tmp_date == x]))){return(0)}
  return(max(unlist(tmp_int[tmp_date == x])))})

intervals_300 <- intervals_300[match(unique(intervals_300$date),intervals_300$date),]

#Set intervasl less than 350W = 0 since they're not true intervals
cutoff = 350
intervals_300$intervals_300[intervals_300$intervals_300 <cutoff] <- 0 
intervals_300$intervals_300 <- unlist(intervals_300$intervals_300)
rm(cutoff)

profile <- merge(profile,intervals_300,by = "date")
rm(intervals_300)


#----------------------------------------------------------------

#Plot graphs
#----------------------------------------------------------------
profile_long <- profile
#profile_long$date <- as.numeric(profile_long$date,origin = "1970-01-01")
profile_long <- melt(profile_long,id.vars = "date")
outcome_long <- profile_long[(profile_long$variable == "intervals_300"),]
outcome_long <- outcome_long[outcome_long$value !=0,]
outcome_long$value <- 0 
outcome_long$variable <- "outcome dates"

setwd("graphs")
if(is.na(match("Coggan_Graphs",list.files() ) )){
dir.create("Coggan_Graphs")}
setwd("Coggan_Graphs")
profile_plot <- ggplot(data =filter(profile_long, variable == "ATL" | variable == "CTL"|variable == "TSB"),
                          aes(x=date,y=value,color = variable))+
                          geom_line(size=1)+
                          #geom_hline(linetype = "dashed",size =1, yintercept =0,color = "Black")+
                          geom_point(data = outcome_long, aes(x = date, y= value) )+
                          scale_color_manual(values = c("Deep Sky Blue","Dark Orange", "Magenta","Lawn Green"))+
                          ggtitle("Plot of Coggan's Metrics")+
                          ylab("TSS")+
                          xlab("Time")+
                          scale_x_date(date_breaks = "1 month",date_labels = "%b%y")
                          
pdf("TSB.pdf")                        
print(profile_plot)
dev.off()
#Great - we have Coggan's graphs -  but can we do better than this????
#----------------------------------------------------------


#Cache profile for later use:
setwd("..")
setwd("..")
cache(variable = "profile")



