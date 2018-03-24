#Script reads in all the raw fit files, since fit is not a supported data file need to do it here
library("ProjectTemplate")
load.project()

#Read in files
#-----------------------------------------------
setwd("data")
files <- list.files()
files <- files[-which((files=="README.md"))]
dat <- list()
names_record <- c( "accumulated_power" ,"altitude","cadence","distance","fractional_cadence",
            "heart_rate","position_lat","position_long","power","speed","temperature","timestamp")


for(i in 1:length(files)){
   dat[[i]] <- as.list(1:14)
   dat[[i]] <- read.fit(files[i])
   
   #For record include all names
   tmp <- names_record[-match(names(dat[[i]]$record),names_record)]
   dat_tmp <- as.data.frame(matrix(0,nrow = length(dat[[i]]$record[,1]),ncol = length(tmp)))
   #replaces missing columns with columns of zeros
   names(dat_tmp) <- tmp
   dat[[i]]$record <- cbind(dat[[i]]$record,dat_tmp)
   dat[[i]]$record <-  dat[[i]]$record[match(names_record,names(dat[[i]]$record))]
   
   #For session include all names
   tmp <- names_session[-match(names(dat[[i]]$session),names_session)]
   dat_tmp <- as.data.frame(matrix(0,nrow = length(dat[[i]]$session[,1]),ncol = length(tmp)))
   #replaces missing columns with columns of zeros
   names(dat_tmp) <- tmp
   dat[[i]]$session <- cbind(dat[[i]]$session,dat_tmp)
   dat[[i]]$session <-  dat[[i]]$session[match(names_session,names(dat[[i]]$session))]
}
#------------------------------------------------



#Check to see if timestamp is every second - it is
# ----------------------------------------------------
# for(i in 1:length(dat)){
#   tmp = dat[[i]]$record$timestamp
#   for(j in 3:(length(dat[[i]]$record$power)-2)){
#     dat[[i]]$record$timestamp[j] = tmp[j] - tmp[(j-1)]
#   }
# }
#------------------------------------------------------


#Report missingness in each file
#------------------------------------------------------
 missingness <- function(data){
   tmp <- matrix(rep(0,length(data$record[1,])),nrow=1)
   results <- as.data.frame(tmp)
   names(results) <- names(data$record)
   results[1,] <- apply(data$record,2,function(x){sum(is.na(x))})
  return(results)
}

missing_list <- lapply(dat,missingness)

tmp <- lapply(missing_list,function(x){length(names(x))})
summary(unlist(tmp))
problems <- which(tmp ==7)
#problem all data files do not have same number of columns
problems <- dat[as.vector(problems)]
#solution = in processing step add all coloumns so all data files are the same.

missingness_summary<- function(var,missing_list){
   tmp <-lapply(missing_list,function(x){x[match(var,names(missing_list[[1]]))] })
   return(summary(unlist(tmp)))
 }

tmp <- lapply(names_record,missingness_summary,missing_list=missing_list)
names(tmp) <- names_record

#genrally low levels of missingness
#imputations:
#set to most recent non na values since our values are very correlated

imputation <- function(data,names){
  imputation_var<- function(var,data){
    tmp<- which(is.na(select(data$record,var)))
    tmp_imp <- rep(0,length(data$record[,1]))
    for(i in tmp){tmp_imp[i]= max(which(as.logical( 1- is.na(select(data$record,var)[seq(1,i),1])*1) ))}
    for(i in tmp){if((tmp_imp[i] == -Inf)){
      tmp_imp[i]= min(which(as.logical(1-is.na(select(data$record,var)[seq(i,length(data$record[,1])),1]*1)))) +(i-1)
    }
    }
    data$record[,match(var,names(data$record))][tmp] <- data$record[,match(var,names(data$record))][tmp_imp[tmp]]
    return(data)
    }
  for(j in names){
    data <- imputation_var(j,data)
  }
  return(data)
  }

dat <- lapply(dat,imputation,names=names_record)
#
# missing_list <- lapply(dat,missingness)
# tmp <- lapply(names,missingness_summary,missing_list=missing_list)
# names(tmp) <- names
# print(tmp)
# missingness now erased
#------------------------------------------------------

#Remove unneeded elements of list provided
#-----------------------------------------------------
drop <- c("file_id","device_settings","sport","event","device_info","activity","file_creator","hrv","unknown")
dat <- lapply(dat, function(x){x[-na.omit(match(drop,names(x)))]})
rm(drop)
#------------------------------------------------------

#Look at autmatic FTP
#------------------------------------------------------
tmp <- lapply(dat,function(data){return(data$zones_target$functional_threshold_power)})
#not that useful only has one change in FTP over the 6 months - disregard and look for 20 minute tests
#------------------------------------------------------

#------------------------------------------------------
#Add some common ride data
#Add 5 second weighted power to the $record for each file#

for(i in 1:length(dat)){
  power_5 <- rep(0,length(dat[[i]]$record$power))
  tmp <- dat[[i]]$record$power
  for(j in 3:(length(dat[[i]]$record$power)-2)){
    power_5[j] = mean(tmp[(j-2):(j+2)])
  }
  dat[[i]]$record = cbind(dat[[i]]$record,as.data.frame(power_5))
}

#Add 30 second weighted power to the $record for each file
#This is used in calculation of TSS and Normalised Power

for(i in 1:length(dat)){
  power_30 <- rep(0,length(dat[[i]]$record$power))
  tmp <- dat[[i]]$record$power
  for(j in 15:(length(dat[[i]]$record$power)-15)){
    power_30[j] = mean(tmp[(j-14):(j+15)])
  }
  dat[[i]]$record = cbind(dat[[i]]$record,as.data.frame(power_30))
}

#Add gradient_5 to the $record - based on the altitude change over 5 seconds

for(i in 1:length(dat)){
  gradient_5 <- rep(0,length(dat[[i]]$record$altitude))
  alt <- dat[[i]]$record$altitude
  dist <- dat[[i]]$record$distance
    for(j in 3:(length(dat[[i]]$record$power)-2)){
    gradient_5[j] = (alt[j+2]-alt[j-2])/(dist[j+2]-dist[j-2])
  }
  dat[[i]]$record = cbind(dat[[i]]$record,as.data.frame(gradient_5))
}

#Add intervals to an intervals tab
intervals_add <- function(data,lengths,number){
  intervals <- data.frame(init =0)
  for(i in lengths){
    if(length(data$record[,1])<i){break}
    tmp <- roll_mean(data$record$power,i,align = "center")
    for(j in 1:number){
      names <- c(names(intervals),paste("interval_",i,"_",j,sep=""))
      intervals <- cbind(intervals,max(tmp))
      names(intervals) <- names
      tmp[max((match(max(tmp),tmp) - ceiling(i/2)),0) : min((match(max(tmp),tmp) + ceiling(i/2)),length(tmp)) ] <- 0
    }
  }
  intervals <- intervals[,-1]
  names <- names(data)
  data[[(length(data)+1)]] <- intervals
  names(data) <- c(names,"intervals")
  return(data)
}
dat <- lapply(dat,intervals_add,lengths = c(5,20,30,60,180,300,600,1200,3600),10)
#Checked intervals on dat[[1]] with Golden Cheetah and match up! yay!

add_date <- function(data,file){
  names <- names(data)
  data[length(data)+1] <- ymd(substring(file,1,10))
  names(data) <- c(names,"date")
  return(data)
}

dat <- mapply(add_date,dat,files,SIMPLIFY = FALSE)
#------------------------------------------------------


#------------------------------------------------------
#Cache Data File
setwd("..")
cache(variable = "dat")
cache(variable = "files")
cache(variable = "names_session")
cache(variable = "names_record")
#------------------------------------------------------



