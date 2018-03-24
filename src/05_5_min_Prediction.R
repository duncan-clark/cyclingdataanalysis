setwd("C:/Users/Duncan/Documents/Academics/UCLA_Academics/Classes/Stats_202B/CyclingDataAnalysis")
library('ProjectTemplate')
load.project()
#source("src/04_Coggan_Metrics.R")


#------------------------------------------------#
#1) Use CTL and ATL in linear model to predict five minute efforts:
CTL_ATL_FTP_lm <- lm(data = profile[(profile$intervals_300 != 0) & (profile$CTL != 0) ,], intervals_300 ~ CTL + ATL + FTP)
summary(CTL_ATL_FTP_lm)

CTL_ATL_lm  <- lm(data = profile[(profile$intervals_300 != 0) & (profile$CTL != 0) ,], intervals_300 ~ CTL + ATL)
summary(CTL_ATL_lm)

TSB_lm <- lm(data = profile[(profile$intervals_300 != 0) & (profile$CTL != 0) ,], intervals_300 ~ TSB)
summary(TSB_lm)

y <- profile[(profile$intervals_300 != 0),]$intervals_300
X <- select(profile,c("ATL","CTL","FTP"))
X <- X[(profile$intervals_300 != 0),]
X <- as.matrix(X)
CTL_ATL_FTP_KRLS <- bigKRLS(y= y,X = X)

#plot(CTL_ATL_lm)
#plot(density(CTL_ATL_lm$residuals))
#plot(density(TSB_lm$residuals))
#doesnt seem  unreasonabley normal
#fit is bad - doesn't seem to help no significant at all CTL and ATL coefficients
#the question is : can we do better???

#------------------------------------------------#
#2) Use session info for each data set to see if we can do any better with more variables,
#here we are including time as a variables in itself
#big thing here is that our observations are very obviously not independent.
#for each observation of our outcome i.e. 5 minute efforts - we need to have variables
#for each of the workout metrics for each time.
#try using just FTP and Day_TSS to start with.
#then include all session variables included in the $session data frame for each entry

####Include FTP####
dat_reg <- data.frame(matrix(0,nrow = length(profile$date),ncol =0))
dat_reg$date <- as.numeric(profile$date)
t_0 <- dat_reg$date[1]
t_n <- dat_reg$date[length(dat_reg$date)]
dat_reg$intervals_300 <- profile$intervals_300
dat_reg <- dat_reg[(dat_reg$intervals_300 != 0),]

tmp <- function(x){
  return(c(paste("FTP_",x,sep=""),paste("Day_TSS_",x,sep="")))
}
names <- lapply(seq(1,t_n - t_0),tmp)
names <- do.call(c,names)
rm(tmp)

for(i in 1:length(names)){
  dat_reg[[i+2]] <- 0
}

Past_TSS_FTP_lookup <- function(date,profile,t_0,t_n){
                        profile$date <- as.numeric(profile$date)
                        tmp <- rep(0,(date- t_0)*2)
                        for(i in seq(1,(date- t_0)*2,by = 2)){
                          tmp[i] <- profile$FTP[profile$date == (date - (i+1)/2)]
                          tmp[(i+1)] <- profile$Day_TSS[profile$date == (date - (i+1)/2)]
                          }
                        return(c(tmp,rep(0,(t_n - date)*2)))
                        }

tmp <- lapply(dat_reg$date,Past_TSS_FTP_lookup, profile = profile,t_0=t_0,t_n=t_n)
tmp <- do.call(rbind,tmp)
dat_reg[,3:length(dat_reg[1,])] <- tmp
names(dat_reg)  <- c("date","intervals_300",names)
rm(tmp)

lambdas <- 10^seq(-3,1,0.001)
TSS_lm_cv <- cv.glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                    lambda = lambdas,nfolds = length(dat_reg$intervals_300),alpha =0)

lambda_opt1 <- TSS_lm_cv$lambda.min
#refine
lambdas <- seq(9,10,0.01)
TSS_FTP_lm_cv <- cv.glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                       lambda = lambdas,nfolds = length(dat_reg$intervals_300),alpha =0)
lambda_opt2 <- TSS_FTP_lm_cv$lambda.min

TSS_FTP_lm <- glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                 lambda = lambda_opt2,alpha =0)

#refine
lambdas <- seq(9,10000,1)
TSS_FTP_lm_cv <- cv.glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                           lambda = lambdas,nfolds = length(dat_reg$intervals_300),alpha =0)
lambda_opt3 <- TSS_FTP_lm_cv$lambda.min

TSS_FTP_lm <- glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                     lambda = lambda_opt3,alpha =0)


tmp  <- sort(coef(TSS_FTP_lm),decreasing = TRUE)
ridge_coef_1 <- cbind(rownames(coef(TSS_FTP_lm))[match(tmp,as.vector(coef(TSS_FTP_lm)))],tmp)

ridge_coef_1_pos <- ridge_coef_1[ridge_coef_1[,2]>0,]
ridge_coef_1_pos <- ridge_coef_1_pos[-1,] #remove intercept
ridge_coef_1_neg <- ridge_coef_1[ridge_coef_1[,2]<0,]

#look at distribution of days that have negative or positive effects.
#positives
ridge_coef_1_pos_days <- sapply(ridge_coef_1_pos[,1],
                                function(x){substring(x,(nchar(x)-2),nchar(x))})
ridge_coef_1_pos_days <- sapply(ridge_coef_1_pos_days,
                                function(x){if(substring(x,1,1) == "_"){return(substring(x,2,3))}
                                  if(substring(x,2,2) == "_"){return(substring(x,3,3))}
                                  else{return(x)}})
names(ridge_coef_1_pos_days) <- NULL
ridge_coef_1_pos_days <- as.numeric(ridge_coef_1_pos_days)
summary(ridge_coef_1_pos_days)

#negatives
ridge_coef_1_neg_days <- sapply(ridge_coef_1_neg[,1],
                                function(x){substring(x,(nchar(x)-2),nchar(x))})
ridge_coef_1_neg_days <- sapply(ridge_coef_1_neg_days,
                                function(x){if(substring(x,1,1) == "_"){return(substring(x,2,3))}
                                  if(substring(x,2,2) == "_"){return(substring(x,3,3))}
                                  else{return(x)}})
names(ridge_coef_1_neg_days) <- NULL
ridge_coef_1_neg_days <- as.numeric(ridge_coef_1_neg_days)
summary(ridge_coef_1_neg_days)

####Exclude FTP lagged####
dat_reg <- data.frame(matrix(0,nrow = length(profile$date),ncol =0))
dat_reg$date <- as.numeric(profile$date)
dat_reg$FTP <- profile$FTP
t_0 <- dat_reg$date[1]
t_n <- dat_reg$date[length(dat_reg$date)]
dat_reg$intervals_300 <- profile$intervals_300
dat_reg <- dat_reg[(dat_reg$intervals_300 != 0),]

tmp <- function(x){
  return(paste("Day_TSS_",x,sep=""))
}
names <- lapply(seq(1,t_n - t_0),tmp)
names <- do.call(c,names)
rm(tmp)

for(i in 1:length(names)){
  dat_reg[[i+3]] <- 0
}

Past_TSS_lookup <- function(date,profile,t_0,t_n){
  profile$date <- as.numeric(profile$date)
  tmp <- rep(0,(date- t_0))
  for(i in seq(1,(date- t_0))){
    tmp[i] <- profile$Day_TSS[profile$date == (date - i)]
  }
  return(c(tmp,rep(0,(t_n - date))))
}

tmp <- lapply(dat_reg$date,Past_TSS_lookup, profile = profile,t_0=t_0,t_n=t_n)
tmp <- do.call(rbind,tmp)
dat_reg[,4:length(dat_reg[1,])] <- tmp
names(dat_reg)  <- c("date","intervals_300","FTP",names)
rm(tmp)

lambdas <- 10^seq(-4,-1,0.0001)
TSS_lm_cv <- cv.glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                       lambda = lambdas,nfolds = length(dat_reg$intervals_300),alpha =0)

lambda_opt1 <- TSS_lm_cv$lambda.min
# #refine
# lambdas <- seq(9,10000,1)
# TSS_lm_cv <- cv.glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
#                        lambda = lambdas,nfolds = length(dat_reg$intervals_300),alpha =0)
# lambda_opt2 <- TSS_lm_cv$lambda.min


TSS_lm <- glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                 lambda = lambda_opt1,alpha =0)

tmp  <- sort(coef(TSS_lm),decreasing = TRUE)
ridge_coef_2 <- cbind(rownames(coef(TSS_lm))[match(tmp,as.vector(coef(TSS_lm)))],tmp)

ridge_coef_2_pos <- ridge_coef_2[ridge_coef_2[,2]>0,]
ridge_coef_2_pos <- ridge_coef_2_pos[-1,] #remove intercept
ridge_coef_2_neg <- ridge_coef_2[ridge_coef_2[,2]<0,]

#look at distribution of days that have negative or positive effects.
#positives
ridge_coef_2_pos_days <- sapply(ridge_coef_2_pos[,1],
                                function(x){substring(x,regexpr("TSS_",x)[1]+4,nchar(x))})
names(ridge_coef_2_pos_days) <- NULL
ridge_coef_2_pos_days <- as.numeric(ridge_coef_2_pos_days)
summary(ridge_coef_2_pos_days)

#negatives
ridge_coef_2_neg_days <- sapply(ridge_coef_2_neg[,1],
                                function(x){substring(x,regexpr("TSS_",x)[1]+4,nchar(x))})
names(ridge_coef_2_neg_days) <- NULL
ridge_coef_2_neg_days <- as.numeric(ridge_coef_2_neg_days)
summary(ridge_coef_2_neg_days)

### larger dates do seem to have more negative coefficients - this is super super weak evidence though
#bascially no inference possible.
#-----------------------------------------------------

#-----------------------------------------------------
#Consider maximum lag of 42 days
dat_reg <- dat_reg[,1:45]

lambdas <- 10^seq(-3,0,0.001)
TSS_42_lm_cv <- cv.glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                       lambda = lambdas,nfolds = length(dat_reg$intervals_300),alpha =0)

lambda_opt1 <- TSS_lm_cv$lambda.min
#refine
lambdas <- 10^seq(-4,-1,0.0001)
TSS_42_lm_cv <- cv.glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                       lambda = lambdas,nfolds = length(dat_reg$intervals_300),alpha =0)

lambda_opt2 <- TSS_lm_cv$lambda.min

TSS_42_lm <- glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                 lambda = lambda_opt1,alpha =0)

tmp  <- sort(coef(TSS_42_lm),decreasing = TRUE)
ridge_coef_2.1 <- cbind(rownames(coef(TSS_42_lm))[match(tmp,as.vector(coef(TSS_42_lm)))],tmp)

ridge_coef_2.1_pos <- ridge_coef_2.1[ridge_coef_2.1[,2]>0,]
ridge_coef_2.1_pos <- ridge_coef_2.1_pos[-1,] #remove intercept
ridge_coef_2.1_neg <- ridge_coef_2.1[ridge_coef_2.1[,2]<0,]

#look at distribution of days that have negative or positive effects.
#positives
ridge_coef_2.1_pos_days <- sapply(ridge_coef_2.1_pos[,1],
                                function(x){substring(x,regexpr("TSS_",x)[1]+4,nchar(x))})
names(ridge_coef_2.1_pos_days) <- NULL
ridge_coef_2.1_pos_days <- as.numeric(ridge_coef_2.1_pos_days)
summary(ridge_coef_2.1_pos_days)

#negatives
ridge_coef_2.1_neg_days <- sapply(ridge_coef_2.1_neg[,1],
                                function(x){substring(x,regexpr("TSS_",x)[1]+4,nchar(x))})
names(ridge_coef_2.1_neg_days) <- NULL
ridge_coef_2.1_neg_days <- as.numeric(ridge_coef_2.1_neg_days)
summary(ridge_coef_2.1_neg_days)

#------------------------------------------------#
#3) Use session info for each data set to see if we can do any better with loads more variables

dat_reg <- data.frame(matrix(0,nrow = length(profile$date),ncol =0))
dat_reg$date <- as.numeric(profile$date)
t_0 <- dat_reg$date[1]
t_n <- dat_reg$date[length(dat_reg$date)]
dat_reg$intervals_300 <- profile$intervals_300
dat_reg <- dat_reg[(dat_reg$intervals_300 != 0),]

tmp <- function(names,x){
  return(lapply(names,function(y){paste(y,"_",x,sep="")}))
}

names <- lapply(seq(1,t_n - t_0),tmp,names= names(dat[[1]]$session))
names <- do.call(c,names)
rm(tmp)

for(i in 1:length(names)){
  dat_reg[[i+2]] <- 0
}

#add session data for the ride with the largest TSS
session_info_add <- function(date,data){
  data <- lapply(data,function(x)
                      {if(x$date == date){return(x)}
                        else{return(NULL)}})
  data <- data[which(as.logical(1-unlist(lapply(data,is.null))*1))]
  if(length(data)==0) return(rep(0,55))
  if(length(data) == 1){return(data[[1]]$session)}
  tmp <- max(unlist(lapply(data,function(x){x$session$training_stress_score})))
  tmp <- lapply(data,function(x){(x$session$training_stress_score == tmp)})
  tmp <- unlist(tmp)
  data <- data[tmp]
  return(data[[1]]$session)
}

tmp <- lapply(profile$date,session_info_add,data=dat)
tmp <- do.call(rbind,tmp)
profile_session <- cbind(profile$date,tmp)
names(profile_session) <- c("date",names_session)

Past_lookups <- function(date,profile,t_0,t_n,var_num){
  profile$date <- as.numeric(profile$date)
  tmp <- rep(0,(date- t_0)*var_num)
  for(i in seq(1,(date- t_0)*var_num,by = var_num)){
    for(j in 0:(var_num-1)){
    var <- profile[,(j+2)]
    tmp[i+j] <- var[profile$date == (date - (i-1)/var_num)]
    }
    }
  return(c(tmp,rep(0,(t_n - date)*var_num)))
}

tmp <- lapply(dat_reg$date,Past_lookups, profile = profile_session,t_0=t_0,t_n=t_n,var_num =55)
tmp <- do.call(rbind,tmp)
dat_reg[,3:length(dat_reg[1,])] <- tmp
names(dat_reg)  <- c("date","intervals_300",names)
rm(tmp)

#Do ridge regression with heavy penalties
lambdas <- seq(0,10000,100)
session_lm_cv <- cv.glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                       lambda = lambdas,nfolds = length(dat_reg$intervals_300),alpha =0)

lambda_opt1 <- session_lm_cv$lambda.min

session_lm <- glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                 lambda = lambda_opt1,alpha =0)

tmp <- sort(coef(session_lm),decreasing = TRUE)
ridge_coef_3 <- cbind(rownames(coef(session_lm))[match(tmp,as.vector(coef(session_lm)))],tmp)

ridge_coef_3_pos <- ridge_coef_3[ridge_coef_3[,2]>0,]
ridge_coef_3_pos <- ridge_coef_3_pos[-1,] #remove intercept
ridge_coef_3_neg <- ridge_coef_3[ridge_coef_3[,2]<0,]

#look at distribution of days that have negative or positive effects.
#positives
ridge_coef_3_pos_days <- sapply(ridge_coef_3_pos[,1],
                                function(x){substring(x,(nchar(x)-2),nchar(x))})
ridge_coef_3_pos_days <- sapply(ridge_coef_3_pos_days,
                                function(x){if(substring(x,1,1) == "_"){return(substring(x,2,3))}
                                            if(substring(x,2,2) == "_"){return(substring(x,3,3))}
                                            else{return(x)}})
names(ridge_coef_3_pos_days) <- NULL
ridge_coef_3_pos_days <- as.numeric(ridge_coef_3_pos_days)
summary(ridge_coef_3_pos_days)

#negatives
ridge_coef_3_neg_days <- sapply(ridge_coef_3_neg[,1],
                                function(x){substring(x,(nchar(x)-2),nchar(x))})
ridge_coef_3_neg_days <- sapply(ridge_coef_3_neg_days,
                                function(x){if(substring(x,1,1) == "_"){return(substring(x,2,3))}
                                  if(substring(x,2,2) == "_"){return(substring(x,3,3))}
                                  else{return(x)}})
names(ridge_coef_3_neg_days) <- NULL
ridge_coef_3_neg_days <- as.numeric(ridge_coef_3_neg_days)
summary(ridge_coef_3_neg_days)

#Look at distribution of different variable types:
variable_extraction <- function(x){
  x <- substring(x,1,(nchar(x) -1))
  for(i in 1:3)
    if(substring(x,nchar(x),nchar(x)) == "_"){return(substring(x,1,nchar(x)-1))}
    else{x <- substring(x,1,nchar(x)-1)}
}

ridge_coef_3 <- as.data.frame(ridge_coef_3,stringsAsFactors =  FALSE)
names(ridge_coef_3) <- c("var","coef")
ridge_coef_3$var_class <- c("intercept",unlist(sapply(ridge_coef_3$var,variable_extraction)))

position_summary <- function(var_name,data){
  return(summary(which(data$var_class == var_name)))
}

posit_sum <- lapply(names_session,position_summary,data = ridge_coef_3)
names(posit_sum) <- names_session
#average fractional cadence is by far the variable that ranks the highest - this gives us some
#confidence that the analysis is a load of rubbish, since this variable basically means nothing.

#------------------------------------------------#
#4) Use session info to see if we can do better with time in power zones:

#use same dat_reg as before but remove any variables that do not contain "time_in_power_zone"

names_power_zones <- names(dat_reg)
tmp <- grep("time_in_power_zone",names_power_zones)
names_power_zones <- names_power_zones[tmp]
dat_reg <- dat_reg[,c(1,2,tmp)]

#Do ridge regression with heavy penalties
lambdas <- seq(0,10000,100)
session_pz_lm_cv <- cv.glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                           lambda = lambdas,nfolds = length(dat_reg$intervals_300),alpha =0)

lambda_opt1 <- session_pz_lm_cv$lambda.min

session_pz_lm <- glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                     lambda = lambda_opt1,alpha =0)

tmp <- sort(coef(session_pz_lm),decreasing = TRUE)
ridge_coef_4 <- cbind(rownames(coef(session_pz_lm))[match(tmp,as.vector(coef(session_pz_lm)))],tmp)

ridge_coef_4_pos <- ridge_coef_4[ridge_coef_4[,2]>0,]
ridge_coef_4_pos <- ridge_coef_4_pos[-1,] #remove intercept
ridge_coef_4_neg <- ridge_coef_4[ridge_coef_4[,2]<0,]

#look at distribution of days that have negative or positive effects.
#positives
ridge_coef_4_pos_days <- sapply(ridge_coef_4_pos[,1],
                                function(x){substring(x,(nchar(x)-2),nchar(x))})
ridge_coef_4_pos_days <- sapply(ridge_coef_4_pos_days,
                                function(x){if(substring(x,1,1) == "_"){return(substring(x,2,3))}
                                  if(substring(x,2,2) == "_"){return(substring(x,3,3))}
                                  else{return(x)}})
names(ridge_coef_4_pos_days) <- NULL
ridge_coef_4_pos_days <- as.numeric(ridge_coef_4_pos_days)
summary(ridge_coef_4_pos_days)

#negatives
ridge_coef_4_neg_days <- sapply(ridge_coef_4_neg[,1],
                                function(x){substring(x,(nchar(x)-2),nchar(x))})
ridge_coef_4_neg_days <- sapply(ridge_coef_4_neg_days,
                                function(x){if(substring(x,1,1) == "_"){return(substring(x,2,3))}
                                  if(substring(x,2,2) == "_"){return(substring(x,3,3))}
                                  else{return(x)}})
names(ridge_coef_4_neg_days) <- NULL
ridge_coef_4_neg_days <- as.numeric(ridge_coef_4_neg_days)
summary(ridge_coef_4_neg_days)

#Look at distribution of different variable types:
variable_extraction <- function(x){
  x <- substring(x,1,(nchar(x) -1))
  for(i in 1:3)
    if(substring(x,nchar(x),nchar(x)) == "_"){return(substring(x,1,nchar(x)-1))}
  else{x <- substring(x,1,nchar(x)-1)}
}

ridge_coef_4 <- as.data.frame(ridge_coef_4,stringsAsFactors =  FALSE)
names(ridge_coef_4) <- c("var","coef")
ridge_coef_4$var_class <- c("intercept",unlist(sapply(ridge_coef_4$var,variable_extraction)))

names_power_zones <- unique(unlist(lapply(names_power_zones,variable_extraction)))


position_summary <- function(var_name,data){
  return(summary(which(data$var_class == var_name)))
}

posit_sum_pz <- lapply(names_power_zones[-1],position_summary,data = ridge_coef_4)
names(posit_sum_pz) <- names_power_zones[-1]
names(posit_sum_pz)[1] <- "time_in_power_zone_1"

#------------------------------------------------#
#5) Use session info to see if we can do better with time in power zones, but restrict to lags less than 42 days

#use same dat_reg as before but remove any variables that do not contain "time_in_power_zone"

dat_reg <- dat_reg[,1:(10*42+3)]

#Do ridge regression with heavy penalties
lambdas <- seq(0,10000,100)
session_pz42_lm_cv <- cv.glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                              lambda = lambdas,nfolds = length(dat_reg$intervals_300),alpha =0)

lambda_opt1 <- session_pz42_lm_cv$lambda.min

session_pz42_lm <- glmnet(x = as.matrix(dat_reg[,c(-1,-2)]), y = dat_reg$intervals_300,family = "gaussian",
                        lambda = lambda_opt1,alpha =0)

tmp <- sort(coef(session_pz42_lm),decreasing = TRUE)
ridge_coef_4.1 <- cbind(rownames(coef(session_pz42_lm))[match(tmp,as.vector(coef(session_pz42_lm)))],tmp)

ridge_coef_4.1_pos <- ridge_coef_4.1[ridge_coef_4.1[,2]>0,]
ridge_coef_4.1_pos <- ridge_coef_4.1_pos[-1,] #remove intercept
ridge_coef_4.1_neg <- ridge_coef_4.1[ridge_coef_4.1[,2]<0,]

#look at distribution of days that have negative or positive effects.
#positives
ridge_coef_4.1_pos_days <- sapply(ridge_coef_4.1_pos[,1],
                                function(x){substring(x,(nchar(x)-2),nchar(x))})
ridge_coef_4.1_pos_days <- sapply(ridge_coef_4.1_pos_days,
                                function(x){if(substring(x,1,1) == "_"){return(substring(x,2,3))}
                                  if(substring(x,2,2) == "_"){return(substring(x,3,3))}
                                  else{return(x)}})
names(ridge_coef_4.1_pos_days) <- NULL
ridge_coef_4.1_pos_days <- as.numeric(ridge_coef_4.1_pos_days)
summary(ridge_coef_4.1_pos_days)

#negatives
ridge_coef_4.1_neg_days <- sapply(ridge_coef_4.1_neg[,1],
                                function(x){substring(x,(nchar(x)-2),nchar(x))})
ridge_coef_4.1_neg_days <- sapply(ridge_coef_4.1_neg_days,
                                function(x){if(substring(x,1,1) == "_"){return(substring(x,2,3))}
                                  if(substring(x,2,2) == "_"){return(substring(x,3,3))}
                                  else{return(x)}})
names(ridge_coef_4.1_neg_days) <- NULL
ridge_coef_4.1_neg_days <- as.numeric(ridge_coef_4.1_neg_days)
summary(ridge_coef_4.1_neg_days)

#Look at distribution of different variable types:
variable_extraction <- function(x){
  x <- substring(x,1,(nchar(x) -1))
  for(i in 1:3)
    if(substring(x,nchar(x),nchar(x)) == "_"){return(substring(x,1,nchar(x)-1))}
  else{x <- substring(x,1,nchar(x)-1)}
}

ridge_coef_4.1 <- as.data.frame(ridge_coef_4.1,stringsAsFactors =  FALSE)
names(ridge_coef_4.1) <- c("var","coef")
ridge_coef_4.1$var_class <- c("intercept",unlist(sapply(ridge_coef_4.1$var,variable_extraction)))

position_summary <- function(var_name,data){
  return(summary(which(data$var_class == var_name)))
}

posit_sum_pz42 <- lapply(names_power_zones[-1],position_summary,data = ridge_coef_4.1)
names(posit_sum_pz42) <- names_power_zones[-1]
names(posit_sum_pz42)[1] <- "time_in_power_zone_1" 

cache(variable = "profile_session")
cache(variable = "ridge_coef_1")
cache(variable = "ridge_coef_2")
cache(variable = "ridge_coef_2.1")
cache(variable = "ridge_coef_3")
cache(variable = "ridge_coef_4")
cache(variable = "ridge_coef_4.1")
cache(variable = "ridge_coef_1_pos_days")
cache(variable = "ridge_coef_1_neg_days")
cache(variable = "ridge_coef_2_pos_days")
cache(variable = "ridge_coef_2_neg_days")
cache(variable = "ridge_coef_2.1_pos_days")
cache(variable = "ridge_coef_2.1_neg_days")
cache(variable = "ridge_coef_3_pos_days")
cache(variable = "ridge_coef_3_neg_days")
cache(variable = "ridge_coef_4_pos_days")
cache(variable = "ridge_coef_4_neg_days")
cache(variable = "ridge_coef_4.1_pos_days")
cache(variable = "ridge_coef_4.1_neg_days")
cache(variable = "posit_sum")
cache(variable = "posit_sum_pz")
cache(variable = "posit_sum_pz42")


