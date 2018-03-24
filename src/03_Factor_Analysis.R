library('ProjectTemplate')
load.project()

###Factor Analysis###

#This script carries out factor analysis on a ride with the hope of identifiying key components of rides

data <- dat[[1]]$record

factanal(x=data,factors=4)

data_standard <- apply(data,MARGIN=2,function(x){(x-mean(x))/sd(x)})
data_standard_var <- (data_standard)%*%t(data_standard)

fa(r= data_standard_var,nfactors = 3)
pvals <- sapply(seq(1,14),function(f){factanal(x=data,factors=f)$PVAL})

factanal(x=data, factors = 2)

#None of this works :(
#Suspect matrix is singualr due to serial correlation???
# This method doesn't really make sense since or observations are not independent.





