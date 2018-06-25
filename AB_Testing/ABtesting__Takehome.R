#Did the test version help increase CTR and total revenue?
library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)


setwd("/Users/exue001c/evagit/Bittiger/ABTesting")
##loading data
data = read.csv("abtest_example_ctr.csv")

##summary data
summary(data) #NA's in userid 275  
str(data) #22960 obs. of  11 variables

##checking missing values for userid
sum(is.na(data$userid))/nrow(data) #~2% missing 

##check distinct userid per group
sqldf("select groups, count(distinct(userid)) from data group by groups")
#     groups count(distinct(userid))
#1   control                    3784
#2 treatment                    3837


##checking for mixed assignment
##same userid is assigned to 2 groups
sqldf("select count(1)
      from(
      select userid, count(*)
      from(
      select userid,groups,count(*)
      from data
      group by userid, groups)
      group by userid
      having count(*)>1)
      ")  #~44

##method2
sqldf("select count(1) from (select userid, count(distinct(groups)) from data group by userid having count(distinct(groups))>1) as a ")

##checking same userid have multiple devices
sqldf("select count(1) from (select userid, count(distinct(deviceid)) from data group by userid having count(distinct(deviceid))>1) as a") #~176

##check if the NA/Mixed/Multiple device is random
##create dummy, if any problems 1, else 0
pb_miss = 1*is.na(data$userid)

userid_mix = as.numeric(sqldf("select userid, count(distinct(groups)) from data group by userid having count(distinct(groups)) >1")[[1]])
fun_pb_mix = function(x){x %in% userid_mix}
pb_mix = 1*sapply(data$userid, FUN = fun_pb_mix)

userid_mulD =sqldf("select userid, count(distinct(deviceid)) from data group by userid having count(distinct(deviceid))>1")[[1]]
fun_pb_mulD = function(x){x %in% userid_mulD}
pb_mulD = 1*sapply(data$userid, FUN = fun_pb_mulD)

##1 device has multiple users
deviceid_mulU=sqldf("select deviceid, count(distinct(userid)) from data group by deviceid having count(distinct(userid))>1")[[1]]
fun_pb_mulD = function(x){x %in% deviceid_mulU}
pb_mulU = 1*sapply(data$deviceid, FUN = fun_pb_mulD)


##for smiplicity, here I created combined column for any problem. in real project, need to seperate each problem cuz they might be caused by differen causes
## 1 => apply to column
data$pb_all = dd=apply(matrix(cbind(pb_miss, pb_mix, pb_mulD), nrow=nrow(data)), 1, max)

##run a simple model to check the covariates
pb_mod = glm(pb_all ~ country+groups + device+date+views+clicks+revenue, data, family = 'binomial')
summary(pb_mod)
## some countries/devices have significant result, need to deep dive. If any bug exists
aggregate(pb_miss, by= list(data$device), FUN=mean)
aggregate(pb_mix, by= list(data$device), FUN=mean)
aggregate(pb_mulD, by= list(data$device), FUN=mean)
aggregate(pb_mulU, by= list(data$device), FUN=mean)

## plot to see if there is correlation between key metrics and problem 
## check if the active users are more likely to have mix assignment.
boxplot(data$views~pb_miss)
boxplot(data$clicks~pb_miss)

boxplot(data$views~pb_mix)
boxplot(data$clicks~pb_mix)

boxplot(data$views~pb_miss)
boxplot(data$clicks~pb_miss)

boxplot(data$views~pb_mulD)
boxplot(data$clicks~pb_mulD)

##no big difference 

##for simplicity, throw away the problematic assignemnts. 
data=data[data$pb_all == 0,]

##sanity check => A/A test
##day1-day3 data was before experiment start
##split the data
data$date=as.Date(data$date)
data_before = data[data$date<(min(data$date)+3),]
data_start = data[data$date>=(min(data$date)+3),]

##compare aggregated CTR between test/control before experiment start
x1=sum(data_before$clicks[data_before$groups=="treatment"])
x2=sum(data_before$clicks[data_before$groups=="control"])
n1=sum(data_before$views[data_before$groups=="treatment"])
n2=sum(data_before$views[data_before$groups=="control"])
##2-sample test for equality of proportions with continuity correction
##x => a vector of counts of successes
##n => a vector of counts of trials; ignored if x is a matrix or a table.
##prop.test => z test
prop.test(x=c(x1,x2), n=c(n1,n2), alternative = "two.sided")
#p-value =0.9581 => pass A/A test

#example:
## Data from Fleiss (1981), p. 139.
## H0: The null hypothesis is that the four populations from which
##     the patients were drawn have the same true proportion of smokers.
## A:  The alternative is that this proportion is different in at
##     least one of the populations.

# smokers  <- c( 83, 90, 129, 70 )
# patients <- c( 86, 93, 136, 82 )
# prop.test(smokers, patients)

##compare other convariates comparable. 
##example device type proportion in 2 groups
par(mfrow=c(2,1))
device_t = table(data_before[data_before$groups=="treatment", "device"])
device_c = table(data_before[data_before$groups=="control", "device"])
barplot(prop.table(device_t), main = "Treatment Group")
barplot(prop.table(device_c), main = "Control Group")

##Hypothesis Testing
x1=sum(data_start$clicks[data_start$groups=="treatment"])
x2=sum(data_start$clicks[data_start$groups=="control"])
n1=sum(data_start$views[data_start$groups=="treatment"])
n2=sum(data_start$views[data_start$groups=="control"])
prop.test(x=c(x1,x2), n=c(n1,n2), alternative = "two.sided")
#p-value = 0.06243 (not significant)

#A/B testing within subgroup(device)
#write an A/B testing function
ztest_by_subgroup = function(data_start, bycol, val){
  data_use=data_start[data_start[bycol]==val,]
  x1=sum(data_use$clicks[data_use$groups=="treatment"])
  x2=sum(data_use$clicks[data_use$groups=="control"])
  n1=sum(data_use$views[data_use$groups=="treatment"])
  n2=sum(data_use$views[data_use$groups=="control"])
  prop.test(x=c(x1,x2), n=c(n1,n2), alternative = "two.sided")
}

#2:23:29







