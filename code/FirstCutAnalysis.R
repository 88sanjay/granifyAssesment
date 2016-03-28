library(ggplot2)
library(plyr)
data <- read.delim2("~/Coursera/Assignments/Granify/DSTask.tsv")

#### EXPLORING THE DATA SET
# SCALING FEATURESET
mmnorm <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
scaled.features <- apply(data[,c(1,2,3,4,5)],2,mmnorm) # minmax scaling chosen as features do not follow standard distribution.
colnames(scaled.features)<-c("SF1","SF2","SF3","SF4","SF5")
## CORRELATION ANALYSIS
corr.features <- cor(scaled.features)
## PRINCIPAL COMPONENTS
p <- prcomp(scaled.features)
summary(p)
## SUMMARY
# ~ 85 % with just Features (5,2,1)
# 2,3,4 are correlated with each other,  1 and 5 are uncorrelated with any of the features, 
# 1 and 5 also seem independent with each of the other features.
# We next see if there are any clusters present amongst the correlated features, 
# This will give us a fundamental understanding of the feature distribution in the entire data set. 
cor.feature.list<- c(2,3,4)
## NOTE -TODO
# Hitograms for all distributions , and joint distribution of 1,5
# library(RColorBrewer)
# rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
# r <- rf(32)
# plt <- qplot(x=scaled.features[,1],y=scaled.features[,5], geom='bin2d')
# plt2 <- plt + stat_bin2d(bins=32) +scale_fill_gradientn(colours=rf(32))
## KMEANS
mydata <- scaled.features[,cor.feature.list]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
qplot(1:15, wss, xlab="Number of Clusters",
     ylab="Within groups sum of squares",geom="smooth")
# Optimal Number of clusters ~ 4
k<-kmeans(mydata,centers=4)
# Summay of clusters %k$centers[order(k$centers[,1]),]
# C1 Low_F2+MID_F3+LOW_F4
# C2 HIGH_F2+MID_F3+MID_F4
# C3 HIGH_F2+HIGH_F3+LOW_F4
# C4 Low_F2+HIGH_F3+HIGH_F4
cluster.ids<-k$cluster
final.data <- cbind(data,scaled.features,cluster.ids)

#### AD EFFICACY
# CAMPAIGN CONVERSION (TG -CG)/CG OVERALL
conversion.CG <- sum(final.data$CONTROL==1 & final.data$RESPONSE==1)/sum(final.data$CONTROL==1)
print(paste("Control group conversion is ",conversion.CG*100,"percent"))
conversion.TG <- sum(final.data$CONTROL==0 & final.data$RESPONSE==1)/sum(final.data$CONTROL==0)
print(paste("Target group conversion is ",conversion.TG*100,"percent"))
delta.conversion <- (conversion.TG - conversion.CG)/ conversion.CG
print(paste("Delta Conversion is ",delta.conversion*100,"percent more than CG"))
# Fishers  test to prove there was impact created
t <- table(final.data$CONTROL,final.data$RESPONSE)
fisher.test(t, alternative = "greater")
# Fisher's Exact Test for Count Data
# data:  t
# p-value = 0.7531
# alternative hypothesis: true odds ratio is greater than 1
# 95 percent confidence interval:
# 0.765397      Inf
# sample estimates:
# odds ratio 
# 0.9306083 
## SUMMARY 
# We observer about 7 % more impact in TG over the CG with current marketing strategies.

## IN DEPTH ANALYSIS 
# AD 1 Health - conversion of AD 1 , for various values of F1,F5 and clusters
# simulation has been run by changing ad.id and feature to group over

ad.id <- 3
feature.to.group.over <- "FEATURE_5"
subset.data <- final.data[final.data$AD_ID==ad.id,]
feature_summary <- ddply(subset.data,c(feature.to.group.over, "CONTROL"),
                         summarize,convert=sum(RESPONSE)/(length(RESPONSE)))
ggplot(data=feature_summary, aes(x=FEATURE_5, y=convert, fill=as.factor(CONTROL))) +
  geom_bar(stat="identity", position=position_dodge()) + guides(fill=FALSE)

## SUMMARY
# AD_1 performs poorly for higher values of feature 1 ( >= 3)
# AD_1 performs poorly for higher values of feature 5 ( >= 4)
# AD_1 Performs poorly for clusters 3 and cluster 4 where FEATURE 3 seems HIGH
# AD_2 Performs poorly for High Feature 1 ( >=3)
# AD_2 Performs poorly for Extreme Feature 5 ( <= 1  & >=4) 
# AD_2 Performs very poorly for clusters 3 and cluster 4 where FEATURE 3 seems HIGH
# AD_3 Performs very poorly for clusters 1 and cluster 3 where FEATURE 4 seems LOW
# AD_3 Performs well only for Low Feature 1 ( ==1)
# AD_3 performs poorly for higher values of feature 5 ( >= 3)

## ALL ADS PERFORM POORLY FOR FEATURE_1 >=3 , AD_2 WORKS_WELL ONLY FOR FEATURE_1==1 
## ALL ADS PERFORM POORLY FOR FEATURE_5 >=4 , AD_2 WORKS_WELL ONLY FOR FEATURE_5==2,3 
## ALL ADS PERFORM POORLY IN CLUSTER 3
## AD_1,AD_2 Performs poorly WHERE FEATURE 3 seems HIGH
## AD_3 Performs very poorly WHERE FEATURE 4 seems LOW
## Further we could look at the decision rules from a decision tree of the features
## to see when should we recommend AD_1 vs AD_2 vs AD_3

#### IDENTIFYING PATTERNS BETWEEN DAY / TIME AND CONVERSION

splitr <- function(x) {
  a <- (strsplit(x=x,split=", "))
  b <- strsplit(x=a[[1]][2],split=":")
  c <- strsplit(x=a[[1]][1],split=" ")
  a[[1]][1]<- c[[1]][2]
  a[[1]][2]<- b[[1]][1]
  return(a)
}

# Spliting Time / Hour info - Which days are more popular, Which days are least ,
# What times typical popular, abberations in behavior
time.split<-as.character(data[,"TIME"])
dim(time.split)<-c(length(time.split),1)
time.split<-apply(time.split,1,splitr)
time.split<-unlist(time.split)
dim(time.split)<-c(2,nrow(data))
time.split<-as.data.frame(t(time.split),stringsAsFactors=FALSE)
colnames(time.split) <- c("DAY","HOUR")
final.data <- cbind(data,time.split)

ad.id <-1
feature.to.group.over <- "HOUR"
subset.data <- final.data[final.data$AD_ID==ad.id,]
feature_summary <- ddply(subset.data,c(feature.to.group.over),
                         summarize,total_clicks=sum(RESPONSE)/length(RESPONSE))
ggplot(data=feature_summary, aes(x=as.integer(HOUR), y=total_clicks)) +
  geom_bar(stat="identity", position=position_dodge()) + guides(fill=FALSE)

# ANALYZE DAY 6 and DAY 13, DAY 8 for AD1 
# ANALYZE DAY 1 and DAY 5 for AD2
# ANALYZE DAY 10,7,13 and  for AD3
# ANALYZE 4 am FOR AD1
# ANALYZE 7,8,9 am FOR AD2
# ANALYZE 7,8,9 am FOR AD3

