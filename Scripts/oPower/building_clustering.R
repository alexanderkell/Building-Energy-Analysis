library(tibble)
library("plyr")
library(dplyr)
library("ggplot2")
library("lubridate")
library(pracma)
library("reshape")
library("reshape2")
library(devtools)
library(ggbiplot)
library(cluster)
library(caret)
library(wavelets)
library(TSclust)

# install.packages("dtwclust")
# install.packages("dtw")

# Set working directory and gather file names
setwd("~/Documents1/PhD/Opower/Data/Data/Run")
filenames = list.files(recursive = T)
# Take a sample of csv filenames for decrease in computation time
filenames10 = sample(filenames,20,replace=FALSE)


# Import files and append name of file
base_data = do.call(rbind,
                    lapply(filenames10, function(x)
                      cbind(file=x,read.csv(x)[,c("Date.Time","Electricity.Facility..kW..Hourly.","Gas.Facility..kW..Hourly.")]
                      )))


# Convert date in dataframe to POSIXct object and add year
base_data$Date.Time = gsub("/",".",gsub(":",".",base_data$Date.Time))
lhs = substr(base_data$Date.Time,2,6)
rhs = gsub('^(.{6}).','.2004 ',base_data$Date.Time)
base_data$Date.Time = paste0(lhs,rhs)

base_data$Date.Time = as.POSIXct(strptime(base_data$Date.Time, "%m.%d.%Y %H.%M.%S"), origin="1900-01-01")

# Add Day, Month, Hour, and Timestamp Data

base_data=transform(base_data,day=weekdays(Date.Time))
base_data=transform(base_data,month=factor(months(Date.Time), levels=months(Date.Time)))
base_data=transform(base_data,hour=hour(Date.Time))
base_data=transform(base_data,timestamp=as.numeric(Date.Time))
base_data=na.omit(base_data)


# Electricity Plot
# ggplot(data=filter(base_data,file=='RESIDENTIAL_LOAD_DATA_E_PLUS_OUTPUT/BASE/USA_MI_Escanaba.AWOS.726480_TMY3_BASE.csv'), aes(factor(hour),Electricity.Facility..kW..Hourly.)) + geom_boxplot() + facet_wrap(~month)
# Gas Plot
# ggplot(data=filter(base_data,file=='USA_PA_State.College-Penn.State.University.725128_TMY3_HIGH.csv'), aes(factor(hour),Gas.Facility..kW..Hourly.)) + geom_boxplot() + facet_wrap(~month)
# Electricity and Gas Plot Overlay
# ggplot(data=subset(base_data,!is.na(month)&file=='USA_WA_Whidbey.Island.NAS.690230_TMY3_HIGH.csv')) +
        # geom_boxplot(aes(factor(hour),Electricity.Facility..kW..Hourly.)) +
        # geom_boxplot(aes(factor(hour),Gas.Facility..kW..Hourly.), color='red') +
        # facet_wrap(~month)

# Electricity Plot
ggplot(data=filter(base_data,file==filenames10), aes(factor(hour),Electricity.Facility..kW..Hourly.)) + geom_boxplot() + facet_wrap(~file, scale="free")



# Features
feature = ddply(base_data, "file", summarise,
                Building_Type = ifelse(is.na(strsplit(strsplit(strsplit(strsplit(as.character(file),"/")[[1]][3],"2004")[[1]][1],"RefBldg")[[1]][2],"New")[[1]][1]),"Residential",strsplit(strsplit(strsplit(strsplit(as.character(file),"/")[[1]][3],"2004")[[1]][1],"RefBldg")[[1]][2],"New")[[1]][1]),
                max_elec=max(Electricity.Facility..kW..Hourly.),
                min_elec=min(Electricity.Facility..kW..Hourly.), 
                avg_elec_consump=mean(Electricity.Facility..kW..Hourly.),
                n_peaks_elec_total=dim(findpeaks(Electricity.Facility..kW..Hourly.))[1],
                )

avg_elec_per_hour_long = ddply(base_data,c("file","hour"),summarise,avg_elec_p_hour = mean(Electricity.Facility..kW..Hourly.))
avg_elec_per_hour = dcast(avg_elec_per_hour_long,file~hour)
avg_elec_per_hour_long = ddply(avg_elec_per_hour_long,"file",mutate, Building_Type = ifelse(is.na(strsplit(strsplit(strsplit(strsplit(as.character(file),"/")[[1]][3],"2004")[[1]][1],"RefBldg")[[1]][2],"New")[[1]][1]),"Residential",strsplit(strsplit(strsplit(strsplit(as.character(file),"/")[[1]][3],"2004")[[1]][1],"RefBldg")[[1]][2],"New")[[1]][1]),scaled_dat=avg_elec_p_hour/mean(avg_elec_p_hour))


ggplot(avg_elec_per_hour_long) + geom_line(aes(hour, avg_elec_p_hour)) + facet_wrap(~file + Building_Type, scale="free") +
  labs(title="Boxplot of Hourly Gas Used over a Year for a Residential Building")+xlab("Hours of each day") + ylab("Gas used hourly (kW)") 


peak_dat = ddply(avg_elec_per_hour_long,"file",summarise,
                 n_peaks_elec=log(dim(findpeaks(avg_elec_p_hour))[1]),
                 highest_pk_time_elec=findpeaks(avg_elec_p_hour)[which.max(findpeaks(avg_elec_p_hour)[,1]),2]-1,
                 highest_pk_height_elec =(findpeaks(scaled_dat)[which.max(findpeaks(scaled_dat)[,1]),1])^2,
                 Second_peak_location_elec = ifelse(is.na(findpeaks(avg_elec_p_hour)[sort(findpeaks(avg_elec_p_hour)[,1], index.return=TRUE, decreasing = TRUE)$ix[2],2]),0,findpeaks(avg_elec_p_hour)[sort(findpeaks(avg_elec_p_hour)[,1], index.return=TRUE, decreasing = TRUE)$ix[2],2]-1),
                 Second_peak_height_elec = ifelse(is.na(findpeaks(scaled_dat)[sort(findpeaks(scaled_dat)[,1], index.return=TRUE, decreasing = TRUE)$ix[2],1]),0,findpeaks(scaled_dat)[sort(findpeaks(scaled_dat)[,1], index.return=TRUE, decreasing = TRUE)$ix[2],1]),
                 std_dev_elec = sd(scaled_dat),
                 sum_of_elec = log(sum(avg_elec_p_hour))
)




features_total = cbind(feature,n_peaks_elec = peak_dat$n_peaks_elec,highest_pk_time_elec = peak_dat$highest_pk_time_elec,highest_pk_height_elec=peak_dat$highest_pk_height_elec, second_peak_time=peak_dat$Second_peak_location_elec,second_peak_height=peak_dat$Second_peak_height_elec,std_dev=peak_dat$std_dev_elec, sum_of_elec=peak_dat$sum_of_elec)


# K Means Clustering of Feature Data

kMeans = kmeans(features_total[-c(1,2)],12,nstart=20)
head(filter(features_total, Building_Type=="Hospital"))

# Silhouette Plot
dissE = daisy(features_total[-c(1,2)])
dE2 = dissE^2
sk2 = silhouette(kMeans$cluster, dE2)
plot(sk2)

par(mfrow=c(3,3))
# Silhouette Plot k = 6
kMeans6 = kmeans(features_total[-c(1,2)],6,nstart=20)
sk2 = silhouette(kMeans6$cluster, dE2)
plot(sk2)

# Silhouette Plot k = 7
kMeans7 = kmeans(features_total[-c(1,2)],7,nstart=20)
sk2 = silhouette(kMeans7$cluster, dE2)
plot(sk2)

# Silhouette Plot k = 8
kMeans8 = kmeans(features_total[-c(1,2)],8,nstart=20)
sk2 = silhouette(kMeans8$cluster, dE2)
plot(sk2)

# Silhouette Plot k =9
kMeans9 = kmeans(features_total[-c(1,2)],9,nstart=20)
sk2 = silhouette(kMeans9$cluster, dE2)
plot(sk2)

# Silhouette Plot k =10
kMeans10 = kmeans(features_total[-c(1,2)],10,nstart=20)
sk2 = silhouette(kMeans10$cluster, dE2)
plot(sk2)

# Silhouette Plot k =11
kMeans11 = kmeans(features_total[-c(1,2)],11,nstart=20)
sk2 = silhouette(kMeans11$cluster, dE2)
plot(sk2)

# Silhouette Plot k =12
kMeans12 = kmeans(features_total[-c(1,2)],12,nstart=20)
sk2 = silhouette(kMeans12$cluster, dE2)
plot(sk2)

# Silhouette Plot k =13
kMeans13 = kmeans(features_total[-c(1,2)],13,nstart=20)
sk2 = silhouette(kMeans13$cluster, dE2)
plot(sk2)

# Silhouette Plot k =14
kMeans14 = kmeans(features_total[-c(1,2)],14,nstart=20)
sk2 = silhouette(kMeans14$cluster, dE2)
plot(sk2)

var(kMeans8$withinss)


#PCA Plot
pca = prcomp(features_total[-c(1,2)])
ggbiplot(pca, groups = factor(kMeans6$cluster), ellipse = TRUE) + geom_text(aes(label=features_total$Building_Type, alpha=0.1), size=2, nudge_x=0.15)

features_total$Cluster=kMeans$cluster

join = merge(avg_elec_per_hour_long,features_total, by='file')
join = join[c(1,2,3,length(join))]


# ggplot(join) + geom_boxplot(aes(factor(hour),avg_elec_p_hour))+facet_wrap(~Cluster, scale="free")
# ggplot(join) + geom_line(aes(hour,avg_elec_p_hour))+facet_wrap(~Cluster, scale="free")

View(features_total[c(2,length(features_total))])





# Using Euclidean Difference Matrix with a Hierarchical Clustering algorithm

elec_per_hour_matrix = data.matrix(avg_elec_per_hour)
distMatrix = dist(elec_per_hour_matrix, method="euclidean")

hc = hclust(distMatrix, method="complete")
plot(hc, labels=features_total$Building_Type)
rect.hclust(hc, h=100, border="blue")
clusterCut = cutree(hc, h=100)
table(clusterCut, features_total$Building_Type)


# DWT


wavelet_feature = diss.DWT(elec_per_hour_matrix)
hc_wavelet = hclust(wavelet_feature, method = "complete")
plot(hc_wavelet, labels=features_total$Building_Type)


# DTW

distMatrix_DTW <- dist(elec_per_hour_matrix, method="DTW")
hc_DTW = hclust(distMatrix_DTW, method="complete")
plot(hc_DTW, labels=features_total$Building_Type)






