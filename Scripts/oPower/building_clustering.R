library(tibble)
library("plyr")
library(dplyr)
library("ggplot2")
library("lubridate")
library(pracma)
library("reshape")
library("reshape2")

# Set working directory and gather file names
# setwd("~/Documents1/PhD/Opower/Data/RESIDENTIAL_LOAD_DATA_E_PLUS_OUTPUT/HIGH")
setwd("~/Documents1/PhD/Opower/Data/COMMERCIAL_LOAD_DATA_E_PLUS_OUTPUT.part1/USA_AK_Anchorage.Intl.AP.702730_TMY3")
filenames = dir()

# Take a sample of csv filenames for decrease in computation time
filenames10 = sample(filenames,12,replace=FALSE)

data1=read.csv(filenames10[1])
data2=read.csv(filenames10[2])
data3=read.csv(filenames10[3])
data4=read.csv(filenames10[4])
data5=read.csv(filenames10[5])
data6=read.csv(filenames10[6])
data7=read.csv(filenames10[7])

# Import files and append name of file
base_data = do.call(rbind, 
                    lapply(filenames10, function(x)
                      cbind(file=x,read.csv(x)[,c("Date.Time","Electricity.Facility..kW..Hourly.","Gas.Facility..kW..Hourly.")]
                      )))
base_data = tbl_df(base_data)

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



# Calculate relative electricity consumption
base_data = ddply(base_data, "file", transform, highest_elec=max(Electricity.Facility..kW..Hourly.)) %>% ddply("file", transform, highest_gas=max(Gas.Facility..kW..Hourly.)) %>% 
 


# Calculate relative electricty + gas consumption
# base_data = ddply(base_data, "file", transform, highest gas=max('Gas.Facility..kW..Hourly.'))


# Electricity Plot
ggplot(data=filter(base_data,file=='RefBldgFullServiceRestaurantNew2004_v1.3_7.1_8A_USA_AK_FAIRBANKS.csv'), aes(factor(hour),Electricity.Facility..kW..Hourly.)) + geom_boxplot() + facet_wrap(~month)
# Gas Plot
ggplot(data=filter(base_data,file=='USA_PA_State.College-Penn.State.University.725128_TMY3_HIGH.csv'), aes(factor(hour),Gas.Facility..kW..Hourly.)) + geom_boxplot() + facet_wrap(~month)
# Electricity and Gas Plot Overlay
ggplot(data=subset(base_data,!is.na(month)&file=='USA_WA_Whidbey.Island.NAS.690230_TMY3_HIGH.csv')) + 
      geom_boxplot(aes(factor(hour),Electricity.Facility..kW..Hourly.)) + 
      geom_boxplot(aes(factor(hour),Gas.Facility..kW..Hourly.), color='red') + 
      facet_wrap(~month)



# Features
feature = ddply(base_data, "file", summarise, 
                max_elec=max(Electricity.Facility..kW..Hourly.),
                max_gas=max(Gas.Facility..kW..Hourly.),
                min_elec=min(Electricity.Facility..kW..Hourly.), 
                min_gas=min(Gas.Facility..kW..Hourly.),
                avg_elec_consump=mean(Electricity.Facility..kW..Hourly.),
                avg_gas_consump=mean(Gas.Facility..kW..Hourly.),
                n_peaks_elec_total=dim(findpeaks(Electricity.Facility..kW..Hourly.))[1]
                )

avg_elec_per_hour_long = ddply(base_data,c("file","hour"),summarise,avg_elec_p_hour = mean(Electricity.Facility..kW..Hourly.))
avg_elec_per_hour = dcast(avg_elec_per_hour_long,file~hour)
colnames(avg_elec_per_hour) = paste(colnames(avg_elec_per_hour),"h elec")

avg_gas_per_hour_long = ddply(base_data,c("file","hour"),summarise,avg_gas_p_hour=mean(Gas.Facility..kW..Hourly.)) 
avg_gas_per_hour = dcast(avg_gas_per_hour_long, file~hour) 
colnames(avg_gas_per_hour) = paste(colnames(avg_gas_per_hour),"h gas")

avg_energy_per_hour = cbind(avg_gas_per_hour_long,avg_elec_per_hour_long[-c(1,2)])

peak_dat = ddply(avg_energy_per_hour,"file",summarise,
                  n_peaks_elec=dim(findpeaks(avg_elec_p_hour))[1],
                  highest_pk_time_elec=findpeaks(avg_elec_p_hour)[which.max(findpeaks(avg_elec_p_hour)[,1]),2]-1,
                  n_peaks_gas=ifelse(is.null(dim(findpeaks(avg_gas_p_hour)))[1],0,dim(findpeaks(avg_gas_p_hour))),
                  highest_pk_time_gas= ifelse(is.null(findpeaks(avg_gas_p_hour)[which.max(findpeaks(avg_gas_p_hour)[,1]),2]),
                                              0,
                                              findpeaks(avg_gas_p_hour)[which.max(findpeaks(avg_gas_p_hour)[,1]),2]-1),
                  Second_peak_location_elec = ifelse(is.na(findpeaks(avg_elec_p_hour)[sort(findpeaks(avg_elec_p_hour)[,1], index.return=TRUE, decreasing = TRUE)$ix[2],2]),0,findpeaks(avg_elec_p_hour)[sort(findpeaks(avg_elec_p_hour)[,1], index.return=TRUE, decreasing = TRUE)$ix[2],2]-1),
                  Second_peak_height_elec = ifelse(is.na(findpeaks(avg_elec_p_hour)[sort(findpeaks(avg_elec_p_hour)[,1], index.return=TRUE, decreasing = TRUE)$ix[2],1]),0,findpeaks(avg_elec_p_hour)[sort(findpeaks(avg_elec_p_hour)[,1], index.return=TRUE, decreasing = TRUE)$ix[2],1])
                 )


ggplot(data=filter(avg_elec_per_hour_long,file=='USA_PA_State.College-Penn.State.University.725128_TMY3_HIGH.csv')) + geom_line(aes(hour,avg_elec_p_hour))
ggplot(data=filter(avg_gas_per_hour_long,file=='USA_PA_State.College-Penn.State.University.725128_TMY3_HIGH.csv')) + geom_line(aes(hour,avg_gas_p_hour))
 
ggplot(data=avg_gas_per_hour_long)+geom_line(aes(hour,avg_gas_p_hour))+facet_wrap(~file, scales="free")
ggplot(data=avg_elec_per_hour_long)+geom_line(aes(hour,avg_elec_p_hour))+facet_wrap(~file, scales="free_y")

features_total = cbind(feature,n_peaks_elec = peak_dat$n_peaks_elec,highest_pk_time_elec = peak_dat$highest_pk_time_elec, second_peak_time=peak_dat$Second_peak_location_elec,second_peak_height=peak_dat$Second_peak_height_elec,n_peaks_gas = peak_dat$n_peaks_gas, highest_pk_time_gas = peak_dat$highest_pk_time_gas,avg_elec_per_hour[-1],avg_gas_per_hour[-1]) 







# K Means Clustering of Feature Data



# kMeans = kmeans(base_data[c('timestamp','Gas.Facility..kW..Hourly.','Electricity.Facility..kW..Hourly.')], 6, nstart=20)
# base_data$cluster = kMeans$cluster

# Group each property by maximum count of each cluster number
# cluster_max_count = ddply(base_data,c('file','cluster'),count,'cluster')

