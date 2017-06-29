library("plyr")
library("ggplot2")
library("lubridate")

# Set working directory and gather file names
setwd("~/Documents1/PhD/Opower/Data/RESIDENTIAL_LOAD_DATA_E_PLUS_OUTPUT/HIGH")
filenames = dir()
# Take a sample of csv filenames for decrease in computation time
filenames10 = sample(filenames,20,replace=FALSE)

# Import files and append name of file
base_data = do.call(rbind, 
                    lapply(filenames10, function(x)
                      cbind(file=x,read.csv(x)[,c("Date.Time","Electricity.Facility..kW..Hourly.")]
                    )))

# Convert date in dataframe to POSIXct object and add year
base_data$Date.Time = gsub("/",".",gsub(":",".",base_data$Date.Time))
lhs = substr(base_data$Date.Time,2,6)
rhs = gsub('^(.{6}).','.2004 ',base_data$Date.Time)
base_data$Date.Time = paste0(lhs,rhs)

# Convert to POSIXct for time
base_data$Date.Time = as.POSIXct(strptime(base_data$Date.Time, "%m.%d.%Y %H.%M.%S"), origin="1900-01-01")


base_data=transform(base_data,day=weekdays(Date.Time))
base_data=transform(base_data,month=factor(months(Date.Time), levels=months(Date.Time)))
base_data=transform(base_data,hour=hour(Date.Time))






# Max value
max_value = sapply(filenames10, function(x)
              cbind(file=x,max(base_data[base_data$file==x,'Electricity.Facility..kW..Hourly.']))
  )

# Relative Electricity Usage Calculation

base_data$relative_elec = lapply(base_data, function(x)
              (x$Electricity.Facility..kW..Hourly.)/max_value[x$file=dimnames(max_value)]
  )


myresult = ddply(base_data, .(Electricity.Facility..kW..Hourly.),
                 highest_elec = max(Electricity.Facility..kW..Hourly.))


base_data=transform(base_data,relative_elec=Electricity.Facility..kW..Hourly./max(base_data[base_data$file==base_data$file,'Electricity.Facility..kW..Hourly.']))

max(base_data[base_data$file=='USA_NJ_Newark.Intl.AP.725020_TMY3_HIGH.csv','Electricity.Facility..kW..Hourly.'])

ggplot(data=subset(base_data,!is.na(month)&file=="USA_NJ_Newark.Intl.AP.725020_TMY3_HIGH.csv"), aes(factor(hour),Electricity.Facility..kW..Hourly.)) + geom_boxplot() + facet_wrap(~month)





