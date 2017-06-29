library("ggplot2")
library("lubridate")

# Import Data

USA_CO_Boulder = read.csv("/Users/b1017579/Documents1/PhD/Opower/Data/RESIDENTIAL_LOAD_DATA_E_PLUS_OUTPUT/HIGH/USA_CO_Boulder-Broomfield-Jefferson.County.AP.724699_TMY3_HIGH.csv")
head(USA_CO_Boulder[,1])

# Strip punctuation
rm_Punct = gsub("/",".",gsub(":",".",USA_CO_Boulder[,1]))

# Add year (2013 as placeholder)
lhs = substr(rm_Punct,2,6)
rhs = gsub('^(.{6}).','.2004 ',rm_Punct)
add_Year = paste0(lhs,rhs)

# Convert to POSIXct for time
USA_CO_Boulder[,1] = (as.POSIXct(strptime(add_Year, "%m.%d.%Y %H.%M.%S"), origin="1900-01-01"))
head(USA_CO_Boulder)

USA_CO_Boulder=transform(USA_CO_Boulder,day=weekdays(Date.Time))
USA_CO_Boulder=transform(USA_CO_Boulder,month=factor(months(Date.Time), levels=months(Date.Time)))
USA_CO_Boulder=transform(USA_CO_Boulder,hour=hour(Date.Time))


# Plot data

ggplot(data=subset(USA_CO_Boulder,!is.na(month)), aes(day,Electricity.Facility..kW..Hourly.,group=day)) + geom_boxplot()+facet_wrap(~month)

ggplot(data=subset(USA_CO_Boulder,!is.na(month)&month=="August"), aes(day,Electricity.Facility..kW..Hourly.,group=day)) + geom_boxplot()+facet_wrap(~month)

ggplot(data=subset(USA_CO_Boulder,!is.na(month)), aes(factor(hour),Electricity.Facility..kW..Hourly.)) + geom_boxplot() 

# 
ggplot(data=subset(USA_CO_Boulder,!is.na(month)), aes(factor(hour),Electricity.Facility..kW..Hourly.)) + geom_boxplot() + facet_wrap(~month)


