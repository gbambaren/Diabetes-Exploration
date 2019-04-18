
library(xlsx)
library(ggplot2)
library(scales)
library(reshape)
library(forecast)
#library(zoo)
# setwd('C:\\Users\\A Sua\\Documents\\FIU\\Spring2019\\DA2\\Project')
# list.files()[2]
# list.files()[3]
# list.files()[4]
list.files()[5]

#diabetes_num_94_14 = read.xlsx(file = list.files()[5], sheetIndex = 1)
diabetes_per_94_16 = read.xlsx(file = list.files()[5], sheetIndex = 3)
#obesity_per_94_16 = read.xlsx(file = list.files()[5], sheetIndex = 3)
#diabetes_cat_11_16 = read.xlsx(file = list.files()[2], sheetIndex = 2)
#### take out state code
diabetes_per_94_16 = diabetes_per_94_16[, -2]
diabetes_per_94_16 = diabetes_per_94_16[-53, ]
for(i in seq(23)){
  a = colnames(diabetes_per_94_16)[i+2]
  colnames(diabetes_per_94_16)[i+2] = substr(a, start = 2, stop=5)
}








##### Diabetes By State
diabetes_per_state = data.frame(t(diabetes_per_94_16[, c(1, 3:25)]))
diabetes_per_state[1, ]
colnames(diabetes_per_state) = diabetes_per_state[1, ]
rownames(diabetes_per_state)[1] = 'Year'
diabetes_per_region = melt(data = diabetes_per_94_16[, -1], id.vars = 'Region', variable_name = 'Year', na.rm = T)
colnames(diabetes_per_region)[3] = 'Percentage'
diabetes_per_region$Year = as.numeric(as.character(diabetes_per_region$Year))



# ################ Pivot Table
# diabetes_per_region = cast(data = diabetes_per_region,formula = Year~Region, fun.aggregate = 'mean', value = 'Percentage')
# ### melt it
# diabetes_per_region = melt(data=diabetes_per_region)
# colnames(diabetes_per_region)[2] ='Percentage'
# diabetes_per_region = melt(data = diabetes_per_94_16[, -1], id.vars = 'Region', variable_name = 'Year', na.rm = T)

# diabetes_per_region = melt(data = diabetes_per_94_16[, -1], id.vars = 'Region', variable_name = 'Year', na.rm = T)
# colnames(diabetes_per_region)[3] = 'Percentage'
# diabetes_per_region$Year = as.numeric(as.character(diabetes_per_region$Year))









################ Pivot Table
diabetes_per_region = cast(data = diabetes_per_region,formula = Year~Region, fun.aggregate = 'mean', value = 'Percentage')
### melt it
diabetes_per_region = melt(data=diabetes_per_region)
colnames(diabetes_per_region)[2] ='Percentage'



midwest=diabetes_per_region[diabetes_per_region$Region=='Midwest', ]
northeast=diabetes_per_region[diabetes_per_region$Region=='Northeast', ]
southeast=diabetes_per_region[diabetes_per_region$Region=='Southeast', ]
southwest=diabetes_per_region[diabetes_per_region$Region=='Southwest', ]
#territory=diabetes_per_region[diabetes_per_region$Region=='Territory', ]
west=diabetes_per_region[diabetes_per_region$Region=='West', ]

midwest = ts(midwest, start = 1994, end=2016)
northeast = ts(northeast, start = 1994, end=2016)
southeast = ts(southeast, start = 1994, end=2016)
southwest = ts(southwest, start = 1994, end=2016)
#territory = ts(territory, start = 1994, end=2016)
west = ts(west, start = 1994, end=2016)


midwest= midwest[, 2]
northeast= northeast[, 2]
southeast= southeast[, 2]
southwest= southwest[, 2]
#territory= territory[, 2]
west= west[, 2]



forecast_midwest = forecast(object = HoltWinters(midwest,gamma = F) , level=c(0.95))
forecast_northeast = forecast(object = HoltWinters(northeast, gamma=F), level=c(0.95))
forecast_southeast = forecast(object = HoltWinters(southeast, gamma=F), level=c(0.95))
forecast_southwest = forecast(object = HoltWinters(southwest, gamma=F), level=c(0.95))
#forecast_territory = forecast(object = territory, level=c(0.95))
forecast_west = forecast(object = HoltWinters(west, gamma=F), level=c(0.95))

##### Generate Point Forecasts
##### Generate Point Forecasts
forecast_midwest_point = forecast_midwest$mean
forecast_northeast_point = forecast_northeast$mean
forecast_southeast_point = forecast_southeast$mean
forecast_southwest_point = forecast_southwest$mean
#forecast_territory_point = forecast_territory$mean
forecast_west_point = forecast_west$mean
##### Lower Forecast
##### Lower Forecast
##### Lower Forecast
lower.95_midwest = forecast_midwest$lower
lower.95_northeast = forecast_northeast$lower
lower.95_southeast = forecast_southeast$lower
lower.95_southwest = forecast_southwest$lower
#lower.95_territory = forecast_territory$lower
lower.95_west = forecast_west$lower
##### Upper Forecast
##### Upper Forecast
##### Upper Forecast
upper.95_midwest = forecast_midwest$upper
upper.95_northeast = forecast_northeast$upper
upper.95_southeast = forecast_southeast$upper
upper.95_southwest = forecast_southwest$upper
#upper.95_territory = forecast_territory$upper
upper.95_west = forecast_west$upper
#forecast_midwest=data.frame(Year=2017:2026, Mean=forecast_midwest$mean, forecast_midwest$lower, forecast_midwest$upper)
#colnames(forecast_midwest)[3:4] = c('X5', 'X95')
dat = data.frame(Year=1994:2026,
                 fitted_midwest = c(midwest, rep(NA, length(forecast_midwest_point))),
                 fitted_northeast = c(northeast, rep(NA, length(forecast_northeast_point))),
                 fitted_southeast = c(southeast, rep(NA, length(forecast_southeast_point))),
                 fitted_southwest = c(southwest, rep(NA, length(forecast_southwest_point))),
                 #itted_territory = c(territory, rep(NA, length(forecast_territory_point))),
                 fitted_west = c(west, rep(NA, length(forecast_west_point))),
                 forecasted_midwest=c(rep(NA, length(midwest)), forecast_midwest_point),
                 forecasted_northeast=c(rep(NA, length(midwest)), forecast_northeast_point),
                 forecasted_southeast=c(rep(NA, length(midwest)), forecast_southeast_point),
                 forecasted_southwest=c(rep(NA, length(midwest)), forecast_southwest_point),
                 #forecasted_territory=c(rep(NA, length(midwest)), forecast_territory_point),
                 forecasted_west=c(rep(NA, length(midwest)), forecast_west_point),
                 lower.95_midwest = c(rep(NA, length(midwest)), lower.95_midwest),
                 lower.95_northeast = c(rep(NA, length(midwest)), lower.95_northeast),
                 lower.95_southeast = c(rep(NA, length(midwest)), lower.95_southeast),
                 lower.95_southwest = c(rep(NA, length(midwest)), lower.95_southwest),
                 #lower.95_territory = c(rep(NA, length(midwest)), lower.95_territory),
                 lower.95_west = c(rep(NA, length(midwest)), lower.95_west),
                 upper.95_midwest = c(rep(NA, length(midwest)), upper.95_midwest),
                 upper.95_northeast = c(rep(NA, length(midwest)), upper.95_northeast),
                 upper.95_southeast = c(rep(NA, length(midwest)), upper.95_southeast),
                 upper.95_southwest = c(rep(NA, length(midwest)), upper.95_southwest),
                 #upper.95_territory = c(rep(NA, length(midwest)), upper.95_territory),
                 upper.95_west = c(rep(NA, length(midwest)), upper.95_west))

dat[, -1] = dat[, -1]/100

ggplot(data = dat) +
  geom_line(aes(x=Year, y=fitted_midwest, colour='Midwest')) +
  geom_line(aes(x=Year, y=fitted_northeast, colour='Northeast')) +
  geom_line(aes(x=Year, y=fitted_southeast, colour='Southeast')) +
  geom_line(aes(x=Year, y=fitted_southwest, colour='Southwest')) +
  #geom_line(aes(x=Year, y=fitted_territory, colour='Territory')) +
  geom_line(aes(x=Year, y=fitted_west, colour='West')) +
  geom_line(data=dat, aes(Year, forecasted_midwest, colour='Midwest'), size=1.0) +
  geom_line(data=dat, aes(Year, forecasted_northeast, colour='Northeast'), size=1.0) +
  geom_line(data=dat, aes(Year, forecasted_southeast, colour='Southeast'), size=1.0) +
  geom_line(data=dat, aes(Year, forecasted_southwest, colour='Southwest'), size=1.0) +
  #geom_line(data=dat, aes(Year, forecasted_territory, colour='Territory'), size=1.0) +
  geom_line(data=dat, aes(Year, forecasted_west, colour='West'), size=1.0) +
  geom_ribbon(data=dat, aes(Year, ymin=lower.95_midwest, ymax=upper.95_midwest, fill='Midwest'), alpha=0.2) +
  geom_ribbon(data=dat, aes(Year, ymin=lower.95_northeast, ymax=upper.95_northeast, fill='Northeast'), alpha=0.2) +
  geom_ribbon(data=dat, aes(Year, ymin=lower.95_southeast, ymax=upper.95_southeast, fill='Southeast'), alpha=0.2) +
  geom_ribbon(data=dat, aes(Year, ymin=lower.95_southwest, ymax=upper.95_southwest, fill='Southwest'), alpha=0.2) +
  #geom_ribbon(data=dat, aes(Year, ymin=lower.95_territory, ymax=upper.95_territory, fill='Territory'), alpha=0.2) +
  geom_ribbon(data=dat, aes(Year, ymin=lower.95_west, ymax=upper.95_west, fill='West'), alpha=0.2) +
  scale_x_continuous(limits = c(1994, 2026), breaks = seq(1994, 2026, by = 2)) +
  labs(fill='U.S. Region', y='Diabetes Percentage') + 
  guides(colour=F) +
  scale_y_continuous(breaks = seq(0,0.25, 0.05), labels = scale::percent) 
  





r