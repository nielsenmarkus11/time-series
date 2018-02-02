library(tidyverse)
library(leaflet)
library(lubridate)
library(randomForest)
library(forecast)
library(prophet)

# Load pollution data
pm25 <- NULL
for (i in 2014:2017){
  tmp <- read.csv(paste0("data/",i,"_pm2.5_SLCounty.csv"))
  pm25 <- rbind(pm25,tmp)
}
pm25$Date <- as.Date(pm25$Date,format="%m/%d/%Y")
# US Environmental Protection Agency. Air Quality System Data Mart [internet database] available at http://www.epa.gov/ttn/airs/aqsdatamart. Accessed Month DD, YYYY.

# What do we have data for?
pm25 %>% 
  mutate(year=year(Date)) %>%
  group_by(AQS_SITE_ID,year) %>%
  summarise(cnt=n()) %>% 
  arrange(AQS_SITE_ID,year)

# Get average by site, then by day
pm25.smry <- pm25 %>% 
  filter(AQS_SITE_ID!=490353013) %>% 
  group_by(AQS_SITE_ID,Date) %>% 
  summarise(pm2.5 = mean(Daily.Mean.PM2.5.Concentration)) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  summarise(pm2.5 = mean(pm2.5))

# Load weather data
weather <- read.csv("data/2014-2017_Weather.csv")
weather$DATE <- as.Date(weather$DATE,format="%Y-%m-%d")
head(weather)


# Calculate inversion
valley <- weather %>% 
  filter(NAME=="SALT LAKE CITY INTERNATIONAL AIRPORT, UT US") %>% 
  select(date=DATE,
         precip=PRCP,
         avg_valley_tmp=TAVG,
         wind=WSF2)
peak <- weather %>% 
  filter(NAME=="LOUIS MEADOW, UT US") %>% 
  select(date=DATE,
         avg_peak_tmp=TAVG)

inversion <- valley %>% 
  left_join(peak, by="date") %>% 
  mutate(inversion=avg_valley_tmp-avg_peak_tmp) %>% 
  select(-avg_valley_tmp,-avg_peak_tmp)

# Visualize sensor locations
w.sensors <- weather %>% distinct(LATITUDE,LONGITUDE)
pm.sensors <- pm25 %>% 
  distinct(AQS_SITE_ID,SITE_LATITUDE,SITE_LONGITUDE) %>% 
  rename(LATITUDE=SITE_LATITUDE, 
         LONGITUDE=SITE_LONGITUDE)

w.icons <- awesomeIcons(
  icon = 'tint',
  iconColor = 'blue',
  markerColor = "white"
)
pm.icons <- awesomeIcons(
  icon = 'cloud',
  iconColor = 'gray',
  markerColor = "black"
)
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(-112, 40.7, zoom = 10) %>% 
  addAwesomeMarkers(data=w.sensors, icon = w.icons) %>% 
  addAwesomeMarkers(data=pm.sensors, icon = pm.icons,label = ~as.character(AQS_SITE_ID))
m

# Now join pm2.5 and weather data
dat <- inversion %>% 
  full_join(pm25.smry, by=c("date"="Date"))

Hmisc::describe(dat)

# What is the relationship between variables
pairs(dat[,-1])

plot(dat$date,dat$inversion,type = 'l', ylim = c(-20,60))
lines(dat$date,dat$pm2.5,col='red')

# Fit a linear model
fit1 <- lm(pm2.5~inversion+wind+precip,data=dat)
summary(fit1)

dat$resid[!is.na(dat$pm2.5)] <- resid(fit1)
par(mfrow = c(1,1))
plot(dat$date,dat$resid)

mean(dat$resid^2,na.rm=TRUE)
# residual plots look suspicious

# Fit a random forest
fit2 <- randomForest(pm2.5~inversion+wind+precip,data=dat[!is.na(dat$pm2.5),], ntree=500)
dat$rf.resid[!is.na(dat$pm2.5)] <- fit2$predicted - dat$pm2.5[!is.na(dat$pm2.5)]
plot(dat$date,dat$rf.resid)
abline(h=0)

plot(fit2$mse,type="l")
fit2$mse[500]
# Better but we still have some odd things going on in our data

# Zoom In
par(mfrow=c(1,2))
plot(dat$date,dat$rf.resid,xlim=c(as.Date(c("2014-01-01","2014-02-28"))),type='b')
abline(h=0)
plot(dat$date,dat$rf.resid,xlim=c(as.Date(c("2017-11-01","2017-12-31"))),type='b')
abline(h=0)
par(mfrow=c(1,1))
plot(dat$date,dat$pm2.5,xlim=c(as.Date(c("2015-04-01","2015-04-30"))),type='b')
abline(h=0)

# Fill in missing
dat$pm2.5[dat$date==as.Date("2015-04-02")]=mean(dat$pm2.5[dat$date %in% as.Date(c("2015-04-01","2015-04-03"))])

# time series
dat.ts <- ts(dat[,5], frequency = 7)
plot(dat.ts)

# Exponential smoothing model with weekly seasonality
fit3 <- ets(dat.ts)
fit4 <- ets(dat.ts,model ="MMM")
fc3 <- forecast(fit3)
fc4 <- forecast(fit4)
par(mfrow=c(2,1))
plot(fc3)
plot(fc4)
par(mfrow=c(1,1))

plot(residuals(fit3),xlim=c(1,60))
fit3$mse

plot(residuals(fit4),xlim=c(1,60))
fit4$mse

# TBATS model with weekly and yearly seasonality
dat.ts2 <- msts(dat[!is.na(dat$pm2.5),5], seasonal.periods=c(7,365.25))
fit5 <- tbats(dat.ts2)
fc5 <- forecast(fit5)
plot(fc5)

plot(residuals(fit5),xlim=c(1,1.5))
abline(h=0)

mean((residuals(fit5))^2)




# ARIMA with weekly and yearly seasonality with regressors
regs <- dat[!is.na(dat$pm2.5),2:4]
fregs <- dat[is.na(dat$pm2.5) & complete.cases(dat[,2:4]),2:4]

z <- fourier(dat.ts2, K=c(2,5))
zf <- fourier(dat.ts2, K=c(2,5), h=25)
fit <- auto.arima(dat.ts2, xreg=cbind(z,regs), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(zf,fregs), h=25)
plot(fc,xlim=c(4.8,5.2))

mean((residuals(fit))^2)






# prophet
pdat <- data.frame(ds=dat$date,
                   y=dat$pm2.5,
                   precip=dat$precip,
                   wind=dat$wind,
                   inversion=dat$inversion)[!is.na(dat$pm2.5),]
fdat <-  data.frame(ds=dat$date,
                    y=dat$pm2.5,
                    precip=dat$precip,
                    wind=dat$wind,
                    inversion=dat$inversion)[complete.cases(dat[,2:4]),]
fit6 <- prophet()
fit6 <- add_regressor(fit6,'precip')
fit6 <- add_regressor(fit6,'wind')
fit6 <- add_regressor(fit6,'inversion')
fit6 <- fit.prophet(fit6,pdat)

forecast <- predict(fit6, fdat)

plot(fit6, forecast)
prophet_plot_components(fit6, forecast)

fit.cv <- cross_validation(fit6,30,units="days",initial = 365*2)
