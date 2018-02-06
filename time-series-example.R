library(tidyverse)
library(gridExtra)
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

# Output data for presentation
write.csv(dat,"data/pollution-ts.csv",row.names = FALSE)

Hmisc::describe(dat)

# What is the relationship between variables
pairs(dat[,-1])

plot(dat$date,dat$inversion,type = 'l', ylim = c(-20,60),xlab = "", ylab="",
     main="Inversion Compared to PM 2.5 Levels")
lines(dat$date,dat$pm2.5,col='red')
legend('topright',legend = c("Inversion","PM 2.5"), col=c("black","red"), lty=1)

# Fit a linear model
fit1 <- lm(pm2.5~inversion+wind+precip,data=dat)
summary(fit1)

dat$resid[!is.na(dat$pm2.5)] <- resid(fit1)

# Plot the residuals
ggplot(dat,aes(date,resid)) + 
  geom_point() + geom_smooth() +
  ggtitle("Linear Regression Residuals",
          subtitle = paste0("RMSE: ",round(sqrt(mean(dat$resid^2,na.rm=TRUE)),2)))

# residual plots look suspicious

# Fit a random forest
fit2 <- randomForest(pm2.5~inversion+wind+precip,data=dat[!is.na(dat$pm2.5),], ntree=500)
dat$rf.resid[!is.na(dat$pm2.5)] <- fit2$predicted - dat$pm2.5[!is.na(dat$pm2.5)]

# Plot the residuals
ggplot(dat,aes(date,rf.resid)) + 
  geom_point() + geom_smooth() +
  ggtitle("Random Forest Residuals",
          subtitle = paste0("RMSE: ",round(sqrt(fit2$mse[500]),2)))

plot(sqrt(fit2$mse),type="l")
round(sqrt(fit2$mse[500]),2)
# Better but we still have some odd things going on in our data

# Zoom In
p1 <- ggplot(dat,aes(date,rf.resid)) + 
  geom_point() + geom_line() +
  xlim(as.Date(c("2014-01-01","2014-02-28"))) + 
  geom_abline(slope=0, intercept = 0, lty=2, col = "blue", lwd = 1.25)

p2 <- ggplot(dat,aes(date,rf.resid)) + 
  geom_point() + geom_line() +
  xlim(as.Date(c("2017-11-01","2017-12-31"))) + 
  geom_abline(slope=0, intercept = 0, lty=2, col = "blue", lwd = 1.25)


grid.arrange(p1, p2, ncol=2, top="Zoom-in of Random Forest Residuals")


# Fill in missing data
dat$pm2.5[dat$date==as.Date("2015-04-02")]=mean(dat$pm2.5[dat$date %in% as.Date(c("2015-04-01","2015-04-03"))])

# time series with 7 day seasonality
dat.ts <- ts(dat[,5], frequency = 7)
plot(dat.ts)

# Exponential smoothing model with weekly seasonality
fit3 <- ets(dat.ts)
fit4a <- ets(dat.ts,model ="AAA")
fit4b <- ets(dat.ts,model ="MMM")
fc3 <- forecast(fit3)
fc4a <- forecast(fit4a)
fc4b <- forecast(fit4b)

plot(fc3)
plot(fc4a)
plot(fc4b)

ets.mod <- rbind(data.frame(day=1:sum(!is.na(dat.ts)),resid=as.numeric(residuals(fit3)), type="Auto"),
                 data.frame(day=1:sum(!is.na(dat.ts)),resid=as.numeric(residuals(fit4a)), type="Additive"),
                 data.frame(day=1:sum(!is.na(dat.ts)),resid=as.numeric(residuals(fit4b)), type="Multiplicative"))


ggplot(ets.mod,aes(day,resid)) + 
  geom_point() + geom_smooth() + 
  facet_grid(type~.,scales="free")+
  ggtitle("Exponential Smoothing Model Residuals with Weekly Seasonality",
          subtitle = paste0("Auto RMSE: ",round(sqrt(fit3$mse),2),
                            "   Additive RMSE: ",round(sqrt(fit4a$mse),2),
                            "   Multiplicative RMSE: ",round(sqrt(fit4b$mse),2)))




# TBATS model with weekly and yearly seasonality
dat.ts2 <- msts(dat[!is.na(dat$pm2.5),5], seasonal.periods=c(7,365.25))
fit5 <- tbats(dat.ts2)
fc5 <- forecast(fit5)
plot(fc5)


tbats.mod <- data.frame(day=1:sum(!is.na(dat.ts)),resid=as.numeric(residuals(fit5)))
ggplot(tbats.mod,aes(day,resid)) + 
  geom_point() + geom_smooth() + 
  ggtitle("TBATS Model Residuals with Weekly and Yearly Seasonality",
          subtitle = paste0("Auto RMSE: ",round(sqrt(mean((residuals(fit5))^2)),2)))


# ARIMA with weekly and yearly seasonality with regressors
regs <- dat[!is.na(dat$pm2.5),2:4]
fregs <- dat[is.na(dat$pm2.5) & complete.cases(dat[,2:4]),2:4]

z <- fourier(dat.ts2, K=c(2,5))
zf <- fourier(dat.ts2, K=c(2,5), h=25)
fit <- auto.arima(dat.ts2, xreg=cbind(z,regs), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(zf,fregs), h=25)
plot(fc,xlim=c(4.8,5.2))

arima.mod <- data.frame(day=1:sum(!is.na(dat.ts)),resid=as.numeric(residuals(fit)))
ggplot(arima.mod,aes(day,resid)) + 
  geom_point() + geom_smooth() + 
  ggtitle("Arima Model Residuals with Seasonality and Regressors",
          subtitle = paste0("RMSE: ",round(sqrt(mean((residuals(fit))^2)),2)))



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

fit6 <- prophet() %>% 
  add_regressor('precip') %>% 
  add_regressor('wind') %>% 
  add_regressor('inversion') %>% 
  fit.prophet(pdat)

summary(fit6)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat


ggplot(fpred,aes(ds,resid)) + 
  geom_point() + geom_smooth() + 
  ggtitle("Prophet with Seasonality and Regressors",
          subtitle = paste0("RMSE: ",round(sqrt(mean(fpred$resid^2)),2)))


plot(fit6, forecast)
prophet_plot_components(fit6, forecast)



fit.cv <- cross_validation(fit6,30,units="days",initial = 365*2)
write.csv(fit.cv,"data/fit.cv.csv", row.names = FALSE)
# read.csv("data/fit.cv.csv",header=TRUE)

fit.cv %>% 
  group_by(cutoff) %>% 
  summarise(rmse=sqrt(mean((y-yhat)^2))) %>% 
  ggplot(.,aes(x=cutoff,y=rmse)) +
  geom_point() + geom_line()


