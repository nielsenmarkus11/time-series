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
  arrange(AQS_SITE_ID,year) %>% 
  print(n=40)

# Get average by site, then by day
pm25.smry <- pm25 %>% 
  # Remove data from sensors that don't span entire time period
  filter(!(AQS_SITE_ID %in% c(490353013,490450003))) %>% 
  group_by(AQS_SITE_ID,Date) %>% 
  summarise(pm2.5 = mean(Daily.Mean.PM2.5.Concentration)) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  summarise(pm2.5 = mean(pm2.5))

# Load weather data
weather <- read.csv("data/2010-2017_Weather.csv")
weather$DATE <- as.Date(weather$DATE,format="%Y-%m-%d") 
weather <- weather %>% 
  filter(DATE>as.Date("2013-12-31"))
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
  mutate(inversion_diff=avg_valley_tmp-avg_peak_tmp,
         inversion=as.numeric((avg_valley_tmp-avg_peak_tmp<0))) %>% 
  select(-avg_valley_tmp,-avg_peak_tmp)

# Visualize sensor locations
w.sensors <- weather %>% distinct(LATITUDE,LONGITUDE)
pm.sensors <- pm25 %>%
  # Remove data from sensors that don't span entire time period
  filter(!(AQS_SITE_ID %in% c(490353013,490450003))) %>%
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

# Add July 4th and New Years holidays
dat$fireworks <- ifelse(dat$date %in% dat$date[(month(dat$date)==7 & day(dat$date) %in% c(1:7,21:27))|
                                               (month(dat$date)==1 & day(dat$date)==1)],
                        1,0)


Hmisc::describe(dat)

# Find missing data
dat[!complete.cases(dat),]
# Fill in missing data
dat$pm2.5[dat$date==as.Date("2015-04-02")]=mean(dat$pm2.5[dat$date %in% as.Date(c("2015-04-01","2015-04-03"))])


# What is the relationship between variables
pairs(dat[,-1])

plot(dat$date,dat$inversion,type = 'l', ylim = c(-20,60),xlab = "", ylab="",
     main="Inversion Compared to PM 2.5 Levels")
lines(dat$date,dat$pm2.5,col='red')
legend('topright',legend = c("Inversion","PM 2.5"), col=c("black","red"), lty=1)

# Fit a linear model
fit1 <- lm(sqrt(pm2.5)~inversion+wind+precip+fireworks,data=dat)
summary(fit1)

dat$resid[!is.na(dat$pm2.5)] <- resid(fit1)

# Plot the residuals
jpeg("/mnt/c/Users/nielsen-laptop/Documents/reg-resid.jpg",height=4.25,width=5.5,res=200
     ,units = "in")
ggplot(dat,aes(date,resid)) + 
  geom_point() + geom_smooth() +
  ggtitle("Linear Regression Residuals",
          subtitle = paste0("RMSE: ",round(sqrt(mean(dat$resid^2,na.rm=TRUE)),2)))
dev.off()


jpeg("/mnt/c/Users/nielsen-laptop/Documents/reg-acf.jpg",height=4.25,width=5.5,res=200
     ,units = "in")
Acf(dat$resid, main="ACF of OLS Residuals")
dev.off()
# residual plots look suspicious


# Fit a random forest
fit2 <- randomForest(sqrt(pm2.5)~inversion+wind+precip+fireworks,data=dat[!is.na(dat$pm2.5),], ntree=500)
dat$rf.resid[!is.na(dat$pm2.5)] <- fit2$predicted - sqrt(dat$pm2.5[!is.na(dat$pm2.5)])

# Plot the residuals
jpeg("/mnt/c/Users/nielsen-laptop/Documents/rf-resid.jpg",height=4.25,width=5.5,res=200
     ,units = "in")
ggplot(dat,aes(date,rf.resid)) + 
  geom_point() + geom_smooth() +
  ggtitle("Random Forest Residuals",
          subtitle = paste0("RMSE: ",round(sqrt(fit2$mse[500]),2)))
dev.off()

Acf(dat$rf.resid, main="ACF of RF Residuals")
Pacf(dat$rf.resid, main="PACF of RF Residuals")

plot(sqrt(fit2$mse),type="l")
round(sqrt(fit2$mse[500]),2)
# Better but we still have some odd things going on in our data

# Zoom In
jpeg("/mnt/c/Users/nielsen-laptop/Documents/rf-zoom.jpg",height=4.25,width=5.5,res=200
     ,units = "in")
p1 <- ggplot(dat,aes(date,rf.resid)) + 
  geom_point() + geom_line() +
  xlim(as.Date(c("2014-01-01","2014-02-28"))) + 
  geom_abline(slope=0, intercept = 0, lty=2, col = "blue", lwd = 1.25)

p2 <- ggplot(dat,aes(date,rf.resid)) + 
  geom_point() + geom_line() +
  xlim(as.Date(c("2017-11-01","2017-12-31"))) + 
  geom_abline(slope=0, intercept = 0, lty=2, col = "blue", lwd = 1.25)


grid.arrange(p1, p2, ncol=2, top="Zoom-in of Random Forest Residuals")
dev.off()

# time series with 7 day seasonality
dat.ts <- sqrt(ts(dat[,"pm2.5"], frequency = 7))
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


jpeg("/mnt/c/Users/nielsen-laptop/Documents/ets-resid.jpg",height=4.25,width=5.5,res=200
     ,units = "in")
ggplot(ets.mod,aes(day,resid)) + 
  geom_point() + geom_smooth() + 
  facet_grid(type~.,scales="free")+
  ggtitle("ETS Residuals with Weekly Seasonality",
          subtitle = paste0("Auto RMSE: ",round(sqrt(fit3$mse),2),
                            "   Additive RMSE: ",round(sqrt(fit4a$mse),2),
                            "   Multiplicative RMSE: ",round(sqrt(fit4b$mse),2)))

dev.off()


# TBATS model with weekly and yearly seasonality
dat.ts2 <- sqrt(msts(dat[!is.na(dat$pm2.5),"pm2.5"], seasonal.periods=c(7,365.25)))
fit5 <- tbats(dat.ts2)
plot(fit5)
fc5 <- forecast(fit5,h=30)
plot(fc5)


jpeg("/mnt/c/Users/nielsen-laptop/Documents/tbats-resid.jpg",height=4.25,width=5.5,res=200
     ,units = "in")
tbats.mod <- data.frame(day=1:sum(!is.na(dat.ts)),resid=as.numeric(residuals(fit5)))
ggplot(tbats.mod,aes(day,resid)) + 
  geom_point() + geom_smooth() + 
  ggtitle("TBATS Resids with Dual Seasonality",
          subtitle = paste0("Auto RMSE: ",round(sqrt(mean((residuals(fit5))^2)),2)))
dev.off()

# ARIMA with weekly and yearly seasonality with regressors
regs <- dat[!is.na(dat$pm2.5),c("precip","wind","inversion","fireworks")]

# Forecast weather
weather.ts <- msts(dat[,c("precip","wind","inversion_diff")],seasonal.periods = c(7,365.25))
precip <- auto.arima(weather.ts[,1])
fprecip <- as.numeric(data.frame(forecast(precip,h=25))$Point.Forecast)
wind <- auto.arima(weather.ts[,2])
fwind <- as.numeric(data.frame(forecast(wind,h=25))$Point.Forecast)
inversion <- auto.arima(weather.ts[,3])
finversion <- as.numeric(data.frame(forecast(inversion,h=25))$Point.Forecast)

fregs <- data.frame(precip=fprecip,wind=fwind,inversion=as.numeric(finversion<0),fireworks=0)

z <- fourier(dat.ts2, K=c(2,5))
zf <- fourier(dat.ts2, K=c(2,5), h=25)
fit <- auto.arima(dat.ts2, xreg=cbind(z,regs), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(zf,fregs), h=25)
plot(fc,xlim=c(4.8,5.2))

arima.mod <- data.frame(day=1:sum(!is.na(dat.ts)),resid=as.numeric(residuals(fit)))

jpeg("/mnt/c/Users/nielsen-laptop/Documents/arima-resid.jpg",height=4.25,width=5.5,res=200
     ,units = "in")
ggplot(arima.mod,aes(day,resid)) + 
  geom_point() + geom_smooth() + 
  ggtitle("Arima Resids with Seasonality and Regressors",
          subtitle = paste0("RMSE: ",round(sqrt(mean((residuals(fit))^2)),2)))
dev.off()


# prophet
pdat <- data.frame(ds=dat$date,
                   y=sqrt(dat$pm2.5),
                   precip=dat$precip,
                   wind=dat$wind,
                   inversion_diff=dat$inversion_diff,
                   inversion=dat$inversion_,
                   fireworks=dat$fireworks)
pfdat <- data.frame(ds=max(dat$date) + 1:25)
pprecip <- pdat %>% 
  select(ds,y=precip) %>% 
  prophet() %>%
  predict(pfdat)

pwind <- pdat %>% 
  select(ds,y=wind) %>% 
  prophet() %>%
  predict(pfdat)

pinversion <- pdat %>% 
  select(ds,y=inversion_diff) %>% 
  prophet() %>%
  predict(pfdat)

fdat <-  data.frame(ds=pfdat$ds,
                    precip=pprecip$yhat,
                    wind=pwind$yhat,
                    inversion=as.numeric(pinversion$yhat<0),
                    fireworks = 0)

fit6 <- prophet() %>% 
  add_regressor('precip') %>% 
  add_regressor('wind') %>% 
  add_regressor('inversion') %>% 
  add_regressor('fireworks') %>% 
  fit.prophet(pdat)

forecast <- predict(fit6, fdat)
fpred <- predict(fit6)
fpred$ds <- as.Date(fpred$ds)
fpred <- pdat %>% left_join(fpred,by="ds")
fpred$resid <- fpred$y - fpred$yhat


jpeg("/mnt/c/Users/nielsen-laptop/Documents/prophet-resid.jpg",height=4.25,width=5.5,res=200
     ,units = "in")
ggplot(fpred,aes(ds,resid)) + 
  geom_point() + geom_smooth() + 
  ggtitle("Prophet with Seasonality and Regressors",
          subtitle = paste0("RMSE: ",round(sqrt(mean(fpred$resid^2)),2)))
dev.off()

plot(fit6, forecast)
prophet_plot_components(fit6, forecast)

years <- 3

fit.cv <- cross_validation(fit6,30,units="days",initial = 365*years)
fit.cv$day <- as.numeric(fit.cv$ds - fit.cv$cutoff)

dats <- unique(fit.cv$cutoff)
# Regression
reg.cv <- NULL
for (i in 1:length(dats)){
  j = dats[i]
  tmp.fit <- lm(sqrt(pm2.5)~inversion+wind+precip,data=dat[dat$date<=j & dat$date>(j-years*365*60*60*24),])
  tmp.fcst <- dat[dat$date>j & dat$date<=(j+30*60*60*24),]
  tmp.fcst$inversion <- dat$inversion[dat$date==j]
  tmp.fcst$wind <- dat$wind[dat$date==j]
  tmp.fcst$precip <- dat$precip[dat$date==j]
  tmp.fcst$yhat <- predict(tmp.fit,newdata = tmp.fcst)^2
  tmp.fcst$cutoff <- j
  reg.cv <- rbind(reg.cv,tmp.fcst)
}

# Random Forests
rf.cv <- NULL
for (i in 1:length(dats)){
  j = dats[i]
  tmp.fit <- randomForest(sqrt(pm2.5)~inversion+wind+precip,
                          data=dat[dat$date<=j & dat$date>(j-years*365*60*60*24) & !is.na(dat$pm2.5),],
                          ntree=500)
  tmp.fcst <- dat[dat$date>j & dat$date<=(j+30*60*60*24),]
  tmp.fcst$inversion <- dat$inversion[dat$date==j]
  tmp.fcst$wind <- dat$wind[dat$date==j]
  tmp.fcst$precip <- dat$precip[dat$date==j]
  tmp.fcst$yhat <- predict(tmp.fit,newdata = tmp.fcst)^2
  tmp.fcst$cutoff <- j
  rf.cv <- rbind(rf.cv,tmp.fcst)
}

# ETS
ets.cv <- NULL
for (i in which(as.POSIXct(dat$date) %in% dats)){
  # i=741
  xshort <- window(dat.ts,start=(i-years*365+1)/7,end=i/7)
  tmp.fit <- ets(xshort,model = "MAN")
  fcst <- predict(tmp.fit, h=30)
  tmp.fcst <- data.frame(fcst)
  tmp.fcst$date <- dat$date[(i+1):(i+30)]
  tmp.fcst$cutoff <- dat$date[i]
  tmp.fcst$y <- dat$pm2.5[(i+1):(i+30)]
  ets.cv <- rbind(ets.cv,tmp.fcst)
}

# tbats.cv <- tsCV(dat.ts2,tbats,h=30,window = 2*365)

# TBATS
tbats.cv <- NULL
for (i in which(as.POSIXct(dat$date) %in% dats)){
  # i=741
  xshort <- window(dat.ts2,start=1+(i-years*365)/365.25,end=1+i/365.25)
  tmp.fit <- tbats(xshort,
                   bc.lower = .4, bc.upper = .5,
                   max.p =0, max.q=4,
                   seasonal.periods = c(7,365.25))
  fcst <- predict(tmp.fit, h=30)
  tmp.fcst <- data.frame(fcst)
  tmp.fcst$date <- dat$date[(i+1):(i+30)]
  tmp.fcst$cutoff <- dat$date[i]
  tmp.fcst$y <- dat$pm2.5[(i+1):(i+30)]
  tbats.cv <- rbind(tbats.cv,tmp.fcst)
}

# ARIMA 
arima.cv <- NULL
for (i in which(as.POSIXct(dat$date) %in% dats)){
  # i=741
  regs <- dat[(i-2*365):i,c("precip","wind","inversion","fireworks")]
  xshort <- msts(dat[(i-2*365):i,"pm2.5"], seasonal.periods=c(7,365.25))
  
  # Forecast weather
  weather.ts <- msts(dat[(i-2*365):i,c("precip","wind","inversion_diff")],
                     seasonal.periods = c(7,365.25))
  precip <- auto.arima(weather.ts[,1])
  fprecip <- as.numeric(data.frame(forecast(precip,h=30))$Point.Forecast)
  wind <- auto.arima(weather.ts[,2])
  fwind <- as.numeric(data.frame(forecast(wind,h=30))$Point.Forecast)
  inversion <- auto.arima(weather.ts[,3])
  finversion <- as.numeric(data.frame(forecast(inversion,h=30))$Point.Forecast)
  
  fregs <- data.frame(precip=fprecip,
                      wind=fwind,
                      inversion=as.numeric(finversion<0),
                      fireworks=dat$fireworks[(i+1):(i+30)])
  
  z <- fourier(xshort, K=c(2,5))
  zf <- fourier(xshort, K=c(2,5), h=30)
  tmp.fit <- arima(sqrt(xshort), order = c(1,0,2), xreg = cbind(z,regs), seasonal=c(0,0,0))
  fcst <- predict(tmp.fit, newxreg=cbind(zf,fregs), h=30)
  tmp.fcst <- data.frame(yhat=as.numeric(fcst$pred^2))
  tmp.fcst$date <- dat$date[(i+1):(i+30)]
  tmp.fcst$cutoff <- dat$date[i]
  tmp.fcst$y <- dat$pm2.5[(i+1):(i+30)]
  arima.cv <- rbind(arima.cv,tmp.fcst)
}

# prophet
prophet.cv <- NULL
for (i in 1:length(dats)){
  # i=1
  j = dats[i]
  pdat <- data.frame(ds=dat$date,
                     y=sqrt(dat$pm2.5),
                     precip=dat$precip,
                     wind=dat$wind,
                     inversion_diff=dat$inversion_diff,
                     inversion=dat$inversion,
                     fireworks=dat$fireworks)[dat$date<=j & dat$date>(j-years*365*60*60*24),]
  pfdat <- data.frame(ds=j + 1:30*60*60*24)
  pprecip <- pdat %>% 
    select(ds,y=precip) %>% 
    prophet() %>%
    predict(pfdat)
  
  pwind <- pdat %>% 
    select(ds,y=wind) %>% 
    prophet() %>%
    predict(pfdat)
  
  pinversion <- pdat %>% 
    select(ds,y=inversion_diff) %>% 
    prophet() %>%
    predict(pfdat)
  
  fdat <-  data.frame(ds=pfdat$ds,
                      y=dat$pm2.5[dat$date>j & dat$date<=(j+30*60*60*24)],
                      precip=pprecip$yhat,
                      wind=pwind$yhat,
                      inversion=as.numeric(pinversion$yhat<0),
                      fireworks = dat$fireworks[dat$date>j & dat$date<=(j+30*60*60*24)])
  
  fit6 <- prophet() %>% 
    add_regressor('precip') %>% 
    add_regressor('wind') %>% 
    add_regressor('inversion') %>% 
    add_regressor('fireworks') %>% 
    fit.prophet(pdat)
  
  forecast <- predict(fit6, fdat)
  forecast$ds <- as.Date(forecast$ds)
  fdat$ds <- as.Date(fdat$ds)
  forecast <- fdat %>% left_join(forecast,by="ds")
  forecast$cutoff <- j
  prophet.cv <- rbind(prophet.cv,forecast)
}

# Combine forecasts to make comparisons
reg.cv2 <- reg.cv %>% 
  mutate(cutoff=as.Date(cutoff),day=as.numeric(date-cutoff),model="Linear Regression") %>% 
  select(date,y=pm2.5,yhat,cutoff,day,model)

rf.cv2 <- rf.cv %>% 
  mutate(cutoff=as.Date(cutoff),day=as.numeric(date-cutoff),model="Random Forest") %>% 
  select(date,y=pm2.5,yhat,cutoff,day,model)

ets.cv2 <- ets.cv %>% 
  mutate(day=as.numeric(date-cutoff), yhat=Point.Forecast^2,model="Exponential Smoothing") %>% 
  select(date,y,yhat,cutoff,day,model)

tbats.cv2 <- tbats.cv %>% 
  mutate(day=as.numeric(date-cutoff), yhat=Point.Forecast^2,model="TBATS") %>% 
  select(date,y,yhat,cutoff,day,model)


arima.cv2 <- arima.cv %>% 
  mutate(day=as.numeric(date-cutoff),model="ARIMA") %>% 
  select(date,y,yhat,cutoff,day,model)

fit.cv2 <- fit.cv %>% 
  mutate(date=as.Date(ds),cutoff=as.Date(cutoff), y=y^2, yhat=yhat^2,
         day=as.numeric(date-cutoff),model="prophet") %>% 
  select(date,y,yhat,cutoff,day,model)

prophet.cv2 <- prophet.cv %>% 
  mutate(date=as.Date(ds),cutoff=as.Date(cutoff), yhat=yhat^2, 
         day=as.numeric(date-cutoff),model="prophet") %>% 
  select(date,y,yhat,cutoff,day,model)

all.cv <- bind_rows(reg.cv2,rf.cv2,ets.cv2,tbats.cv2,arima.cv2,prophet.cv2)

# read.csv("data/fit.cv.csv",header=TRUE)

jpeg("/mnt/c/Users/nielsen-laptop/Documents/comp-cutoff.jpg",height=4.25,width=5.5,res=200
     ,units = "in")
all.cv %>% 
  group_by(model,cutoff) %>% 
  summarise(rmse=sqrt(mean((y-yhat)^2))) %>% 
  ggplot(.,aes(x=cutoff,y=rmse,group=model,color=model)) +
  geom_line(alpha=.75) + geom_point(alpha=.75) +
  theme(legend.position = "bottom")
dev.off()


jpeg("/mnt/c/Users/nielsen-laptop/Documents/comp-horizon.jpg",height=4.25,width=5.5,res=200
     ,units = "in")
all.cv %>% 
  group_by(model,day) %>% 
  summarise(rmse=sqrt(mean((y-yhat)^2))) %>% 
  ggplot(.,aes(x=day,y=rmse,group=model,color=model)) +
  geom_line(alpha=.75) + geom_point(alpha=.75) +
  theme(legend.position = "bottom")
dev.off()


jpeg("/mnt/c/Users/nielsen-laptop/Documents/comp-all.jpg",height=4.25,width=7.5,res=200
     ,units = "in")
ggplot(all.cv,aes(date,yhat,group=as.factor(cutoff),color=as.factor(cutoff)))+
  geom_line()+
  geom_line(aes(y=y),color="black",alpha=.15)+#geom_point(aes(y=y),color="black",alpha=.15)+
  facet_wrap(~model)+ guides(color="none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
dev.off()

# Output for shiny
save(list=c("weather","pm25","inversion","dat","all.cv","tbats.mod","ets.mod","arima.mod","fpred"),
     file="data/ts-dat.Rdat")
