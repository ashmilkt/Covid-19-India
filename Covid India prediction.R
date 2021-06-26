#Covid 19 prediction
#needed libraries
pacman::p_load(covid19.analytics,dplyr,prophet,lubridate,ggplot2)

#taking the data set of covid 19
covid_data= covid19.data(case='ts-confirmed')

#take only Indian data from the whole world data
asd=covid_data%>%
  filter(Country.Region=="India")

#checking the data
str(asd)
type_of(asd)
class(asd)

#converting rows into columns
asd=data.frame(t(asd))



#change the date from index to a variable
asd=cbind(asd,ds=rownames(asd),row.names=NULL)
asd=asd[,c(2,1)]

#renaming column
colnames(asd)=c("ds","y")

#removing other rows than date and case
asd=asd[-c(1:4),]

str(asd)
#specifying R the order of date is y-m-d
asd$ds=ymd(asd$ds)
asd$y=as.numeric(asd$y)

str(asd)

#ploting the data to see the str
plot(asd)
qplot(ds,y,data = asd, main = "Covid in India")

#forecasting
## for using prophet forcaster,the two variable names should be
### ds<- date and y<- values

#now forecasting
m=prophet(asd) #we don't have enough data to capture seasonality

##PREDICTION === making a data frame for future 28 days with the given data
future=make_future_dataframe(m,periods = 28,freq = "day",include_history = T)
forecast=predict(m,future)

##now ploting the forecast
plot(m,forecast)
dyplot.prophet(m,forecast)

##finding the forecast components
prophet_plot_components(m, forecast)

#model perfomance
pred=forecast$yhat[1:521]
actual= m$history$y
plot(actual, pred)
abline(lm(pred~actual),col="blue")

##summary to see accuracy
summary(lm(pred~actual))
