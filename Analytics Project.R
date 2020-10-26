rm(list = ls()) #clears GE
unemp <- read.csv("https://raw.githubusercontent.com/BenAGelman/Analytics-Project/main/Analytics%20Project%20Data.csv?token=ARCYOTMPDD5RCHFRPTLURT27SRXJG") #downloads unemployment dataset
dim(unemp) #pritns dimensions of df
summary(unemp) #provides six number summary, checking for NA's
View(unemp) #prints table view of data
mean(unemp$Unemployment.Rate) #prints average unemployment rate
sd(unemp$Unemployment.Rate) #pritns standard deviation of unemployment
var(unemp$Unemployment.Rate) #variance of unemployment rate
plot(unemp$Unemployment.Rate, type = 'l')
plot(Unemployment.Rate~Population, unemp) #creates a plot with population on x 
#and unemployment rate on y 
plot(Civilian.Labor.Force~Unemployment.Rate, unemp) #creates a plot of unemployment rate, x
# and civilian labor force y
plot(Population~Civilian.Labor.Force, unemp)
plot(Employed~Unemployed, unemp)
plot(Unemployed~Employed, unemp)
plot(unemp$Unemployment.Rate~unemp$Civilian)
class(unemp$Population)
#INSTALL THE GGPLOT2 PACKAGE (MUST BE DONE ONCE)
install.packages('ggplot2')
#LOAD THE GGPLOT2 LIBRARY (MUST BE DONE EVERY TIME)
library(ggplot2)
plot_a<-ggplot(unemp, aes(x = Population, y = Unemployment.Rate, color = Region)) + # crates a plot of Population against Unemployment rate colored by Region
  geom_point() +
  ggtitle('Unemployment Rate Compared to Population') + #creates title
  theme(plot.title =  element_text(hjust = .5)) + #centers title
  xlab('Population') + #creates x-axis label
  ylab('Unemployment Rate') #creates y-axis label
plot_a

plot_b<-ggplot(unemp, aes(GDP..billions., Unemployment.Rate, color = Region)) + #creates a plot of GDP against Unemployment Rate and colors by region 
  geom_point() +
  ggtitle('Unemployment Rate Compared to GDP') + #adds title
  theme(plot.title = element_text(hjust = .5)) + #centers title
  xlab('State GDP (Billions)') + #adds x-axis label
  ylab('Unemployment Rate') #adds y-axis label
plot_b

plot_c<-ggplot(unemp, aes(Region, GDP..billions.)) + 
  geom_point()+ #plots Region against GDP colored by unemployment rate
  ggtitle('GDP by Region') + #adds title
  theme(plot.title = element_text(hjust = .5)) + #centers title
  xlab('Region') + #adds x-axis label
  ylab('GDP') #adds y-axis label
plot_c

plot_d<-ggplot(unemp, aes(Population, Unemployment.Rate)) + 
  geom_violin() + #crates a compact density violin 
  #plot of population and unemployment rate
  ggtitle('Population vs. Unemployment Rate Violin Plot') + 
  #adds title
  theme(plot.title = element_text(hjust = .5)) + 
  #centers title
  xlab('Population') + #adds x-axis label
  ylab('Unemployment Rate') #adds y-axis label
plot_d

plot_e<-ggplot(unemp, aes(Population, Civilian.Labor.Force, color = Region)) + # crates a plot of Population against Unemployment rate colored by Region
  geom_point() +
  ggtitle('Civilian Labor Force Compared to Population') + #creates title
  theme(plot.title =  element_text(hjust = .5)) + #centers title
  xlab('Population') + #creates x-axis label
  ylab('Civilian Labor Force') #creates y-axis label
plot_e
ploft_f<-hist(unemp$Unemployment.Rate)
plot_f<-unemp$Unemployment.Rate
m<-mean(plot_f)
std<-sqrt(var(plot_f))
hist(plot_f,
  xlab = "Unemployment Rate", #creates x-axis label
  ylab = "Frequency", #creates y-axis label
  main = "Unemployment Rate Histogram") #creates title
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=3, add=TRUE, yaxt="n") #creates normal curve above histogram to show how data is skewed 
MODEL1<- lm(Unemployment.Rate~Population, unemp) #creates model object
summary(MODEL1) #Reports summary output of model object
plot_g<-ggplot(unemp, aes(Region, Unemployment.Rate)) + 
  geom_point()+ #plots Region against 
  #GDP colored by unemployment rate
  ggtitle('Unemployment Rate by Region') + #adds title
  theme(plot.title = element_text(hjust = .5)) + 
  #centers title
  xlab('Region') + #adds x-axis label
  ylab('Unemployment Rate') #adds y-axis label
plot_g
