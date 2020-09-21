##installing necessary packages
install.packages(c("tidyverse", "ggplot2", "visdat", "ggpubr", "moments"))

##importing necessary libraries
library(tidyverse)
library(ggplot2)
library(visdat)
library(ggpubr)
library(moments)

#set project file's source as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

##loading the dataset (csv file)
##The dataset has data about restaurant's name, location,price, ratings
Zomato = read.csv("13_Zomato.csv")

##loading country codes dataset (xlsx file)
##this file has country codes for restaurants
Country_Codes = readxl::read_excel(path = "Country-Code.xlsx",sheet=1)

##rows and columns of dataset
##the dataset has 9551 rows and 21 columns
sprintf("Number of Rows:  %d ",dim(Zomato)[1])
sprintf("Number of Columns:  %d ",dim(Zomato)[2])

##names of columns
##understanding the names of columns in the dataset
names(Zomato)

##Checking head of loaded dataset in order to understand structure of data
##load the first 6 rows from both dataset files
head(Zomato)
head(Country_Codes)

##Merging dataframes to add the country name to the restaurant data
Zomato = merge(Zomato, Country_Codes, by.x='Country.Code', by.y='Country Code', all.x=TRUE)
sprintf("Dimensions After Merging:  %d * %d ",dim(Zomato)[1],dim(Zomato)[2])

##data cleaning

##Verifying that Restaurant.ID does not have duplicates in dataset
##if sum of IDs isn't equal to no. of rows then it means we have duplicates
##we don't find any duplicates

sum(table(Zomato$Restaurant.ID))

##Summary of dataset
summary(Zomato)


##Visualization of null Values present in dataset
##the graph shows how many null values are present in the dataset
##the graph is blank which means no null values
vis_miss(Zomato)

##preprocessing

##count is dataframe of country column in dataset
count <- as.data.frame(table(Zomato$Country))
count

##plot of no. of hotels in countries present in dataset
ggplot(count,aes(x=Var1,y=Freq)) + geom_bar(stat='identity', fill='blue') +
  labs(title='Country wise restaurant count') + 
  xlab('Country')+ylab('Restaurant Count') + coord_flip()

##Since Significant amount of data is for Indian resaturants 
##Rest of the analysis is going to be for Indian Restaurants
Zomato_india <- Zomato[Zomato$Country == "India",]
str(Zomato_india)

##Correcting factors since columns Country.Code , Restaurant.ID , 
##Price.range are identified as int and chr 
##we convert them to factor
Zomato_india$Country.Code <- factor(Zomato_india$Country.Code)
Zomato_india$Price.range <- factor(Zomato_india$Price.range)
Zomato_india$Restaurant.ID <- factor(Zomato_india$Restaurant.ID)
Zomato_india$Country <- factor(Zomato_india$Country)
sum(table(Zomato_india$Restaurant.ID))

##kurtosis function
abcd = Zomato_india$Votes
kurtosis(abcd)
##positive kurtosis indicates a fat-tailed distribution
plot(abcd)

##the long tail can be seen in this plot
ggplot(Zomato_india, aes(Votes), xlim=c(0,9000), ylim=c(0,900)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()


##city-wise restaurant count
city_count <- as.data.frame(table(Zomato_india$City))
city_count
ggplot(subset(city_count, Freq>0),aes(x=Var1,y=Freq)) + 
  geom_bar(stat='identity', fill='blue') +
  labs(title='City wise restaurant count') + 
  xlab('City')+ylab('Restaurant Count') +  coord_flip()


##price range distribution
##price ranges are: 1 is 0-500
##2 is 500-1000
##3 is 1000-1500
##4 is 1500+
##max. no. of restaurants is in price range one i.e. 0-500
ggplot(data = Zomato_india, aes(factor(Price.range), fill=factor(Price.range))) + geom_bar() + 
  ggtitle("Price Range Distribution") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Price Range') + ylab('Number of Restaurants') +
  scale_fill_viridis_d() +
  guides(fill=guide_legend(title="Price Catagory Legend"))

##average cost for two distribution
##the average cost for two is mainly between 0-2000
#The highest is 8000
ggplot(data = Zomato_india, aes(x=Average.Cost.for.two)) + geom_histogram(bins=75)


##plot of cost for two v/s aggregate rating
ggplot(data = Zomato_india,aes(x=Average.Cost.for.two, y=Aggregate.rating,color=Rating.color,size=Average.Cost.for.two)) + 
  geom_point(alpha=0.5)
##not sufficient to draw conclusive data


##plot of votes v/s rating
ggplot(data = Zomato_india, aes(x=Aggregate.rating, y = Votes, color=Rating.color, alpha =0.75)) + 
  geom_point(show.legend=T)
##no. of votes increases as rating increases

#boxplot using facets
qplot(Has.Table.booking,Average.Cost.for.two,data=Zomato_india, facets=~Price.range, main="Table Booking v/s Price range") + 
  geom_boxplot(aes(fill = Has.Table.booking))

##Analysis of some FAQs asked by customers

##Yes/No Stats about Indian Restaurants
p1 <- ggplot(data = Zomato_india) + geom_bar(aes(x=factor(Has.Online.delivery)), fill=c('red','blue')) + 
  xlab('Has Online Delivery') +ylab('No of Restaurants') 
##print(p1)

p2 <- ggplot(data = Zomato_india) + geom_bar(aes(x=factor(Has.Table.booking)),fill=c('red','blue')) + 
  xlab('Has Table Booking') + ylab('No of Restaurants') 
##print(p2)

p3 <- ggplot(data = Zomato_india) + geom_bar(aes(x=factor(Is.delivering.now)),fill=c('red','blue')) + 
  xlab('Is Delivering Now') + ylab('No of Restaurants') 
##print(p3)

p4 <- ggplot(data = Zomato_india) + geom_bar(aes(x=factor(Switch.to.order.menu)),fill=c('red')) + 
  xlab('Switch to Order menu') + ylab('No of Restaurants') 
##print(p4)

gridExtra::grid.arrange(p1,p2,p3,p4,ncol=2,nrow=2,top="Quick Yes/No Stats About Indian Restaurants")

##6000 restaurants don't have online delivery
##less than 2000 restaurants don't have table booking
##almost none of the restaurants are delivering now
##none of the restaurants have switch to order menu


##correlation analysis

##pearson method is used


##cor says that cor() can be applied to 'numeric vector, matrix or data frame'
##cor.test requires 'numeric vectors of data values'


#correlation b/w votes and rating
cor.test(Zomato_india$Aggregate.rating, Zomato_india$Votes, method= "pearson")
##p-value is less than 5% which shows that the correlation is significant
##the correlation co-efficient is 0.28  which shows +ve correlation


##correlation b/w ratings and average cost for two
cor(Zomato_india$Aggregate.rating, Zomato_india$Average.Cost.for.two, method = "pearson")
##the correlation co-efficient is 0.34 which shows +ve correlation
cor(Zomato_india$Average.Cost.for.two, Zomato_india$Votes, method= "pearson")
##the correlation co-efficient is 0.28 which shows +ve correlation


library(ggpubr);
ggscatter(Zomato_india, x="Average.Cost.for.two", y="Votes", combine = FALSE, merge=FALSE,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Average.Cost.for.two", ylab = "Votes")



##plotting the quantile quantile plots
##exponential curve indicates data is skewed
ggqqplot(Zomato_india$Votes, ylab="Votes")
##exponential curve indicates data is skewed
ggqqplot(Zomato_india$Average.Cost.for.two, ylab = "Average cost for two")


##Regression analysis
x <- Zomato_india$Average.Cost.for.two
y <- Zomato_india$Votes

##we want to find relation b/w x and y variables
relation<- lm(x~y)
print(relation)

plot(x,y,col = "blue",main = "Rating v/s Cost for 2",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Rating",ylab = "Cost for 2")
##the intercept is 569.5672 and y value is 0.3921
##regression line equation: 0.3921 = Mx + 569.5672



##prediction
x
a <- data.frame(x=mean(x))
a
result<- predict(relation,a)
print(result)
##rating will be 579.3701 for a hotel with average cost for two as 1000 rupees



