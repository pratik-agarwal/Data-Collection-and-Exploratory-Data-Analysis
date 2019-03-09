#Aman Bhayana - amanbhay
#Pratik Agarwal - pagarwal

# ----------------- Problem 1--------------------------
sales1<-c(12,14,16,29,30,45,19,20,16, 19, 34, 20) # combining the numbers in a lis using c() function and assigning it to a symbol called sales1
sales2<-rpois(12,34)  # random numbers, Poisson distribution, mean at 34, 12 numbers
par(bg="cornsilk")
# ploting the sales 1 data where range on y axis is between 0 and 100.
plot(sales1, col="blue", type="o", ylim=c(0,100), xlab="Month", ylab="Sales" )
title(main="Sales by Month")
lines(sales2, type="o", pch=22, lty=2, col="red") # lines are used to show sales2 without opening a new window
grid(nx=NA, ny=NULL)
legend("topright", inset=.05, c("Sales1","Sales2"), fill=c("blue","red"), horiz=TRUE) # For legend

#--------------------Problem 2--------------------------

sales<-read.table(file.choose(), header=T) # Reading data ffrom the file chosen from the file browser. 
sales  # to verify that data has been read
barplot(as.matrix(sales), main="Sales Data", ylab= "Total",beside=T, col=rainbow(5)) # Plotting barplot with matrix

#--------------------Problem 3--------------------------
fn<-boxplot(sales,col=c("orange","green"))$stats

text(1.45, fn[3,2], paste("Median =", fn[3,2]), adj=0, cex=.7)
text(0.45, fn[3,1],paste("Median =", fn[3,1]), adj=0, cex=.7)
grid(nx=NA, ny=NULL)

#--------------------Problem 4--------------------------
fb1<-read.csv(file.choose()) # Reading the csv file by choosing the fil from the file explorer
aapl1<-read.csv(file.choose()) # Reading the csv file by choosing the fil from the file explorer. There can be an alternate way for reading csv such as providing the url where the data is available.
par(bg="cornsilk")
#Plotting the Close column from the file read into aapl1 symbool. Not sure why are we using this when we are creating a histogram in the last line
plot(aapl1$Adj.Close, col="blue", type="o", ylim=c(100,220), xlab="Days", ylab="Price" ) 
lines(fb1$Adj.Close, type="o", pch=22, lty=2, col="red")
legend("topright", inset=.05, c("Apple","Facebook"), fill=c("blue","red"), horiz=TRUE)
p1<-hist(aapl1$Adj.Close, col=rainbow(8))
#--------------------Problem 5--------------------------

data() # Listing all the datasets available
attach(mpg)
head(mpg)
summary(mpg)
#after analysis remove the data from the memory
detach(mpg)


library (help=datasets)
library(datasets)
head(uspop)
plot(uspop) # Plotting the uspop dataset

#--------------------Problem 6---------------------------

library("ggmap")
library("maptools")
library(maps)
#Use the Google Cloud API Key
register_google(key = 'AIzaSyBChIhz8FzwkvhgiJEC4fFqgMuXpMEofAM') 
visited <- c("SFO", "Chennai", "London", "Melbourne", "Lima,Peru", "Johannesbury, SA")

ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(visit.x  ,visit.y, col="red", pch=36)

#---------------------Problem 7---------------------------
attach(mtcars)
mtcars
# Return the first 6 rows of mtcars dataset
head(mtcars)
# Plot the colums of mtcars by specifying their indices in the combine function. 
plot(mtcars[c(1,3,4,5,6)], main="MTCARS Data")
plot(mtcars[c(1,3,4,6)], main="MTCARS Data")
plot(mtcars[c(1,3,4,6)], col=rainbow(5),main="MTCARS Data")

#---------------------Problem 8---------------------------

library(ggplot2)
ggplot(mtcars, aes(x=mpg, y=disp)) + geom_point()

