#Descriptive Analysis
library(fBasics)


df <- read.csv("Sales_gl.csv")

result <- aggregate(cbind(Sales, Profit) ~ Sub.Category, data = df, FUN = sum)

# Print the result
print(result)


attach(Sales_gl)
#summary of sales variable
summary(Sales)
#Statistics of Sales , profit 
basicStats(data.frame(Sales,Profit))# Check the column names in the dataframe
names(df)


#mean of Sales
mean(Sales)

#median of Sales 
median(Sales)


#mode of  Sales 
mode <- function(Sales) {
  Sale <- unique(Sales)
  Sale[which.max(tabulate(match(Sales,Sale)))]
}
mode_value <- mode(Sales)
print(paste("Mode:", mode_value)) 
#Range of Sales
range(Sales)

#quantile of sales
quantile(Sales, c(0.25, 0.5, 0.75))

#Histogram Sales vs profit
hist(Sales,main = 'Histogram of sales',xlab ='Sales',ylab = 'profit',col='Green')
#Histogram Quantity
hist(Quantity,main = 'Histogram of Quantity',xlab ='quantity',ylab = 'frequency',col='Green', breaks = 20)

#Histogram Cycle 

#box plot for quantity 
boxplot(Quantity,main="Boxplot of quantity",xlab="quantiy",ylab="values")
quantile(Quantity,c(0.25, 0.5, 0.75))

#Comparing box plots

ggplot(Sales_gl,aes(x=Sales_gl$`Sub-Category`, y=Sales_gl$Profit))+
  geom_boxplot()+ labs(title= "box plot of quantity by catergory",
                             x= "category",
                             y= "profit")


#skewness 
skewness(`Cycle time`)


#Descriptive Analysis 

#Histogram for Cycle Time
hist(`Cycle time`,main = "Histogram Of Cycle Time",
     xlab="Cycle time",
     ylab="frequency",
     col = "green",
     border="black",
     breaks=20)

#To Determine the summary of the cycle time
summary(`Cycle time`)

#To Determine the Detail summary of the cycle time
basicStats(data.frame(`Cycle time`))

#To Determine the mean median of the cycle time
mean(`Cycle time`)

#To Determine the median of the cycle time
median(`Cycle time`)


# comparing the distribution of Sales  for each subcategory of products
ggplot(Sales_gl,aes(x=`Sub-Category`, y=Sales))+
  geom_boxplot(fill = "lightgreen")+ labs(title= "Sales Distribution by Sub-category",
                                          x= "Sub-category",
                                          y= "Sales")
basicStats(data.frame(`binders`))


# comparing the distribution of profits  for each subcategory of products
ggplot(Sales_gl,aes(x=`Sub-Category`,y=Profit))+
  geom_boxplot(fill="lightgreen")+labs(title= "Profit Distribution by Sub-category",
                                       x= "Sub-category",
                                       y= "Profit")


# comparing the distribution of Sales  for each Segment
ggplot(Sales_gl,aes(x=Segment,y=Sales))+
  geom_boxplot(fill="lightblue")+labs(title ="Sales Distribution by segment",
                                      x="Segment",
                                      y="Sales")


#comparing the distribution of profits  for each Segment

ggplot(Sales_gl,aes(x=Segment,y=Profit))+
  geom_boxplot(fill="lightblue")+labs(title = "Profit Distribution by Segement",
                                       x="Segment",
                                       y="profit")

mean(Profit)
median(Profit)


df <- read.csv("Sales_gl.csv")

result <- aggregate(cbind(Sales, Profit) ~ Sub.Category, data = df, FUN = sum)

# Print the result
print(result)


getwd()  # Check current working directory
list.files()  # List files in the current working directory


df <- read.csv("Sales_gl.csv")

result <- aggregate(cbind(Sales, Profit) ~ Sub.Category, data = df, FUN = median)

# Print the result
print(result)


#To read the .csv file
df <- read.csv("Sales_gl.csv")

#To  Determine the total sales and total profit of each segment
result <- aggregate(cbind(Sales, Profit) ~Segment, data = df, FUN = sum)

# Print the result
print(result)

#To read the .csv file
df <- read.csv("Sales_gl.csv")

#To  Determine the  median of sales and profit of each segment
result <- aggregate(cbind(Sales, Profit) ~Segment, data = df, FUN = median)

# Print the result
print(result)


