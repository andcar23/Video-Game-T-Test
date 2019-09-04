#Andrew Carroll
#March 28, 2019

#Loads dataset into R
vg = read.csv(file.choose())
View(vg)
library(summarytools)
library(plyr)
library(ggplot2)
library(lsr)

#Subset Shooter and Action genres into a new object
vg2 <- subset(vg, Genre == "Action" | Genre == "Shooter", select = c(Name, Genre, Global_Sales))
View(vg2)

#removes unused factor levels from Genre
levels(vg2$Genre)
vg2$Genre <-factor(vg2$Genre)
levels(vg2$Genre)

#Creates objects for the Genre and Global_Sales variables
Genre <- vg2$Genre
Sales <- vg2$Global_Sales



#Descriptive Statistics for IV & DV
IVtable = count(vg2$Genre)
IVtable

DVtable = descr(vg2$Global_Sales)
DVtable

#t-test
t.test(Sales ~ Genre, conf.level = 0.95)

#Effect Size test

cohensD(Sales ~ Genre)

#Making table for ggplot's barplot
shooter = subset(vg2, Genre == "Shooter")
action = subset(vg2, Genre == "Action")
sum(shooter$Global_Sales)
sum(action$Global_Sales)

VG_table = data.frame( Genre = factor(c("Shooter", "Action"), levels = c("Shooter", "Action")),
                               Sales = c(1037.37, 1751.18))
VG_table

#Making bar graph that shows the number of sales globally between Shooter and Action Games
theme_set(theme_gray(base_size = 18))
Barplot <- ggplot(data = VG_table, aes(x = Genre, y = Sales, fill = VG_table )) + geom_bar(colour = "black", fill ="#DD8888", stat="identity")

#Add labels to Barplot since it was not working in first function
Barplot + ggtitle("Number of Global Game Sales by Genre") +
  xlab("Genre") + ylab("Number of Sales in Millions")

#Standard deviation for shooter and action games
sd(shooter$Global_Sales)
sd(action$Global_Sales)
