library(tidyverse)
library(gridExtra)
library(class)
library(usmap)
library(ggplot2)
library(foreign)
library(haven)
library(ggplot2)
library(foreign)
library(ggplot2)
library(GGally)
library(haven)
library(magrittr)
library(data.table)
library(dplyr)
library(plyr)
library(dplyr)
library(factoextra)
library(ggplot2)
library(ggmap)
library(nycflights13)
library(tidyverse)
library(datasets)
library(readxl)
library(tidyverse) 
library(magrittr)
library(DataExplorer)
library(maps)
library(plotly)
library(DT)
library(tidytext)
library(plyr)
library(gridExtra)
library(factoextra)
library(GGally)
library(readxl)
library(tidyverse) 
library(magrittr)
library(DataExplorer)
library(maps)
library(plotly)
library(DT)
library(tidytext)
library(gridExtra)
library(factoextra)
library(GGally)
library(gridExtra)
library(graphics)
library(mice)
set.seed(2021)

Beers4=read.csv("/Users/owner/Desktop/homework/unit8/Beers 4.csv")

Breweries = read.csv("/Users/owner/Desktop/homework/unit8/Breweries.csv")

df1 = data.frame(table(Breweries$State))
colnames(df1) = c("State","Breweries")
df1 = df1[order(-df1$Breweries),]
view(df1)
row.names(df1) = NULL
view(df1)
df1


##--------------------

merged = merge(x = Beers4, y = Breweries, by.x = "Brewery_id", by.y = "Brew_ID", all=TRUE)
merged = merge(x = Beers4, y = Breweries, by.x = "Brewery_id", by.y = "Brew_ID", all.x=TRUE)
view(merged)
merged = merged %>% rename(Beer = Name.x,Brewery = Name.y)
head(merged,6)
tail(merged,6)

#---------------------------------------------------------
### 3. Address the missing values in each column.


#visualize the missing data
library(mice)

plot_missing(merged)
#Use mice package to predict missing values
#mice_mod <- mice(merged, method='rf')
#mice_complete <- complete(mice_mod)

sum(is.na(merged))
which(is.na(merged)) 

for(i in 1:ncol(merged)){
  merged[is.na(merged[,i]), i] <- mean(merged[,i], na.rm = TRUE)
}

plot_missing(merged)

##ABV and IBU Columns contain missing values. ABV column contains 62 missing values and IBU column contains 1005 missing values. Since ABV contains smaller number of missing values, we can use state level medians to replace those values. IBU contains lot of missing values, Because of that we use a predictive regression model for predict IBU value based on ABV.**


#--------------------------------
#Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.

summary(merged)


#--------------------------------------

#  Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
df2=data.frame(merged$State,merged$ABV,merged$IBU)
colnames(df2)=c("State","ABV","IBU")
view(df2)
summarise(df2,max=max(ABV))
row.names(df2) = NULL
df2 = df2[order(-df2$ABV),]
head(df2)[1,]

summarise(df2,max=max(IBU))
df2 = df2[order(-df2$IBU),]
head(df2)[1,]


#ABV.max = aggregate(merged$ABV~merged$State, FUN = max)
#ABV.max[order(-ABV.max$ABV),][1,]

##Colorado has the Maximum ABV at 0.128.**\
##Oregon has the Maximum IBU at 138.**

#--------------------------------

#Null hypothesis: the ABV is normally distributed?
shapiro.test(merged$ABV)

#data:  merged$ABV
#W = 0.93727, p-value < 2.2e-16


ggplot(merged, aes(x = ABV)) + 
  geom_histogram(color="blue", fill="skyblue") +
  theme_bw() +
  ggtitle("Histogram for ABV")

summary(merged$ABV)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00100 0.05000 0.05700 0.05977 0.06700 0.12800 
#--------------------------

ggplot(df2, aes(x = ABV, y = IBU)) + 
  geom_point(color = "blue")+
  geom_smooth(method = "lm",se = F, color = "red")+
  ggtitle("Scatterplot for ABV vs IBU") + 
  theme_bw()
#----------------------------