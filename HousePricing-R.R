#import dataset
df<- read.csv("House_Price.csv", header=TRUE)

#view data
head(df)

#get structure of data
str(df)

#univariate analysi
summary(df)

#plot histogram
hist(df$crime_rate) #did not give any major info

pairs(~price + crime_rate + n_hot_rooms + rainfall, data = df) 
#shows n_hot_rooms and rainfal has outliers
#crime rate has a different relationship wit price, hence needs to be modified 
  #to get a linear relationship with price

#get barplot of the 3 categorical variables
barplot(table(df$airport)) #nothing suspicious
barplot(table(df$waterbody)) #nothing suspicious
barplot(table(df$bus_ter)) #only 1 value hence not useful in the data set

#observations
#1: n_hot_rooms has outliers
#2: hos_beds has missing values
#3: bus terminal is useless value
#4: crime rate has some functional relationship with price

#Outlier treatment
quantile(df$n_hot_rooms,0.99)
uv = quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>uv] <- uv 

#checking changes
summary(df$n_hot_rooms)

lv = 0.3 * quantile(df$rainfall, 0.01)
df$rainfall[df$rainfall < lv] <- lv

#checking changes
summary(df$rainfall)

#handling missing values with mean
mean(df$n_hos_beds,na.rm = TRUE)

#positions with na values
which(is.na(df$n_hos_beds))

df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds,na.rm = TRUE)

#checking changes
summary(df$n_hos_beds)

#plot pair plot of price and crime data
pairs(~price+crime_rate, data=df)
plot(df$price, df$crime_rate)

#transform to logrithmic format
df$crime_rate = log(1+df$crime_rate)

#get new variable to represent dist 1,2,3,4
df$avg_dist = (df$dist1 + df$dist2 + df$dist3 + df$dist4)/4

#view the new data
View(df)

#delete dist1,2,3,4
df2 <- df[ ,-7:-10] 
df <- df2   #reassign back to df
rm(df2)     #delete df2

#remove unnecessary column bus_terminal
df <- df[ , -14]
