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
