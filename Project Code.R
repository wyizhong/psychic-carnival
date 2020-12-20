setwd("~/Desktop/MET CS544/Final Project")
library(foreign)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotrix)
library(sampling)
# Install required packages
par(mfrow = c(1,1))

data = read.csv("SuicideRates.csv") # Import dataset of suicide rate
data = data[data$year <= "2015", ] # Remove the data of year 2016 (data is incomplete in 2016) to get rid of possible outliers
names(data)[7] = "rate" # Change the name of a variable in order to make it easier to use
names(data)[1] = "region"


# List all countries that appear in the dataset
country = as.data.frame(unique(data$region))

# Summary of the total number of people who committed suicide by year. 
# Numeric data
suicide_sum_year = aggregate(data$suicides_no, by = list(data$year), FUN = sum)
colnames(suicide_sum_year) = c("Year", "Sum")
plot(suicide_sum_year, type = "l", col = "red", bty = "l") # Plot of the summary above
par(new = TRUE)
data$gdp = as.numeric(gsub(",","", data$gdp_for_year....))
gdp_year = unique(data[c("year", "gdp")])
gdp_year_sum = aggregate(gdp_year$gdp, by = list(gdp_year$year), FUN = sum)
colnames(gdp_year_sum) = c("Year", "GDP")
plot(gdp_year_sum, type = "l", axes = FALSE, xlab = "", ylab = "", col = "darkblue")
legend("bottomright", c("Numbers of suicide", "GDP"), cex = 0.7, ncol = 1, fill = c("red", "darkblue"))
par(new = TRUE)
gdppc_year = unique(data[c("year", "gdp_per_capita....")])
gdppc_year_sum = aggregate(gdppc_year$gdp_per_capita...., by = list(gdppc_year$year), FUN = sum)
colnames(gdppc_year_sum) = c("Year", "GDPPC")
plot(gdppc_year_sum, type = "l", axes = FALSE, xlab = "", ylab = "", col = "darkgreen")
legend("bottomright", c("Numbers of suicide", "GDP", "GDP per capita"), cex = 0.7, ncol = 1, fill = c("red", "darkblue", "darkgreen"))
# General findings
# The total number of people that committed suicide went up from about 1988 and stayed relatively stable from around 1995 to 2013.
# After 2013 and probably until now, the number is experiencing a major decrease. 

# Categorical data
male = sum(subset(data$suicides_no, data$sex == "male"))
female = sum(subset(data$suicides_no, data$sex == "female"))
gender_sum = c(male, female)
piepercent = round(100 * gender_sum / sum(gender_sum), 1)
pie3D(gender_sum, labels = piepercent ,col = c("blue", "violet"), explode = 0.2, theta = 1) 
legend("topright", c("Female","Male"), cex = 0.8,ncol=1,fill = c("violet", "blue"), bty = "n")

NO.Of_Suicidies = data$suicides_no
Year = data$year
Sex = factor(data$sex)
ggplot(data, aes(x=NO.Of_Suicidies, y=Year, color=Sex))+geom_point(shape=19)+labs(x="No.of Suicides",y="year",title="No. of Suicide In Each Year")

# Multivariable data
data1985 = data[which(data$year == 1985), ]
data1995 = data[which(data$year == 1995), ]
data2005 = data[which(data$year == 2005), ]
data2015 = data[which(data$year == 2015), ]
datasub = rbind(data1985, data1995, data2005, data2015)
p1 = ggplot(datasub, aes(x = datasub$year, y = datasub$rate, fill=datasub$generation)) + geom_bar(position = "dodge", stat = "identity")
p1 + xlab("Year") + ylab("Sum") + labs(fill = "Generation")
p2 = ggplot(datasub, aes(x = datasub$year, y = datasub$rate, fill = datasub$age)) + geom_bar(position = "dodge", stat = "identity")
p2 + xlab("Year") + ylab("Sum") + labs(fill = "Ages")

# Central Limit Theorem
samples = 1e4
sample.size = 30
data_Mil_1 = subset(data, (generation == "Millenials")&(year < 1996))
data_Mil_2 = subset(data, (generation == "Millenials")&(year >= 1996)&(year < 2001))
data_Mil_3 = subset(data, (generation == "Millenials")&(year >= 2001)&(year < 2006))
data_Mil_4 = subset(data, (generation == "Millenials")&(year >= 2006)&(year < 2011))
data_Mil_5 = subset(data, (generation == "Millenials")&(year >= 2011))
data_Mil = list(data_Mil_1, data_Mil_2, data_Mil_3, data_Mil_4, data_Mil_5)

year_interval = c("(1991-1995)", "(1996-2000)", "(2001-2005)", "(2006-2010)","(2011-2015)")
average_rate_gdp = matrix(nrow = 5, ncol = 2)
sd_rate_gdp = matrix(nrow = 5, ncol = 2)
for (j in 1:5){
  rate_gdp = cbind(data_Mil[[j]]$rate, data_Mil[[j]]$gdp_for_year....)
  N = dim(rate_gdp)[1]
  mean_rate_gdp = matrix(nrow = samples, ncol = 2)
  for (i in 1:samples){
    ord = sample(1:N, size = sample.size, replace = FALSE)
    mean_rate_gdp[i,] = apply(rate_gdp[ord,], 2, FUN = mean)
  }
  par(mfrow = c(1,2))
  hist(mean_rate_gdp[,1], col = hcl(0), xlab = "Mean Suicide Rate", prob = TRUE
       , main = paste("Distribution of Suicide Rate", year_interval[j]))
  hist(mean_rate_gdp[,2], col = hcl(0), xlab = "Mean GDP", prob = TRUE, main = 
         paste("Distribution of GDP", year_interval[j]))
  average_rate_gdp[j, ] = apply(mean_rate_gdp, 2, FUN = mean)
  sd_rate_gdp[j, ] = apply(mean_rate_gdp, 2, FUN = sd)
}
par(mfrow = c(1,1))
row.names(average_rate_gdp) = year_interval
colnames(average_rate_gdp) = c("Mean Suicide Rate", "Mean GDP for Year")
row.names(sd_rate_gdp) = year_interval
colnames(sd_rate_gdp) = c("SD of Suicide Rate", "SD of GDP for Year")

# Sampling Methods
# Random Sampling
summary(data$rate)
set.seed(233)
s1 = srswor(200, nrow(data))
s1[s1 != 0]
rows = (1:nrow(data))[s1!=0]
rows = rep(rows, s1[s1 != 0])
sample1 = data[rows, ]
summary(sample1$rate)

# Systematic Sampling
N = nrow(data)
n = 200
k = floor(N / n)
r = sample(k, 1)
s2 = seq(r, by = k, length = n)
sample2 <- data[s2, ]
summary(sample2$rate)

# Stratified Sampling
freq = table(data$generation)
freq
stsizes = 200 * freq / sum(freq)
st2 = strata(data, stratanames = c("generation"), size = stsizes, method = "srswor", description = TRUE)
sample3 = getdata(data, st2)
summary(sample3$rate)