#ESH Data Task - Sujin Oh
#Task: Investigate Georgia
#Investigate all of Georgia's districts and only districts
#Do so by aggregating by 'applicant_ben' (billed entity number)
library(dplyr)
library(ggplot2)

data <- read.csv("~/Desktop/Sujin's folder/data.csv", stringsAsFactors=TRUE) #renamed data set as 'data'
#View(data)

dim(data) #n=26068; m=29
names(data)
summary(data) #applicant_type factors: District = 25082, School = 986
str(data$connect_type) #25 Connection Types

#Question 1:
#For the state of Georgia's districts, what is the nature of the distribution 
#of total_cost by connection type at common bandwidths (100, 1000)? 
#***You'll have to aggregate bandwidth and total cost***
bandwidth100 <- filter(data, applicant_type == "District", bandwidth_in_mbps == 100)
bandwidth1k <- filter(data, applicant_type == "District", bandwidth_in_mbps == 1000)


data100 <- bandwidth100 %>% 
  group_by(connect_type) %>%
  summarise(min_cost = min(total_cost, na.rm = TRUE), 
            mean_cost = mean(total_cost, na.rm = TRUE),
            median_cost = median(total_cost, na.rm = TRUE),
            max_cost = max(total_cost, na.rm = TRUE),
            aggregate_total_cost = sum(total_cost, na.rm = TRUE), 
            n = n()) %>%
  arrange(desc(n))

head(data100)
#18 connection types for broadband width 100


data1k <- bandwidth1k %>% 
  group_by(connect_type) %>%
  summarise(min_cost = min(total_cost, na.rm = TRUE), 
            mean_cost = mean(total_cost, na.rm = TRUE),
            median_cost = median(total_cost, na.rm = TRUE),
            max_cost = max(total_cost, na.rm = TRUE),
            aggregate_total_cost = sum(total_cost, na.rm = TRUE), 
            n = n()) %>%
  arrange(desc(n))

head(data1k)
#17 connection types for broadband width 1000
  
#Visualizations
#par(mfrow=c(2,2))

#For bandwidth = 100
a <- ggplot(data100[1:4,], aes(factor(connect_type), aggregate_total_cost))
a + geom_bar(stat="identity") + ggtitle("Bandwidth = 100 mbps")

data100[1:4,]
#Explanation: 
#Based on the distribution of the top 4 connection types on the plot for bandwidth = 100, 
#we find that Ethernet has the highest aggregate total cost, followed by Lit Fiber, 
#then by Standalone Internet Access, and lastly by Cable Modem. Furthermore, based on the summary statistics of
#these four connections, there is a high positive skew of total cost for alll for connection types. One possible
#interpretation may be that that there are some exorbitantly highly costs (outliers) that districts are currently paying
#for these connections that skew the data despite relatively low median costs. Another can be that there are some 
#districts that are significantly larger than the other schools districts, explaining for the much higher cost.
#Nevertheless, to would be wise to locate the districts paying high costs to determine the reason.

#For bandwidth = 1000
b <- ggplot(data1k[1:4,], aes(factor(connect_type), aggregate_total_cost))
b + geom_bar(stat="identity") + ggtitle("Bandwidth = 1000 mbps")

data1k[1:4,]
#Explanation: 
#Based on the distribution of the top 4 connection types on the plot for bandwidth = 1000, 
#we find that Lit Fiber has the highest aggregate total cost, followed by Ethernet, 
#then by Dark Fiber Service, and lastly by Standalone Internt Access. Furthermore, based on the summary statistics of 
#these four connections there is a high positive skew of total cost for alll four connection types similar to 
#the distribution of costs for bandwidth = 100. Based on average costs, for both bandwidth levels, it seems that 
#Ethernet is most expensive on average, albeit widely implemented by the districts.




#Question 2:
#For the state of Georgia's districts, is there a relationship between 
#FRL and total_cost, agnostic for connection type and bandwidth (in kbps), 
#for each locale? You'll have to aggregate total cost and connect locale.
str(data$locale)
summary(data$locale)

#omit obs with no locale placement
data$locale[data$locale==""] <- NA
data <- na.omit(data)


#transforming bandwidth mbps to kbps
data1 <- filter(data, applicant_type == "District")
data1$bandwidth_in_mbps <- data1$bandwidth_in_mbps * 1024
data1 <- rename(data1, bandwidth_in_kbps = bandwidth_in_mbps)

#regression model
m0 <- lm(total_cost ~ frl_pct, data1) #reduced model
m1 <- lm(total_cost ~ frl_pct + locale, data1) #full model
summary(m0)
#plot(m0) #checking model assumptions
summary(m1)
#plot(m1) #checking model assumptions

#Notes:
#Recall, I omitted the observations with no locale specification. Hence, intercept is based on locale "Rural".
#Fairly low R-squared value. Questionable that model assumptions are met. 
#Statistical significant of frl_pct variable and locale at the "Suburban" and "Urban" level.
#This potentially means that for every one unit increase in frl_pct there's a 86120 unit increase in total cost 
#from local "Rural" total cost holding all else constant. For every one unit (district) increase in the "Suburban" 
#locale, there is also a 36524 unit increase from locale "Rural"'s total cost, holding all else constant to explain 
#locale "Suburban" total cost. For every one unit (district) increase in the "Urban" 
#locale, there is also a 85895 unit increase from locale "Rural"'s total cost, holding all else constant to locale
#"Urban"'s total cost.


anova(m0,m1) #partial F-test using anova
#We find high F-stat: 157.92 and statistically significant p-value.
#Results show that we can reject H0 (that locale does not have any affect on total cost, B2=0) 
#at the 5% significance level. Locale provides statistically significant information on the total cost
#of internet connection in addition to FRL, particularly in the "Suburban" and Urban" locale. 


