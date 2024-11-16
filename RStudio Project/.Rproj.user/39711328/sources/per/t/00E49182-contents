#insert data
marketing <- read.csv("marketing_campaign.csv",sep = "\t", header = T)

library(tidyverse)

options(warn=-1)
df=data.frame(marketing)

#checking the data
head(df)

#checking for the missing values
missing.values <- df %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels =(missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot = missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('#adcae6', 'red'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x ='Variable', y = "% of missing values")
percentage.plot

df.clean <- na.omit(df)

#feature selection
df.clean['Age'] <- 2024-df.clean$Year_Birth
df.clean <- df.clean %>% filter(Age <= 80)
df.clean['Child'] <- df.clean$Kidhome + df.clean$Teenhome
df.clean['Spending'] <- df.clean$MntWines + df.clean$MntFruits + df.clean$MntMeatProducts + df.clean$MntFishProducts +
  df.clean$MntSweetProducts + df.clean$MntGoldProds
df.clean['AcceptedPromotion'] <- df.clean$AcceptedCmp1 + df.clean$AcceptedCmp2 + df.clean$AcceptedCmp3 + 
  df.clean$AcceptedCmp4 + df.clean$AcceptedCmp5
df.clean['Edu'] <- ifelse(df.clean$Education %in% c("Basic", "2n Cycle"), "Undergraduate",
                          ifelse(df.clean$Education == "Graduation", "Graduate",
                                 ifelse(df.clean$Education %in% c("Master", "PhD"), "Postgraduate", "")))

df.clean['EducationLevel'] <- as.numeric(ifelse(df.clean$Edu == "Undergraduate",1,
                                                ifelse(df.clean$Edu == "Graduate", 2,
                                                       ifelse(df.clean$Edu == "Postgraduate", 3, 0))))

df.clean['MaritalStat'] <- ifelse(df.clean$Marital_Status == "Married" | df.clean$Marital_Status == "Together", "Partner",
                                  ifelse(df.clean$Marital_Status %in% c("Absurd", "Widow", "YOLO", "Divorced", "Single"), "Alone", ""))

df.clean['RelStatus'] <- ifelse(df.clean$MaritalStat == "Partner", 2, 1)

df.clean['Purchase'] <- df.clean$NumDealsPurchases + df.clean$NumWebPurchases + df.clean$NumCatalogPurchases + df.clean$NumStorePurchases

  
df.clean=df.clean[c(-1,-2,-3,-6,-10,-11,-12,-13,-14,-15.-16,-17,-18,-19,-21,-22,-23,-24,-25)]

head(df.clean)
df.clean=df.clean[c(-1,-4,-6, -7,-16,-18)]

#graph
hist(df.clean$Income, col = rainbow(length(unique(df.clean$Income))), xlim = c(0, 250000), ylim = c(0,1200))
hist(df.clean$Purchase, col = rainbow(length(unique(df.clean$Purchase))))
hist(df.clean$Spending, col = rainbow(length(unique(df.clean$Spending))))
hist(df.clean$Age, col = rainbow(length(unique(df.clean$Age))))

avg_spending <- aggregate(df.clean$Spending, by = list(Age = df.clean$Age), FUN = mean)

barplot(avg_spending$x, 
        names.arg = avg_spending$Age, 
        col = rainbow(length(unique(df.clean$Age))), 
        cex.axis = 0.6, 
        cex.names = 0.6,
        las = 2,
        xlab = "Age", 
        ylab = "Average Spending", 
        main = "Correlation Between Customers' Age and Average Spendings")

boxplot(df.clean$Income~df.clean$Education, col = rainbow(length(unique(df.clean$Education))), 
        cex.axis = 0.7, 
        xlab = "Education", 
        ylab = "Income", 
        main = "Correlation Between Customers' Education and Income", 
        ylim = c(0,150000))

avg_purchase <- aggregate(df.clean$Income, by = list(Purchase = df.clean$Purchase), FUN = mean)

barplot(avg_purchase$x, 
        names.arg = avg_purchase$Purchase, 
        col = rainbow(length(unique(df.clean$Purchase))), 
        cex.axis = 0.6,
        cex.names = 0.6,
        las = 2,
        xlab = "Purchase", 
        ylab = "Income", 
        main = "Correlation Between Customers' Income and Purchase")

boxplot(df.clean$Purchase~df.clean$AcceptedPromotion, col = rainbow(length(unique(df.clean$AcceptedPromotion))))

library(corrplot)

#Getting correlation matrix 
cust_cor <- cor(df.clean[, !(names(df.clean) %in% c("Z_CostContact", "Z_Revenue"))])

# Visualize correlation matrix
corrplot(cust_cor, method = "color", addCoef.col = "white")

#KMeans
library(factoextra)

#sum(is.na(df.clean))  # Mengecek nilai NA
#sum(is.nan(as.matrix(df.clean)))  # Mengecek nilai NaN
#sum(is.infinite(as.matrix(df.clean)))  # Mengecek nilai Inf


fviz_nbclust(df.clean,kmeans,method="wss")+geom_vline(xintercept=3,linetype=2)

set.seed(123)
km.res <- kmeans(df.clean, 3, nstart = 10)

print(km.res$centers)
print(km.res$size)
print(km.res$betweenss/km.res$totss)
  
df.clean <- df.clean[, sapply(df.clean, function(x) var(x, na.rm = TRUE) != 0)]

fviz_cluster(km.res, df.clean, geom = "point", ellipse.type = "norm", repel = TRUE)

#ploting  #income , spending, purchase
df.clean['cluster']=as.factor(km.res$cluster)
head(df.clean)
attach(df.clean)

# Plot for Income
incomeplot = ggplot(df.clean, aes(x=cluster, y=Income, fill=cluster)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=TRUE) +
  ggtitle("Clusters' Boxplot of Income") +
  labs(x = "Cluster", y = "Income") +
  ylim(0, 200000)


# Plot for Spending
spenplot = ggplot(df.clean, aes(x=cluster, y=Spending, fill=cluster)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=T) +
  ggtitle("Clusters' Boxplot of Spending") +
  labs(x = "Cluster", y = "Spending")

# Plot for Purchase
purchaseplot = ggplot(df.clean, aes(x=cluster, y=Purchase, fill=cluster)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=T) +
  ggtitle("Clusters' Boxplot of Purchase") +
  labs(x = "Cluster", y = "Purchase")

# Display the plots
incomeplot
spenplot
purchaseplot