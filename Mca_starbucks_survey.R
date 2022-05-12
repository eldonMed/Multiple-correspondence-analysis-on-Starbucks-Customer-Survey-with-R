#1
#import data
data <- read.csv("starbucks customer survey.csv")
class(data)

# change col type to factor
for(i in 1:ncol(data)) { 
  data[ , i] <- as.factor(data[ , i])
}
data$'X2..Your.Age'<-factor(data$'X2..Your.Age',levels = c('Below 20','From 20 to 29','From 30 to 39','40 and above'))
data$"X4..What.is.your.annual.income."<-factor(data$"X4..What.is.your.annual.income.",levels = c("Less than RM25,000","RM25,000 - RM50,000","RM50,000 - RM100,000","RM100,000 - RM150,000","More than RM150,000"))
data$"X5..How.often.do.you.visit.Starbucks."<-factor(data$"X5..How.often.do.you.visit.Starbucks.",levels = c("Daily","Weekly","Monthly","Rarely","Never"))
data$"X7..How.much.time.do.you.normally..spend.during.your.visit."<-factor(data$"X7..How.much.time.do.you.normally..spend.during.your.visit.",levels = c("Below 30 minutes","Between 30 minutes to 1 hour","Between 1 hour to 2 hours","Between 2 hours to 3 hours","More than 3 hours"))
data$"X8..The.nearest.Starbucks.s.outlet.to.you.is...."<-factor(data$"X8..The.nearest.Starbucks.s.outlet.to.you.is....",levels = c("within 1km","1km - 3km","more than 3km"))

#check col types ,sapply(data, class)
str(data)

#2,3 bar plots
library(ggplot2)
ggplot(data) +
  aes(x = X1..Your.Gender) +
  geom_bar()
ggplot(data) +
  aes(x = X2..Your.Age ) +
  geom_bar()
ggplot(data) +
  aes(x = X3..Are.you.currently.....) +
  geom_bar()
ggplot(data) +
  aes(x = X5..How.often.do.you.visit.Starbucks.) +
  geom_bar()
ggplot(data) +
  aes(x = X7..How.much.time.do.you.normally..spend.during.your.visit.) +
  geom_bar()
ggplot(data) +
  aes(x = X8..The.nearest.Starbucks.s.outlet.to.you.is....) +
  geom_bar()

ggplot(data) +
  aes(x = X4..What.is.your.annual.income.) +
  geom_bar()
#3
barplot(table(data$"X1..Your.Gender"))

#4 disjonctif
library(ade4)
datadis <-acm.disjonctif(data)
#rename columns
names(datadis)[1] <- 'Female'
names(datadis)[2] <- 'Male'

names(datadis)[3] <- 'Below 20'
names(datadis)[4] <- 'From 20 to 29'
names(datadis)[5] <- 'From 30 to 39'
names(datadis)[6] <- '40 and above'

names(datadis)[7] <- 'Employed'
names(datadis)[8] <- 'Housewife'
names(datadis)[9] <- 'Self-Employed'
names(datadis)[10] <- 'Student'

names(datadis)[11] <- 'Less than 25k'
names(datadis)[12] <- '25K-50K'
names(datadis)[13] <- '50K-100K'
names(datadis)[14] <- '100K-150K'
names(datadis)[15] <- 'More than 150K'

names(datadis)[16] <- 'Daily'
names(datadis)[17] <- 'weekly'
names(datadis)[18] <- 'Monthly'
names(datadis)[19] <- 'Rarely'
names(datadis)[20] <- 'Never'

names(datadis)[21] <- 'Below 30mins'
names(datadis)[22] <- '30mins to 1h'
names(datadis)[23] <- '1h to 2h'
names(datadis)[24] <- '2h to 3h'
names(datadis)[25] <- 'More than 3h'

names(datadis)[26] <- 'Within 1km'
names(datadis)[27] <- '1km to 3km'
names(datadis)[28] <- 'More than 3km'

#dataframe to contingency
as.data.frame.matrix(data[, c( 3, 4)])
library(dplyr)
my_table_0 <- table(data$X3..Are.you.currently.....,data$X4..What.is.your.annual.income.)
my_table_1 <- as.data.frame.matrix(my_table_0)

#AFC
library ("FactoMineR")
res.ca <- CA (my_table_1,)

# Statistiques de khi2
chi2 <- 1944.456
df <- (nrow (my_table_1) - 1) * (ncol (my_table_1) - 1)
pval <- pchisq (chi2, df = df, lower.tail = FALSE)
pval

library ("factoextra")
eig.val <- get_eigenvalue (res.ca)
eig.val

fviz_screeplot (res.ca) +
  geom_hline (yintercept = 33.33, linetype = 2, color = "red")

fviz_ca_biplot (res.ca, repel = TRUE)

