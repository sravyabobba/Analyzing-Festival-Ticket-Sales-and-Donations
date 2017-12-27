#Reading the CSV file
final <- read.csv("FinalProject.csv")
str(final)
summary(final)

###########################################################################################
# Answer 1 #
###########################################################################################

# Answer 1a(i)
nrow(subset(final, final$Total.Donations > 0)) #[1] 1574
nrow(subset(final, final$Total.Donations >= 100)) #[1] 979
nrow(subset(final, final$Total.Donations >= 500)) #[1] 422
nrow(subset(final, final$Total.Donations >= 1000)) #[1] 237

# Answer 1a(ii)
Afinal <- subset(final, select = c(35:46))

colSums(Afinal != 0)
# X2004.Donation X2005.Donation X2006.Donation X2007.Donation X2008.Donation X2009.Donation 
# 374            425            407            479            431            509 
# X2010.Donation X2011.Donation X2012.Donation X2013.Donation X2014.Donation X2015.Donation 
# 445            416            400            431            472            458

colSums(Afinal >= 100)
# X2004.Donation X2005.Donation X2006.Donation X2007.Donation X2008.Donation X2009.Donation 
# 182            223            202            231            221            201 
# X2010.Donation X2011.Donation X2012.Donation X2013.Donation X2014.Donation X2015.Donation 
# 199            213            227            244            307            257 

colSums(Afinal >= 500)
# X2004.Donation X2005.Donation X2006.Donation X2007.Donation X2008.Donation X2009.Donation 
# 34             41             38             39             40             37 
# X2010.Donation X2011.Donation X2012.Donation X2013.Donation X2014.Donation X2015.Donation 
# 30             32             41             68             87             64

colSums(Afinal >= 1000)
# X2004.Donation X2005.Donation X2006.Donation X2007.Donation X2008.Donation X2009.Donation 
# 19             25             21             19             22             18 
# X2010.Donation X2011.Donation X2012.Donation X2013.Donation X2014.Donation X2015.Donation 
# 14             20             26             49             59             46 


# Answer 1a(iii)
hist(final$Total.Donations, xlab = "Total donations", ylab = "Frequency")
ahist = subset(final, final$Total.Donations > 0)
hist(ahist$Total.Donations, xlab = "Total donations", ylab = "Frequency")
ahist = subset(final, final$Total.Donations > 0 & final$Total.Donations <= 10000)
hist(ahist$Total.Donations, xlab = "Total donations", ylab = "Frequency")

hist(final$X2014.Donation, xlab = "2014 donations", ylab = "Frequency")
ahist2014 = subset(final, final$X2014.Donation > 0)
hist(ahist2014$X2014.Donation, xlab = "2014 donations", ylab = "Frequency")

hist(final$X2015.Donation, xlab = "2015 donations", ylab = "Frequency")
ahist2015 = subset(final, final$X2015.Donation > 0)
hist(ahist2015$X2015.Donation, xlab = "2015 donations", ylab = "Frequency")


# Answer 1a(iv)
ascatter = subset(final, final$X2014.Donation > 0 & final$X2015.Donation > 0)
plot(ascatter$X2014.Donation, ascatter$X2015.Donation, xlab ="2014 Donations" , ylab ="2015 Donations")
ascatter = subset(final, final$X2014.Donation > 0 & final$X2014.Donation <= 5000 & 
                         final$X2015.Donation > 0 & final$X2015.Donation <= 5000)
plot(ascatter$X2014.Donation, ascatter$X2015.Donation, xlab ="2014 Donations" , ylab ="2015 Donations")

# Answer 1b(i)

nrow(subset(final, final$Total.Tickets > 0)) #[1] 29845

# Answer 1b(ii)
Bfinal <- subset(final, select = c(7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33))

colSums(Bfinal != 0)
# X2002.Qty X2003.Qty X2004.Qty X2005.Qty X2006.Qty X2007.Qty X2008.Qty X2009.Qty X2010.Qty 
# 4068      3715      4603      4150      5017      4950      4562      5961      4761 
# X2011.Qty X2012.Qty X2013.Qty X2014.Qty X2015.Qty 
# 6085      5311      5604      6147      6663 

# Answer 1b(iii)
hist(final$Total.Ticket.Revenue, xlab = "Total Ticket Revenue", ylab = "Frequency")
bhist = subset(final, final$Total.Ticket.Revenue > 0)
hist(bhist$Total.Ticket.Revenue, xlab = "Total Ticket Revenue", ylab = "Frequency")
bhist = subset(final, final$Total.Ticket.Revenue > 0 & final$Total.Ticket.Revenue <= 6000)
hist(bhist$Total.Ticket.Revenue, xlab = "Total Ticket Revenue", ylab = "Frequency")

hist(final$X2014.Total.Cost, xlab = "2014 Ticket Cost", ylab = "Frequency")
bhist2014 = subset(final, final$X2014.Total.Cost > 0 & final$X2014.Total.Cost <= 1000)
hist(bhist2014$X2014.Total.Cost, xlab = "2014 Ticket Cost", ylab = "Frequency")

hist(final$X2015.Total.Cost, xlab = "2015 Ticket Cost", ylab = "Frequency")
bhist2015 = subset(final, final$X2015.Total.Cost > 0 & final$X2015.Total.Cost <= 1000)
hist(bhist2015$X2015.Total.Cost, xlab = "2015 Ticket Cost", ylab = "Frequency")

# Answer 1b(iv)
bscatter = subset(final, final$X2014.Total.Cost > 0 & final$X2015.Total.Cost > 0)
plot(bscatter$X2014.Total.Cost, bscatter$X2015.Total.Cost, xlab ="2014 Ticket Cost" , 
                                                           ylab ="2015 Ticket Cost")
bscatter = subset(final, final$X2014.Total.Cost > 0 & final$X2014.Total.Cost <= 2000 & 
                         final$X2015.Total.Cost > 0 & final$X2015.Total.Cost <= 2000)
plot(bscatter$X2014.Total.Cost, bscatter$X2015.Total.Cost, xlab ="2014 Ticket Cost" , 
                                                           ylab ="2015 Ticket Cost")

# Answer 1c(i)
plot(final$Total.Donations, final$Total.Ticket.Revenue, xlab = "Total Donations",
                                                        ylab = "Total Ticket Revenue")


# Answer 1c(ii)
cscatter = subset(final, final$Total.Donations > 0 & final$Total.Donations < 20000 &
                    final$Total.Ticket.Revenue > 0 & final$Total.Ticket.Revenue < 15000)
plot(cscatter$Total.Donations, cscatter$Total.Ticket.Revenue, xlab = "Total Donations",
                                                              ylab = "Total Ticket Revenue")

###########################################################################################
# Answer 2 #
###########################################################################################

# Data cleansing
nrow(subset(final, X2002.Qty > 10 | X2003.Qty > 10 | X2004.Qty > 10 | X2005.Qty > 10 | X2006.Qty > 10 | 
              X2007.Qty > 10 | X2008.Qty > 10 | X2009.Qty > 10 | X2010.Qty > 10 | X2011.Qty > 10 | 
              X2012.Qty > 10 | X2013.Qty > 10 | X2014.Qty > 10 | X2015.Qty > 10)) 
#[1] 2932
# Thus we can remove these 2932 records considering these as group ticket sales.
final = final[(final$X2002.Qty <= 10 & final$X2003.Qty <= 10 & final$X2004.Qty <= 10 &
               final$X2005.Qty <= 10 & final$X2006.Qty <= 10 & final$X2007.Qty <= 10 & 
               final$X2008.Qty <= 10 & final$X2009.Qty <= 10 & final$X2010.Qty <= 10 & 
               final$X2011.Qty <= 10 & final$X2012.Qty <= 10 & final$X2013.Qty <= 10 & 
               final$X2014.Qty <= 10 & final$X2015.Qty <= 10),]

# Answer 2a(i)
Clustdonor = subset(final, select = c(35:45))
#Normalizing the data
#install.packages("DMwR")
library(DMwR)
Donornorm=scale(Clustdonor)
summary(Donornorm)
str(Donornorm)

set.seed(5000)
DonorKMC = kmeans(Donornorm, centers = 3)
table(DonorKMC$cluster)
#     1     2     3 
# 26988    94     5

DonorKMC = kmeans(Donornorm, centers = 4)
table(DonorKMC$cluster)
#  1     2     3     4 
#  8   100     5 26974

DonorKMC = kmeans(Donornorm, centers = 5)
table(DonorKMC$cluster)
#  1     2     3     4     5 
#  3    10     1 26959   114
# We can stop at 4

DonorKMC = kmeans(Donornorm, centers = 6)
table(DonorKMC$cluster)
#     1     2     3     4     5     6 
#    76     5    49 26945     4     8 

DonorKMC$size
DonorKMC$centers

# #install.packages("cluster")
# library(cluster)
# clusplot(Clustdonor, DonorKMC$cluster, main='2D representation of the Cluster solution',
#          color=TRUE, shade=TRUE,
#          labels=2, lines=0)
# table(Donornorm[,1],DonorKMC$cluster)

clust1 = unscale(subset(Donornorm, DonorKMC$cluster == 1),Donornorm)
clust2 = unscale(subset(Donornorm, DonorKMC$cluster == 2),Donornorm)
clust3 = unscale(subset(Donornorm, DonorKMC$cluster == 3),Donornorm)
clust4 = unscale(subset(Donornorm, DonorKMC$cluster == 4),Donornorm) 
clust5 = unscale(subset(Donornorm, DonorKMC$cluster == 5),Donornorm)
clust6 = unscale(subset(Donornorm, DonorKMC$cluster == 6),Donornorm)

Clustdonor1 = subset(final, final$Total.Donations>0, select = c(35:45))
#Normalizing the data
#install.packages("DMwR")
library(DMwR)
Donornorm1=scale(Clustdonor1)
summary(Donornorm1)
str(Donornorm1)


DonorKMC1 = kmeans(Donornorm1, centers = 3)
table(DonorKMC1$cluster)
#  1   2  3
# 18 991  4

DonorKMC1 = kmeans(Donornorm1, centers = 4)
table(DonorKMC1$cluster)
# 1   2   3   4 
# 5 924  79   5

DonorKMC1 = kmeans(Donornorm1, centers = 5)
table(DonorKMC1$cluster)
# 1   2   3   4   5 
# 6 905   5   8  89 
# We can stop at 4

DonorKMC1 = kmeans(Donornorm1, centers = 6)
table(DonorKMC1$cluster)
#  1   2   3   4   5   6 
#  4 891   6   8   5  99


DonorKMC1$size
DonorKMC1$centers

# #install.packages("cluster")
# library(cluster)
# clusplot(Clustdonor, DonorKMC$cluster, main='2D representation of the Cluster solution',
#          color=TRUE, shade=TRUE,
#          labels=2, lines=0)
# table(Donornorm[,1],DonorKMC$cluster)

clust11 = unscale(subset(Donornorm1, DonorKMC1$cluster == 1),Donornorm1)
clust12 = unscale(subset(Donornorm1, DonorKMC1$cluster == 2),Donornorm1)
clust13 = unscale(subset(Donornorm1, DonorKMC1$cluster == 3),Donornorm1)
clust14 = unscale(subset(Donornorm1, DonorKMC1$cluster == 4),Donornorm1) 
clust15 = unscale(subset(Donornorm1, DonorKMC1$cluster == 5),Donornorm1)
clust16 = unscale(subset(Donornorm1, DonorKMC1$cluster == 6),Donornorm1)

# Answer 2a(ii)
#install.packages("ggplot2")
library(ggplot2)

Year = c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)
ClusterNo = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
              3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
              5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6)
df = data.frame(Year, ClusterNo)
df$Total_Donation = c (colSums(clust11), colSums(clust12), colSums(clust13), colSums(clust14), 
                       colSums(clust15), colSums(clust16))
ggplot(df, aes(Year, Total_Donation, col = as.factor(ClusterNo))) + geom_line()

# Answer 2b
lmdonor = subset(final, final$Total.Donations > 0, select = c(21:32, 40:46))
str(lmdonor)
summary(lmdonor)

DonorReg = lm(lmdonor$X2014.Donation ~ X2009.Qty + X2010.Qty + X2011.Qty + X2012.Qty + X2013.Qty + 
                                       X2009.Total.Cost + X2010.Total.Cost + X2011.Total.Cost +
                                       X2012.Total.Cost + X2013.Total.Cost +
                                       X2009.Donation + X2010.Donation + X2011.Donation +
                                       X2012.Donation + X2013.Donation, data=lmdonor)
summary(DonorReg)

DonorReg = lm(lmdonor$X2014.Donation ~ X2009.Donation + X2010.Donation + X2011.Donation + X2012.Donation
                                       + X2013.Donation, data=lmdonor)
summary(DonorReg)

# We can see that linear regression model is not giving good output (R squared is just 4%).
# So we will use classification trees as an alternative to determine whether a donation is expected
# or not in next year.

###############################################################
#install.packages("caTools")
library(caTools)
#install.packages("ROCR")
library(ROCR)
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("caret")
library(caret)

nrow(lmdonor[(lmdonor$X2014.Donation > 0),]) # 254
nrow(lmdonor[(lmdonor$X2015.Donation > 0),]) # 248
lmdonor$Y2014 = ifelse(lmdonor$X2014.Donation <= 0,0,1)
lmdonor$Y2015 = ifelse(lmdonor$X2015.Donation <= 0,0,1)
# Not splitting data as we can consider data of year 2014 as training dataset and 2015 as test dataset.

# spl= sample.split(lmdonor$Y2014, SplitRatio = 0.7)
# donTrain = subset(lmdonor, spl == TRUE)
# donTest = subset(lmdonor, spl == FALSE)

#CART model
donTree = rpart(Y2014 ~ X2009.Donation + X2010.Donation + X2011.Donation + X2012.Donation + 
                        X2013.Donation, data= lmdonor, method= "class", minbucket=4)
#Choosing minbucket as 4, having good accuracy (similar to other vales) and not very complex structure

prp(donTree)
rpart.plot(donTree,tweak=1.2)
print(donTree)

# Testing model on 2014 donations
PredictTree = predict(donTree, newdata = lmdonor, type = "class")
confusionMatrix(PredictTree, lmdonor$Y2014)
table(lmdonor$Y2014, PredictTree)
# PredictTree
#      0   1
# 0  732  27
# 1  156  98
(732+98)/(732+27+98+156) # Accuracy on data of Year 2014 is 81.93%

# Answer 2c

#Make predictions using Tree on 2015 donations
PredictTree = predict(donTree, newdata = lmdonor, type = "class")
confusionMatrix(PredictTree, lmdonor$Y2015)
table(lmdonor$Y2015, PredictTree)
# PredictTree
#     0   1
# 0 721  44
# 1 167  81
(721+81)/(721+44+167+81) # Accuracy = 79.17%

# Answer 2d
## prediction for 2016
X2016_Donation = lmdonor
preds <- predict(donTree, data = newdata[lmdonor], type = c("prob"))
preds2016 = as.data.frame(preds, replace=TRUE)
X2016_Donation$Y2016No = preds2016$`0`
X2016_Donation$Y2016Yes = preds2016$`1`

###########################################################################################
# Answer 3 #
###########################################################################################

# Answer 3a(i)
ClustTH = subset(final, final$Total.Tickets>0, 
                  select = c(7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31))

THnorm=scale(ClustTH)
summary(THnorm)
str(THnorm)

THKMC = kmeans(THnorm, centers = 3)
table(THKMC$cluster)
#    1     2     3 
# 2658  1427 22828

THKMC = kmeans(THnorm, centers = 4)
table(THKMC$cluster)
#    1     2     3     4 
# 2529   804 21074  2506 

THKMC = kmeans(THnorm, centers = 5)
table(THKMC$cluster)
#    1     2     3     4     5 
# 1830 19701   457  2234  2691 

THKMC = kmeans(THnorm, centers = 6)
table(THKMC$cluster)
#    1     2     3     4     5     6 
# 2082  1893   401 18778  1636  2123 

# We can stop here.
THKMC$size
THKMC$centers

ClustTH1 = unscale(subset(THnorm, THKMC$cluster == 1),THnorm)
ClustTH2 = unscale(subset(THnorm, THKMC$cluster == 2),THnorm)
ClustTH3 = unscale(subset(THnorm, THKMC$cluster == 3),THnorm)
ClustTH4 = unscale(subset(THnorm, THKMC$cluster == 4),THnorm) 
ClustTH5 = unscale(subset(THnorm, THKMC$cluster == 5),THnorm)
ClustTH6 = unscale(subset(THnorm, THKMC$cluster == 6),THnorm)


# Answer 3a(ii)
Year3 = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 
          2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
          2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
          2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
          2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
          2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)
ClusterNo3 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
               3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
               5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6)
df3 = data.frame(Year3, ClusterNo3)
df3$Total_Ticket_Sales = c (colSums(ClustTH1), colSums(ClustTH2), colSums(ClustTH3), colSums(ClustTH4), 
                            colSums(ClustTH5), colSums(ClustTH6))
ggplot(df3, aes(Year3, Total_Ticket_Sales, col = as.factor(ClusterNo3))) + geom_line()


# Answer 3b
THdata = subset(final, final$Total.Ticket.Revenue > 0, select = c(21:34))
str(THdata)
summary(THdata)
nrow(THdata[(THdata$X2014.Total.Cost > 0),]) # 4489 out of 24743
nrow(THdata[(THdata$X2015.Total.Cost > 0),]) # 5035 out of 24743
THdata$Y2014 = ifelse(THdata$X2014.Total.Cost <= 0,0,1)
THdata$Y2015 = ifelse(THdata$X2015.Total.Cost <= 0,0,1)
# Not splitting data as we can consider data of year 2014 as training dataset and 2015 as test dataset.

# spl= sample.split(THdata$Y2014, SplitRatio = 0.7)
# THTrain = subset(THdata, spl == TRUE)
# THTest = subset(THdata, spl == FALSE)

#CART model
THTree = rpart(Y2014 ~ X2009.Total.Cost + X2010.Total.Cost + X2011.Total.Cost + X2012.Total.Cost + 
                       X2013.Total.Cost, data= THdata, method= "class", minbucket=4)
#Choosing minbucket as 4, having good accuracy (similar to other vales) and not very complex structure

prp(THTree)
rpart.plot(THTree,tweak=1.2)
print(THTree)

# Testing model on 2014 donations
PredictTree = predict(THTree, newdata = THdata, type = "class")
confusionMatrix(PredictTree, THdata$Y2014)
table(THdata$Y2014, PredictTree)
# PredictTree
#       0     1
# 0 19850   404
# 1  3766   723
(19850+723)/(19850+404+3766+723) # Accuracy on Year 2014 data = 83.21%

# Answer 3c

#Make predictions using Tree on 2015 Ticket revenues
PredictTree = predict(THTree, newdata = THdata, type = "class")
confusionMatrix(PredictTree, THdata$Y2015)
table(THdata$Y2015, PredictTree)
# PredictTree
#       0     1
# 0 19211   497
# 1  4405   630
(19211+630)/(19211+497+4405+630) # Accuracy = 80.19%

# Answer 3d
## prediction for 2016
X2016_Ticket_Sales = THdata
preds1 <- predict(THTree, data = newdata[THdata], type = c("prob"))
preds12016 = as.data.frame(preds1, replace=TRUE)
X2016_Ticket_Sales$Y2016No = preds12016$`0`
X2016_Ticket_Sales$Y2016Yes = preds12016$`1`

###########################################################################################
# Answer 4 #
###########################################################################################

# Answer 4a(i)
ClustJoint = subset(final, select = c(12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34:46))
ClustJoint = subset(final, final$Total.Donations>0 | final$Total.Ticket.Revenue>0, 
                 select = c(12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34:46))

#Normalizing the data
Jointnorm=scale(ClustJoint)
summary(Jointnorm)
str(Jointnorm)

set.seed(5000)
JointKMC = kmeans(Jointnorm, centers = 3)
table(JointKMC$cluster)
#     1     2     3 
#  1569 23366    19

JointKMC = kmeans(Jointnorm, centers = 4)
table(JointKMC$cluster)
#    1     2     3     4 
# 2078 22104    19   753 

JointKMC = kmeans(Jointnorm, centers = 5)
table(JointKMC$cluster)
#   1     2     3     4     5 
#  15  1848  1821   269 21001

JointKMC = kmeans(Jointnorm, centers = 6)
table(JointKMC$cluster)
#    1     2     3     4     5     6 
# 1527  1820    15 20018  1306   268
# We can stop here.

JointKMC$size
JointKMC$centers

clustJ1 = unscale(subset(Jointnorm, JointKMC$cluster == 1),Jointnorm)
clustJ2 = unscale(subset(Jointnorm, JointKMC$cluster == 2),Jointnorm)
clustJ3 = unscale(subset(Jointnorm, JointKMC$cluster == 3),Jointnorm)
clustJ4 = unscale(subset(Jointnorm, JointKMC$cluster == 4),Jointnorm) 
clustJ5 = unscale(subset(Jointnorm, JointKMC$cluster == 5),Jointnorm)
clustJ6 = unscale(subset(Jointnorm, JointKMC$cluster == 6),Jointnorm)

# Answer 4a(ii)
Year4 = c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
Description4 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)

df4 = data.frame(Year4, Description4)
df4$Total = c (colSums(clustJ1), colSums(clustJ2), colSums(clustJ3), colSums(clustJ4), 
                       colSums(clustJ5), colSums(clustJ6))
Year4 = c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
          2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
          2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
          2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
          2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
          2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
ClusterNo4 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
               3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
               5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6)
df5 = data.frame(Year4, ClusterNo4)
df5$Total_Donations = c (subset(df4$Total, df4$Description4 == 1))
df5$Total_Ticket_Rev = c (subset(df4$Total, df4$Description4 == 2))
ggplot(df5, aes(Year4, Total_Donations, col = as.factor(ClusterNo4))) + geom_line()
df5 = subset(df5, df5$Total_Ticket_Rev <= 25000)
ggplot(df5, aes(Year4, Total_Ticket_Rev, col = as.factor(ClusterNo4))) + geom_line()

# Answer 4(b)
# We will take data from Year 2010 onwards.
ClustJointb = subset(final, select = c(24, 26, 28, 30, 32, 34:46))
ClustJointb = subset(final, final$Total.Donations>0 | final$Total.Ticket.Revenue>0, 
                    select = c(24, 26, 28, 30, 32, 34:46))

#Normalizing the data
Jointnormb=scale(ClustJointb)
summary(Jointnormb)
str(Jointnormb)

set.seed(5000)
JointKMCb = kmeans(Jointnormb, centers = 3)
table(JointKMCb$cluster)
#     1     2     3 
# 23207    19  1728 

JointKMCb = kmeans(Jointnormb, centers = 4)
table(JointKMCb$cluster)
#    1     2     3     4 
#  977    19 21577  2381 

JointKMCb = kmeans(Jointnormb, centers = 5)
table(JointKMCb$cluster)
#     1     2     3     4     5 
# 19920  2316    13  2181   524 

JointKMCb = kmeans(Jointnormb, centers = 6)
table(JointKMCb$cluster)
#     1     2     3     4     5     6 
# 18443  1550   540    19  2020  2382 
# We can stop here.

JointKMCb$size
JointKMCb$centers

clustbJ1 = unscale(subset(Jointnormb, JointKMCb$cluster == 1),Jointnormb)
clustbJ2 = unscale(subset(Jointnormb, JointKMCb$cluster == 2),Jointnormb)
clustbJ3 = unscale(subset(Jointnormb, JointKMCb$cluster == 3),Jointnormb)
clustbJ4 = unscale(subset(Jointnormb, JointKMCb$cluster == 4),Jointnormb) 
clustbJ5 = unscale(subset(Jointnormb, JointKMCb$cluster == 5),Jointnormb)
clustbJ6 = unscale(subset(Jointnormb, JointKMCb$cluster == 6),Jointnormb)

###########################################################################################
# Answer 5 #
###########################################################################################

# Answer 5a

# Answer 5a(i)
# Lets use 2015 donation predictions from Answer 2c
# PredictTree
#     0   1
# 0 721  44
# 1 167  81
# States that 802 donors donated out of 1013 who were supposed to donate. 
# Marketing team should send them some rewards/offers/loyality to motivate them for keep donating.
# These are reliable donors and marketing team can spend some budget on them to increase donations.

# Answer 5a(ii)
# Marketing team must look for donors who are always active and can use Answer 2a(ii) graph resluts to 
# focus on below donors for 2016.
# Cluster #1 - Recently started very high donations
# Cluster #2 - Continous donations but donated more in recent years (2013, 2014 and 2015)
# Cluster #6 - Continous donations across the time

# Markting team should send them recent successes and graphs of increased popularity of the festival
# with appreciations for the donors to keep connection with them.


# Answer 5b(i)
# Lets use 2015 Ticket sales predictions from Answer 3c
# PredictTree
#       0     1
# 0 19211   497
# 1  4405   630
# States that 19,841 ticketholders were there however expected number was 24,743.
# Marketing team should offer them some discounts on tickets to motivate them for keep coming to the 
# festival and buying tickets. A membership on discounted rate could also be offered.

# Answer 5b(ii)
# Marketing team must look for ticketholders who are regular attendees and can use Answer 3a(ii) graph 
# resluts to focus on below donors for 2016.
# Cluster #1 Ticketholders - All time high but may be attending alternate years
# Marketing team should offer membership so they buy tickets every year.

# Cluster #2 Ticketholders - Constant ticketholders
# Cluster #4 Ticketholders - No tickets in early years but picked rapidly in recent years.
# For these 2 groups, Marketing team should give them rewards like some cashback to be used to buy 
# tickets next year to maintain these ticketholders.

# Answer 5c
# Marketing team must look for groups based on total revenue and can use Answer 4a(ii) graphs resluts
# to focus on below donors for 2016.

# Cluster #4 Patrons - Make high donations and higer the ticket revenue as well.
# Marketing team should offer high rewards and proper loyalty to these patrons as these make the highest
# revenue by adding both ticket costs and donations. Festival organizers should personally send them 
# thank you notes in order to keep them connected.

# Cluster #1 Patrons - Make very high donations continously and some ticket revenue.
# Marketing team should offer better ticket rates to this group as they make high donations.

# Cluster #6 Patrons - Make low donations but contribute towards higher ticket revenue.
# Marketing team should send them results of recent donations trend to motivate them to donate more.
# Additionally, free tickets or rewards should be offered for increasing their donations.