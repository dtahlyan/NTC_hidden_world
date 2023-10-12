#install.packages("psych") #uncomment to install packages first if not already installed
#install.packages("lavaan")
#install.packages("poLCA")
#install.packages('GPArotation')
#isntall.packages('corrplot)
#install.package('ggplot2)
#install.packages('parameters')

#load packages
library(psych) #EFA
library(lavaan) #CFA and SEM
library(poLCA) # LCA
library(GPArotation) #different rotations in EFA
library(corrplot) #correlation plot
library(ggplot2) #plots in R
library(stats) #in built stats package
library(parameters) #parameter processing for psych

#EFA using stats package (in built in R)
#-------------------------------------------------------------------------------
data(bfi) #inbuild dataset in psych package from the Big Five Personality Trait Dataset
bfi <- as.data.frame(bfi) #change to dataframe
bfi <- bfi[complete.cases(bfi[,c(1:25)]),] #remove rows with NAs for indicators variables; not the best way to handle missing data

#create a correlation matrix and see if any sets of indictors have correlations or not
testRes = cor.mtest(bfi[,c(1:25)], conf.level = 0.95)
cormat<- cor(bfi[,c(1:25)])
corrplot(cormat,p.mat=testRes$p,insig='blank')

#KMO Test of sampling adequacy
KMO(bfi[,c(1:25)]) #higher value is better

#barlett test
cortest.bartlett(cormat, n = 2436) #test of whether correlation matrix is identity

#run EFA with 7 factors
efa2 <-  factanal(bfi[,c(1:25)],factors = 5,rotation = 'oblimin') #try different rotations: varimax, oblimin, none
print(efa2, sort = FALSE, cutoff = 0.3)

#Eigen values and screeplot
eigenvals <- eigen(cormat) #sum of square of loading for a factor in various indicators
scree(cormat, factors = FALSE)


#------------------------------------------------------------------------------
#CFA
bfi$female = ifelse(bfi$gender == 2, 1, 0)
model <- '
#latent variables
A =~   A1 + A2 + A3 + A4 + A5
C =~   C1 + C2 + C3 + C4 + C5
E =~   E1 + E2 + E3 + E4 + E5
N =~   N1 + N2 + N3 + N4 + N5
O =~   O1 + O2 + O3 + O4 + O5

#regression
A ~ age + female
C ~ age + female
E ~ age + female
N ~ age + female
O ~ age + female
'

fit <- sem(model,data=bfi,std.lv = TRUE, ordered = c('A1', 'A2','A3','A4', 'A5',
                                                     'C1', 'C2','C3','C4', 'C5',
                                                     'E1', 'E2','E3','E4', 'E5',
                                                     'N1', 'N2','N3','N4', 'N5',
                                                     'O1', 'O2','O3','O4', 'O5'),estimator = 'WLSMV')

summary(fit, standardized = TRUE, fit.measures=TRUE, rsquare = TRUE)



#------------------------LCA Starts Here----------------------------------------
#LCA_data
data(cheating)

testRes = cor.mtest(cheating[,c(1:4)], conf.level = 0.95)
cormat<- cor(cheating[,c(1:4)])
corrplot(cormat, p.mat = testRes$p)

#Students responded either (1) no or (2) yes as to whether
#they had ever lied to avoid taking an exam (LIEEXAM),
#lied to avoid handing a term paper in on time (LIEPAPER),
#purchased a term paper to hand in as their own or had obtained a copy of an exam prior to taking the exam
#(FRAUD), or
#copied answers during an exam from someone sitting near to them (COPYEXAM).
# GPA of the students is also avaiable on an 5-point ordered scale (1 to 5)

#LCA unconditional
f <- cbind(LIEEXAM,
           LIEPAPER,
           FRAUD,
           COPYEXAM)~1 #indicators used and ~1 specifies no covariates


BIC <- c() #lower BIC is better
for (i in 2:5){
  ch2 <- poLCA(f,cheating,nclass=i,nrep = 100)
  BIC <- append(BIC, ch2$bic)
}

plot(c(2,3,4,5), BIC, type = 'b', xlab = 'Number of Classes', ylab = 'BIC')
ch2 <- poLCA(f,cheating,nclass=2,nrep = 100,graphs = TRUE)
cheating <- cbind(cheating, ch2$posterior)

for (i in 1:5){
  print(paste("For those with GPA level ", i, 
              ", mean probability of being in class 2 is :", 
              toString(round(mean(cheating[which(cheating$GPA == i),]$'2'),3)), 
              sep = "")) 
}

mean_prob <- c()
for (i in 1:5){
  mgpa <- mean(cheating[which(cheating$GPA == i),]$'2')
  mean_prob <- append(mean_prob, mgpa)
}

plot(c(1,2,3,4,5), mean_prob, type = 'b', xlab = "GPA Level",
     ylab = "Mean Probability of Class 2")

#LCA conditional
f2 <- cbind(LIEEXAM,LIEPAPER,FRAUD,COPYEXAM)~GPA
ch2c <- poLCA(f2,cheating,nclass=2,graphs = TRUE, nrep = 100)


#-------------------------------------------------------------------------------
#SEM example 1 - social capital (no data provided)
model <- '
          #latent variables
          Netnov =~   q27cnt + Prestige_entropy  + Prestige_score
          NeighEngage =~  q23b  + q24a + q24b
          CommEngage =~ q25e  + q25a + q25c
          SocialSup =~ proximity  + gender_hom  + strength2


          #regression
          Netnov ~ CommEngage #+ NeighEngage
          act_diverse ~ SocialSup + Netnov + worker + inc +  age40to60 + mt60 + graduate
          act_count ~ SocialSup + Netnov  +  graduate + inc  +  age40to60 + mt60 + worker

          # residual correlation
          act_diverse ~~ act_count
          CommEngage ~~ NeighEngage


          q27cnt~~Prestige_score
          q27cnt~~Prestige_entropy
          #proximity~~strength2
          q23b ~~ q24b


          Netnov~~0*NeighEngage
          Netnov~~0*CommEngage
          SocialSup~~0*NeighEngage
          SocialSup~~0*CommEngage

          '

fit <- sem(model,data=my_data,std.lv = TRUE, ordered = c('q23b', 'q24a','q24b',
                                                         'q25e','q25a','q25c'),estimator = 'WLSMV',se='robust')
summary(fit, standardized = TRUE, fit.measures=TRUE, rsquare = TRUE)

#------------------results start here-------------------------------------------
# see http://www.davidakenny.net/cm/fit.htm for info on model fit measures
## lavaan 0.6-8 ended normally after 79 iterations
##
##   Estimator                                       DWLS
##   Optimization method                           NLMINB
##   Number of model parameters                        54
##
##   Number of observations                          1434
##
## Model Test User Model:
##                                               Standard      Robust
##   Test Statistic                               807.354     682.174
##   Degrees of freedom                               129         129
##   P-value (Chi-square)                           0.000       0.000
##   Scaling correction factor                                  1.257
##   Shift parameter                                           40.075
##        simple second-order correction
##
## Model Test Baseline Model:
##
##   Test statistic                              5823.506    3523.942
##   Degrees of freedom                                91          91
##   P-value                                        0.000       0.000
##   Scaling correction factor                                  1.670
##
## User Model versus Baseline Model:
##
##   Comparative Fit Index (CFI)                    0.882       0.839
##   Tucker-Lewis Index (TLI)                       0.917       0.886
##
##   Robust Comparative Fit Index (CFI)                            NA
##   Robust Tucker-Lewis Index (TLI)                               NA
##
## Root Mean Square Error of Approximation:
##
##   RMSEA                                          0.061       0.055
##   90 Percent confidence interval - lower         0.057       0.051
##   90 Percent confidence interval - upper         0.065       0.059
##   P-value RMSEA <= 0.05                          0.000       0.027
##
##   Robust RMSEA                                                  NA
##   90 Percent confidence interval - lower                        NA
##   90 Percent confidence interval - upper                        NA
##
## Standardized Root Mean Square Residual:
##
##   SRMR                                           0.036       0.036
##
## Parameter Estimates:
##
##   Standard errors                           Robust.sem
##   Information                                 Expected
##   Information saturated (h1) model        Unstructured
##
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   Netnov =~
##     q27cnt            0.203    0.019   10.594    0.000    0.305    0.657
##     Prestige_ntrpy    0.048    0.006    8.491    0.000    0.072    0.405
##     Prestige_score    0.100    0.009   10.963    0.000    0.151    0.688
##   NeighEngage =~
##     q23b              0.766    0.049   15.557    0.000    0.766    0.766
##     q24a              0.716    0.043   16.650    0.000    0.716    0.716
##     q24b              0.736    0.050   14.712    0.000    0.736    0.736
##   CommEngage =~
##     q25e              0.683    0.037   18.376    0.000    0.683    0.683
##     q25a              0.728    0.040   18.006    0.000    0.728    0.728
##     q25c              0.588    0.045   13.173    0.000    0.588    0.588
##   SocialSup =~
##     proximity         1.638    0.069   23.730    0.000    1.638    0.930
##     gender_hom        0.192    0.016   11.890    0.000    0.192    0.307
##     strength2         1.032    0.047   21.780    0.000    1.032    0.784
##
## Regressions:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   Netnov ~
##     CommEngage        1.122    0.135    8.330    0.000    0.747    0.747
##   act_diverse ~
##     SocialSup         0.161    0.046    3.486    0.000    0.161    0.093
##     Netnov            0.607    0.055   11.041    0.000    0.912    0.526
##     worker            0.323    0.116    2.778    0.005    0.323    0.071
##     inc               0.121    0.022    5.528    0.000    0.121    0.153
##     age40to60        -0.246    0.101   -2.446    0.014   -0.246   -0.070
##     mt60             -0.574    0.113   -5.059    0.000   -0.574   -0.143
##     graduate          0.766    0.094    8.110    0.000    0.766    0.216
##   act_count ~
##     SocialSup         0.045    0.022    2.033    0.042    0.045    0.056
##     Netnov            0.250    0.024   10.480    0.000    0.376    0.468
##     graduate          0.254    0.045    5.634    0.000    0.254    0.154
##     inc               0.056    0.010    5.562    0.000    0.056    0.154
##     age40to60        -0.218    0.046   -4.728    0.000   -0.218   -0.133
##     mt60             -0.300    0.054   -5.578    0.000   -0.300   -0.161
##     worker            0.177    0.054    3.249    0.001    0.177    0.084
##
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##  .act_diverse ~~
##    .act_count         0.631    0.042   14.974    0.000    0.631    0.718
##   NeighEngage ~~
##     CommEngage        0.567    0.043   13.210    0.000    0.567    0.567
##  .q27cnt ~~
##    .Prestige_score    0.054    0.004   12.730    0.000    0.054    0.973
##    .Prestige_ntrpy    0.008    0.002    3.573    0.000    0.008    0.140
##  .q23b ~~
##    .q24b              0.153    0.067    2.279    0.023    0.153    0.352
##  .Netnov ~~
##     NeighEngage       0.000                               0.000    0.000
##     CommEngage        0.000                               0.000    0.000
##   NeighEngage ~~
##     SocialSup         0.000                               0.000    0.000
##   CommEngage ~~
##     SocialSup         0.000                               0.000    0.000
##
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .q27cnt            0.533    0.040   13.369    0.000    0.533    1.151
##    .Prestige_ntrpy    0.833    0.021   39.806    0.000    0.833    4.669
##    .Prestige_score    0.210    0.019   10.926    0.000    0.210    0.961
##    .q23b              0.000                               0.000    0.000
##    .q24a              0.000                               0.000    0.000
##    .q24b              0.000                               0.000    0.000
##    .q25e              0.000                               0.000    0.000
##    .q25a              0.000                               0.000    0.000
##    .q25c              0.000                               0.000    0.000
##    .proximity         3.929    0.153   25.763    0.000    3.929    2.232
##    .gender_hom        0.184    0.056    3.298    0.001    0.184    0.294
##    .strength2         2.719    0.116   23.544    0.000    2.719    2.065
##    .act_diverse       2.913    0.144   20.229    0.000    2.913    1.682
##    .act_count         0.961    0.070   13.800    0.000    0.961    1.196
##    .Netnov            0.000                               0.000    0.000
##     NeighEngage       0.000                               0.000    0.000
##     CommEngage        0.000                               0.000    0.000
##     SocialSup         0.000                               0.000    0.000
##
## Thresholds:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##     q23b|t1           0.510    0.112    4.561    0.000    0.510    0.510
##     q24a|t1           0.400    0.112    3.579    0.000    0.400    0.400
##     q24b|t1           0.690    0.115    6.021    0.000    0.690    0.690
##     q25e|t1           1.195    0.129    9.249    0.000    1.195    1.195
##     q25a|t1           1.744    0.151   11.540    0.000    1.744    1.744
##     q25c|t1           1.407    0.139   10.104    0.000    1.407    1.407
##
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .q27cnt            0.122    0.009   13.231    0.000    0.122    0.568
##    .Prestige_ntrpy    0.027    0.001   21.866    0.000    0.027    0.836
##    .Prestige_score    0.025    0.002   12.609    0.000    0.025    0.527
##    .q23b              0.413                               0.413    0.413
##    .q24a              0.487                               0.487    0.487
##    .q24b              0.458                               0.458    0.458
##    .q25e              0.534                               0.534    0.534
##    .q25a              0.470                               0.470    0.470
##    .q25c              0.654                               0.654    0.654
##    .proximity         0.417    0.187    2.230    0.026    0.417    0.135
##    .gender_hom        0.355    0.017   21.351    0.000    0.355    0.906
##    .strength2         0.669    0.074    9.040    0.000    0.669    0.386
##    .act_diverse       1.766    0.096   18.430    0.000    1.766    0.589
##    .act_count         0.437    0.022   20.158    0.000    0.437    0.677
##    .Netnov            1.000                               0.443    0.443
##     NeighEngage       1.000                               1.000    1.000
##     CommEngage        1.000                               1.000    1.000
##     SocialSup         1.000                               1.000    1.000
##
## Scales y*:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##     q23b              1.000                               1.000    1.000
##     q24a              1.000                               1.000    1.000
##     q24b              1.000                               1.000    1.000
##     q25e              1.000                               1.000    1.000
##     q25a              1.000                               1.000    1.000
##     q25c              1.000                               1.000    1.000
##
## R-Square:
##                    Estimate
##     q27cnt            0.432
##     Prestige_ntrpy    0.164
##     Prestige_score    0.473
##     q23b              0.587
##     q24a              0.513
##     q24b              0.542
##     q25e              0.466
##     q25a              0.530
##     q25c              0.346
##     proximity         0.865
##     gender_hom        0.094
##     strength2         0.614
##     act_diverse       0.411
##     act_count         0.323
##     Netnov            0.557


#-------------------------------------------------------------------------------
#SEM example 2 - telework satisfaction (no data provided)
#full mimic model
model <- '
#measurement model
benefits =~  V25 + V24 + V39 + V26 + workflex
barriers =~  V21 + V22 + V26 + workflex

#structural model for latent variables
benefits ~ V10 + V12 + V40 + V29
barriers ~ V20 + V38 + V40

#regression equation for ordered satisfaction variable
V2 ~ V6 + age2 + V11 + V18 + V30 + V31 + V40  + V28 + benefits + barriers

#residual correlations
benefits ~~ 0*barriers
'

fit <- sem(model, data = data, ordered = c("V25", "V24", "V21", "V22", "V26", "V39", "V2", "workflex"))
summary(fit,standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

#-------------------------------------------------------------------------------
#LCA example 1 - impact of remote work on work aspects (no data provided)
opinion_data[opinion_data == "missing"] <- 1
opinion_data[opinion_data == "Very positive"] <- 2
opinion_data[opinion_data == "Somewhat positive"] <- 2
opinion_data[opinion_data == "Neither negative nor positive"] <- 3
opinion_data[opinion_data == "Somewhat negative"] <- 4
opinion_data[opinion_data == "Very negative"] <- 4


opinion_data$Q322_1 <- as.numeric(opinion_data$Q322_1)
opinion_data$Q322_2 <- as.numeric(opinion_data$Q322_2)
opinion_data$Q322_3 <- as.numeric(opinion_data$Q322_3)
opinion_data$Q322_4 <- as.numeric(opinion_data$Q322_4)
opinion_data$Q322_5 <- as.numeric(opinion_data$Q322_5)
opinion_data$Q322_6 <- as.numeric(opinion_data$Q322_6)
opinion_data$Q322_7 <- as.numeric(opinion_data$Q322_7)
opinion_data$Q322_8 <- as.numeric(opinion_data$Q322_8)
opinion_data$Q322_9 <- as.numeric(opinion_data$Q322_9)
opinion_data$Q322_10 <- as.numeric(opinion_data$Q322_10)
opinion_data$Q322_11 <- as.numeric(opinion_data$Q322_11)
opinion_data$Q322_12 <- as.numeric(opinion_data$Q322_12)

library(poLCA)
f <- cbind(Q322_1,
           Q322_2,
           Q322_3,
           Q322_4,
           Q322_5,
           Q322_6,
           Q322_7,
           Q322_8,
           Q322_9,
           Q322_10,
           Q322_11,
           Q322_12)~1


for (i in 1:8){
  lca2 <- poLCA(f,opinion_data,nclass=i,nrep = 50)
  print(lca2$bic)

}

lca2 <- poLCA(f,opinion_data,nclass=6,nrep = 50)

