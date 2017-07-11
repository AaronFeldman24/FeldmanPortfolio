merged = read.csv(choose.files(), header=TRUE)
attach(merged)

model1 = lm(CSEVERITY~PROPDMG+VEHICLES+factor(MAJORCAUSE)+factor(CRCOMANNER)
            +factor(FIRSTHARM)+factor(LIGHT)+factor(CSURFCOND)+factor(WEATHER1)
            +factor(ROADTYPE)+factor(EJ)+factor(Street_ID)+factor(MEDTYPE)
            +factor(MEDWIDTH)+NUMLANES+factor(LANETYPE1)+factor(LANETYPE2)
            +factor(LANETYPE3)+factor(LANETYPE4)+factor(LANETYPE5)
            +factor(LANETYPE6)+factor(LANETYPE7)+factor(LANETYPE8)
            +factor(LANETYPE9)
)

summary(model1)


# CSURFCOND, ROADTYPE, MEDTYPE, MEDWIDTH, NUMLANES

model1 = lm(factor(CSEVERITY)~factor(CSURFCOND)+factor(ROADTYPE)+factor(MEDTYPE)+
              factor(MEDWIDTH)+factor(NUMLANES))
step(model1)

summary(model1)


model2 = lm(CSEVERITY~factor(ROADTYPE)+factor(MEDTYPE)+
              factor(MEDWIDTH)+factor(NUMLANES))

anova(model2,model1)


attach(merged)
d1 = table(CSEVERITY,ROADTYPE)
d1 = data.frame(d1)
attach(d1)

#library(nnet)
model1 = multinom(factor(CSEVERITY)~factor(ROADTYPE), weights = Freq)
summary(model1)

model0 = multinom(factor(CSEVERITY)~1,weights=Freq)

LRTS1 = 2*(logLik(model1)-logLik(model0))
LRTS1 #1612.266
qchisq(.95,84) #106.3948
# Model1 is an improvement over null model


attach(merged)
d2 = table(CSEVERITY,ROADTYPE,MEDWIDTH)
d2 = data.frame(d2)
attach(d2)

#library(nnet)
model2 = multinom(factor(CSEVERITY)~factor(ROADTYPE)+factor(MEDWIDTH), weights = Freq)
summary(model2)

#model0 = multinom(factor(CSEVERITY)~1,weights=Freq)

LRTS2 = 2*(logLik(model2)-logLik(model0))
LRTS2 #1427.117
qchisq(.95,408) #456.0957
# Model1 is an improvement over null model



LRTS3 = 2*(logLik(model2)-logLik(model1))
LRTS3





## Model 1 but with d2

#library(nnet)
model3 = multinom(factor(CSEVERITY)~factor(ROADTYPE), weights = Freq)
summary(model3)

anova(model3,model2)





attach(merged)
d4 = table(CSEVERITY,MEDTYPE)
d4 = data.frame(d4)
attach(d4)


model4 = multinom(factor(CSEVERITY)~factor(MEDTYPE), weights = Freq)







####### Binomial ########

merged$MEDTYPE2 = factor(merged$MEDTYPE, c(1,2,3,4,5,0))
attach(merged)
table(MEDTYPE2)
table(MEDTYPE)



model5 = glm(BSEVERITY ~ factor(CSURFCOND)+factor(ROADTYPE)+factor(MEDTYPE)+
               factor(MEDWIDTH)+factor(NUMLANES),
               family=binomial(link = "logit"))

summary(model5)
step(model5)


model6 = glm(factor(BSEVERITY) ~ factor(CSURFCOND) + factor(ROADTYPE) + 
               factor(MEDTYPE), family = binomial(link = "logit"))
summary(model6)


df = cbind(BSEVERITY, fitted.values(model6), residuals(model6))
View(df)


# Null Model:
model0 = glm(BSEVERITY ~ 1, family = binomial(link = "logit"))

#LRTS
2*(logLik(model6) - logLik(model0)) #377.2333
qchisq(.95,35) #49.80185
# model6 is an improvement over null model

AIC(model6)
BIC(model6)


# Pseudo R2
(logLik(model0) - logLik(model6))/logLik(model0)
# A PSR2 of 0.0155 is very weak shows a low variance
#  of severity being explained by the chosen predictor variables



## Backwards Regression ##

model7 = glm(BSEVERITY ~ factor(CSURFCOND) + factor(ROADTYPE),
             family = binomial(link = "logit"))
anova(model7,model6,test="Chisq")
# P-value = 1.167e-05 < alpha = 0.05, so MEDTYPE is significant



model8 = glm(BSEVERITY ~ factor(CSURFCOND) + 
               factor(MEDTYPE), family = binomial(link = "logit"))
anova(model8,model6,test="Chisq")
# P-value = 0


model9 = glm(BSEVERITY ~ factor(ROADTYPE) + 
            factor(MEDTYPE), family = binomial(link = "logit"))
anova(model9,model6,test="Chisq")
# P-value = 0


plot(fitted.values(model6),residuals(model6,"pearson"))
plot(fitted.values(model6),residuals(model6))


## Probit model ##

model10 = glm(BSEVERITY ~ factor(CSURFCOND) + factor(ROADTYPE) + 
                factor(MEDTYPE), family = binomial(link = "probit"))
model0p = glm(BSEVERITY ~ 1, family = binomial(link = "probit"))

# Pseudo R2
(logLik(model0p) - logLik(model10))/logLik(model0p)
# A PSR2 of 0.0154 is very weak shows a low variance
#  of severity being explained by the chosen predictor variables

plot(fitted.values(model10),residuals(model10,"pearson"))
plot(fitted.values(model10),residuals(model10))

AIC(model10)
BIC(model10)

#LRTS
2*(logLik(model10) - logLik(model0p)) #374.9101
qchisq(.95,35) #49.80185
# model10 is an improvement over null model


## Complementary log-log model ##

model11 = glm(BSEVERITY ~ factor(CSURFCOND) + factor(ROADTYPE) + 
                factor(MEDTYPE), family = binomial(link = "cloglog"))
model0c = glm(BSEVERITY ~ 1, family = binomial(link = "cloglog"))

# Pseudo R2
(logLik(model0c) - logLik(model11))/logLik(model0c)
# A PSR2 of 0.0155 is very weak shows a low variance
#  of severity being explained by the chosen predictor variables


plot(fitted.values(model11),residuals(model11,"pearson"))
plot(fitted.values(model11),residuals(model11))

AIC(model11)
BIC(model11)

#LRTS
2*(logLik(model11) - logLik(model0p)) #377.9101
qchisq(.95,35) #49.80185
# model11 is an improvement over null model











###### Testing on CLogLog ######

set.seed(1234)

smp_size <- floor(.3*nrow(merged))
train_ind <- sample(seq_len(nrow(merged)), size=smp_size)

train <- merged[train_ind,]
test <- merged[-train_ind,]


model20 = glm(BSEVERITY ~ factor(CSURFCOND) + factor(ROADTYPE) + 
                factor(MEDTYPE), family = binomial(link = "cloglog"),
                data = train)

predTest <- predict(model20,test,type="response")
head(predTest)
df <- cbind(test[,"BSEVERITY"],predTest)


error <- predTest - test[,"BSEVERITY"]
se <- (predTest - test[,"BSEVERITY"])**2
mse <- mean(se)
mse


ave <- mean(abs(error))



predTest <- round(predTest,1)
predTest

error <- predTest - test[,"BSEVERITY"]
se <- (predTest - test[,"BSEVERITY"])**2
mse <- mean(se)
mse


ave <- mean(abs(error))

df2 <- cbind(test,predTest)

df3 <- subset(df2, subset=(BSEVERITY == 1))

df3[,c("BSEVERITY","predTest")]

df4 <- cbind(df3[,"BSEVERITY"])











##### Artificial subset #####



zeros <- subset(merged,subset = (BSEVERITY == 0))

set.seed(123)

sample1 <- zeros[sample(nrow(zeros), 2567), ]
sample2 <- subset(merged,subset = (BSEVERITY == 1))

sample3 <- rbind(sample1,sample2)

write.csv(sample3, file = "sample3.csv",row.names=FALSE)



smp_size <- floor(.3*nrow(merged))
train_ind <- sample(seq_len(nrow(merged)), size=smp_size)
