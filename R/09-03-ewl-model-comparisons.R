library(cgwtools)

load('models/Girl_Models_EWL')


#Obtain Average -2 Log Liklihood

mlogs <- data.frame(matrix(nrow = 20, ncol = 20))
i = 1
j = 1
while (i <=  20) {
  for (j in 1:20) {
    stats <- anova(age_1_ewl_girls$analyses[[i]], covs_2_ewl_girls$analyses[[j]])
    mlogs[i,j] <- stats$LR.stat[2]
  }
  i <- i+1
}

#Obtain Average LR Test Statistic and p-value

mcomp1LL <- median(as.matrix(mlogs))
mcomp1p <-pchisq(mcomp1LL, df = 2, lower.tail = FALSE)

#Repeat for 2 and 3
mlogs <- data.frame(matrix(nrow = 20, ncol = 20))
i = 1
j = 1
while (i <=  20) {
  for (j in 1:20) {
    stats <- anova(covs_2_ewl_girls$analyses[[i]], covs_3_ewl_girls$analyses[[j]])
    mlogs[i,j] <- stats$LR.stat[2]
  }
  i <- i+1
}

mcomp2LL <- median(as.matrix(mlogs))
mcomp2p <-pchisq(mcomp2LL, df = 3, lower.tail = FALSE)

mlogs <- data.frame(matrix(nrow = 20, ncol = 20))
i = 1
j = 1
while (i <=  20) {
  for (j in 1:20) {
    stats <- anova(covs_3_ewl_girls$analyses[[i]], covs_4_ewl_girls$analyses[[j]])
    mlogs[i,j] <- stats$LR.stat[2]
  }
  i <- i+1
}

mcomp3LL <- median(as.matrix(mlogs))
mcomp3p <-pchisq(mcomp3LL, df = 4, lower.tail = FALSE)

aics1 <- vector(length = 20)
for (i in 1:20) {
  aics1[i] <- age_1_ewl_girls$analyses[[i]]$info$AIC }
aic1med <- median(as.numeric(aics1))

aics2 <- vector(length = 20)
for (i in 1:20) {
  aics2[i] <- covs_2_ewl_girls$analyses[[i]]$info$AIC }
aic2med <- median(as.numeric(aics2))

aics3 <- vector(length = 20)
for (i in 1:20) {
  aics3[i] <- covs_3_ewl_girls$analyses[[i]]$info$AIC }
aic3med <- median(as.numeric(aics3))

aics4 <- vector(length = 20)
for (i in 1:20) {
  aics4[i] <- covs_4_ewl_girls$analyses[[i]]$info$AIC }
aic4med <- median(as.numeric(aics4))


#put into table
mcomp_table <- data.frame(matrix(nrow = 4, ncol = 6))
mcomp_table[1, ] <- c('Age Model' , aic1med ,'', '', '', '')
mcomp_table[2, ] <- c('Step 1 Covs' , aic2med , 'Age vs. Step 1 Covs', mcomp1LL, 2,  mcomp1p)
mcomp_table[3, ] <- c('Step 2 Covs' , aic3med, 'Step 1 Covs vs. Step 2 Covs', mcomp2LL, 3, mcomp2p)
mcomp_table[4, ] <- c('Step 3 Covs' , aic4med , 'Step 2 Covs vs. Step 3 Age x Cov interactions', mcomp3LL, 4, mcomp3p)
colnames(mcomp_table) <- c('Model', 'AIC', 'Model Comparison', 'Median -2LL difference', 'df', 'p-value')
mcomp_table$`Median -2LL difference` <- signif(as.numeric(mcomp_table$`Median -2LL difference`), digits = 4)
mcomp_table$`p-value` <- signif(as.numeric(mcomp_table$`p-value`), digits = 2)
mcomp_table$AIC <- signif(as.numeric(mcomp_table$AIC), digits = 7)

girls_mcomp_table <- mcomp_table

save(girls_mcomp_table, file = 'models/ewl_mcomp_tables.Rdata')

#Boys Below

load('models/Boy_Models_EWL')


#Obtain Average -2 Log Liklihood

mlogs <- data.frame(matrix(nrow = 20, ncol = 20))
i = 1
j = 1
while (i <=  20) {
  for (j in 1:20) {
    stats <- anova(age_1_ewl_boys$analyses[[i]], covs_2_ewl_boys$analyses[[j]])
    mlogs[i,j] <- stats$LR.stat[2]
  }
  i <- i+1
}

#Obtain Average LR Test Statistic and p-value

mcomp1LL <- median(as.matrix(mlogs))
mcomp1p <-pchisq(mcomp1LL, df = 2, lower.tail = FALSE)

#Repeat for 2 and 3
mlogs <- data.frame(matrix(nrow = 20, ncol = 20))
i = 1
j = 1
while (i <=  20) {
  for (j in 1:20) {
    stats <- anova(covs_2_ewl_boys$analyses[[i]], covs_3_ewl_boys$analyses[[j]])
    mlogs[i,j] <- stats$LR.stat[2]
  }
  i <- i+1
}

mcomp2LL <- median(as.matrix(mlogs))
mcomp2p <-pchisq(mcomp2LL, df = 3, lower.tail = FALSE)

mlogs <- data.frame(matrix(nrow = 20, ncol = 20))
i = 1
j = 1
while (i <=  20) {
  for (j in 1:20) {
    stats <- anova(covs_3_ewl_boys$analyses[[i]], covs_4_ewl_boys$analyses[[j]])
    mlogs[i,j] <- stats$LR.stat[2]
  }
  i <- i+1
}

mcomp3LL <- median(as.matrix(mlogs))
mcomp3p <-pchisq(mcomp3LL, df = 4, lower.tail = FALSE)

aics1 <- vector(length = 20)
for (i in 1:20) {
  aics1[i] <- age_1_ewl_boys$analyses[[i]]$info$AIC }
aic1med <- median(as.numeric(aics1))

aics2 <- vector(length = 20)
for (i in 1:20) {
  aics2[i] <- covs_2_ewl_boys$analyses[[i]]$info$AIC }
aic2med <- median(as.numeric(aics2))

aics3 <- vector(length = 20)
for (i in 1:20) {
  aics3[i] <- covs_3_ewl_boys$analyses[[i]]$info$AIC }
aic3med <- median(as.numeric(aics3))

aics4 <- vector(length = 20)
for (i in 1:20) {
  aics4[i] <- covs_4_ewl_boys$analyses[[i]]$info$AIC }
aic4med <- median(as.numeric(aics4))


#put into table
mcomp_table <- data.frame(matrix(nrow = 4, ncol = 6))
mcomp_table[1, ] <- c('Age Model' , aic1med ,'', '', '', '')
mcomp_table[2, ] <- c('Step 1 Covs' , aic2med , 'Age vs. Step 1 Covs', mcomp1LL, 2,  mcomp1p)
mcomp_table[3, ] <- c('Step 2 Covs' , aic3med, 'Step 1 Covs vs. Step 2 Covs', mcomp2LL, 3, mcomp2p)
mcomp_table[4, ] <- c('Step 3 Covs' , aic4med , 'Step 2 Covs vs. Step 3 Age x Cov interactions', mcomp3LL, 4, mcomp3p)
colnames(mcomp_table) <- c('Model', 'AIC', 'Model Comparison', 'Median -2LL difference', 'df', 'p-value')
mcomp_table$`Median -2LL difference` <- signif(as.numeric(mcomp_table$`Median -2LL difference`), digits = 4)
mcomp_table$`p-value` <- signif(as.numeric(mcomp_table$`p-value`), digits = 2)
mcomp_table$AIC <- signif(as.numeric(mcomp_table$AIC), digits = 7)

boys_mcomp_table <- mcomp_table

resave(boys_mcomp_table, file = 'models/ewl_mcomp_tables.Rdata')

rm(list = ls())
