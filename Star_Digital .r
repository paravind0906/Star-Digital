library("rstatix")

# read file
dat <- read.csv('star_digital.csv')

Total <- dat[,'imp_1'] + dat[,'imp_2'] + dat[,'imp_3'] + dat[,'imp_4'] + dat[,'imp_5']+ dat[,'imp_6']

attach(dat)
t.test(Total~test)

t.test(purchase~test)

#Logistic regression
logitmod = glm(purchase~test,data=dat,family="binomial")
summary(logitmod)

imp_16 <- dat[,'imp_1'] + dat[,'imp_2'] + dat[,'imp_3'] + dat[,'imp_4'] + dat[,'imp_5'] + dat[,'imp_6'] 

imp_16_test = imp_16 * dat$test

fit1 <- glm(dat$purchase ~ imp_16 + imp_16_test, data = dat, family = binomial())
summary(fit1)

imp_15 <- dat[,'imp_1'] + dat[,'imp_2'] + dat[,'imp_3'] + dat[,'imp_4'] + dat[,'imp_5']

imp_15_test = imp_15 * dat$test

imp_6_test = dat$imp_6 * dat$test

fit2 <- glm(dat$purchase ~ imp_15 + imp_15_test + dat$imp_6 + imp_6_test, data = dat, 
            family = binomial())
summary(fit2)

log_purch <- -0.171919 + 0.019603 + 0.014437*1 +   0.004068 + 0.013344*0
purch_odds <- exp(log_purch)
p_purch <- purch_odds/(1+purch_odds)
p_purch #Probability of Purchase in this scenario is 0.4665971

log_purch <- -0.171919 + 0.019603 + 0.014437*0 +   0.004068 + 0.013344*1
purch_odds <- exp(log_purch)
p_purch <- purch_odds/(1+purch_odds)
p_purch #Probability of Purchase in this scenario is 0.4663251
