# Star Digital

Analyzing consumer response to ad decsions on the Star Digital Platform


```R
library("rstatix")
```

    
    Attaching package: ‘rstatix’
    
    
    The following object is masked from ‘package:stats’:
    
        filter
    
    



```R
# read file
dat <- read.csv('star_digital.csv')
```

The data consists of ~25,000 customers who have been exposed to Star Digital Ad Campaigns. The column "test" is inidcative of the Test/Control gorup of each customer. If a customer is present in the Control group - they are shown only Star Digital Ads. If a customer is present in the test group, they are shown only Charity Ads. Purchase flag is also provided, which indicates a customers purchase. Our aim is to use this dataset to conculde the effectiveness of Star Digitals Ad campaing strategy

### Is online advertising effective for Star Digital? In other words, is there a difference in conversion rate between the treatment and control groups?

> We perform a T-Test, to understand if the means of Conversion Rate between Test & Control is significantly different


```R
Total <- dat[,'imp_1'] + dat[,'imp_2'] + dat[,'imp_3'] + dat[,'imp_4'] + dat[,'imp_5']+ dat[,'imp_6']

attach(dat)
t.test(Total~test)
```


    
    	Welch Two Sample t-test
    
    data:  Total by test
    t = 0.12734, df = 3204.4, p-value = 0.8987
    alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    95 percent confidence interval:
     -0.8658621  0.9861407
    sample estimates:
    mean in group 0 mean in group 1 
           7.929217        7.869078 



### Interpretation

Group 0 is the Control Group & Group 1 is Test Group. By looking at the total impressions between groups we can conclude that there is no significant difference between the groups. Thus our study is not unnecessarily biased. We can proceed to perform a T-Test to understand effect on Purchase


```R
t.test(purchase~test)
```


    
    	Welch Two Sample t-test
    
    data:  purchase by test
    t = -1.8713, df = 3309.2, p-value = 0.06139
    alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    95 percent confidence interval:
     -0.039289257  0.000916332
    sample estimates:
    mean in group 0 mean in group 1 
          0.4856928       0.5048792 



### Interpretation

The T-Test shows a significant difference between the 2 groups. In this case, we see that the mean for Test(Group 1) > Control(Group 0) We can conclude the Ad strategy of Star Digital is indeed effective


```R
#Logistic regression
logitmod = glm(purchase~test,data=dat,family="binomial")
summary(logitmod)
```


    
    Call:
    glm(formula = purchase ~ test, family = "binomial", data = dat)
    
    Deviance Residuals: 
       Min      1Q  Median      3Q     Max  
    -1.186  -1.186   1.169   1.169   1.202  
    
    Coefficients:
                Estimate Std. Error z value Pr(>|z|)  
    (Intercept) -0.05724    0.03882  -1.474   0.1404  
    test         0.07676    0.04104   1.871   0.0614 .
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    (Dispersion parameter for binomial family taken to be 1)
    
        Null deviance: 35077  on 25302  degrees of freedom
    Residual deviance: 35073  on 25301  degrees of freedom
    AIC: 35077
    
    Number of Fisher Scoring iterations: 3



### Interpretation

By fitting a Logisitc Regression model on the dummy column "test", we see that a customer in the test group has odds of purchasing of 1.019 (e^(-0.05724+0.07676)) whereas customers in the control group have odds of purchasing of 0.94 (e^(-0.05724))

## Is there a frequency effect of advertising on purchase? In particular, the question is whether increasing the frequency of advertising (number of impressions) increases the probability of purchase?

> To understand purchase frequencey effect, we combine the ads through sites 1 - 6, and also create an interaction variable by multiplying with "test" column to isolate effects related to Test & Control groups


```R
imp_16 <- dat[,'imp_1'] + dat[,'imp_2'] + dat[,'imp_3'] + dat[,'imp_4'] + dat[,'imp_5'] + dat[,'imp_6'] 

imp_16_test = imp_16 * dat$test

fit1 <- glm(dat$purchase ~ imp_16 + imp_16_test, data = dat, family = binomial())
summary(fit1)
```


    
    Call:
    glm(formula = dat$purchase ~ imp_16 + imp_16_test, family = binomial(), 
        data = dat)
    
    Deviance Residuals: 
        Min       1Q   Median       3Q      Max  
    -4.9091  -1.1272   0.1306   1.2150   1.2485  
    
    Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
    (Intercept) -0.181875   0.014584 -12.471  < 2e-16 ***
    imp_16       0.016228   0.002676   6.065 1.32e-09 ***
    imp_16_test  0.015055   0.002930   5.139 2.76e-07 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    (Dispersion parameter for binomial family taken to be 1)
    
        Null deviance: 35077  on 25302  degrees of freedom
    Residual deviance: 34190  on 25300  degrees of freedom
    AIC: 34196
    
    Number of Fisher Scoring iterations: 5



### Interpretation

From the above results, we see that both the variables are significant as p-value<<<0.01. The exponent of coefficient of imp_16 is 1.016, which means that imp_16 increase 1, the odds of purchase go up by 1.6% - means as frequency increases, odds of purchasing increases propotionally.

The exponent of coefficient of imp_16_test is 1.015, which means that imp_16_test increases by 1, the odds of purchase go up by 1.5% - Specifically for the Test group as the number of Charity ads increases, odds of purchasing increases proprtionally.

Based on the above results, we can conclude that there is an effect of frequency of ad purchases.

## How does the conversion effectiveness of Sites 1-5 compare with that of Site 6?


> For this, we combine the impressions from Sites 1-5, and also create interaction variables by multiplying with "Test"


```R
imp_15 <- dat[,'imp_1'] + dat[,'imp_2'] + dat[,'imp_3'] + dat[,'imp_4'] + dat[,'imp_5']

imp_15_test = imp_15 * dat$test

imp_6_test = dat$imp_6 * dat$test

fit2 <- glm(dat$purchase ~ imp_15 + imp_15_test + dat$imp_6 + imp_6_test, data = dat, 
            family = binomial())
summary(fit2)
```


    
    Call:
    glm(formula = dat$purchase ~ imp_15 + imp_15_test + dat$imp_6 + 
        imp_6_test, family = binomial(), data = dat)
    
    Deviance Residuals: 
        Min       1Q   Median       3Q      Max  
    -5.1260  -1.1198   0.1187   1.2215   1.2495  
    
    Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
    (Intercept) -0.171919   0.014669 -11.720  < 2e-16 ***
    imp_15       0.019603   0.003265   6.005 1.92e-09 ***
    imp_15_test  0.014437   0.003562   4.054 5.04e-05 ***
    dat$imp_6    0.004068   0.004263   0.954   0.3399    
    imp_6_test   0.013344   0.005321   2.508   0.0121 *  
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    (Dispersion parameter for binomial family taken to be 1)
    
        Null deviance: 35077  on 25302  degrees of freedom
    Residual deviance: 34166  on 25298  degrees of freedom
    AIC: 34176
    
    Number of Fisher Scoring iterations: 5



### Interpretation

From the model result we can see that the coefficient of imp_15_test is higher than that of imp_6_test, which means that for test group, imp_15 has bigger influence that imp_6 on customer purchase. We can conclude that advertising on Site1-5 has a bigger impact on purchase than Site 6.

The exponent of coefficient of imp_15_test is 1.0145, which means that consumers in the test group have higher odds of purchasing (the odds of purchase go up by 1.45%)

The exponent of coefficient of imp_6_test is 1.0134, which means that consumers in the test group have higher odds of purchasing (the odds of purchase go up by 1.34%)

Site 1-5 is more effective regarding conversion of customer especially if Charity Ads are shown over standard Star Digital Ads

### Which sites should Star Digital advertise on? In particular, should it put its advertising dollars in Site 6 or in Sites 1 through 5?

> We analyze this considering two different scenarios and their probabilities are calculated using logistic regression model

Scenario 1 : Charity Ads are shown in Sites1-5(test) & Star Digital Ads are shown in Site 6(Control)


```R
log_purch <- -0.171919 + 0.019603 + 0.014437*1 +   0.004068 + 0.013344*0
purch_odds <- exp(log_purch)
p_purch <- purch_odds/(1+purch_odds)
p_purch #Probability of Purchase in this scenario is 0.4665971
```


0.466597076146713


Scenario 2 : Charity Ads are shown in Sites6(test) & Star Digital Ads are shown in Site 1-5(Control)


```R
log_purch <- -0.171919 + 0.019603 + 0.014437*0 +   0.004068 + 0.013344*1
purch_odds <- exp(log_purch)
p_purch <- purch_odds/(1+purch_odds)
p_purch #Probability of Purchase in this scenario is 0.4663251
```


0.466325055625666


### Analysis

Cost/Impression Site 1-5 = 0.025 \ Probability of Buying if advertised through Sites 1-5 = 0.4665971 \ Expected value of Customer Lifetime= 1200*0.4665971 = 559.9165

Cost/Impression Site 6 = 0.020 \ Probability of Buying if advertised through Site 6 = 0.4663251 \ Expected value of Customer Lifetime= 1200*0.4663251 = 559.5901

Based on the above results, it seems like Advertising through Sites 1-5 is marginally better. In %, Advertising in Sites 1-5 earns more than Site 6 bu 0.058%. But by looking at the cost, advertising on Sites1-5 is 25% more expensive than advertising on Site 6. By looking at the marginal increase in revenue, the cost is not justified.

Thus, it would be prudent to advertise on Site 6.
