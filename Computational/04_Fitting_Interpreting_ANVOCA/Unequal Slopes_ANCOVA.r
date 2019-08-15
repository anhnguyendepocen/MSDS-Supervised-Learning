library(lessR)
library(readxl)

path.work <- "E:/GitHub/MSDS-RegressionAnalysis/data"
path.home <- "D:/Projects/MSDS-RegressionAnalysis/data"

ex_conf <- read_excel("ANCOVA Example_confederate.xlsx")

mydata<-data.frame(ex_conf)
mydata

Plot(anxiety, diff, by=group, fit=TRUE, data=mydata)

y<-mydata$diff
x<-mydata$anxiety
d1<-mydata$d_1
d2<-mydata$d_2
d3<-mydata$d_3
d4<-mydata$d_4

anx2<-x*d2
anx3<-x*d3
anx4<-x*d4

contrasts(factor) <- contr.sum(3)

######  Test for Interaction - UNEQUAL Slopes  #####
###### Full Model

full<-lm(y~x+d2+d3+d4+anx2+anx3+anx4)
anova(full)
summary(full)

######  Reduced Model 

reduced<-lm(y~x+d2+d3+d4)
anova(reduced)
summary(reduced)


FULL MODEL RESULTS

Y_hat = -82.37 + 5.185*X -33.11*d2 - 45.27*d3 -5.247*d4 
         1.77*anx2 + 2.087*anx3 + 0.347*anx4

SS(REG) = 4010.1 + 1.2+417.1+9.1 + ... + 0.8 =     (7)
SS(ERR) = 783.6  (12)


REDUCED MODEL RESULTS

Y_had = -98.12 + 6.05*X -2.319*d2 - 11.043*d3 + 2.10*d4

SS(reg) = 4010.1 + 1.2+417.1+9.1 =   (4)
SS(err) = 817.5  (15)

TEST FOR UNEQUAL SLOPES


F = [SS(Reg-FULL) - SS(Reg-Reduced)] / 3
    -------------------------------------
         SS(ERR - FUll) / 12

155167.3416 - -0.9702

49038.2159 - 92.8978

21946.7894 + 29.4119

116464.20119 - 40.74529

18536.05260 - 27.09052

-16983.75754  + 21.72258