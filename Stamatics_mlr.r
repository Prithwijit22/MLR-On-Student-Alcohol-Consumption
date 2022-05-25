# Reading the csv file
library(readxl)
D=read.csv("A:/Regression Project/New Project/student-mat.csv")
sum(is.na(D))     #Detection for missing value


library(dplyr)
library(colorDF)
D %>% colorDF(theme = "bw")

summary(D)



#boxplot(D,col = "pink")
library(DataExplorer)
library(ggplot2)
plot_histogram(D,ggtheme = theme_bw())
plot_bar(D,ggtheme = theme_bw())

corrplot(D)

#Function for binary encoding
Binary = function(y,x)
{
  z=NULL
  for ( i in 1:length(x))
  {
    if (x[i]==y[1])
      z[i]=1
    else 
      z[i]=0
  }
  return(z)
}
school = Binary(unique(D$school),D$school)
sex = Binary(unique(D$sex),D$sex)
address = Binary(unique(D$address),D$address)
famsize = Binary(unique(D$famsize),D$famsize)
Pstatus = Binary(unique(D$Pstatus),D$Pstatus)

#Function for one-hot encoding
Encoding = function(x,y)
{
  D1 = data.frame()
  for (i in 1:length(y))
  {
    for (j in 1:(length(x)-1))
    {
      if (y[i]==x[j])
        D1[i,j]=1
      else
        D1[i,j]=0
      if (y[i]==x[length(x)])
        D1[i,j]=0
    }
  }
  return(D1)
}
Mjob = Encoding(unique(D$Mjob),D$Mjob)
colnames(Mjob) <- c("Mjob1","Mjob2","Mjob3","Mjob4")
Fjob = Encoding(unique(D$Fjob),D$Fjob)
colnames(Fjob) <- c("Fjob1","Fjob2","Fjob3","Fjob4")
reason = Encoding(unique(D$reason),D$reason)
colnames(reason) <- c("reason1","reason2","reason3")
guardian = Encoding(unique(D$guardian),D$guardian)
colnames(guardian) <- c("guardian1","guardian2")
schoolsup = Binary(unique(D$schoolsup),D$schoolsup)
famsup = Binary(unique(D$famsup),D$famsup)
paid = Binary(unique(D$paid),D$paid)
activities = Binary(unique(D$activities),D$activities)
nursery = Binary(unique(D$nursery),D$nursery)
higher = Binary(unique(D$higher),D$higher)
internet = Binary(unique(D$internet),D$internet)
romantic = Binary(unique(D$romantic),D$romantic)
D$school = school
D$sex = sex
D$address =address
D$famsize = famsize
D$Pstatus = Pstatus
D$schoolsup = schoolsup
D$famsup = famsup
D$paid = paid
D$activities =activities
D$nursery = nursery
D$higher = higher
D$internet = internet
D$romantic = romantic 
Raw_Y = D$G3
D1=cbind(D[-ncol(D)],Mjob,Fjob,reason,guardian)
D1 = D1[-c(9:12)]
str(D1)                                #Design matrix
Y = scale(Raw_Y,center = TRUE,scale = TRUE)#Response variable
str(D1)
dim(D1)
Raw_data = D1

D1 = scale(as.matrix(D1),center = TRUE,scale = TRUE)
par(mfrow = c(1,1))
boxplot(D1,col = "pink",main = paste("Boxpot corresponding to all the predictors"))

#Continuous Data frame Creation
#age,Medu,Fedu,traveltime,studytime,famrel,freetime,goout,health,absences,G1,G2
Data_cont = data.frame(D$age,D$Medu,D$Fedu,D$traveltime,D$studytime,D$famrel,D$freetime,D$goout,D$Dalc,D$Walc,D$failures,D$health,D$absences,D$G1,D$G2)
Data_cont
boxplot(Data_cont,col = "yellowgreen",main = paste("Boxplot for continuos predictor"))

As = data.frame(Y,D1)
As %>% colorDF(theme = "bw")


######################################
#####     Data Cleaning    ###########
######################################

# Outlier Detection
#1. Visualization


par(mfrow = c(7,6),mar = c(2,2,1,1))
for (i in 2:ncol(X))
{
  hist(D1[,i],main=paste(colnames(D1)[i]),xlab = paste(colnames(D1)[i]),col = "orange")
}
library(ggplot2)
library(DataExplorer)
plot_histogram(D1,ggtheme = theme_bw())
plot_bar(D1,ggtheme = theme_bw())
plot_correlation(D1)
plot_correlation(data.frame(Y,Data_cont[,-11]))
#create_report(D)

par(mar = c(10,4,2,2))
library(corrplot)
corrplot(cor(as.matrix(data.frame(Y,D1[,-11]))),method = "circle",type = "lower",title = paste("Cicle Correlogram"),mar = c(2,2,2,2))





#Outlier Removal
par(mfrow = c(1,1))
i=1
while(i<=ncol(Data_cont))
{
  if (length(boxplot.stats(Data_cont[,i])$out) > 0)
  {
    out = boxplot.stats(Data_cont[,i])$out
    outlier = which(Data_cont[,i] %in% c(out))
    for (j in 1:length(outlier))
    {
      Data_cont[outlier[j],i] = median(Data_cont[,i])
    }
  }
  i = i + 1
}
boxplot (Data_cont,col = "yellowgreen",main = paste("Boxplot continuous after removing the outliers"))
D1 = data.frame(D1)
Dataclass = D1[,-which(names(D1) %in% c("age","Medu","Fedu","traveltime","studytime","famrel","freetime","goout","Dalc","Walc","failures","health","absences","G1","G2"))]
dim(Dataclass)
D1 = data.frame(as.data.frame(scale(Data_cont,center = TRUE,scale = TRUE)),Dataclass)
colnames(D1[,11])
D1 = D1[,-11]
dim(D1)
boxplot(D1,col = "yellowgreen",main = paste("Boxplot after removing the outliers"))
D1

summary(lm(Y~D.Dalc+D.Walc+romantic,data = D1))




#Linear Regression
Model = lm(Y~.,as.data.frame(D1))
summary(Model)

library(olsrr)
ols_plot_comp_plus_resid(Model)
ols_test_breusch_pagan(Model)  #Homoscedastisity checking   
ols_plot_diagnostics(Model)    #Model Diagnostics
ols_plot_resid_fit(Model)
ols_plot_resid_stud_fit(Model)
ols_eigen_cindex(Model)        # same as VDF        
ols_test_normality(Model)
ols_plot_resid_lev(Model)
ols_plot_resid_qq(Model)
#Residual Analysis
#par(mfcol=c(12,12), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
Y_hat = predict.lm(Model)  #Predicted Y
res_hat = residuals.lm(Model)               #residuals "observed error"
Predicted = data.frame(Y,Y_hat,res_hat)
ggplot(data = Predicted,mapping = aes(res_hat)) + geom_histogram(color = "red",fill = "pink",binwidth = 0.2) + ggtitle("Histogram for residuals")                  #histogram for residuals
ggplot(data = Predicted,mapping = aes(Y_hat)) + geom_histogram(color = "darkgreen",fill = "yellowgreen",binwidth = 0.2) + ggtitle("Histogram for Predicted Y")                             #histogram for response
#plot(D1)
ggplot(data = Predicted,mapping = aes(Y)) + geom_histogram(color = "blue",fill = "lightblue",binwidth = 0.2) + ggtitle("Histogram for Observed Y")   

#heatmap(as.matrix(data.frame(Y,Data_cont)),Rowv = NA,Colv = NA)


#View(D1)
par(mfrow =c(1,1))
qqnorm(res_hat)
qqline (res_hat,col="red")


ggplot(data = Predicted,mapping = aes(res_hat,Y_hat)) + geom_point(color = "blue",fill = "blue") + ggtitle("residual plot residual vs predicted")

ggplot(data = Predicted,mapping = aes(res_hat,Y)) + geom_point(color = "blue",fill = "blue") + ggtitle("Residual plot residual vs Y")


e_i=res_hat[-length(res_hat)]
e_i_1 = res_hat[-1]
residual = data.frame(e_i,e_i_1)
ggplot(data = residual,mapping = aes(e_i,e_i_1)) + geom_point(fill = "red",color = "red") + ggtitle("Check for residual Auto-Correlation")




#Transformation of response and again model fitting
De = data.frame(Raw_Y,Raw_data)
De = De[,-12]
par(mfrow =c(1,1))
plot_correlation(data.frame(Raw_Y,Data_cont[,-11]))

De= De [,-1]
De


x = seq(0.5,9,by = 0.1)
adj = NULL
for (i in 1:length(x) )
{
  new_Y = asinh(Raw_Y)^(x[i])
  MM = lm(new_Y~.,data = De)
  adj[i] = summary(MM)$adj.r.squared
}
library(ggplot2)
R_sq = data.frame(x,adj)
p = ggplot(data = R_sq,mapping = aes(x,adj)) + geom_point() + ggtitle("Plot for Adjusted R_Squared Values for Different Powers") + labs(y= "Adjusted R-Squared Values")
data1 = data.frame(5.8,adj[54])
p1 = p + geom_point(data1,mapping = aes(5.8,adj[54]),shape = 23,fill = "red",color = "red",size = 3) + annotate ("text",x=5.8,y=0.85,label = "Maximised \nvalue")
data2 = data.frame(7.0,adj[66])
p1 + geom_point(data2,mapping = aes(7.0,adj[66]),shape = 1,fill = "blue",color = "blue",size = 3) + annotate ("text",x=7.0,y=0.85,label = "Used Value")
for (i in 1:length(x))
{
  if (x[i]==7.0)
  {
    print(i)
    print(x[i])
    print(adj[i])
  }
}

######   So the power is 5.8

new_Y1 = asinh(Raw_Y)^(5.8)
M = lm(new_Y1~.,data = De)
summary(M)
qqnorm(M$residuals)
qqline (M$residuals,col="red")



library(olsrr)
ols_plot_comp_plus_resid(M)
ols_test_breusch_pagan(M)  #Homoscedastisity checking   
ols_plot_diagnostics(M)    #Model Diagnostics
ols_plot_resid_stud_fit(M)
ols_eigen_cindex(M)        # same as VDF        
ols_test_normality(M)
ols_plot_resid_lev(M)
ols_plot_resid_qq(M)
















# Taking the power as 7.0
new_Y = asinh(Raw_Y)^(7.0)
MM = lm(new_Y~.,data = De)
summary(MM)
qqnorm(MM$residuals)
qqline (MM$residuals,col="red")



library(olsrr)
ols_plot_comp_plus_resid(MM)
ols_test_breusch_pagan(MM)  #Homoscedastisity checking   
ols_plot_diagnostics(MM)    #Model Diagnostics
ols_plot_resid_stud_fit(MM)
ols_eigen_cindex(MM)        # same as VDF        
ols_test_normality(MM)
ols_plot_resid_lev(MM)
ols_plot_resid_qq(MM)


#High leverage and large residuals removal
De = data.frame(new_Y,De)

dim(De)
colnames(De)
De = De[-277,]
new_Y = De$new_Y


De = De[,-1]
dim(De)

ols_plot_resid_lev(MM)        #again leverage plot

MM = lm(new_Y~.,data = De)    #again model checking
summary(MM)

#Histograms plotting
Y_hat_new = predict.lm(MM)
res_hat_new = MM$residuals
Predicted_final = data.frame(new_Y,Y_hat_new,res_hat_new)
ggplot(data = Predicted_final,mapping = aes(res_hat_new)) + geom_histogram(color = "red",fill = "pink") + ggtitle("Histogram for residuals for final model")                  #histogram for residuals
ggplot(data = Predicted_final,mapping = aes(Y_hat_new)) + geom_histogram(color = "darkgreen",fill = "yellowgreen") + ggtitle("Histogram for Predicted Y for final model")                             #histogram for response
#plot(D1)
ggplot(data = Predicted_final,mapping = aes(new_Y)) + geom_histogram(color = "blue",fill = "lightblue",binwidth =750) + ggtitle("Histogram for Observed Y for final model") 










library(DescTools)
sort(VIF(MM))
De = within(De,rm("Fjob2"))
MM = lm(new_Y~.,data = De)
summary(MM)
sort(VIF(MM))





#All possible
library(olsrr)
for_aic = ols_step_forward_aic(MM)
plot(for_aic)
summary(for_aic$model)


ba_aic = ols_step_backward_aic(MM)
plot(ba_aic)
summary(ba_aic$model)


both_aic = ols_step_both_aic(MM)
plot(both_aic)
both_aic$predictors
Dataframe = data.frame(De$G2,De$G1,De$age,De$famrel,De$paid,De$Fjob3,De$guardian1,De$activities,De$Medu,De$Mjob4,De$absences,De$Fedu,De$studytime)
summary(lm(new_Y~.,Dataframe))



