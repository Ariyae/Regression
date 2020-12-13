#Regression Analysis on challenger dataset

launcher <- read.csv(file.choose(), header= T)
str(launcher)
b<-cov(launcher$temperature, launcher$distress_ct)/var(launcher$temperature)
b
a=mean(launcher$distress_ct)-b*mean(launcher$temperature)
a
p<-cov(launcher$temperature,launcher$distress_ct)/(sd(launcher$temperature)*sd(launcher$distress_ct))
p #correlation

#another way of correlation
cor<-cor(launcher$temperature,launcher$distress_ct)
cor

#Multiple linear regression
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}

View(launcher)
reg(y = launcher$distress_ct, x = launcher[2])
reg(y = launcher$distress_ct, x = launcher[2:4])
