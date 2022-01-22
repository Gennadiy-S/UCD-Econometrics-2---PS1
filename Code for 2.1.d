
> dag4<-dag.init(outcome = NULL, exposure = NULL, covs = c(1, 1, 2), arcs=c(0, 1, 0, 2, 1, 2, 2, -1, 1, -1, 3, 2, 3, -1), symbols=c("F","D", "O"), x.name = "Female", cov.names = c("Discrimination", "Occupation", "Ability"), y.name = "Earnings")
> dag.draw(dag4, numbering=FALSE);

#This is for dagR package to see the relationships mapped out, testing it. 


n <- 1000
A <- rnorm(n)
F<-ifelse(runif(n)>0.5,1,0)
D<-F
O<-ifelse(A>median(A)&F<0.5,1,0)
Y<- -1*D+0.66*A+O+rnorm(n)
 dataset <- data.frame(Y=Y, A=A, O=O,D=D)
save(dataset,file="nb2.RData")
 model1 <- lm(Y ~ D, data = dataset)
summary(model1)

save(dataset,file="nb2.RData")
model1 <- lm(Y ~ D, data = dataset)
summary(model1)

model2 <- lm(Y ~ D + O, data = dataset)
summary(model2)

model3 <- lm(Y ~ D + O +A, data = dataset)
summary(model3)

model4 <- lm(Y ~ D +A, data = dataset)
summary(model4)

