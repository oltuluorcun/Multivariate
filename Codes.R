## Data preparation
data <- read.table("data.txt", header = TRUE, sep = "\t")
dim(data)
head(data,20)
data.names <- data[,c(2:3,24,25)]
data.num <- data[,c(4:7,9,10,12,13,15:18,20:23,26)]
data.num2 <- data[,-c(2:3,24:25)]
head(data.num)
head(data.names)
dim(data.names);dim(data.num)
data.s <- as.data.frame(round(scale(data.num),4))
data.st <- as.data.frame(cbind(data.names,data.st))
head(data.st)
data.st <- data.frame(data.names, data.s)
head(data.s)
## Correlation
M <- cor(data.s)
corrplot(M, method = "circle")
#corrplot(M, method = "color")
corrplot(M, method = "number")
corrplot(M, type = "upper", tl.pos = "d")
corrplot(M, add = TRUE, type = "lower", col = "Black", method = "number",
         diag = FALSE, tl.pos = "n", cl.pos = "n")
## Standardization Data
data2 <- data[,-c(1,2,3,25,26)]
data3 <- data.frame(round(scale(data2),4))

## Normality test
shapiro_test_df <- function(df, bonf= TRUE, alpha= 0.05) {
  l <- lapply(df, shapiro.test)
  s <- do.call("c", lapply(l, "[[", 1))
  p <- do.call("c", lapply(l, "[[", 2))
  if (bonf == TRUE) {
    sig <- ifelse(p > alpha / length(l), "H0", "Ha")
  } else {
    sig <- ifelse(p > alpha, "H0", "Ha")
  }
  return(list(statistic= s, p.value= p,significance= sig,
              method= ifelse(bonf == TRUE, "Shapiro-Wilks test with Bonferroni Correction",
                             "Shapiro-Wilks test without Bonferroni Correction")))
}
shapiro_test_df(a)
## MVN
mvn(a, subset=NULL, mvnTest = c("mardia"), covariance = TRUE, tol = 1e-25, alpha = 0.5, scale = FALSE,
    desc = TRUE, transform = "sqrt", R = 1000, univariateTest = c("SW"), univariatePlot = "none",
    multivariatePlot = "none", multivariateOutlierMethod = "none",
    showOutliers = FALSE, showNewData = FALSE)
warnings()
result<-mvn(a,mvnTest= "royston") #bunu kullan
#r2
r2=sreg$r.squared
r2
r2adj=sreg$adj.r.squared
r2adj
r2pred=1-PRESS/SST
r2pred
# Simple regression
fit=lm(IR ~ . , data=a)
summary(fit)
## Multicollinearity
vif(fit)
library(mctest)
omcdiag(x,y)
imcdiag(x,y)
## Outlier /Multivariate(?)
mod <- lm(IR ~ ., data=a)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*",col="dodgerblue3", cex=2, main="Influential Obs by Cooks distance") # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="violetred4") # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red") # add labels
outlierTest(fit)
reg = lm(unlist(y)~x)
reg
sreg=summary(reg)
areg=anova(reg)
sreg
areg
SST=sum((areg$'Sum Sq')[1:length(areg$'Sum Sq')])
SST
c.di=cooks.distance(reg) #cooksd
c.di
hii=lm.influence(reg)$hat #leverages
hii
ei=residuals(reg) #Residuals
ei
di=studres(reg) #studentized Residuals
press_res=ei/(1-hii) #press resids
press_res
PRESS=sum(press_res^2) #PRESS score
PRESS
## Constant Variance test
shapiro.test(ei)
bptest(reg)
sigma <- sigma.hat(reg)
residual.plot(reg, ei, sigma)
plot(ei, main="Residual Plot", xlab="Observations",ylab="Residuals", col="palevioletred4",pch=20, cex=1.0)
c.di=as.data.frame(c.di)
mu=mean(ei)
## Multinominal for position
head(data)
subsetMulti <- data[,-c(1:3)]
subsetMulti
mp2 <- vglm(Position ~ APG + BPG + X3PM + DRB + ORB + PF + IR + MPG ,
            data = subsetMulti, family = multinomial, method="vglm.fit")
summary(mp2)
## Factor Analysis
head(data)
data.names <- data[,c(2:3,24,25)]
data.num <- data[,c(4:7,9,10,12,13,15:18,20:23,26)]
data.st <- as.data.frame(round(scale(data.num),4))
head(data.st)
## Evaluating the “factorability” of our data with KMO
KMO(data.st)
data.st.d<-data.st[,-10]
KMO(data.st.d)
## Choosing number of factors to explain matrix.
parallel<- fa.parallel(data.st.d,fm="ML",fa = "fa")
parallel
fit <- fa(data.st.d, nfactors = 3, max.iter = 100, rotate = "varimax", fm = "ML")
fit$communality
fa.diagram(fit)
## Classification On The Basis Of Team ( Final Four Team or Not)
head(data)
dim(mydata)
dim(data)
head(data)
mydata<-data[,-c(1,2,3,25)]
head(mydata)
mydata$GP<-as.numeric(mydata$GP)
sapply(mydata, class)
dim(mydata)
head(mydata)
## FF<-mydata[,21]
data.num<-mydata[,-21]
data.st <- as.data.frame(round(scale(data.num),4))
mydata<-cbind(data.st,FF)
head(mydata)
validation_index <- createDataPartition(mydata$FF, p=0.80, list=FALSE)
## Select 20% of the data for validation
mydata.test <- mydata[-validation_index,]
# Use the remaining 80% of data to training and testing the models
mydata.train <- mydata[validation_index,]
sapply(mydata.train, class)
percentage <- prop.table(table(mydata.train$FF)) * 100
cbind(freq=table(mydata.train$FF), percentage=percentage)
##
df <- data.frame(
  group = c("Final Four", "Not Final Four Team"),
  value = c(25, 71)
)
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie
##
x <- mydata.train[,c(1:21)]
y <- mydata.train[,22]
plot(y)
featurePlot(x=x, y=y, plot="box")
## Density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
## Run algorithms using 10-fold cross validation
control <- tr
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
## a) linear algorithms
set.seed(7)
fit.lda <- train(FF~., data=mydata.train, method="lda", metric=metric, trControl=control)
## b) nonlinear algorithms
## CART
set.seed(7)
fit.cart <- train(FF~., data=mydata.train, method="rpart", metric=metric, trControl=control)
## kNN
set.seed(7)
fit# .knn <- train(FF~., data=mydata.train, method="knn", metric=metric, trControl=control)
## c) advanced algorithms
## SVM
set.seed(7)
fit.svm <- train(FF~., data=mydata.train, method="svmRadial", metric=metric, trControl=control)
## Random Forest
set.seed(7)
fit.rf <- train(FF~., data=mydata.train, method="rf", metric=metric, trControl=control)
summary(results)
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
## Compare accuracy of models
dotplot(results)
## Summarize Best Model
print(fit.knn)
fit.knn$modelInfo
## Estimate skill of KNN on the test dataset
predictions <- predict(fit.cart, mydata.test)
confusionMatrix(predictions, mydata.test$FF)
## Classification On The Basis Of Position
level(mydata$Position)
head(data)
dim(data)
mydata<-data[,-c(1,2,3,24)]
head(data)
head(mydata)
mydata$GP<-as.numeric(mydata$GP)
sapply(mydata, class)
Position<-mydata[,21]
data.num<-mydata[,-21]
data.st <- as.data.frame(round(scale(data.num),4))
mydata<-cbind(data.st,Position)
head(mydata)
validation_index <- createDataPartition(mydata$Position, p=0.80, list=FALSE)
## select 20% of the data for validation
mydata.test <- mydata[-validation_index,]
## use the remaining 80% of data to training and testing the models
mydata.train <- mydata[validation_index,]
percentage <- prop.table(table(mydata$Position)) * 100
cbind(freq=table(mydata$Position), percentage=percentage)
## pie
df <- data.frame(
  group = c("Center", "Forward", "Guard"),
  value = c(18, 49, 52)
)
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie
summary(mydata)
head(mydata)
## plot(y)
x <- mydata.train[,c(1:21)]
y <- mydata.train[,22]
head(mydata.train)
## Density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
## with variables
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
set.seed(7)
fit.lda <- train(Position~., data=mydata.train, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Position~., data=mydata.train, method="rpart", metric=metric, trControl=control)
## kNN
set.seed(7)
fit.knn <- train(Position~., data=mydata.train, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
## SVM
set.seed(7)
fit.svm <- train(Position~., data=mydata.train, method="svmRadial", metric=metric, trControl=control)
## Random Forest
set.seed(7)
fit.rf <- train(Position~., data=mydata.train, method="rf", metric=metric, trControl=control)
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
## Compare accuracy of models
dotplot(results)
## Summarize Best Model
print(fit.rf)
fit.rf$modelInfo
## Estimate skill of RF on the test dataset
predictions <- predict(fit.rf, mydata.test)
confusionMatrix(predictions, mydata.test$Position)
## K-means
data.names <- data[,c(2:3,24,25)]
data.num <- data[,c(4:7,9,10,12,13,15:18,20:23,26)]
data.num2 <- data[,-c(2:3,24:25)]
head(data.num)
head(data.names)
dim(data.names);dim(data.num)
data.s <- as.data.frame(round(scale(data.num),4))
head(data.st)
data.st <- data.frame(data.names, data.s)
head(data.s)
data.num2 <- data.frame(round(scale(data.num2),4))
datadata <- data.frame(data.names, data.num2)
names(datadata)
data.cluster3 <- kmeans(datadata[,-c(1:5)],3)#hepsi
data.cluster3
table(data.cluster3$cluster,datadata$Position)
a3 <- ggplot(datadata, aes(PPG,MPG, colour = as.factor(data.cluster3$cluster), label = Position)) +
  geom_point() + geom_text(aes(label= Position),hjust=0, vjust=0)
print(a3 + scale_colour_manual(values = c("Blue", "Red", "dark green")))
## Multivariate Regression
names(data.st)
set.seed(7)
testNumbers <- sample(1:119,size = 24, replace = FALSE)
testSet <- data.st[testNumbers,-c(1:3)]
trainSet <- data.st[-testNumbers,-c(1:3)]
head(trainSet)
modelMPG <- lm(MPG ~ . ,data=trainSet)
summary(modelMPG)
ols_step_forward_p(modelMPG,details = TRUE)
model2MPG <- lm(MPG ~ FGA + SPG + PF + DRB + APG + X3PM + ORB + Position + X3PA , data = trainSet)
summary(model2MPG)
model3MPG <- lm(MPG ~ FGA + SPG + PF + DRB + APG + X3PM + ORB + Position, data = trainSet)
summary(model3MPG)
## Adequecy check
resid <- model3MPG$residuals
qqnorm(resid);qqline(resid)
shapiro.test(resid)
cd <- cooks.distance(model3MPG)
plot(cd, pch="*", cex=2, main="Influential Obs by Cooks distance") # plot cook's distance
abline(h = 4*mean(cd, na.rm=T), col="red") # add cutoff line
text(x=1:length(cd)+1, y=cd, labels=ifelse(cd>4*mean(cd, na.rm=T),names(cd),""), col="red")
## Constant var
plot(model3MPG,1)
dataNum <- data[,-c(1:3,24,25)]
head(dataNumS)
dataNumS <- data.frame(round(scale(dataNum),4))
dataName <- data[,c(1:3,24,25)]
pcaNew <- princomp(dataNumS)
names(pcaNew)
print(pcaNew)
summary(pcaNew, loadings = TRUE)
biplot(pcaNew)
set.seed(7)
testNumbers <- sample(1:119,size = 24, replace = FALSE)
testSet <- dataNumS[testNumbers,]
trainSet <- dataNumS[-testNumbers,]
set.seed(7)
pcr_model <- pcr(MPG ~ ., data = trainSet, scale = TRUE,ncomp = 5, validation = "CV")
pcr_pred <- predict(pcr_model, testSet, ncomp = 3)
mean((pcr_pred - y_test)^2)
summary(pcr_model)
validationplot(pcr_model, val.type = "MSE")
predplot(pcr_model)
res_pcr <- pcr_model$residuals
shapiro.test(res_pcr)
qqnorm(res_pcr);qqline(res_pcr)
coefs <- pcr_model$coefficients ########ORÇUN BAKACAK BURAYA!!!!
pcr_model$validation
scatter3D(PC1, PC2, PC3, pch = 16,phi=0, bty = "g",ticktype = "detailed")
p <- plot_ly(PC, x = ~PC1, y = ~PC2, z = ~PC3,
             marker = list(color = ~PC4, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Miles/(US) gallon',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
## Create a shareable link to your chart
chart_link = api_create(p, filename="osman")
chart_link
## Cluster Analysis
head(data)
FF <- data[which(data$FF=="F4"),]
data.names.ff <- FF[,c(2:3)]
data.num.ff <- FF[,c(4:7,9,10,12,13,15:18,20:23)]
data.st.ff <- as.data.frame(round(scale(data.num.ff),4))
d <- dist(data.st.ff, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit,labels = data.names.ff[,1], main = "Cluster Dendogram for Final Four" , xlab = "") # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
## Draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")