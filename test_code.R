#相关系数图 
library(PerformanceAnalytics) 
library(corrplot) 
 
x=read.table('clipboard',header=T);x 
chart.Correlation(x, histogram=TRUE, pch=19) 
corrplot.mixed(cor(x,method="pearson"),lower.col="black", number.cex= .7,mar = c(2,2,3,2)) 
 
#随机森林特征重要性排序 
wine = read.table("clipboard",header=T);wine 
#将数据集分为训练集和测试集,比例为7:3 
train_sub = sample(nrow(wine),7/10*nrow(wine)) 
train_data = wine[train_sub,] 
test_data = wine[-train_sub,] 
library(pROC) #绘制ROC曲线 
library(randomForest) 
#随机森林 
wine_randomforest <- randomForest(yt~x1+x2+x3+x4+x5+x6+x7+x8+x9, 
                                    data = train_data, 
                                    ntree =500, 
                                    mtry=3, 
                                    importance=TRUE , 
                                    proximity=TRUE) 
#查看变量的重要性 
wine_randomforest$importance 
varImpPlot(wine_randomforest, main = "variable importance",font = 2,lwd = 5,col = "red",col.axis = "blue",font.axis = 2,cex = 1,cex.axis = 5,pch = 16,cex.lab = 0.7,font.lab = 2) 
 
#arima模型 
library(tseries) 
library(forecast) 
library(fUnitRoots) 
library(fGarch) 
y1=read.table("clipboard") 
head(y1) 
na.omit(y1) 
y11<-ts(y1,start=c(2009,1),end=c(2021,12),frequency=12) 
plot(y11,type="l",xlab="时间",ylab="小麦现货价格") 
ts.plot(diff(y11)) 
acf(y11,main="自相关图")#拖尾 
pacf(y11,main="偏自相关图")#拖尾 
trainy11<-ts(as.vector(y11[1:148]),frequency=12,start=c(2009,1)) 
#看拆掉之后的训练集图形 
plot(trainy11,type="l",xlab="时间",ylab="小麦现货价格") 
acf(trainy11,main="自相关图")  #画出自相关图 
pacf(trainy11,main="偏自相关图")   #画出偏自相关图 
tsdisplay(trainy11) 
adf.test(trainy11)#接受原假设，存在单位根 
s1<-diff(trainy11,1) 
plot(s1,type="l",xlab="时间",ylab="一阶差分结果") 
acf(s1,main="自相关图")  #画出自相关图 
pacf(s1,main="偏自相关图")   #画出偏自相关图 
#进一步使用adf检验，看一下是否存在单位根（验证平稳性） 
adf.test(s1)#拒绝原假设，序列平稳 
tsdisplay(s1) 
 
a1=arima(trainy11,order=c(2,1,1),method="ML") 
a2=arima(trainy11,order=c(12,1,1),fixed=c(NA,NA,0,0,0,0,0,0,0,0,0,NA,NA),transform.pars = FALSE)#AIC=907.17 
res2<-a2$residuals^2 
tsdiag(a2) 
Box.test(a2$residuals, type="Ljung-Box",lag=12)#接受h0 
Box.test(res2, type="Ljung-Box",lag=12)#拒绝H0 
#然后使用tsdiag看一下各自的结果，图中表明残差标准差基本都在[-1,1]之间， 
#残差的自回归都为0（两虚线内），Ljung-Box检验的p值都在0.05之上，结果不错。 
qqnorm(a2$residuals) 
qqline(a2$residuals) 
 
fore2<-forecast(a2,h=5,level=c(99.5)) 
plot(fore2,ylim=c(1700,3000)) 
lines(fore2$fitted,col="green") 
lines(y11,col="red") 
 
#神经网络 
library(neuralnet) 
options(digits=3) 
x=read.table("clipboard",header=T);x 
maxs=apply(x,2,max);maxs 
maxs[1] 
mins=apply(x,2,min);mins 
mins[1] 
m=(maxs-mins)[1];m 
x1=as.data.frame(scale(x,center=mins,scale=maxs-mins));x1 
attach(x1) 
train.cse=x1[1:146,];train.cse 
test.cse=x1[147:151,];test.cse 
 
#滞后1期 
cse.net=neuralnet(yt~yt.1+ yt.12+x1+x2+x4+x5+x7+x8,data=train.cse,hidden=c(5,4), 
threshold=0.01,act.fct="tanh",linear.output=T) 
#滞后2期 
cse.net=neuralnet(yt~yt.1+yt.2+ yt.12+x1+x2+x4+x5+x7+x8,data=train.cse,hidden=c(5,4), 
threshold=0.01,act.fct="tanh",linear.output=T) 
#滞后3期 
cse.net=neuralnet(yt~yt.1+yt.2+yt.3+ yt.12+x1+x2+x4+x5+x7+x8,data=train.cse,hidden=c(5,4), 
threshold=0.01,act.fct="tanh",linear.output=T) 
#neuralnet包的误差函数默认为sse，没有mse，本次研究只有1个输出神经元，可以理解为mse 
#单变量预测 
cse.net=neuralnet(yt~yt.1+yt.2+yt.3+yt.12,data=train.cse,hidden=c(5,4), 
threshold=0.01,act.fct="tanh",linear.output=T) 
ythat=cse.net$net.result[[1]]*m+mins[1];ythat 
plot(cse.net) 
 
rmse=function(x,y){sqrt(mean((x-y)^2))} 
tyt=train.cse$yt*m+mins[1];tyt 
rmse(ythat,tyt) 
 
#测试集 
ythat2=compute(cse.net,test.cse[,2:10])$net.result*m+mins[1];ythat2 
tyt2=test.cse$yt*m+mins[1];tyt2 
rmse(ythat2,tyt2) 
 
#将数据结果写入 
write.table(ythat[,1], file = "C:\\Users\\mujin1922\\Desktop\\Rresult1", append = FALSE, quote = TRUE, sep = " ", 
eol = "\n", na = "NA", dec = ".", row.names = TRUE, 
col.names = TRUE, qmethod = c("escape", "double"),fileEncoding = "")
