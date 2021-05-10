library(ggplot2)          # ggplot()
library(caret)            # createDataPartition() / train()

# 读取数据
voice <- read.csv('C://Users//friendship-W//Desktop//voice.csv')

str(voice)# 查看数据集的基本数据结构
sum(is.na(voice))# 查看数据的缺失状况

cn <- colnames(voice[-21]) # 统计数据的列名(去除最后一列)
colnames(voice)
## 绘制分组密度曲线图
gp<-lapply(cn, function(x) {#最后生成结果为列表
      ggplot(data = voice, aes(x = eval(parse(text = x)), fill = label)) + #x轴的数据就是每一列的数据,填充颜色根据男女的性别来填充
      geom_density(alpha = 0.3) +   # 绘制密度曲线的函数，alpha控制填充色的透明度
      xlab(x) +                     # 设置x轴标签
      ggtitle(paste(x, "density", sep= " "))}) # 设置每张图片的标题（列名+density）,paste连接字符串的函数，
library(Rmisc)            # multiplot()
multiplot(gp[[1]], gp[[2]], gp[[3]], gp[[4]], cols = 2)  # 展示前4张图片
multiplot(gp[[5]], gp[[6]], gp[[7]], gp[[8]], cols = 2)
multiplot(gp[[9]], gp[[10]], gp[[11]], gp[[12]], cols = 2)
multiplot(gp[[13]], gp[[14]], gp[[15]], gp[[16]], cols = 2)
multiplot(gp[[17]], gp[[18]], gp[[19]], gp[[20]], cols = 2)

set.seed(1023) # 设定随机种子
# 按照性别以7:3的比例进行分层抽样，并且以矩阵的形式返回索引
index <- createDataPartition(voice$label, p = 0.7, list = FALSE)
traindata <- voice[index, ] # 创建训练集
testdata <- voice[-index, ] # 创建测试集
table(voice$label)#查看数据
table(traindata$label)
table(testdata$label)

#决策树C50
library(C50)
C50_tree<-C5.0(traindata[-21],traindata$label)
plot(C50_tree)
C50_predict<-predict(C50_tree,testdata)#在测试集上预测
C50.table<-table(actual=testdata$label,predict=C50_predict)
C50.table
C50_ratio<-sum(diag(C50.table))/sum(C50.table)
C50_ratio
#优化
x<-C5.0Control(subset =F,CF=0.25,winnow=T,noGlobalPruning=F,minCases =10)
c50_tree1=C5.0(label~.,traindata,trials=5,control =x)
summary(c50_tree1)
plot(c50_tree1,uniform=T,branch=0.1,margin=0.2,main= "Classification",compress=T,type="simple")
C50_predict<-predict(c50_tree1,testdata)#在测试集上预测
C50.table1<-table(actual=testdata$label,predict=C50_predict)
C50.table1
C50_ratio1<-sum(diag(C50.table1))/sum(C50.table1)
C50_ratio1

# #朴素贝叶斯(不需要把最后一列转化为数值型)
# library(e1071)
# # library(gmodels)
# nb.model <- naiveBayes(label~.,data = traindata)
# #预测结果
# nb_predict <- predict(nb.model,newdata =testdata)
# #生成实际与预测交叉表和预测精度
# nb.table <- table(actual=testdata$label,predict=nb_predict)
# nb_ratio <- sum(diag(nb.table))/sum(nb.table)
# #分析结果
# nb.table
# nb_ratio
# # #朴素贝叶斯优化
# # nb.model2 <- naiveBayes(label~.,data = traindata, laplace = 1) 
# # nb_pred2 <- predict(nb.model2, testdata)
# # nb.table <- table(actual=testdata$label,predict=nb_pred2)
# # nb_ratio <- sum(diag(nb.table))/sum(nb.table)
# # nb.table
# # nb_ratio


#标准化
library(caret)
standard<-preProcess(voice,method='range')
voice1<-predict(standard,voice)
train1<-voice1[index,]
test1<-voice1[-index,]

#KNN预测，确定k的值
library(class)
results=c() 
for(i in 3:10) {  #一般而言,k在3到10之间取值
  set.seed(1500)
  pred_knn<-knn(train1[-21],test1[-21],train1$label,i)
  Table<-table(pred_knn,testdata$label)
  accuracy<-sum(diag(Table))/sum(Table) 
  results<-c(results,accuracy)
}
plot(x=3:10,y=results,type='b',col='blue',xlab='k',ylab='accuracy')
#KNN建模
set.seed(1500)
pred_knn<-knn(train=train1[-21],test= test1[-21],cl=train1$label, k=7)
knn.table<-table(actual=test1$label,predict=pred_knn)
knn.table
knn_ratio<-sum(diag(knn.table))/sum(knn.table)
knn_ratio
##比较几种算法
# #KNN和朴素贝叶斯比较
# pred_results<-data.frame(knn=pred_knn,nb=nb_predict,testdata$label)
# index2<-which(pred_results$knn!=nb_predict) 
# pred_results[index2, ]#展示不同的预测值
#KNN和C50比较
pred_results<-data.frame(knn=pred_knn,C50=C50_predict,testdata$label)
index3<-which(pred_results$knn!=C50_predict) 
pred_results[index3, ]#展示不同的预测值