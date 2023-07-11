#library(lineNotify)
#Sys.setenv(LINE_API_TOKEN="ymFJerKZVmmypTAshhIlTpNuJPlDLP5rocVJfdWXAYe")

# Function 5*2 CV return Trainset position
repCV=function(fold, ntest=2, nfold=5){
  combine=combn(nfold,ntest)
  times=ncol(combine)
  CV=NULL
  for(i in 1:times){
    temp=NULL
    train=setdiff(c(1:nfold),combine[,i])
    for(j in 1:length(train)){
      temp=c(temp,fold[[train[j]]])
    }
    CV[[i]]=temp
  }
  return(CV)
}

# Create train and valid dataset, train dataset will be devided by 5*2 CV
# valid dataset scaled
CV10val=function(data,seed=5){
  data_test=NULL
  data_df=NULL
  data_cl=NULL
  n=nrow(data)
  f=ncol(data)
  colnames(data)=paste("v",1:ncol(data),sep="")
  set.seed(seed)
  validfold=createFolds(y=data[,f],k=10)
  valid=validfold[[1]]
  data_test=as.data.frame(data[valid,])
  data_df=data[-valid,]
  data_cl=data_df[,f]
  data_df=data_df[,-f]
  #scale test set by training+validation sets' mean and sd
  mean=colMeans(data_df); sd=apply(as.matrix(data_df), 2, sd)
  data_test[,-f]=t((t(data_test[,-f])-mean)/sd)
  if(sum(is.na(data_test))!=0){
    data_test[is.na(data_test)]=0
  }
  #scale
  data_df=scale(data_df)
  if(sum(is.na(data_df))!=0){
    data_df=if.na(data_df)
    dfname=colnames(data_df)
    data_test=cbind.data.frame(data_test[,dfname],data_test[,ncol(data_test)])
    }
  set.seed(seed)
  fold_data=createFolds(y=data_cl,k=5)
  train_data=repCV(fold_data)
  return(list(data_df,data_cl,data_test,train_data))
}

# 5*2Cv can run multiple times by this function
CV100=function(data,seed=5,test.num = 1){
  data_test=NULL
  data_df=NULL
  data_cl=NULL
  n=nrow(data)
  f=ncol(data)
  colnames(data)=paste("v",1:ncol(data),sep="")
  set.seed(seed)
  testfold=createFolds(y=data[,f],k=10)
  # test.num is the sequence number of test set
  test=testfold[[test.num]]

  data_test=as.data.frame(data[test,])
  data_df=data[-test,]
  data_cl=data_df[,f]
  data_df=data_df[,-f]
  #scale test set by training+validation sets' mean and sd
  mean=colMeans(data_df); sd=apply(as.matrix(data_df), 2, sd)
  data_test[,-f]=t((t(data_test[,-f])-mean)/sd)
  if(sum(is.na(data_test))!=0){
    data_test[is.na(data_test)]=0
  }
  #scale
  data_df=scale(data_df)
  if(sum(is.na(data_df))!=0){
    data_df=if.na(data_df)
    dfname=colnames(data_df)
    data_test=cbind.data.frame(data_test[,dfname],data_test[,ncol(data_test)])
    }
  set.seed(seed)
  fold_data=createFolds(y=data_cl,k=5)
  train_data=repCV(fold_data)
  return(list(data_df,data_cl,data_test,train_data))
}

#Bootstrap method
CV.boots=function(data,seed=5,test.num){
  data_test=NULL
  data_df=NULL
  data_cl=NULL
  n=nrow(data)
  f=ncol(data)
  colnames(data)=paste("v",1:ncol(data),sep="")
  set.seed(seed)
  testfold=createFolds(y=data[,f],k=5)
  # test.num is the sequence number of test set
  test=testfold[[test.num]]

  data_test=as.data.frame(data[test,])
  data_df=data[-test,]
  data_cl=data_df[,f]
  data_df=data_df[,-f]
  #scale test set by training+validation sets' mean and sd
  mean=colMeans(data_df); sd=apply(as.matrix(data_df), 2, sd)
  data_test[,-f]=t((t(data_test[,-f])-mean)/sd)
  if(sum(is.na(data_test))!=0){
    data_test[is.na(data_test)]=0
  }
  #scale
  data_df=scale(data_df)
  if(sum(is.na(data_df))!=0){
    data_df=if.na(data_df)
    dfname=colnames(data_df)
    data_test=cbind.data.frame(data_test[,dfname],data_test[,ncol(data_test)])
    }
  ## Bootstrap for train data
  set.seed(seed)
  train.Fold = createBootstrapFold(data_cl, sample.size = round(nrow(data_df)), k=10)
  return(list(data_df,data_cl,data_test,train.Fold))
}

#Bootstrap
##Output position code of each class
createBootstrapFold.for.class = function(label, sample.size){
  n.class = nlevels(label)
  class.code = NULL
  class.percent = NULL
  class.Fold = NULL
  for(i in 1:n.class){
    class.code[[i]] = which(label==levels(label)[i])
    class.percent[i] = length(class.code[[i]])/length(label)
    class.Fold[[i]] = sample(class.code[[i]], size = round(sample.size*class.percent[i]), replace = TRUE)
  }
  return(unlist(class.Fold))
}
##Create k bootstrap samples
createBootstrapFold = function(label, sample.size, k){
  boots.Fold = NULL
  for(i in 1:k){
    boots.Fold[[i]] = createBootstrapFold.for.class(label = label, sample.size = sample.size)
  }
  return(boots.Fold)
}





# Function: Delete Missing value after scale
if.na=function(data){
  if(sum(is.na(data))!=0){
    na=which(!complete.cases(t(data)))
    data=data[,-c(na)]
  }
  if(sum(is.na(data))==0)return(data)
  else print("There still have missing values")
}

#within and between classes sum of squares
#input dimension reduced dataset and the labels of training set
#this function output the within class sum square and between classes sum square
class.dist=function(data,labels){
  class=list()
  dims=ncol(data)
  WSS=NULL; level=nlevels(labels)
  for(i in 1:level){
    c=which(labels==(levels(labels)[i]))
    class[[i]]=as.matrix(data[c,])
    y=class[[i]]; mean=NULL; WSS[i]=0
    for(k in 1:ncol(class[[i]]))mean[k]=mean(class[[i]][,k])
    for(j in 1:nrow(class[[i]]))WSS[i]=WSS[i]+sum((y[j,]-mean)^2)
    WSS[i]=WSS[i]/(nrow(class[[i]])*dims)
  }
  BSS=matrix(NA,ncol=level,nrow=level-1)
  for(i in 1:(level-1)){
    temp=NULL;j=i
    while(j<level){
      j=j+1
      dis=matrix(NA,nrow=nrow(class[[i]]),ncol=nrow(class[[j]]))
      for(k in 1:nrow(class[[i]])){
        for(l in 1:nrow(class[[j]])){
          temp=rbind(class[[i]][k,],class[[j]][l,])
          dis[k,l]=as.numeric(dist(temp))^2
        }
      }
      BSS[i,j]=sum(dis)/(nrow(class[[i]])*nrow(class[[j]])*dims)
    }
  }
  BSS.total=sum(BSS,na.rm=TRUE)/choose(level,2)
  return(mean(WSS)/BSS.total)
}

#NN result exchange to classification result
class.result=function(pre,cl){
  result=NULL
  level=levels(cl)
  if(is.matrix(pre)&&ncol(pre)>1){
    for(i in 1:nrow(pre)){
      result[i]=level[which.max(pre[i,])]
      }
    result=factor(result, levels=level)
    return(result)
  }
  else{
    #means pre is bionomial log regression
    for(i in 1:length(pre)){
      result[i]=
      ifelse(pre[i]<0.5,level[1],level[2])
    }
    result=factor(result, levels=level)
    return(result)
  }
}

# Rprop+ only CV function
Rprop_all=function(data,seed=5){
  NN_ALL=NULL
  set.seed(seed)
  label=data[,ncol(data)]
  data=data[,-ncol(data)]
  test.fold=createFolds(y=label,k=10)
  for(i in 1:10){
    test.code=test.fold[[i]]
    train.data=data[-test.code,]
    test.data=data[test.code,]
    train.data=as.data.frame(cbind(label[-test.code],train.data))
    train.data[,1]=as.factor(train.data[,1])
    names(train.data)[1]="label"
    test.data=as.data.frame(test.data)
    hn=10
    hn1=round(0.6*hn); hn2=round(0.4*hn)
    nn.fit=neuralnet(label~.,data=train.data,hidden=c(hn1,hn2))
    nn.pre=predict(nn.fit,newdata=test.data)
    Score=score(nn.pre,label[test.code])
    NN_ALL=rbind(NN_ALL,Score)
  }
  return(NN_ALL)
}

# Logistic regression model for all gene (without gene select)
LogitAll=function(df,cl,train){
  Log=NULL
  for(k in 1:10){
    df_test=df[-train[[k]],]
    df_train=df[train[[k]],]
    cl_train=as.factor(cl[train[[k]]])
    cl_test=as.factor(cl[-train[[k]]])
    cl_train=as.factor(cl_train)
    data=as.data.frame(cbind(cl_train,df_train))
    data[,1]=as.factor(data[,1])
    names(data)[1]="label"
    newdata=as.data.frame(df_test)
    log.fit=multinom(label~., data=data, trace=FALSE)
    log.pre=predict(log.fit,newdata=newdata)
    log.pre=prelevel.exc(log.pre, cl_test)
    Acc=length(which(log.pre==cl_test))/length(cl_test)
    Log=c(Log,Acc)
    }
  return(Log)
}

# Evaluation metrics
# Macro F1 and micro F1 score function, for binary and multi-class
f1_score <- function(predicted, expected, positive.class="M") {
    #predicted <- factor(as.character(predicted), levels=unique(as.character(expected)))
    expected  <- as.factor(expected)
    cm = as.matrix(table(expected, predicted))

    precision <- diag(cm) / colSums(cm)
    recall <- diag(cm) / rowSums(cm)
    f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))

    #Assuming that F1 is zero when it's not possible compute it
    f1[is.na(f1)] <- 0

    #Binary F1
    if(positive.class == "1")return(as.numeric(f1[1]))
    #Multi-class macro-averaged F1
    if(positive.class == "M")return(mean(f1))
}

oneVsAll = function(cm,i){
    v = c(cm[i,i], rowSums(cm)[i] - cm[i,i], colSums(cm)[i] - cm[i,i], sum(cm)-rowSums(cm)[i] - colSums(cm)[i] + cm[i,i])
  return(matrix(v, nrow = 2, byrow = T))
}

f1micro_score=function(cm){
  s = matrix(0, nrow = 2, ncol = 2)
 for(i in 1 : nrow(cm)){s = s + oneVsAll(cm,i)}
  avgAccuracy = sum(diag(s)) / sum(s)
  #Because s is symmetric matrix,micro precison, micro recall and micro F1 are same
  micro_prf = (diag(s) / apply(s,1, sum))[1]
  return(c(avgAccuracy,micro_prf))
}
# Integrate scores in one function
# predicted is a probability matrix
# Be care if the predict classifier outputs labels!
score = function(predicted, expected, preprob=NULL,Method=NULL,positive.class="M"){
  #if(Method=="RF"){
   # prelabel=predicted
   # preprob=preprob
  #}
  expected=as.factor(as.numeric(expected))
  if(is.matrix(predicted)){
    if(ncol(predicted)==1){
      preprob=cbind(1-predicted,predicted)
      prelabel=class.result(predicted,expected)
    }
    else{
      preprob=predicted
      prelabel=class.result(predicted,expected)
    }
  }
  if(is.factor(predicted)){
    prelabel=predicted
    preprob=class.ind(predicted)
  }
  if(levels(prelabel)[1]!=levels(expected)[1]){
    print("levels is different!")
    break
  }
  #part 1
  #macrof1=f1_score(prelabel, expected, positive.class)
  cm = as.matrix(table(expected, prelabel))
  temp=f1micro_score(cm)
  #microf1=temp[2]
  avgAcc=temp[1]
  #kappa=kappa2(as.matrix(cbind(prelabel, expected)))
  #kappa.v=kappa[[5]]
  #kappa.p=kappa[[8]]
  #AUC part
  labelpre.matrix=cbind(class.ind(expected),preprob)
  colnames(labelpre.matrix)=c(paste("C",1:nlevels(expected),"_true",sep=""),paste("C",1:nlevels(expected),"_pred_m",sep=""))
  ROC=multi_roc(labelpre.matrix)$AUC$m
  #return(c("macrof1"=macrof1,"microf1"=microf1,"avgAcc"=avgAcc,"Kappa value"=kappa.v,"Kappa pvalue"=kappa.p,unlist(ROC)))
  return(c("avgAcc"=avgAcc,unlist(ROC)[c("macro","micro")]))
}

# Logistic regression model for CV function after feature selection
# mutinom
# ga_select is a list
Logit=function(df,cl,train,ga_select){
  Log=NULL
  ga_select=ga_select[[1]]
  for(k in 1:10){
    df_test=df[-train[[k]],]
    df_train=df[train[[k]],]
    cl_train=as.factor(cl[train[[k]]])
    cl_test=as.factor(cl[-train[[k]]])
    cl_train=as.factor(cl_train)
    data=as.data.frame(cbind(cl_train,df_train[,ga_select[k,]]))
    data[,1]=as.factor(data[,1])
    names(data)[1]="label"
    newdata=as.data.frame(df_test[,ga_select[k,]])
    log.fit=multinom(label~., data=data, trace=FALSE)
    log.pre=as.matrix(predict(log.fit,newdata=newdata,type="probs"))
    Score=score(log.pre,cl_test)
    Log=rbind(Log,Score)
    }
  return(Log)
}


# Extract selected genes
# return a list, because length of each fold is different
selected.gene=function(GANN,k=10){
  select=list()
  for(i in 1:k){
    select[[i]]=GANN[[i]][[2]]
  }
  select_matrix=NULL
  for(i in 1:10){
    select_matrix=rbind(select_matrix,t(as.matrix(select[[i]])))
  }
  Freq=as.data.frame(table(select_matrix))
  bestgene=Freq[which(Freq[,2]>1),1]
  return(list(select_matrix,bestgene))
}

# Output selected gene names by gene number
# Output:
# list1,Freq dataframe, names and frequences correspondingly
# list2, bestgene, frequence>=2 genes' names
getname=function(select,genename){
  names=NULL
  Freq=as.data.frame(table(select[[1]]))
  names=genename[Freq[,1]]
  Freq[,1]=names
  bestgene=names[select[[2]]]
  return(list(Freq,bestgene))
}

# Output multiple ggplot graphs on one page
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Output manifold plot
#1. Iso/LLE plot for all genes
#2. Iso/LLE plot for all selected genes
#3. Iso/LLE plot for bestgenes
#4. heatmap for selected genes
#3 graphs in one plot for ccomparsion

# GA search prograss
gaplot=function(GAresult){
  par(mfrow=c(2,5));
  for(i in 1:10){
    plot(GAresult[[i]][[1]],main=paste("Fold",i,"GA Search"),cex.main=0.9)
  }
}

### Valid function, 2nd evaluation way for Log
#valid Logistic
test.log=function(df,cl,valid,select){
  #selectgene=as.numeric(factor(levels(select[[2]]),levels = c(1:ncol(df))))
  bestgene=as.numeric(factor(select[[2]],levels = c(1:ncol(df))))
  df_test=valid[,-ncol(valid)]
  df_test=df_test[,bestgene]
  cl_test=as.factor(valid[,ncol(valid)])
  df_train=df[,bestgene]
  cl_train=cl
  data=as.data.frame(cbind(cl_train,df_train))
  data[,1]=as.factor(data[,1])
  names(data)[1]="label"
  newdata=as.data.frame(df_test)
  log.fit=multinom(label~., data=data, trace=FALSE)
  log.pre=as.matrix(predict(log.fit,newdata=newdata,type="probs"))
  Score=score(log.pre,cl_test)
  return(Score)
}

# valid for Rprop+
test.nn=function(df,cl,valid,select){
  #selectgene=as.numeric(factor(levels(select[[2]]),levels = c(1:ncol(df))))
  bestgene=as.numeric(factor(select[[2]],levels = c(1:ncol(df))))
  df_test=valid[,-ncol(valid)]
  if(ncol(df_test)!=ncol(df)){
    diff=setdiff(colnames(df_test),colnames(df))
    df_test=df_test[,!colnames(df_test) %in% diff]
  }
  df_test=df_test[,bestgene]
  cl_test=as.factor(valid[,ncol(valid)])
  df_train=df[,bestgene]
  cl_train=cl
  data=as.data.frame(cbind(cl_train,df_train))
  data[,1]=as.factor(data[,1])
  names(data)[1]="label"
  newdata=as.data.frame(df_test)
  hn=length(bestgene)
  hn1=round(0.6*hn); hn2=round(0.4*hn)
  nn.fit=neuralnet(label~.,data=data,hidden=c(hn1,hn2,nlevels(cl)))
  nn.pre=predict(nn.fit,newdata=newdata)
  Score=score(nn.pre,cl_test)
  return(Score)
}

# All metrics' 10fold wilcox test result correspondingly
getmean=function(result,metric){
  result1=result[1:10,]
  result2=result[11:20,]
  result3=result[21:30,]
  return(c(mean(result1[,metric]),mean(result2[,metric]),mean(result3[,metric])))
}
getwilcox=function(result){
  #paired=T so is not Mann-Whitney test
  #alternative hypothesis is "greater"
  model1=result[1:10,]
  model2=result[11:20,]
  model3=result[21:30,]
  #Isomap p1 LLe p2
  p1=NULL; p2=NULL
  for(i in 1:ncol(result)){
    options(warn=-1)
    p1=c(p1,wilcox.test(model1[,i],model3[,i],alternative = "greater",paired = TRUE)$p.value)
    options(warn=-1)
    p2=c(p2,wilcox.test(model2[,i],model3[,i],alternative = "greater",paired = TRUE)$p.value)
  }
  return(rbind(p1,p2))
}

# Grid search for LLE's dim and k value
# use DBIndex
k.range=c(5:20)

# Tunning
# For each model
# UMAP is supervised, but no CV, init = spca
# DBIndex as evaluation
tuning=function(df,cl,k.range,modelname){
  n=length(k.range)
  index=NULL
  intrisic.dim=NULL
  #Intrinsic dimension estimate
  for(i in 1:10){
    random_sub = sample(c(1:ncol(df)),size=30)
    intrisic.dim[i] = lbmle(df[,random_sub],k1=20,k2=30)
  }
  intrisic.dim = round(mean(intrisic.dim))
  if(modelname=="Isomap"){
    for(i in 1:n){
    fit.Iso=Isomap.silent(df,dim=intrisic.dim,k=k.range[i])
    index[i]=DBIndex(fit.Iso[[1]],labels=cl)
    }
    }
  optparam=which.min(index)
  return(list("Optk"=k.range[optparam],"Optdim"=intrisic.dim))
}

# GA kofnGA
# Objective function for kofnGA, v is pos
ObjFun_kofn=function(v,data,labels,method,opdim=NULL,opk=NULL){
  sub.data=as.matrix(data[,v])
  if(method=="Isomap"){
    map.fit=Isomap.silent(data=sub.data,dims=opdim,k=opk,verbose=FALSE)
    map.fit=map.fit[[1]]
  }
  if(method=="LLE"){
    map.fit=LLE(sub.data,dim=opdim,k=opk)
  }
  #if(method=="UMAP"){
  #  map.fit=umap(sub.data,y=labels,n_neighbors=neighbors,n_components=components,learning_rate =0.5,init="spca", n_epochs = 200)
  #}
  if(method=="Dist"){
   map.fit=sub.data
  }
  if(method=="MDS"){
    map.dist=stats::dist(sub.data)
    map.fit=stats::cmdscale(map.dist,k=opdim)
    # In cmdscale(), k is the maximum dimension of the space which the data are to be represented in.
  }
  return(DBIndex(map.fit,labels))
}
#Valid top 10 best individual of GA, and use the best one
kofn_mani_2=function(df, cl, train, method, ga.k, opdim=NULL, opk=NULL, ga.mutation, control, Grid){
  GANN_kofn=list()
  for(k in 1:10){
    df_train=NULL; df_test=NULL; cl_train=NULL; cl_test=NULL
    df_train=df[train[[k]],]
    cl_train=as.factor(cl[train[[k]]])
    test.code=setdiff(seq(1:nrow(df)),train[[k]])
    df_test=df[test.code,]
    cl_test=as.factor(cl[test.code])
    ga.fit=NULL
    ga.fit=kofnGA(data=df_train, n=ncol(df_train), k=ga.k, OF=ObjFun_kofn,
                  ngen=100, labels=cl_train, method=method, opdim=opdim, opk=opk, mutprob = ga.mutation, keepbest=5)
    ga.top10=ga.fit[["pop"]][1:10,]
    ga.top10.best=ga_top10_valid(ga.top10, k=k, df_train, df_test, cl_train, cl_test, control, Grid)
    ga.select=ga.top10[ga.top10.best[1],]
    GANN_kofn[[k]]=list("GA.model"=ga.fit,"bestsol"=ga.select,"bestsol.Acc"=ga.top10.best["bestAcc"])
  }
  return(GANN_kofn)
}
#Valid each top10 individual, and return the best one's code
ga_top10_valid=function(ga.top10, k, df_train, df_test, cl_train, cl_test, control, Grid){
  Acc=NULL
  for(i in 1:10){
    gene.subset=ga.top10[i,]
    data=as.data.frame(cbind(cl_train,df_train[,gene.subset]))
    data[,1]=as.factor(data[,1])
    names(data)[1]="label"
    newdata=as.data.frame(df_test[,gene.subset])
    model.tune <-train(label~.,data=data,trace=F,
                       method="svmRadial",
                       trControl = control,
                       tuneGrid = Grid )
    if(var(model.tune$results$Accuracy)==0){
      print("tuning not valid!!!")
      notify_msg(paste(k,"tuning not valid!!!"))
    }
    ksvm.fit <- ksvm(y=cl_train,x=as.matrix(data[,-1]),
                     kernel = "rbfdot",
                     kpar = list(sigma = model.tune$bestTune$sigma),
                     C=model.tune$bestTune$C)
    svm.pre=predict(ksvm.fit,newdata)
    Acc[i]=diag(table(svm.pre, cl_test))/length(cl_test)
    }
    bestsol=which.max(Acc)
    return(c(bestsol,"bestAcc"=Acc[bestsol]))
}


# According Information gain to delete noise genes
dnoise <- function(df,cl){
  info=information_gain(x=df,y=cl)
  noise=which(info[,2]==0)
  newdf=df[,-noise]
  return(cbind.data.frame(newdf,cl))
}

write.name <- function(select1,select2,select3,names,dataname,fileadd){
  write.table(getname(select1,names)[[2]],file = fileadd,col.names = paste(dataname,"Iso"))
  write.table(getname(select2,names)[[2]],file = fileadd,col.names = paste(dataname,"LLE"),append = TRUE)
  write.table(getname(select3,names)[[2]],file = fileadd,col.names = paste(dataname,"DB"),append = TRUE)
}

write.test.result <- function(test_result,fileadd){
  write.table(as.character(c(substitute(test_result))),file = fileadd,col.names = FALSE,row.names = FALSE,sep = ",",
              append = TRUE, qmethod = "double")
  write.table(test_result,col.names = TRUE,row.names = FALSE,sep = ",", qmethod = "double",
              file=fileadd,append = TRUE)
}

# SVM with grid search tuning
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats =2)
tGrid       <-  expand.grid(sigma=(0.1:10)*0.01, C= (1:10)*1)
SVM=function(data, newdata, cl_train, cl_test, control, Grid){
    model.tune <-caret::train(label~.,data=data,trace=F,
                   method="svmRadial",
                   trControl = control,
                   tuneGrid = Grid )
    if(var(model.tune$results$Accuracy)==0){
      print("tuning not valid!!!")
    }
    ksvm.fit <- kernlab::ksvm(y=cl_train,x=as.matrix(data[,-1]),
                   kernel = "rbfdot",
                   kpar = list(sigma = model.tune$bestTune$sigma),
                   C=model.tune$bestTune$C, prob.model=TRUE)
    svm.pre=kernlab::predict(ksvm.fit,newdata,type="probabilities")
    return(score(svm.pre, cl_test))
}

# valid for SVM
test.svm=function(df,cl,valid,select,control = fitControl, Grid = tGrid){
  #selectgene=as.numeric(factor(levels(select[[2]]),levels = c(1:ncol(df))))
  #Freq=as.data.frame(table(select[[1]]))
  #bestgene=Freq[which(Freq[,2]>2),1]
  #bestgene=as.numeric(factor(bestgene,levels = c(1:ncol(df))))
  bestgene=as.numeric(factor(select[[2]],levels = c(1:ncol(df))))
  df_test=valid[,-ncol(valid)]
  if(ncol(df_test)!=ncol(df)){
    diff=setdiff(colnames(df_test),colnames(df))
    df_test=df_test[,!colnames(df_test) %in% diff]
  }
  df_test=df_test[,bestgene]
  cl_test=as.factor(valid[,ncol(valid)])
  df_train=df[,bestgene]
  cl_train=cl
  data=as.data.frame(cbind(cl_train,df_train))
  data[,1]=as.factor(data[,1])
  names(data)[1]="label"
  newdata=as.data.frame(df_test)
  model.tune <-caret::train(label~.,data=data,trace=F,
                     method="svmRadial",
                     trControl = control,
                     tuneGrid = Grid )
  ksvm.fit <- ksvm(y=cl_train,x=as.matrix(data[,-1]),
                   kernel = "rbfdot",
                   kpar = list(sigma = model.tune$bestTune$sigma),
                   C=model.tune$bestTune$C, prob.model=TRUE)
  svm.pre=predict(ksvm.fit,newdata,type="probabilities")
  Score=score(svm.pre,cl_test)
  return(Score)
}

# Rprop+ only CV function
SVM_all=function(data,seed=5, control = fitControl, Grid = tGrid){
  SVM_ALL=NULL
  label=data[,ncol(data)]
  data=data[,-ncol(data)]
  set.seed(seed)
  test.fold=createFolds(y=label,k=10)
  for(i in 1:10){
    test.code=test.fold[[i]]
    train.data=data[-test.code,]
    test.data=data[test.code,]
    train.data[,1]=as.factor(train.data[,1])
    names(train.data)[1]="label"
    test.data=as.data.frame(test.data)
    model.tune <-train(label~.,data=train.data,trace=F,
                     method="svmRadial",
                     trControl = control,
                     tuneGrid = Grid )
    ksvm.fit <- ksvm(y=label[-test.code],x=as.matrix(train.data),
                   kernel = "rbfdot",
                   kpar = list(sigma = model.tune$bestTune$sigma),
                   C=model.tune$bestTune$C, prob.model=TRUE)
    svm.pre=predict(ksvm.fit,test.data,type="probabilities")
    Score=score(svm.pre, label[test.code])
    SVM_ALL=rbind(SVM_ALL,Score)
  }
  return(SVM_ALL)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
