library(kofnGA)
library(rpart)
library(randomForest)
library(varSelRF)

#Parameters
ntree=500

#Functions
#getTree: get one DT prediction
getpre=function(x,tree){
  n=nrow(x)
  depth=nrow(tree)
  result=NULL
  for(i in 1:n){
    p=x[i,]
    j=1
    while(j<=depth){
      if(tree[j,"status"]==1){
        point=as.numeric(p[tree[j,"split var"]])
        if(point<=tree[j,"split point"]){daughter=tree[j,"left daughter"]}
        if(point>tree[j,"split point"]){daughter=tree[j,"right daughter"]}
        j=daughter
      }
      if(tree[j,"status"]!=1){result[i]=tree[j,"prediction"]; break}
    }
  }
  return(result)
}

#get all DT's reasults in initial RF
getallpre=function(x,RF){
  result=NULL
  for(i in 1:ntree){
    tree=getTree(RF,k=i,labelVar=F)
    result=cbind(result,getpre(x,tree))
  }
  result[which(result==1)]=0
  result[which(result==2)]=1
  return(result)
}

#oob samples predict result of one tree
#Return oob sample predict accuracy of all tree
oob.tree=function(rf.r,outcome,inbag){
  oob=NULL
  for(k in c(1:ncol(rf.r))){
    oobpre=NULL;oobreal=NULL
    oobpre=rf.r[which(inbag[,k]==0),k]
    oobreal=outcome[which(inbag[,k]==0)]
    oob[k]=length(which(oobpre!=oobreal))/length(which(inbag[,k]==0))
  }
  return(oob)
}

#Sub-RF result for all samples
#For each sample, if the case of being predicted as 1
#is more than 0.5*times of being oob, the result of this sub-RF is 1
oob.rf=function(rf.r,outcome,inbag,prob=0.5){
  oob.r=NULL
  for(i in 1:length(outcome)){
    if(sum(rf.r[i,which(inbag[i,]==0)])>(prob*length(which(inbag[i,]==0))))oob.r[i]=1
    else oob.r[i]=0
  }
  return(length(which(oob.r!=outcome))/length(outcome))
}

#fitness function
fitness = function(v,i.pop.r,inbag,outcome){
  rf.r=i.pop.r[,v]
  inbag.sub=inbag[,v]
  oob.r=NULL
  cl=levels(outcome)
  for(i in 1:length(outcome)){
    max.vote=0; max.class=NULL
    for(c in 1:length(cl)){
      if(sum(rf.r[i,which(inbag.sub[i,]==0)]==c)>=max.vote){
        max.vote=sum(rf.r[i,which(inbag.sub[i,]==0)]==c)
        max.class=c
        }
    }
    oob.r[i]=cl[max.class]
  }
  #return(oob.r)
  return(length(which(oob.r!=outcome))/length(outcome))
}

getvalidpre=function(x,RF,bestsol){
  result=NULL
  for(i in bestsol){
    tree=getTree(RF,k=i,labelVar=F)
    result=cbind(result,getpre(x,tree))
  }
  return(result)
}


#Bootstrap method
CV=function(data,seed=5,test.num){
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
  return(list(data_df,data_cl,data_test))
}

simulate.gene = function(madparams,fparams,dparams){
  madsim_class=madsim(mdata=NULL,n=madparams$n,ratio=madparams$ratio,fparams=fparams,dparams=dparams,sdn=madparams$sdn,rseed=madparams$rseed)
  madsim_data=madsim_class$xdata
  sim_label=as.factor(c(rep(0,fparams$m1),rep(1,fparams$m2))) #control and treatment samples
  sim_DE=as.factor(madsim_class$xid) #differentially gene expression
  #add outlying sampl
  induced=which(sim_DE==1) #induced genes
  suppressed=which(sim_DE==-1) #suppressed genes
  n_out=round(fparams$m2*0.2)
  out_l=fparams$m1+fparams$m2-n_out+1; out_u=fparams$m1+fparams$m2
  for(j in out_l:out_u){
    noise=rnorm(length(c(induced,suppressed)),mean=0,sd=1)
    madsim_data[c(induced,suppressed),j]=madsim_data[c(induced,suppressed),j]+noise
  }
  sim_data=t(madsim_data)
  return(list(data=sim_data,label=sim_label,DE=sim_DE))
}


GARF_Main = function(data, j, ntree){
    # Pre-process
    CV_data = CV(data, test.num=j)
    data_df = CV_data[[1]]
    data_cl = CV_data[[2]]
    data_test = CV_data[[3]]
    test_df = data_test[,-ncol(data_test)]
    test_cl = data_test[,ncol(data_test)]

    #input data
    mtry = round(sqrt(ncol(data)-1))
    cl = levels(data_cl)

    #RF
    i.RF = randomForest(x=data_df, y=data_cl, ntree=ntree,mtry=mtry,keep.inbag=TRUE)
    i.pop.r = getallpre(data,i.RF)
    inbag = i.RF$inbag

    #GA
    GARF.fit = kofnGA(n=ntree,k=ntree/10,OF=fitness,inbag=inbag,i.pop.r=i.pop.r,outcome=data_cl,
                      mutprob=0.1,keepbest=5,verbose=20,ngen=300, popsize=100)
    valid.r = getvalidpre(test_df,i.RF,bestsol=GARF.fit$bestsol)
    valid.r.pre=NULL
    for(i in 1:nrow(test_df)){
        max.vote=0; max.class=NULL
        for(c in 1:length(cl)){
            if(sum(valid.r[i,]==c)>=max.vote){
                max.vote=sum(valid.r[i,]==c)
                max.class=c
            }
        }
    valid.r.pre[i]=cl[max.class]
    }
    acc.v=length(which((valid.r.pre==test_cl)==TRUE))/length(test_cl)
    rf.valid=factor(as.vector(predict(i.RF,newdata=test_df)))
    acc.ini=length(which((rf.valid==test_cl)==TRUE))/length(test_cl)
    return(list("GA model"=GARF.fit,"GARF Result"=acc.v, "RF Result"=acc.ini))
}

