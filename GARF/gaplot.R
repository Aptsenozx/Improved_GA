plot.GAsearch.self <- function(x, type=c("l","l"), lty=c(1,1), pch=c(-1,-1), col=c("blue", "red"), lwd=c(1,1), ...) {

  ng <- length(x$old$obj) - 1
  lb = min(c(x$old$obj, x$old$avg))
  ub = max(c(x$old$obj, x$old$avg))
  bestloc = which(x$old$obj == min(x$old$obj))[1] - 1

  matplot(0:ng, cbind(x$old$avg, x$old$obj), type=type, lty=lty, pch=pch, col=col, lwd=lwd,
          xlab="Generation", ylab="Objective function value", mgp=c(1.5,0.5,0),...)

  txt <- c("Pop average", "Pop best", "Last improvement")
  # Need to handle plot inputs as either numeric or character.
  if(is.character(lty)) leglty <- c(lty, "blank") else leglty <- c(lty, 0)
  if(is.character(pch)) legpch <- c(pch, "x") else legpch <- c(pch, 19)
  if(is.character(col)) legcol <- c(col, "black") else legcol <- c(col,1)

  points(bestloc, x$old$obj[bestloc + 1], pch=legpch[3])

  legend("topright", legend=txt, pch=legpch, col=legcol,
         lwd=c(lwd,1), bty="n", lty=leglty, cex=0.8)

}
gaplot=function(GAresult){
  par(mfrow=c(2,3),mar=c(3.5,2.8,2.5,1));
  for(i in 1:5){
    plot.GAsearch.self(GAresult[[(i-1)*3+1]],main=paste("Fold",i,"GARF Search"),
         col=c("forestgreen","orange"),cex.main=0.9)
  }
}

#gaplot(CNS_GARF)
#gaplot(Colon_GARF)
#gaplot(Leu_GARF)
#gaplot(Lung_GARF)

#
mean(sapply(c(1,4,7,10,13),function(X)summary(Lung_GARF[[X]])$OFvals[4]))


RF_Main = function(data, j, ntree){
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
  error = 1-sum(diag(i.RF[["confusion"]][,c(1:3)]))/nrow(data_df)
  return(error)
}

#CNS.RF.error=sapply(c(1:5),function(X)RF_Main(CNS, X, ntree=500))
#Colon.RF.error=sapply(c(1:5),function(X)RF_Main(Colon, X, ntree=500))
#Leu.RF.error=sapply(c(1:5),function(X)RF_Main(Leu, X, ntree=500))
#Lung.RF.error=sapply(c(1:5),function(X)RF_Main(Lung, X, ntree=500))

