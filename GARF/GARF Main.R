Colon_GARF = sapply(c(1:5),function(X)GARF_Main(Colon, j=X, ntree=500))
CNS_GARF = sapply(c(1:5),function(X)GARF_Main(CNS, j=X, ntree=500))
Leu_GARF = sapply(c(1:5),function(X)GARF_Main(Leu, j=X, ntree=500))
Lung_GARF = sapply(c(1:5),function(X)GARF_Main(Lung, j=X, ntree=500))

Bre_result=NULL
for(i in 1:5){
  Bre_result=rbind(Bre_result,c(Bre_GARF[[(i-1)*3+2]],Bre_GARF[[(i-1)*3+3]]))
}#low
CNS_result=NULL
for(i in 1:5){
  CNS_result=rbind(CNS_result,c(CNS_GARF[[(i-1)*3+2]],CNS_GARF[[(i-1)*3+3]]))
}#high a little
Colon_result=NULL
for(i in 1:5){
  Colon_result=rbind(Colon_result,c(Colon_GARF[[(i-1)*3+2]],Colon_GARF[[(i-1)*3+3]]))
}#same
Leu_result=NULL
for(i in 1:5){
  Leu_result=rbind(Leu_result,c(Leu_GARF[[(i-1)*3+2]],Leu_GARF[[(i-1)*3+3]]))
}#high
Lung_result=NULL
for(i in 1:5){
  Lung_result=rbind(Lung_result,c(Lung_GARF[[(i-1)*3+2]],Lung_GARF[[(i-1)*3+3]]))
}#high
Srbct_result=NULL
for(i in 1:5){
  Srbct_result=rbind(Srbct_result,c(Srbct_GARF[[(i-1)*3+2]],Srbct_GARF[[(i-1)*3+3]]))
}




library(madsim)
n_samples = c(100)
n_genes = c(2000,5000,10000)
All_Sim_result = NULL
  for(i in 1:length(n_genes)){
    # i is the seq number of sim data
    simulation = NULL
    sim_data = NULL
    fparams = data.frame(m1 = 50, m2 = 50, shape2 = 4, lb = 4, ub = 12,pde=0.02,sym=0.5)
    dparams = data.frame(lambda1 = 0.13, lambda2 = 2.5, muminde = 0.5, sdde = 0.5)
    madparams = data.frame(n=n_genes[i], ratio=0, sdn=0.4, rseed=5)
    simulation = simulate.gene(madparams=madparams,fparams=fparams,dparams=dparams)
    sim_data = cbind.data.frame(simulation$data,simulation$label)
    sim.result = GARF_Main(sim_data, j=1, ntree=500)
    All_Sim_result[[i]] = sim.result
    }

