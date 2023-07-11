source("C:/Users/wangz/OneDrive/MasterResearch/ResearchCode/environment.R")
source("C:/Users/wangz/OneDrive/MasterResearch/ResearchCode/Function3.R")

GA_Manifold_Main_gavalid=function(data, address, ga.k = 30, ga.mutation = 0.05,
                                  i=NULL, j=NULL, data.names=NULL,
                                  control=fitControl, Grid=tGrid)
    {
    #dataname = as.character(c(substitute(data)))
    dataname = datanames[i]
    GA_models = NULL
    GA_select = NULL
    write.table(paste(dataname," ","j=",j,"n.samples=",nrow(data),"n.genes=",ncol(data),"k=",ga.k,"mutation=",ga.mutation),
                file = paste(address,"result",".csv",sep=""),col.names = FALSE,row.names = FALSE,sep = ",",
                append = TRUE, qmethod = "double")

    # Pre-process
    CV_data = CV.boots(data, test.num=j)
    data_df = CV_data[[1]]
    data_cl = CV_data[[2]]
    data_valid = CV_data[[3]]
    data_train = CV_data[[4]]
    notify_msg("CV OK!")

    # Tuning parameter k
    Isotune = tuning(data_df,data_cl,k.range,"Isomap")
    params = Isotune
    notify_msg("Tunning OK!")
    cat("\014")

    # Training models
    # GA parameters

    timestart = proc.time()
    GAIso_kofn = kofn_mani_2(data_df,data_cl,data_train,method = "Isomap",
                             ga.k = ga.k, ga.mutation = ga.mutation,opdim = params[["Optdim"]], opk = params[["Optk"]],
                             control = control, Grid = Grid)
    timeend = proc.time()
    Iso_time = (timeend-timestart)[1:2]
    GA_models[[1]] = GAIso_kofn
    notify_msg(paste(dataname,"GAIso OK!"))
    cat("\014")

    timestart<-proc.time()
    GADBI_kofn = kofn_mani_2(data_df,data_cl,data_train, ga.k = ga.k, ga.mutation = ga.mutation, method="Dist",
                             control = control, Grid = Grid)
    timeend<-proc.time()
    DB_time = (timeend-timestart)[1:2]
    GA_models[[3]] = GADBI_kofn
    notify_msg("GADBI OK!")
    cat("\014")

    timestart<-proc.time()
    GAMDS_kofn = kofn_mani_2(data_df,data_cl,data_train, ga.k = ga.k, ga.mutation = ga.mutation, opdim = params[[1]][["Optdim"]], method="MDS",
                             control = control, Grid = Grid)
    timeend<-proc.time()
    MDS_time = (timeend-timestart)[1:2]
    GA_models[[4]] = GAMDS_kofn
    notify_msg(paste(dataname,"GAMDS OK!"))
    cat("\014")

    names(GA_models)=c("GAIso","GADBI","GAMDS")

    # Time
    models_time = rbind(Iso_time,DB_time,MDS_time)
    rownames(models_time) = c("Iso","DB","MDS")
    #write.table(models_time,file=paste(address,dataname,".txt",sep=""),append=TRUE)

    notify_msg("Training results and Time save OK!")

    # Result
    GAIsok_select=selected.gene(GAIso_kofn)
    GADBIk_select=selected.gene(GADBI_kofn)
    GAMDSk_select=selected.gene(GAMDS_kofn)
    GA_select = list(GAIsok_select, GADBIk_select, GAMDSk_select)
    names(GA_select) = c("GAIsok_select", "GADBIk_select", "GAMDSk_select")


    # Test result Log
    test_Log = test.log(data_df,data_cl,data_valid,GAIsok_select)
    test_Log = rbind.data.frame(test_Log,test.log(data_df,data_cl,data_valid,GADBIk_select))
    test_Log = rbind.data.frame(test_Log,test.log(data_df,data_cl,data_valid,GAMDSk_select))
    names(test_Log) = c("avgAcc","macro","micro")
    rownames(test_Log) = c("Isomap","DBIndex","MDS")
    write.test.result(test_Log, fileadd=paste(address,"result",".csv",sep=""))
    notify_msg(paste("macro",test_Log[1,2],test_Log[2,2],test_Log[3,2],"micro",test_Log[1,3],test_Log[2,3],test_Log[3,3],sep="/"))

    # Test result NN
    test_NN = test.nn(data_df,data_cl,data_valid,GAIsok_select)
    test_NN = rbind.data.frame(test_NN,test.nn(data_df,data_cl,data_valid,GADBIk_select))
    test_NN = rbind.data.frame(test_NN,test.nn(data_df,data_cl,data_valid,GAMDSk_select))
    names(test_NN) = c("avgAcc","macro","micro")
    rownames(test_NN) = c("Isomap","DBIndex","MDS")
    write.test.result(test_NN, fileadd=paste(address,"result",".csv",sep=""))
    notify_msg(paste("macro",test_NN[1,2],test_NN[2,2],test_NN[3,2],"micro",test_NN[1,3],test_NN[2,3],test_NN[3,3],sep="/"))

    notify_msg(paste(dataname,"Test OK!"))

    GA.M.Result = list(GA_models, GA_select, test_Log, test_NN, models_time, params)
    names(GA.M.Result) = c("GA_models", "GA_select", "test_Log", "test_NN", "Time", "parameters")
    save(GA.M.Result, file = paste(address,dataname,j,"-Result",".RDATA",sep=""))
}


All.Data = list(Bre, CNS, Colon, Leu, Lung, Lym, MLL, Ova, Srbct)
datanames=c("Breast","CNS","Colon","Leukemia","Lung","Lymphoma","MLL","Ovarian","Srbct")
names(All.Data) = datanames

Leu_All_Result=NULL
i=4
for(j in 4:5){
    GA_Manifold_Main_gavalid(All.Data[[i]], i=i,j=j, data.names=datanames, address="c://Users//wangz//OneDrive//ResearchResult//NoNoise-GAvalid-Result//")
}
