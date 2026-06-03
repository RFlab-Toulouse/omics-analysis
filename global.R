options(xtable.include.colnames=T)
options(xtable.include.rownames=T)
#Packages
#rm(list=ls())
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("zoo")
usePackage("plotly")
usePackage("missMDA")#imputepca
usePackage("ggplot2")#Graphs
usePackage("stats")
usePackage("tidyr")
usePackage("dplyr")
usePackage("stringr")
usePackage("e1071")#svm
usePackage("pROC")#roccurve
usePackage("devtools")
usePackage("readxl")
usePackage("superml")
usePackage("shiny")
usePackage("shinythemes")
usePackage("bslib")
usePackage("fmsb")#radarchart
# if (!is.element("factoextra", installed.packages()[,1]))
#   install_github("kassambara/factoextra")
#usePackage("factoextra")#PCA graphs
usePackage("reshape2")#melt function
usePackage("xlsx")#import fichier xls#Fonctions
usePackage("randomForest")
usePackage("missForest")
usePackage("Hmisc")
usePackage("corrplot")
usePackage("penalizedSVM")
usePackage("DT")
usePackage("shinycssloaders")
usePackage("writexl")
usePackage("glmnet")#for lasso, elasticnet, ridge regression
usePackage("survival")#for cox regression
usePackage("xgboost")#for xgboost gradient boosting
usePackage("lightgbm")#for lightgbm gradient boosting
usePackage("class") #for k-nearest neighbors
usePackage("Boruta") 
#install.packages("Boruta", type = "binary")
usePackage("Rtsne")
usePackage("umap")
usePackage("ggraph")
usePackage("pheatmap")
usePackage("caret")
usePackage("iml")
usePackage("lime")


##########################
importfile<-function (datapath,extension,NAstring="NA",sheet=1,skiplines=0,dec=".",sep=","){
  # datapath: path of the file
  #extention: extention of the file : csv, xls, ou xlsx
  if(extension=="csv"){
    toto <<- read.csv2(datapath,header = F,sep =sep,dec=dec,na.strings = NAstring,stringsAsFactors = F,row.names=NULL,check.names = F )
  }
  if(extension=="xlsx"){
    options(warn=-1)
    filerm<<-file.rename(datapath,paste(datapath, ".xlsx", sep=""))
    options(warn=0)
    toto <<- read_excel(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = sheet) %>% as.data.frame()
    #toto <<- read_xlsx(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = sheet)
    #toto <-read.xlsx2(file = datapath,sheetIndex = sheet)
    #toto <-read_excel(datapath,na=NAstring,col_names = F,skip = skiplines,sheet = sheet)
  }
  #remove empty column
  if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
    toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
  #remove empty row
  if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
    toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
  print(class(toto))
  
  rnames<-as.character(as.matrix(toto[,1]))
  cnames<-as.character(as.matrix(toto[1,]))
  toto<-toto[,-1]
  toto<-toto[-1,]
  row.names(toto)<-rnames[-1]
  colnames(toto)<-cnames[-1]
  
  toto<-as.data.frame(toto)
  rownames(toto)<-rnames[-1]
  colnames(toto)<-cnames[-1]
  return(toto)
}

# downloaddataset <- function(x,file,cnames=T,rnames=T){
#   ext<-strsplit(x = file,split = "[.]")[[1]][2]
#   if(ext=="csv"){
#     if(sum(cnames,rnames)==2){
#       write.csv(x,file)
#       }
#     else{
#       write.table(x,file,col.names = cnames,row.names = rnames,sep=";",dec=".")
#       }
#   }
#   if(ext=="xlsx"){
#     write.xlsx(x,file,col.names = cnames,row.names =rnames )
#   }
#   
# }

# df <- reactive({
#   req(input$learningfile)
#   file <- input$learningfile
#   ext <- tools::file_ext(file$datapath)
#   
#   req(file)
#   validate(need(ext == "xlsx", "Veuillez télécharger un fichier CSV"))
#   
#   df = read_excel(file$datapath)
#   print(head(df))
#   return( df)
# })


downloaddataset <- function(x,file,cnames=T,rnames=T){
  ext = tools::file_ext(file)
  if(ext=="csv"){
    if(sum(cnames,rnames)==2){
      write.csv(x,file)
    }
    else{
      write.table(x,file,col.names = cnames,row.names = rnames,sep=";",dec=".")
    }
  }
  if(ext=="xlsx"){
    #write.xlsx(x,file,col.names = cnames,row.names =rnames )
    writexl::write_xlsx(x,file, col_names = cnames)
  }
  
}

downloadplot <- function(file){
  ext<-strsplit(x = file,split = "[.]")[[1]][2]
  
  if(ext=="png"){
    png(file)
  }
  if(ext=="jpg"){
    jpeg(file)
  }  
  if(ext=="pdf"){
    pdf(file) 
  }     
}
# renamvar<-function(names){
#   #rename the duplicate name by adding ".1, .2 ....
#   #toto is a vector of the col names of the tab
#   names[is.na(names)]<-"NA"
#   for(i in 1:length(names)){
#     ind <- which(names%in%names[i])
#     if(length(ind)>1){
#       nb<-c(1:length(ind))
#       newnames<-paste(names[ind],".",nb,sep="")
#       
#       names[ind]<-newnames
#     }
#   }
#   return(names)
# }
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

transformdata<-function(toto,transpose,zeroegalNA){
  #   if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
  #     toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
  #   #remove empty rows
  #   if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
  #     toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
  #   #remove empty columns
  
  # transpose du data frame
  if(transpose){
    toto<-t(toto)
  }
  
  if(zeroegalNA){
    toto[which(toto==0,arr.ind = T)]<-NA
  }
  
  toto<-as.data.frame(toto[,c(colnames(toto)[1],sort(colnames(toto)[-1]))])
}

confirmdata<-function(toto){
  toto<-as.data.frame(toto)
  toto[,1]<-as.factor(as.character(toto[,1]))
  for (i in 2:ncol(toto)){
    toto[,i]<-as.numeric(as.character(toto[,i]))
  }
  return(toto)
}

importfunction<-function(importparameters){
  previousparameters<-NULL
  validation<-NULL
  learning<-NULL
  
  if(is.null(importparameters$learningfile)&is.null(importparameters$modelfile)){return()}
  
  if(!is.null(importparameters$modelfile) ){
    load(file = importparameters$modelfile$datapath)
    previous<-state
    learning<-previous$data$LEARNING
    validation<-previous$data$VALIDATION
    #lev<-previous$data$LEVELS
    previousparameters<-previous$parameters
  }
  
  if(!is.null(importparameters$learningfile)  ){
    #if(importparameters$confirmdatabutton==0){
    datapath<- importparameters$learningfile$datapath
    #datapath <- input$learningfile$datapath
    #print(datapath)
    #print(paste(datapath, ".xlsx", sep=""))
    #out<<-tryCatch(
    learning<-importfile(datapath = datapath,extension = importparameters$extension,NAstring=importparameters$NAstring,
                         sheet=importparameters$sheetn,skiplines=importparameters$skipn,dec=importparameters$dec,sep=importparameters$sep)
    #              ,error=function(e) e )
    #            if(any(class(out)=="error")){tablearn<-data.frame()}
    #            else{tablearn<<-out}
    #            validate(need(ncol(tablearn)>1 & nrow(tablearn)>1,"problem import"))
    
    learning<-transformdata(toto = learning,transpose=importparameters$transpose,zeroegalNA=importparameters$zeroegalNA)
    
    #}
    if(importparameters$confirmdatabutton!=0){
      learning<-confirmdata(toto = learning)
      if(importparameters$invers){learning[,1]<-factor(learning[,1],levels = rev(levels(learning[,1])))}
      
      #learning<-learning[-which(apply(X = learning,MARGIN=1,function(x){sum(is.na(x))})==ncol(learning)),]
      
      #       lev<-levels(x = tablearn[,1])
      #       print(lev)
      #       names(lev)<-c("positif","negatif")
    }
    # else{lev<-NULL}
  }
  
  
  if(!is.null(importparameters$validationfile)  ){
    
    # if(importparameters$confirmdatabutton==0){
    datapathV<- importparameters$validationfile$datapath
    # out<<-tryCatch(
    validation<-importfile(datapath = datapathV,extension = importparameters$extension,
                           NAstring=importparameters$NAstring,sheet=importparameters$sheetn,skiplines=importparameters$skipn,dec=importparameters$dec,sep=importparameters$sep)
    #             ,error=function(e) e)
    #             if(any(class(out)=="error")){tabval<-NULL}
    #            else{tabval<<-out}
    #            validate(need(ncol(tabval)>1 & nrow(tabval)>1,"problem import"))
    validation<-transformdata(toto = validation,transpose=importparameters$transpose,zeroegalNA=importparameters$zeroegalNA)
    
    
    # }
    if(importparameters$confirmdatabutton!=0){
      validation<-confirmdata(toto = validation)
      if(importparameters$invers){validation[,1]<-factor(validation[,1],levels = rev(levels(validation[,1])))}
      
      #validation<-validation[-which(apply(X = validation,MARGIN=1,function(x){sum(is.na(x))})==ncol(validation)),]
      
    }
    
  }
  
  res<-list("learning"=learning,"validation"=validation,previousparameters=previousparameters)#,"lev"=lev)
  return(res)
}


# selectvar<-function(resPCA,toto){
#   #select variables which are correlate to the axes correlate to the cotegorial variable of the first column
#   restri<-dimdesc(resPCA,axes = c(1:(min(ncol(toto),10)-1)) )
#   varquali<-vector()
#   score<-0
#   #restri is a dimdesc data
#   for (i in 1:length(restri)){
#     if ( !is.null(restri[[i]]$quali ) ) {
#       score<-score+restri[[i]]$quali[[1]]
#       varquali<-c(varquali,row.names(restri[[i]]$quanti))
#     }
#   }
#   #score<-1- ( ( (1+score)*(nrow(toto)-1) )/(nrow(toto)-ncol(toto)-1) )
#   return(list("varquali"=varquali,"score"=score))
# }

# selectdata<-function(toto){
#   #remove variable  with less than 2 value and replace 0 by NA
#   n<-ncol(toto)
#   toto[which(toto==0 ,arr.ind = T )]<-NA
#   vec<-rep(T,length=n)
#   for(i in 2:n){
#     vec[i]<-( (length(unique(toto[,i]))>2) )
#   }
#   #rm var with less than 3 values (0 or NA , and 2 other (important for the rempNA PCA))
#   toto<-toto[,as.logical(vec)]
#   return(toto)
# }

selectdatafunction<-function(learning,selectdataparameters){
  learningselect<-selectprctvalues(toto = learning,prctvalues = selectdataparameters$prctvalues,selectmethod =selectdataparameters$selectmethod)
  if(selectdataparameters$NAstructure==T){
    if(selectdataparameters$structdata=="selecteddata"){learning<-learningselect}
    restestNAstructure<-testNAstructure(toto = learning,threshold = selectdataparameters$thresholdNAstructure,maxvaluesgroupmin=selectdataparameters$maxvaluesgroupmin,
                                        minvaluesgroupmax=selectdataparameters$minvaluesgroupmax)
    if(!is.null(restestNAstructure)){
      learningselect<-cbind(learningselect[,!colnames(learningselect)%in%restestNAstructure$restestNAstructure$names],restestNAstructure$varNAstructure)}
  }
  else{restestNAstructure<-NULL}
  
  return(list(learningselect=learningselect,structuredfeatures=restestNAstructure$varNAstructure,datastructuredfeatures=restestNAstructure$restestNAstructure))
}

testObject <- function(object){
  #test if the object is in the global environnement
  exists(as.character(substitute(object)))
}

selectprctvalues<-function(toto,prctvalues=100,selectmethod="nogroup"){ 
  n<-ncol(toto)
  if (selectmethod=="nogroup"){
    NAvec<-vector(length =max(n,0) )
    for(i in 1:n){
      NAvec[i]<-  (sum(!is.na(toto[,i]))/nrow(toto)  ) 
    }
    vec<-(NAvec>=(prctvalues/100))
    
  } 
  
  if(selectmethod!="nogroup"){
    nbcat<-length(levels(toto[,1]))
    tabgroup<-matrix(nrow = nbcat, ncol=n )
    for(i in 1:nbcat){
      tab<-toto[which(toto[,1]==levels(toto[,1])[i]),]
      for(j in 1:(n) ){
        tabgroup[i,j]<-(sum(!is.na(tab[,j]))/nrow(tab))  
      }  
    }
    if(selectmethod=="onegroup"){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){(max (x) >= (prctvalues/100)) }) 
    }
    if(selectmethod=="bothgroups"){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){(min (x) >= (prctvalues/100)) }) 
    }
  }
  totoselect<-toto[,as.logical(vec)]
}

heatmapNA<-function(toto,maintitle="Distribution of NA",graph=T){
  
  if(ncol(toto)==1){errorplot(text = " No structured variables")}
  else{
    names<- paste(toto[,1],1:length(toto[,1]))
    tab<-as.data.frame(toto[,-1])
    tab[which(!is.na(tab) ,arr.ind = T )]<-"Value"
    tab[which(is.na(tab) ,arr.ind = T )]<-"NA"
    #tab<-cbind(paste(toto[,1],1:length(toto[,1])),tab)
    tab<-apply(tab,2,as.factor)
    rownames(tab)<-names
    if(!graph){ return(cbind(rownames(toto),tab))}
    if(graph){
      tabm <- melt(tab)
      #tabm<-tabm[-c(1:nrow(toto)),]
      colnames(tabm)<-c("individuals","variables","value")
      tabm$variables<-as.character(tabm$variables)
      tabm$individuals<-as.character(tabm$individuals)
      if(ncol(toto)>60){
        ggplot(tabm, aes(variables, individuals)) + geom_tile(aes(fill = value)) + scale_fill_manual(values=c("lightgrey","steelblue"),name="")+ 
          ggtitle(maintitle) + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
      }
      else{
        ggplot(tabm, aes(variables, individuals)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_manual(values=c("lightgrey","steelblue"))+ 
          ggtitle(maintitle) + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
      }
    }
  }
}

distributionvalues<-function(toto,prctvaluesselect,nvar,maintitle="Number of variables according to\nthe % of values's selected",graph=T,ggplot=T){
  percentagevalues<-seq(0,1,by = 0.01)
  prctall<-apply(X = toto,MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto)
  prctvalueswhithoutgroup<-sapply(X = percentagevalues,FUN = function(x,prct=prctall){sum(x<=prct)})
  prctlev1<-apply(X = toto[which(toto[,1]==levels(toto[,1])[1]),],MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[1]),])
  prctlev2<-apply(X = toto[which(toto[,1]==levels(toto[,1])[2]),],MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[2]),])
  
  nvareachgroups<-sapply(X = percentagevalues,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x<=apply(rbind(prct1,prct2),2,min))})  
  nvaronegroup<-sapply(X = percentagevalues,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x<=apply(rbind(prct1,prct2),2,max))})  
  
  distribvalues<-data.frame("percentagevalues"=percentagevalues,"all samples"=prctvalueswhithoutgroup,"each groups"= nvareachgroups,"at least one group"=nvaronegroup)
  if(!graph)(return(distribvalues))
  col<-gg_color_hue(ncol(distribvalues)-1)
  if(!ggplot){
    matplot(x=distribvalues$percentagevalues,distribvalues[,-1],type=c("l","l"),lty = c(1,1,1),
            col=c("red","green","blue"), xlab="percentage of values selected",ylab="Number of variables",main=maintitle)
    legend("bottomright",colnames(distribvalues[,-1]),col=c("red","green","blue"),lty=1)
    abline(v = prctvaluesselect,lty=3,col="grey")
    abline(h = nvar,lty=3,col="grey")
  }
  if (ggplot){
    distribvalueslong<- melt(distribvalues,id.vars = "percentagevalues",variable.name = "select_method",value.name = "number_of_variables")  # convert to long format
    p<-ggplot(data=distribvalueslong,
              aes(x=percentagevalues, y=number_of_variables, colour=select_method)) +geom_line()+
      ggtitle(maintitle)
    p+theme(plot.title=element_text( size=15),legend.text=element_text(size=10),legend.title=element_text(color = 0),legend.position=c(0.20,0.15))+
      geom_vline(xintercept=prctvaluesselect,linetype=3)+
      geom_hline(yintercept=nvar,linetype=3)
  }
}

# ============================================================================
# replaceNA_fit : comme replaceNA, mais retourne aussi les paramètres appris
# sur le train pour pouvoir les appliquer à la validation.
# ============================================================================
replaceNA_fit <- function(toto, rempNA="z", pos=FALSE, NAstructure=FALSE,
                          thresholdstruct=0.05, maxvaluesgroupmin=100, minvaluesgroupmax=0){
  imputation_params <- list(method=rempNA)
  
  if(NAstructure){
    totoNAstruct <- replaceproptestNA(toto=toto, threshold=thresholdstruct,
                                      rempNA=rempNA, maxvaluesgroupmin, minvaluesgroupmax)
    toto[,colnames(totoNAstruct)] <- totoNAstruct
  }
  
  if(rempNA == "none" | sum(is.na(toto))==0){ return(list(toto=toto, imputation_params=imputation_params)) }
  
  cnames  <- colnames(toto)
  class   <- toto[,1]
  cat_lev <- levels(class)
  toto    <- as.data.frame(toto[,-1], optional=TRUE)
  n       <- ncol(toto)
  
  if(rempNA == "z"){
    toto[which(is.na(toto), arr.ind=TRUE)] <- 0
  }
  if(rempNA == "moy"){
    # apprendre les moyennes sur le train
    col_means <- colMeans(toto, na.rm=TRUE)
    col_means[is.nan(col_means)] <- 0
    imputation_params$col_means <- col_means
    toto <- na.aggregate(toto)
  }
  if(rempNA == "moygr"){
    group_means <- list()
    for(i in seq_along(cat_lev)){
      tab <- toto[which(class==cat_lev[i]), , drop=FALSE]
      gm  <- colMeans(tab, na.rm=TRUE)
      gm[is.nan(gm)] <- 0
      group_means[[cat_lev[i]]] <- gm
      tab <- na.aggregate(tab)
      toto[which(class==cat_lev[i]),] <- tab
    }
    imputation_params$group_means <- group_means
    toto[which(is.na(toto), arr.ind=TRUE)] <- 0
  }
  if(rempNA == "pca"){
    nindiv <- nrow(toto)
    prctnacol <- apply(X=toto, MARGIN=2, FUN=function(x){
      if(sum(!is.na(x))<=0){ x<-rep(0, length=nindiv) } else { x }
    })
    pca_ncp  <- min(n-1, 5)                                   
    pca_res  <- imputePCA(prctnacol, ncp=pca_ncp, method.cv="Kfold")
    imputation_params$pca_res    <- pca_res
    imputation_params$pca_ncp    <- pca_ncp                   
    imputation_params$train_data <- prctnacol
    toto <- as.data.frame(pca_res$completeObs)
    if(pos){ toto[which(toto<0, arr.ind=TRUE)] <- 0 }
  }
  if(rempNA == "missforest"){
    mf_res <- missForest(toto, maxiter=5)
    imputation_params$mf_res    <- mf_res
    imputation_params$train_data <- as.data.frame(mf_res$ximp) # train imputé
    toto <- mf_res$ximp
    if(pos){ toto[which(toto<0, arr.ind=TRUE)] <- 0 }
  }
  
  toto <- cbind(class, toto)
  toto[which(is.na(toto), arr.ind=TRUE)] <- 0
  colnames(toto) <- cnames
  
  return(list(toto=toto, imputation_params=imputation_params))
}


proptestNA<-function(toto){
  group<-toto[,1]
  toto[,1]<-as.character(toto[,1])
  toto[which(!is.na(toto),arr.ind=T)]<-"value"
  toto[which(is.na(toto),arr.ind=T)]<-"NA"
  pval<-vector("numeric",length = ncol(toto))
  lessgroup<-vector("character",length = ncol(toto))
  prctmore<-vector("numeric",length = ncol(toto))
  prctless<-vector("numeric",length = ncol(toto))
  for (i in 1:ncol(toto)){
    conting<-table(group,factor(toto[,i],levels=c("value","NA")))
    options(warn=-1)
    res<-prop.test(conting)
    options(warn=0)
    pval[i]<-res$p.value
    prctmore[i]<-max(res$estimate)
    prctless[i]<-min(res$estimate)
    if(res$estimate[1]==res$estimate[2]){ lessgroup[i]<-"NA"}
    else{lessgroup[i]<-rownames(conting)[which(res$estimate==min(res$estimate))]}
  }
  pval[is.na(pval)]<-1
  return(data.frame("pval"=pval,"lessgroup"=lessgroup,"prctless"=prctless,"prctmore"=prctmore,"names"=colnames(toto)))
}

testNAstructure<-function(toto,threshold=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){
  class<-toto[,1]
  resproptest<-proptestNA(toto=toto)
  vecond<-c(resproptest$pval<=threshold & resproptest$prctless<=(maxvaluesgroupmin/100) & resproptest$prctmore>=(minvaluesgroupmax/100))
  if(sum(vecond)>0){
    resp<-resproptest[vecond,]
    totopropselect<-data.frame(toto[,vecond])
    colnames(totopropselect)<-resp$names
    totopropselect<-as.data.frame(totopropselect[, order(resp[,2])])
    colnames(totopropselect)<-resp$names[order(resp[,2])]
  }
  else{return(NULL)}
  
  return(list("varNAstructure"=totopropselect,"restestNAstructure"=resp))
}

transformdatafunctionBinairy <- function(learningselect, 
                                  structuredfeatures, 
                                  datastructuresfeatures, 
                                  transformdataparameters) {
  learningtransform <- learningselect
  train_params <- list()
  
  # 1. NAstructure
  if(!is.null(structuredfeatures)){
    for(i in 1:ncol(structuredfeatures)){
      learningtransform[which(is.na(structuredfeatures[,i]) & learningselect[,1] == as.character(datastructuresfeatures[i,"lessgroup"])),
                        as.character(datastructuresfeatures[i,"names"])] <- 0
    }
  }
  
  # 2. Log
  if(transformdataparameters$log){
    learningtransform[,-1] <- transformationlog(x = learningtransform[,-1] + 1, logtype = transformdataparameters$logtype)
  }
  
  # 3. Arcsin
  if(transformdataparameters$arcsin){
    train_params$arcsin_min <- apply(learningtransform[,-1], 2, min, na.rm = TRUE)
    train_params$arcsin_max <- apply(learningtransform[,-1], 2, max, na.rm = TRUE)
    learningtransform[,-1] <- apply(X = learningtransform[,-1], MARGIN = 2,
                                    FUN = function(x){ (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T)) })
    learningtransform[,-1] <- asin(sqrt(pmax(0, pmin(1, as.matrix(learningtransform[,-1])))))  # ← pmax/pmin ajouté
  }
  
  # 4. Imputation  ← remonté avant la standardisation
  res_imputation <- replaceNA_fit(toto = learningtransform, rempNA = transformdataparameters$rempNA, pos = TRUE, NAstructure = FALSE)
  learningtransform       <- res_imputation$toto
  train_params$imputation <- res_imputation$imputation_params
  
  # 5. Standardisation  ← descend après l'imputation
  if(transformdataparameters$standardization){
    sdlearningtransform <- apply(X = learningtransform[,-1], MARGIN = 2, FUN = sd, na.rm = TRUE)
    train_params$sd_scale <- sdlearningtransform
    learningtransform[,-1] <- scale(learningtransform[,-1], center = FALSE, scale = sdlearningtransform)
  }
  
  return(list(learningtransform = learningtransform, train_params = train_params))
}

# transformdatafunctionBinairy<-function(learningselect,structuredfeatures,datastructuresfeatures,transformdataparameters){
#   learningtransform<-learningselect
#   if(!is.null(structuredfeatures)){
#     for(i in 1:ncol(structuredfeatures)){
#       learningtransform[which(is.na(structuredfeatures[,i])&learningselect[,1]==as.character(datastructuresfeatures[i,"lessgroup"])),as.character(datastructuresfeatures[i,"names"])]<-0
#     }
#   }
#   if(transformdataparameters$log){ 
#     learningtransform[,-1]<-transformationlog(x = learningtransform[,-1]+1,
#                                               logtype=transformdataparameters$logtype)}
#   if(transformdataparameters$arcsin){
#     learningtransform[,-1]<-apply(X = learningtransform[,-1],MARGIN = 2,FUN = function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))})
#     learningtransform[,-1]<-asin(sqrt(learningtransform[,-1]))
#   }
#   if(transformdataparameters$standardization){
#     learningtransformsd<<-learningtransform
#     sdlearningtransform<-apply(X = learningtransform[-1],MARGIN = 2,FUN = sd,na.rm=T)
#     #print('sdlearningtransform')
#     #print(sdlearningtransform)
#     learningtransform[,-1]<-scale(learningtransform[,-1],center = F,scale=sdlearningtransform)
#     #learningtransform[,-1]<-scale(learningtransform[,-1], center = F, scale = TRUE)
#   }
#   learningtransform<-replaceNA(toto=learningtransform,rempNA=transformdataparameters$rempNA,pos=T,NAstructure = F)
#   
#   return(learningtransform)
# }

transformationlog<-function(x,logtype){
  if(logtype=="log10"){x<-log10(x)}
  if(logtype=="log2"){x<-log2(x)}
  if(logtype=="logn"){x<-log(x)}
  return(x)
}

histplot<-function(toto,graph=T){
  
  # data<-data.frame("values"=as.vector(as.matrix(toto[,-1])))
  if(is.data.frame(toto) || is.matrix(toto)){
    data<-data.frame("values"=as.vector(as.matrix(toto[,-1, drop=FALSE])))
  } else {
    data<-data.frame("values"=as.vector(toto))
  }
  
  if(graph==F){ return(datahistogram(data = data,nbclass = 20))}
  if(graph==T){
    ggplot(data=data,aes(x=values) )+ 
      geom_histogram(col="lightgrey",fill="steelblue",bins=20)+ggtitle("Distribution of values")+
      theme(plot.title = element_text(size=15))+
      annotate("text",x=Inf,y=Inf,label=paste(nrow(data),"values"),size=6,vjust=2,hjust=1.5)
  }
}
datahistogram<-function(data,nbclass){
  dh<-hist(data[,1],nclass=nbclass,plot=F)
  minclass<-dh$breaks[-(length(dh$breaks))]
  maxclass<-dh$breaks[2:(length(dh$breaks))]
  count<-dh$counts
  res<-data.frame("count"=count,"minclass"=minclass,"maxclass"=maxclass)
}

replaceNA<-function(toto,rempNA="z",pos=F,NAstructure=F,thresholdstruct=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){ 
  #rempNA: remplace Non ATtributes values by zero("z"), the mean of the colum (moy), 
  # the mean in each group define by the factor of the first column(moygr), itarative pca (pca), or keep th NA
  if(NAstructure){
    totoNAstruct<-replaceproptestNA(toto = toto,threshold = thresholdstruct ,rempNA =rempNA,maxvaluesgroupmin,minvaluesgroupmax)
    toto[,colnames(totoNAstruct)]<-totoNAstruct
  }
  
  if (rempNA == "none" | sum(is.na(toto))==0 ) {return(toto)}
  cnames<-colnames(toto)
  class<-(toto[,1])
  cat<-levels(class)
  toto<-as.data.frame(toto[,-1],optional = T)
  #toto<-apply(toto,MARGIN = 2,function(x)as.numeric(x))
  n<-ncol(toto) 
  #par default je remplace les NA par 0
  if (rempNA == "z") {
    toto[which(is.na(toto),arr.ind = T)]<-0
  }
  if (rempNA== "moy") {
    toto<-na.aggregate(toto)}
  if(rempNA=="moygr"){
    
    for (i in 1:length(cat)){
      tab<-toto[which(class==cat[i]),]
      tab<-na.aggregate(tab)
      toto[which(class==cat[i]),]<-tab
    }
    toto[which(is.na(toto) ,arr.ind = T )]<-0
  }
  if (rempNA == "pca"){
    
    #prise en compte des liaisons entre variable et de la ressemblance entre individus    
    #nb<-estim_ncpPCA(toto[,(nbqualisup+1):n],ncp.min = 0,ncp.max = 10,method.cv = "Kfold")    #take a lot time
    nindiv<-nrow(toto)
    prctnacol<-apply(X = toto,MARGIN = 2,FUN=function(x){ if(sum(!is.na(x))<=0){x<-rep(0,length=nindiv)}
      else{x}})
    toto<-imputePCA(prctnacol,ncp = min(n-1,5),method.cv="Kfold")$completeObs
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
    toto<-as.data.frame(toto)
    
  }
  if(rempNA=="missforest"){
    toto<-missForest(toto,maxiter = 5)$ximp
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
  }
  
  toto<-cbind(class,toto)
  toto[which(is.na(toto),arr.ind = T)]<-0
  
  colnames(toto)<-cnames
  
  return(toto)
}

mdsplot<-function(toto,ggplot=T,maintitle="MDS representation of the individuals",graph=T){
  # toto =  toto$learningtransform
  if(!is.data.frame(toto) && !is.matrix(toto)) stop("mdsplot: toto doit être un data.frame ou une matrice")
  class<-toto[,1]
  toto<-toto[-1]
  d <- dist(toto) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  x <- fit$points[,1]
  y <- fit$points[,2] 
  coord<-(data.frame("class"=class,x,y))
  if(!graph){return(coord)}
  if(!ggplot){
    colr<-c("red","blue")
    
    plot(x, y, xlab="", ylab="",pch=20,main=maintitle, type="p",col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    text(x, y, labels = row.names(toto), cex=.7,col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    legend("topleft",legend=levels(class),text.col = colr)
  }
  #MDS ggplot
  if(ggplot){
    p <- ggplot(coord, aes(x, y,label=rownames(toto)))
    p + geom_text(aes(colour = class))+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}

heatmapplot<-function(toto,ggplot=T,maintitle="Heatmap of the transform data ",scale=F,graph=T){
  print(str(toto))
  # toto <- toto$learningtransform
  cat("colnames of toto : \n")
  print(colnames(toto))
  row.names(toto)<- paste(toto[,1],1:length(toto[,1]))
  
  toto<-as.matrix(toto[,-1])
  if(!graph){return(toto)}
  #colnames(toto)<-seq(1:ncol(toto))
  if(scale)toto<-scale(toto, center = F, scale = TRUE)
  if(!ggplot){
    heatmap.2(toto,Rowv = NA,Colv=F,trace="none",dendrogram = "none",key=T,margins=c(2,4),keysize=1.30,main=maintitle)
  }
  if(ggplot){
    titi<-melt(toto,value.name = "Intensity")
    colnames(titi)<-c("Individuals","Variables","Intensity")
    titi[,2]<-as.character(titi[,2])
    ggplot(titi, aes( Variables, Individuals,fill = Intensity),colour=NA) + geom_raster()+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}

#############
testfunction<-function(tabtransform,testparameters){
  #condition tests
  if (testparameters$SFtest){
    datatesthypothesis<-SFtest(tabtransform,shaptest=T,Ftest=T,threshold=0.05)
  }
  else{datatesthypothesis<-data.frame()}
  
  #diff test
  if(testparameters$test=="notest"){
    tabdiff<-tabtransform
    datatest<-NULL
    testparameters<-NULL
    useddata<-NULL
    multivariateresults<-NULL
  }
  else if(testparameters$test%in%c("lasso","elasticnet","ridge")){
    # Multivariate selection methods
    multivariateresults<-multivariateselection(toto = tabtransform,
                                               method = testparameters$test,
                                               lambda = testparameters$lambda,
                                               alpha = testparameters$alpha,
                                               nlambda = 100)
    datatest<-multivariateresults$results
    
    if(nrow(datatest)==0){
      print("no variables selected by multivariate method")
      tabdiff<<-data.frame()
      useddata<-NULL
    }
    else{
      selected_vars<-multivariateresults$selected_vars
      indvar<-(colnames(tabtransform)%in%selected_vars)
      indvar[1]<-T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
      useddata<-data.frame("names"=datatest$name,
                           "coefficient"=datatest$coefficient,
                           "logFC"=datatest$logFoldChange,
                           "mean1"=datatest$mean_group1,
                           "mean2"=datatest$mean_group2)
    }
  }else if (testparameters$test=="clustEnet"){
    # Clustering + Elastic Net selection method
    cat("Running Clustering + ElasticNet variable selection...\n")
    
    # Get parameters with defaults
    n_clusters <- if(!is.null(testparameters$n_clusters)) testparameters$n_clusters else 100
    n_bootstrap <- if(!is.null(testparameters$n_bootstrap)) testparameters$n_bootstrap else 500
    alpha_enet <- if(!is.null(testparameters$alpha)) testparameters$alpha else 0.5
    min_selection_freq <- if(!is.null(testparameters$min_selection_freq)) testparameters$min_selection_freq else 0.5
    preprocess <- if(!is.null(testparameters$preprocess)) testparameters$preprocess else TRUE
    min_patients <- if(!is.null(testparameters$min_patients)) testparameters$min_patients else 20
    
    multivariateresults <- clustEnetSelection(toto = tabtransform,
                                              n_clusters = n_clusters,
                                              n_bootstrap = n_bootstrap,
                                              alpha_enet = alpha_enet,
                                              min_selection_freq = min_selection_freq,
                                              preprocess = preprocess,
                                              min_patients = min_patients)
    datatest <- multivariateresults$results
    
    cat("number of selected variables :  ", length(multivariateresults$selected_vars), "\n")
    
    if(nrow(datatest)==0){
      print("no variables selected by clustering + elasticnet method")
      tabdiff<<-data.frame()
      useddata<-NULL
    }
    else{
      selected_vars <- multivariateresults$selected_vars
      indvar <- (colnames(tabtransform) %in% selected_vars)
      indvar[1] <- T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
      useddata <- data.frame("names"=datatest$name,
                             "SelectionFrequency"=datatest$SelectionFrequency,
                             "logFC"=datatest$logFoldChange,
                             "mean1"=datatest$mean_group1,
                             "mean2"=datatest$mean_group2)
    }
  }else if (testparameters$test=="boruta"){
    # Boruta variable selection method
    cat("Running Boruta variable selection...\n")
    
    multivariateresults <- borutaSelection(toto = tabtransform)
    datatest <- multivariateresults$results
    
    cat("number of selected variables :  ", length(multivariateresults$selected_vars), "\n")
    
    if(nrow(datatest)==0){
      print("no variables selected by Boruta method")
      tabdiff<<-data.frame()
      useddata<-NULL
    }
    else{
      selected_vars <- multivariateresults$selected_vars
      indvar <- (colnames(tabtransform) %in% selected_vars)
      indvar[1] <- T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
      useddata <- data.frame("names"=datatest$name,
                             # "Importance"=datatest$Importance,
                             # "logFC"=datatest$logFoldChange,
                             "mean1"=datatest$mean_group1,
                             "mean2"=datatest$mean_group2)
    }
  }
  else{
    # Univariate tests (Wtest, Ttest)
    multivariateresults<-NULL
    datatest<-diffexptest(toto = tabtransform,test = testparameters$test )
    #differential expressed
    logFC<-datatest[,5]
    if(testparameters$adjustpval){pval<-datatest[,3]}
    if(!testparameters$adjustpval){pval<-datatest[,2]}
    datatestdiff<-datatest[which( (pval<testparameters$thresholdpv)&abs(logFC)>testparameters$thresholdFC ),]
    if(dim(datatestdiff)[1]==0){
      print("no differentially expressed variables")
      tabdiff<<-data.frame()
    }
    else{
      indvar<-(colnames(tabtransform)%in%datatestdiff$name)
      indvar[1]<-T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
    }
    useddata<-data.frame("names"=datatest[,1],
                         "pval"=pval,
                         "logFC"=datatest[,5],
                         "mean1"=datatest[,9],
                         "mean2"=datatest[,10])
  }
  return(list("tabdiff"=tabdiff,
              "datatest"=datatest,
              "hypothesistest"=datatesthypothesis,
              "useddata"=useddata,
              "testparameters"=testparameters,
              "multivariateresults"=multivariateresults))
}


diffexptest<-function(toto,test="Wtest"){ 
  #fonction test if the variables (in column) of toto (dataframe) are differently 
  #expressed according to the first variable (first column) (two groups : OP Tem)
  #test= Ttes: porsuit a sTudent test for each column (parmetric test), the sample have to be normal and with the same variance
  #Wtest : willcoxon test (nonparametric), assume that dispersion a on the same scale
  group<-toto[,1]
  toto<-toto[,-1]
  pval<-vector()
  adjustpval<-vector()
  mlev1<-vector()
  namelev1<-levels(group)[1]
  mlev2<-vector()
  namelev2<-levels(group)[2]
  FC1o2<-vector()
  FC2o1<-vector()
  auc<-vector()
  resyounden<-matrix(ncol = 4,nrow = ncol(toto))
  for (i in 1:max(1,ncol(toto)) ){
    lev1<-toto[which(group==namelev1),i]
    lev2<-toto[which(group==namelev2),i]
    mlev1[i]<-mean(lev1,na.rm = T)+0.0001
    mlev2[i]<-mean(lev2,na.rm = T)+0.0001
    
    FC1o2[i]<-mlev1[i]/mlev2[i]
    FC2o1[i]<-mlev2[i]/mlev1[i]
    auc[i]<-pROC::auc(pROC::roc(group,toto[,i],quiet=TRUE))
    resyounden[i,]<-younden(response = group,predictor = toto[,i])
    if( test=="Ttest"){pval[i]<-t.test(x = lev1,y = lev2)$p.value}
    else if( test=="Wtest"){pval[i]<-wilcox.test(lev1 ,lev2,exact = F)$p.value } 
  } 
  pval[which(is.na(pval))]<-1
  adjustpval<-p.adjust(pval, method = "BH")
  logFC1o2<-log2(abs(FC1o2))
  logFC2o1<-log2(abs(FC2o1))
  
  
  listgen<-data.frame(colnames(toto),pval,adjustpval,auc,FC1o2,logFC1o2,FC2o1,logFC2o1,mlev1,mlev2,resyounden) 
  colnames(listgen)<-c("name",paste("pval",test,sep = ""),paste("BHadjustpval",test,sep = ""),"AUC",paste("FoldChange ",namelev1,"/",namelev2,sep = ""),paste("logFoldChange ",namelev1,"/",namelev2,sep = ""),
                       paste("FoldChange ",namelev2,"/",namelev1,sep = ""),paste("logFoldChange ",namelev2,"/",namelev1,sep = ""),paste("mean",namelev1,sep = ""),paste("mean",namelev2,sep = ""),
                       "younden criterion","sensibility younden","specificity younden","threshold younden") 
  return(listgen)
}

younden<-function(response,predictor){
  res<-pROC::roc(response,predictor,quiet=T)
  youndenscore<-res$sensitivities+res$specificities-1
  best<-which(youndenscore==max(youndenscore))[1] # Only the first best is kept
  youndenbest<-youndenscore[best]
  sensiyounden<-res$specificities[best]
  speciyounden<-res$sensitivities[best]
  thresholdyounden<-res$thresholds[best]
  return(c(youndenbest,sensiyounden,speciyounden,thresholdyounden))
}

##########################
# Multivariate variable selection functions
##########################

multivariateselection<-function(toto, method="lasso", lambda=NULL, alpha=0.5, nlambda=100){
  # Function for multivariate variable selection using regularization methods
  # toto: dataframe with first column as group (factor) and other columns as features
  # method: "lasso" (alpha=1), "elasticnet" (0<alpha<1), "ridge" (alpha=0)
  # lambda: regularization parameter (NULL for automatic selection via CV)
  # alpha: elastic net mixing parameter (0=ridge, 1=lasso)
  # nlambda: number of lambda values to test
  
  # IMPORTANT: Encode group so that 1 = first level (positif), 0 = second level (negatif)
  lev <- levels(toto[,1])
  group <- ifelse(toto[,1] == lev[1], 1, 0)
  x <- as.matrix(toto[,-1])
  
  # Set alpha based on method
  if(method == "lasso"){
    alpha <- 1
  } else if(method == "ridge" | method == "cox"){
    alpha <- 0
  } else if(method == "elasticnet"){
    # alpha is provided by user, default 0.5
  }
  
  # Perform stratified cross-validation to find optimal lambda if not provided
  if(is.null(lambda)){
    set.seed(20011203)
    mv_k <- min(5, nrow(toto)-1)
    mv_folds <- create_stratified_folds(toto[,1], k = mv_k)
    mv_foldid <- folds_to_foldid(mv_folds, nrow(toto))
    cvfit <- cv.glmnet(x, group, family="binomial", alpha=alpha, nlambda=nlambda,
                       type.measure="auc", foldid=mv_foldid
    )
    lambda <- cvfit$lambda.min  # lambda that gives minimum CV error
    lambda_1se <- cvfit$lambda.1se  # lambda within 1 SE of minimum
  } else {
    cvfit <- NULL
    lambda_1se <- lambda
  }
  
  # Fit model with optimal lambda
  fit <- glmnet(x, group, family="binomial", alpha=alpha, lambda=lambda)
  
  # Extract coefficients
  coef_matrix <- as.matrix(coef(fit))
  coef_values <- coef_matrix[-1, 1]  # Remove intercept
  names(coef_values) <- colnames(x)
  
  # Select non-zero coefficients
  selected_vars <- names(coef_values[coef_values != 0])
  
  # Calculate additional statistics for selected variables
  if(length(selected_vars) > 0){
    # AUC for each selected variable
    auc_values <- sapply(selected_vars, function(var){
      pROC::auc(pROC::roc(group, x[, var], quiet=TRUE))
    })
    
    # Mean values by group
    mlev1 <- colMeans(x[which(group==0), selected_vars, drop=FALSE], na.rm=TRUE)
    mlev2 <- colMeans(x[which(group==1), selected_vars, drop=FALSE], na.rm=TRUE)
    
    # Fold change : class 1 sur class 2:  case versus control
    # class 1 : first level (positif)
    # class 2 : second level (negatif)
    FC1o2 <- mlev1 / (mlev2 + 0.0001)
    logFC1o2 <- log2(abs(FC1o2))
    
    # Create results dataframe
    results <- data.frame(
      name = selected_vars,
      coefficient = coef_values[selected_vars],
      AUC = auc_values,
      FoldChange = FC1o2,
      logFoldChange = logFC1o2,
      mean_group1 = mlev1,
      mean_group2 = mlev2,
      stringsAsFactors = FALSE
    )
    
    # Sort by absolute coefficient value
    results <- results[order(abs(results$coefficient), decreasing=TRUE), ]
  } else {
    results <- data.frame()
  }
  
  # Return results with model information
  return(list(
    results = results,
    selected_vars = selected_vars,
    all_coefficients = coef_values,
    lambda = lambda,
    lambda_1se = lambda_1se,
    alpha = alpha,
    cvfit = cvfit,
    fit = fit,
    method = method
  ))
}

##########################
# Clustering + Elastic Net selection function
##########################

preprocess_peptides <- function(peptide_data, min_patients = 20) {
  n_nonzero <- colSums(peptide_data != 0, na.rm = TRUE)
  keep_peptides <- n_nonzero >= min_patients
  
  variances <- apply(peptide_data, 2, var, na.rm = TRUE)
  keep_var <- variances > 1e-10
  
  return(peptide_data[, keep_peptides & keep_var, drop=FALSE])
}

varselClust <- function(toto, n_clusters = 100, n_bootstrap = 500, alpha_enet = 0.5,
                        min_selection_freq = 0.5, preprocess = TRUE, min_patients = 20){
  
  withProgress(message = 'Selecting variables in progress...', value = 0, {
    
    # Extract group and data
    lev <- levels(toto[,1])
    group <- ifelse(toto[,1] == lev[1], 1, 0)
    y <- group
    data <- as.matrix(toto[,-1])
    
    # Optional preprocessing
    if(preprocess && ncol(data) > min_patients){
      incProgress(0.05, detail = "Data pre-processing...")
      cat("Preprocessing data: filtering low variance and low frequency variables...\n")
      data_preprocessed <- preprocess_peptides(data, min_patients = min_patients)
      if(ncol(data_preprocessed) < ncol(data)){
        cat(sprintf("  Preprocessing: %d → %d variables (removed %d)\n",
                    ncol(data), ncol(data_preprocessed), ncol(data) - ncol(data_preprocessed)))
        data <- data_preprocessed
      }
    }
    
    if(ncol(data) == 0){
      warning("No variables remaining after preprocessing")
      return(list(
        selected_peptides_per_cluster = character(0),
        final_selected_peptides = character(0),
        selection_frequencies = data.frame()
      ))
    }
    
    # Step 1: Clustering based on Spearman correlation
    incProgress(0.1, detail = sprintf("Clustering (%d variables)...", ncol(data)))
    cat(sprintf("Step 1: Clustering %d variables into %d clusters...\n", ncol(data), n_clusters))
    correlation_matrix <- cor(data, use = "pairwise.complete.obs", method = "spearman")
    #distance_matrix <- 1 - abs(correlation_matrix)
    distance_matrix <- sqrt(2 - 2*correlation_matrix)
    distance_matrix[is.na(distance_matrix)] <- 1
    hc <- hclust(as.dist(distance_matrix), method = "ward.D2")
    k <- min(n_clusters, ncol(data))
    clusters <- cutree(hc, k = k)
    
    # Step 2: Select one variable per cluster using Wilcoxon test
    incProgress(0.05, detail = "Cluster selection...")
    cat(sprintf("Step 2: Selecting one variable per cluster (Wilcoxon test)...\n"))
    selected_peptides <- c()
    
    for (i in 1:k){
      cluster_peptides <- names(clusters[clusters == i])
      
      if (length(cluster_peptides) > 1){
        p_values <- c()
        for (peptide in cluster_peptides){
          test_result <- tryCatch({
            wilcox.test(data[, peptide] ~ y, exact = FALSE)
          }, error = function(e){
            list(p.value = 1)
          })
          p_values <- c(p_values, test_result$p.value)
        }
        min_p_value_index <- which.min(p_values)
        selected_peptide <- cluster_peptides[min_p_value_index]
      } else {
        selected_peptide <- cluster_peptides[1]
      }
      selected_peptides <- c(selected_peptides, selected_peptide)
    }
    
    data_clust <- data[, selected_peptides, drop=FALSE]
    cat(sprintf("  Selected %d variables (one per cluster)\n", ncol(data_clust)))
    
    # Step 3: Bootstrap + Elastic Net selection (70% de la progression)
    incProgress(0, detail = sprintf("Bootstrap + Elastic Net (0/%d)...", n_bootstrap))
    cat(sprintf("Step 3: Bootstrap + Elastic Net selection (%d iterations)...\n", n_bootstrap))
    set.seed(123)
    selected_peptides_list <- list()
    
    progress_step <- 0.7 / n_bootstrap  # 70% du total pour le bootstrap
    
    for (b in 1:n_bootstrap) {
      if(b %% 50 == 0) {
        incProgress(progress_step * 50, 
                    detail = sprintf("Bootstrap: %d/%d (%.1f%%)", b, n_bootstrap, (b/n_bootstrap)*100))
        cat(sprintf("  Bootstrap iteration: %d/%d\n", b, n_bootstrap))
      }
      
      bootstrap_indices <- sample(1:nrow(data_clust), replace = TRUE)
      X_bootstrap <- data_clust[bootstrap_indices, , drop=FALSE]
      y_bootstrap <- y[bootstrap_indices]
      
      lasso_model <- tryCatch({
        cv.glmnet(as.matrix(X_bootstrap),
                  y_bootstrap,
                  family = "binomial",
                  alpha = alpha_enet)
      }, error = function(e){
        NULL
      })
      
      if(!is.null(lasso_model)){
        coef_lasso <- coef(lasso_model, s = "lambda.min")
        selected_peptides_iter <- rownames(coef_lasso)[which(coef_lasso != 0)][-1]
        selected_peptides_list[[b]] <- selected_peptides_iter
      }
    }
    
    # Step 4: Count selection frequencies
    incProgress(0.05, detail = "Frequency calculation...")
    peptide_selection_counts <- table(unlist(selected_peptides_list))
    data_of_frequencies <- sort(peptide_selection_counts, decreasing = TRUE)
    data_of_frequencies_df <- as.data.frame(data_of_frequencies)
    colnames(data_of_frequencies_df) <- c("Variable", "SelectionCount")
    data_of_frequencies_df$SelectionFrequency <- data_of_frequencies_df$SelectionCount / n_bootstrap
    
    # Step 5: Select final variables
    incProgress(0.05, detail = "Final selection...")
    threshold_count <- ceiling(n_bootstrap * min_selection_freq)
    final_selected_peptides <- names(peptide_selection_counts[peptide_selection_counts >= threshold_count])
    
    cat(sprintf("  Final selection: %d variables selected in >= %.0f%% of bootstraps (threshold: %d/%d)\n",
                length(final_selected_peptides), min_selection_freq * 100, threshold_count, n_bootstrap))
    
    incProgress(0, detail = "Done!")
    
    return(list(
      selected_peptides_per_cluster = selected_peptides,
      final_selected_peptides = final_selected_peptides,
      selection_frequencies = data_of_frequencies_df,
      n_clusters = k,
      n_bootstrap = n_bootstrap,
      alpha = alpha_enet,
      min_selection_freq = min_selection_freq
    ))
    
  }) # Fin withProgress
}

##########################
# Wrapper function for clustering + elasticnet to match other test methods
##########################

clustEnetSelection <- function(toto, n_clusters = 100, n_bootstrap = 500,
                               alpha_enet = 0.5, min_selection_freq = 0.5,
                               preprocess = TRUE, min_patients = 20){
  # Run varselClust
  clust_result <- varselClust(toto,
                              n_clusters = n_clusters,
                              n_bootstrap = n_bootstrap,
                              alpha_enet = alpha_enet,
                              min_selection_freq = min_selection_freq,
                              preprocess = preprocess,
                              min_patients = min_patients)
  
  selected_vars <- clust_result$final_selected_peptides
  
  # If no variables selected, return empty results
  if(length(selected_vars) == 0){
    return(list(
      results = data.frame(),
      selected_vars = character(0),
      all_coefficients = numeric(0),
      clust_result = clust_result,
      method = "clustEnet"
    ))
  }
  
  # Calculate statistics for selected variables (similar to multivariateselection)
  lev <- levels(toto[,1])
  group <- ifelse(toto[,1] == lev[1], 1, 0)
  x <- as.matrix(toto[,-1])
  
  # Get selection frequencies for selected variables
  freq_df <- clust_result$selection_frequencies
  freq_values <- freq_df$SelectionFrequency[match(selected_vars, freq_df$Variable)]
  
  # AUC for each selected variable
  auc_values <- sapply(selected_vars, function(var){
    pROC::auc(pROC::roc(group, x[, var], quiet=TRUE))
  })
  
  # Mean values by group
  mlev1 <- colMeans(x[which(group==0), selected_vars, drop=FALSE], na.rm=TRUE)
  mlev2 <- colMeans(x[which(group==1), selected_vars, drop=FALSE], na.rm=TRUE)
  
  # Fold change
  FC1o2 <- mlev1 / (mlev2 + 0.0001)
  logFC1o2 <- log2(abs(FC1o2))
  
  # Create results dataframe
  results <- data.frame(
    name = selected_vars,
    SelectionFrequency = freq_values,
    AUC = auc_values,
    FoldChange = FC1o2,
    logFoldChange = logFC1o2,
    mean_group1 = mlev1,
    mean_group2 = mlev2,
    stringsAsFactors = FALSE
  )
  
  # Sort by selection frequency
  results <- results[order(results$SelectionFrequency, decreasing=TRUE), ]
  
  # Return results
  return(list(
    results = results,
    selected_vars = selected_vars,
    all_frequencies = clust_result$selection_frequencies,
    clust_result = clust_result,
    method = "clustEnet"
  ))
}

PlotPca = function(data, y, title = "PCA of selected peptides") {
  pca_result = prcomp(data, center = TRUE, scale. = TRUE)
  
  # Calculer la variance expliquée
  var_explained <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)
  
  pca_data = data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    Group = as.factor(y)
  )
  
  ggplot(pca_data, aes(x = PC1, y = PC2, color = Group)) +
    geom_point(size = 3, alpha = 0.7) +
    #stat_ellipse(aes(fill = Group), geom = "polygon", alpha = 0.1, show.legend = FALSE) +
    labs(
      title = title, 
      x = paste0("PC1 (", var_explained[1], "% variance)"),
      y = paste0("PC2 (", var_explained[2], "% variance)")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, face = 'bold'),
      axis.text.y = element_text(size = 10, face = 'bold'),
      plot.title = element_text(size = 15, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10)
    )
}


# selection de variable par l'ago de Boruta 
borutaSelection =  function(toto, maxRuns = 100, seed = 123) {
  set.seed(seed)
  lev <- levels(toto[,1])
  group <- ifelse(toto[,1] == lev[1], 1, 0)
  data <- toto[,-1]
  
  boruta_result <- Boruta(x = data, y = group, maxRuns = maxRuns, doTrace = 0)
  
  # selected_vars <- names(boruta_result$finalDecision)[boruta_result$finalDecision == "Confirmed"]
  selected_vars <- getSelectedAttributes(boruta_result)
  cat('nombre de variables selectionné : ', length(selected_vars) , "\n")
  
  if(length(selected_vars) > 0){
    # AUC for each selected variable
    auc_values <- sapply(selected_vars, function(var){
      pROC::auc(pROC::roc(group, data[, var], quiet=TRUE))
    })
    
    # Mean values by group
    mlev1 <- colMeans(data[which(group==0), selected_vars, drop=FALSE], na.rm=TRUE)
    mlev2 <- colMeans(data[which(group==1), selected_vars, drop=FALSE], na.rm=TRUE)
    
    # Fold change : class 1 sur class 2:  case versus control
    # class 1 : first level (positif)
    # class 2 : second level (negatif)
    FC1o2 <- mlev1 / (mlev2 + 0.0001)
    logFC1o2 <- log2(abs(FC1o2))
    
    # Create results dataframe
    results <- data.frame(
      name = selected_vars,
      AUC = auc_values,
      FoldChange = FC1o2,
      logFoldChange = logFC1o2,
      mean_group1 = mlev1,
      mean_group2 = mlev2,
      stringsAsFactors = FALSE
    )
    
    # Sort by absolute coefficient value
    # results <- results[order(abs(results$coefficient), decreasing=TRUE), ]
  } else {
    results <- data.frame()
  }
  
  return(list(
    results =  results,
    name =  selected_vars,
    selected_vars = selected_vars,
    boruta_result = boruta_result,
    mean_group1 = mlev1,
    mean_group2 = mlev2
  ))
}


# variable selection using Sparse generalized canonical correlation analysis 
# library(RGCCA)
# RGCCA_selection <- function(toto, n_components = 2, sparsity = 0.1, seed = 123) {
#   set.seed(seed)
#   lev <- levels(toto[,1])
#   group <- ifelse(toto[,1] == lev[1], 1, 0)
#   data <- as.matrix(toto[,-1])
#   
#   rgcca_result <- rgcca(data, Y = group, ncomp = n_components, sparsity = sparsity)
#   
#   # Extract selected variables based on non-zero loadings
#   selected_vars <- colnames(data)[which(rgcca_result$loadings != 0)]
#   
#   return(list(
#     results = data.frame(name = selected_vars),
#     selected_vars = selected_vars,
#     rgcca_result = rgcca_result
#   ))
# }
# 
# 
# # variables selection using intNMF 
# library(intNMF)
# intNMF_selection <- function(toto, n_clusters = 2, seed = 123) {
#   set.seed(seed)
#   lev <- levels(toto[,1])
#   group <- ifelse(toto[,1] == lev[1], 1, 0)
#   data <- as.matrix(toto[,-1])
#   intnmf_result <- intNMF(data, Y = group, nclusters = n_clusters)
#   # Extract selected variables based on non-zero loadings
#   selected_vars <- colnames(data)[which(intnmf_result$loadings != 0)]
#   return(list(
#     results = data.frame(name = selected_vars),
#     selected_vars = selected_vars,
#     intnmf_result = intnmf_result
#   ))
# }
# 
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("iClusterPlus")

# variables selection using iClusterPlus
library(iClusterPlus)
iClusterPlus_selection <- function(toto, n_clusters = 2, seed = 123) {
  set.seed(seed)
  lev <- levels(toto[,1])
  group <- ifelse(toto[,1] == lev[1], 1, 0)
  data <- as.matrix(toto[,-1])
  iCluster_result <- iClusterPlus(data, Y = group, nclusters = n_clusters)
  # Extract selected variables based on non-zero loadings
  selected_vars <- colnames(data)[which(iCluster_result$loadings != 0)]
  return(list(
    results = data.frame(name = selected_vars),
    selected_vars = selected_vars,
    iCluster_result = iCluster_result
  ))
}

volcanoplot<-function(logFC,pval,thresholdFC=0,thresholdpv=0.05,graph=T,maintitle="Volcano plot",completedata){
  ##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off
  
  threshold <- (as.numeric(abs(logFC) > thresholdFC &pval< thresholdpv ) +1)*2
  listgen<-data.frame("logFC"=logFC,"pval"=pval,"threshold"=threshold)
  if(!graph){return(completedata)}
  ##Construct the plot object
  g = ggplot(data=listgen, aes(x=logFC, y=-log10(pval))) +
    geom_point(alpha=0.4, size=1.75, colour=threshold) +
    theme(legend.position = "none") +
    #xlim(c(-(max(listgen$logFC)+0.2), max(listgen$logFC)+0.2)) + ylim(c(0, max(-log10(listgen$pval))+0.2)) +
    xlab("log2 fold change") + ylab("-log10 p-value")+
    ggtitle(maintitle)+theme(plot.title=element_text( size=15))+
    annotate("text",x=Inf,y=Inf,label=paste(substring(colnames(completedata)[3],first=4)),size=6,vjust=2,hjust=1.5)
  
  g
} 

barplottest<-function(feature,logFC,levels,pval,mean1,mean2,thresholdpv=0.05,thresholdFC=1,graph=T,maintitle="Mean by group for differentially expressed variables"){
  feature<-rep(feature,each=2)
  group<-rep(c(levels[1],levels[2]),times=(length(feature)/2))
  group<-factor(group,levels =c(levels[1],levels[2]))
  pval2<-rep((pval< thresholdpv),each=2)
  logFC2<-rep((abs(logFC)> thresholdFC),each=2) 
  mean<-vector() 
  mean[seq(from=1,to=length(feature),by = 2)]<-mean1
  mean[seq(from=2,to=length(feature),by = 2)]<-mean2
  data<-data.frame(feature,group,pval,logFC,mean,logFC2,pval2)
  data<-data[order(data$pval),]
  if(!graph){
    data<-data[order(data[,1]),]
    return(data[which((data$pval2==TRUE)& (data$logFC2==TRUE)),c(1,2,5)])}
  else{
    ggplot(data[which( ( data$pval2) & (data$logFC2) ),], aes(feature, mean,fill=group))+
      geom_bar(stat="identity", position="dodge")+ 
      ggtitle(maintitle)+
      theme(plot.title=element_text( size=15), 
            axis.text.y = element_text(size=10, face =  'bold'),
            legend.title = element_text(size=11, face = "bold"),
            legend.text = element_text(size=10, face = 'bold'),
            axis.title.y = element_text(size=12, face = "bold"),
            axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5, hjust=0.51, face = 'bold')
      )
  }
}
errorplot<-function(text=paste("error /n","text error")){
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, text,cex = 1.6, col = "black")}

barplottestSF<-function(toto,graph=T){
  #toto: dataframe res from conditiontest function
  if(!graph){return(toto)}
  rescond<-vector()
  for (i in (1:nrow(toto))){
    if(toto$samplenorm[i]=="norm" & toto$varequal[i]!="varequal"){rescond[i]<-"norm"}
    else if(toto$samplenorm[i]=="norm" & toto$varequal[i]=="varequal"){rescond[i]<-"both"}
    else if( toto$samplenorm[i]!="norm" &toto$varequal[i]=="varequal"){rescond[i]<-"varequal"}
    else{rescond[i]<-"none"}
    
  }
  data<-as.factor(rescond)
  p<-qplot(factor(data), geom="bar", fill=factor(data))
  p+ggtitle("Repartition of the variables according to the test results")+
    theme(plot.title=element_text( size=15), 
          axis.text.y = element_text(size=10, face =  'bold'),
          legend.title = element_text(size=11, face = "bold"),
          legend.text = element_text(size=10, face = 'bold'),
          axis.title.y = element_text(size=12, face = "bold"),
          axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5, hjust=0.51, face = 'bold')
    )
}

SFtest<-function(toto,shaptest=T,Ftest=T,threshold=0.05){
  x<-toto[,1]
  toto<-toto[,-1]
  pvalF<-vector()
  pvalnormlev1<-vector()
  pvalnormlev2<-vector()
  vlev1<-vector()
  vlev2<-vector()
  samplenorm<-vector()
  varequal<-vector()
  conditiontest<-data.frame("name"=colnames(toto))
  for (i in 1:ncol(toto) ){
    lev1<-toto[which(x==levels(x)[1]),i]
    lev2<-toto[which(x==levels(x)[2]),i]
    if(shaptest){
      #pvalnormTem[i]<-shapiro.test(Tem)$p.value
      
      out<- tryCatch(shapiro.test(lev1)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev1[i]<-1
      else{pvalnormlev1[i]<-out}
      
      out<- tryCatch(shapiro.test(lev2)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev2[i]<-1
      else{pvalnormlev2[i]<-out}
      
      if((pvalnormlev2[i]>=threshold) & (pvalnormlev1[i]>=threshold)){samplenorm[i]<-"norm"}
      else{samplenorm[i]<-"notnorm"}
    }
    if(Ftest){
      #to perform a fisher test the value have to be normal
      pvalF[i]<-var.test(lev1,lev2)$p.value
      if(is.na(pvalF[i]))pvalF[i]<-1
      vlev1[i]<-var(lev1)
      vlev2[i]<-var(lev2)
      if(pvalF[i]>=threshold){varequal[i]<-"varequal"}
      else{varequal[i]<-"varnotequal"}
    }
  }
  if(shaptest){ conditiontest<-data.frame(conditiontest,pvalnormlev1,pvalnormlev2,"samplenorm"=samplenorm)
  colnames(conditiontest)<-c("names",paste("pvalshapiro",levels(x)[1],sep=""),paste("pvalshapiro",levels(x)[2],sep = ""),"samplenorm")
  }
  if(Ftest){conditiontest<-data.frame(conditiontest,"pvalF"=pvalF,"variancelev1"=vlev1,"variancelev2"=vlev2,"varequal"=varequal)}
  return(conditiontest) 
}

####

# ── Stratified CV helper functions ─────────────────────────────────────────────

#' Create stratified k-fold cross-validation indices
#' Ensures each fold preserves the class distribution of the target variable
#' @param y Target vector (factor or numeric)
#' @param k Number of folds
#' @param seed Random seed for reproducibility
#' @return List of k integer vectors, each containing test indices for that fold
create_stratified_folds <- function(y, k = 5, seed = 20011203) {
  set.seed(seed)
  if (is.factor(y)) {
    classes <- levels(y)
  } else {
    classes <- unique(y)
  }
  fold_ids <- rep(0L, length(y))
  for (cl in classes) {
    idx <- which(y == cl)
    idx <- sample(idx)
    fold_ids[idx] <- rep(1:k, length.out = length(idx))
  }
  folds <- lapply(1:k, function(i) which(fold_ids == i))
  names(folds) <- paste0("Fold", 1:k)
  return(folds)
}

#' Compute AUC using the Mann-Whitney U statistic (no external dependency)
#' @param y_true True labels (factor or 0/1 numeric)
#' @param scores Predicted probabilities for the positive class
#' @param pos_level Which level is positive (default: first level of factor)
#' @return AUC value between 0 and 1
compute_auc_manual <- function(y_true, scores, pos_level = NULL) {
  if (is.factor(y_true)) {
    if (is.null(pos_level)) pos_level <- levels(y_true)[1]
    true_bin <- as.integer(y_true == pos_level)
  } else {
    true_bin <- as.integer(y_true)
  }
  r     <- rank(scores)
  n_pos <- sum(true_bin)
  n_neg <- length(true_bin) - n_pos
  if (n_pos == 0 || n_neg == 0) return(0.5)
  (sum(r[true_bin == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

#' Convert foldid vector from stratified folds (for cv.glmnet compatibility)
#' @param folds List of fold test indices (output of create_stratified_folds)
#' @param n Total number of observations
#' @return Integer vector of fold assignments (1..k)
folds_to_foldid <- function(folds, n) {
  foldid <- rep(0L, n)
  for (i in seq_along(folds)) foldid[folds[[i]]] <- i
  return(foldid)
}

# ── Stratified Grid Search functions ───────────────────────────────────────────

#' Stratified Grid Search for Random Forest
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector (factor)
#' @param param_grid List with n_estimators, max_features, min_samples_split
#' @param n_folds Number of stratified CV folds
#' @param scoring Scoring metric(s) - first element used for selection
#' @return List with best_params and best_score
tune_rf_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("auc", "accuracy")) {
  folds <- create_stratified_folds(y, k = n_folds)
  cat(sprintf("RF stratified grid search (%d-fold)...\n", n_folds))
  
  if (is.null(param_grid)) {
    param_grid <- list(
      n_estimators      = c(100, 500, 1000),
      max_features      = c("sqrt", "log2"),
      min_samples_split = c(1, 5, 10)
    )
  }
  
  p <- ncol(X)
  mtry_values <- unique(sapply(param_grid$max_features, function(mf) {
    if (is.character(mf) && mf == "sqrt")  return(max(1, floor(sqrt(p))))
    if (is.character(mf) && mf == "log2")  return(max(1, floor(log2(p))))
    return(as.numeric(mf))
  }))
  
  pos_level  <- levels(y)[1]
  best_score <- -Inf
  best_params <- NULL
  
  for (nt in param_grid$n_estimators) {
    for (mt in mtry_values) {
      for (ns in param_grid$min_samples_split) {
        fold_scores <- sapply(folds, function(test_idx) {
          train_idx <- setdiff(1:nrow(X), test_idx)
          tryCatch({
            mdl   <- randomForest(x = X[train_idx, , drop = FALSE], y = y[train_idx],
                                  ntree = nt, mtry = mt, nodesize = ns, importance = FALSE)
            probs <- randomForest:::predict.randomForest(mdl, X[test_idx, , drop = FALSE], type = "prob")
            compute_auc_manual(y[test_idx], probs[, pos_level], pos_level)
          }, error = function(e) NA_real_)
        })
        sc <- mean(fold_scores, na.rm = TRUE)
        if (!is.na(sc) && sc > best_score) {
          best_score  <- sc
          best_params <- list(n_estimators = nt, max_features = mt, min_samples_split = ns)
        }
      }
    }
  }
  cat(sprintf("  Best: ntree=%d, mtry=%d, nodesize=%d, AUC=%.4f\n",
              best_params$n_estimators, best_params$max_features, best_params$min_samples_split, best_score))
  list(best_params = best_params, best_score = best_score)
}

#' Stratified Grid Search for XGBoost
tune_xgb_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("auc", "accuracy")) {
  folds <- create_stratified_folds(y, k = n_folds)
  cat(sprintf("XGBoost stratified grid search (%d-fold)...\n", n_folds))
  
  if (is.null(param_grid)) {
    param_grid <- list(
      n_estimators  = c(50, 100, 200),
      max_depth     = c(3, 6, 9),
      learning_rate = c(0.01, 0.1, 0.3),
      gamma         = c(0, 0.1),
      subsample     = c(0.8, 1.0)
    )
  }
  
  if (is.factor(y)) {
    pos_level <- levels(y)[1]
    y_num     <- ifelse(y == pos_level, 1, 0)
  } else {
    y_num <- y
  }
  X_mat <- as.matrix(X)
  
  combos <- expand.grid(param_grid, stringsAsFactors = FALSE)
  best_score  <- -Inf
  best_params <- NULL
  
  for (i in 1:nrow(combos)) {
    row <- combos[i, ]
    params <- list(objective = "binary:logistic", eval_metric = "auc",
                   max_depth = row$max_depth, eta = row$learning_rate,
                   gamma = row$gamma, subsample = row$subsample,
                   min_child_weight = if ("min_child_weight" %in% names(row)) row$min_child_weight else 1)
    nrounds <- row$n_estimators
    
    fold_scores <- sapply(folds, function(test_idx) {
      train_idx <- setdiff(1:nrow(X_mat), test_idx)
      tryCatch({
        dtrain <- xgb.DMatrix(data = X_mat[train_idx, , drop = FALSE], label = y_num[train_idx])
        dtest  <- xgb.DMatrix(data = X_mat[test_idx, , drop = FALSE],  label = y_num[test_idx])
        mdl    <- xgb.train(params = params, data = dtrain, nrounds = nrounds, verbose = 0)
        preds  <- predict(mdl, dtest)
        compute_auc_manual(y_num[test_idx], preds)
      }, error = function(e) NA_real_)
    })
    sc <- mean(fold_scores, na.rm = TRUE)
    if (!is.na(sc) && sc > best_score) {
      best_score  <- sc
      best_params <- as.list(row)
    }
  }
  cat(sprintf("  Best: nrounds=%d, max_depth=%d, eta=%.3f, AUC=%.4f\n",
              best_params$n_estimators, best_params$max_depth, best_params$learning_rate, best_score))
  list(best_params = best_params, best_score = best_score)
}

#' Stratified Grid Search for Naive Bayes
tune_nb_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("auc", "accuracy")) {
  folds <- create_stratified_folds(y, k = n_folds)
  cat(sprintf("NaiveBayes stratified grid search (%d-fold)...\n", n_folds))
  
  if (is.null(param_grid)) {
    param_grid <- list(laplace = c(0, 0.5, 1, 2, 5))
  }
  
  pos_level   <- levels(y)[1]
  best_score  <- -Inf
  best_params <- NULL
  
  for (lap in param_grid$laplace) {
    fold_scores <- sapply(folds, function(test_idx) {
      train_idx <- setdiff(1:length(y), test_idx)
      tryCatch({
        mdl   <- e1071::naiveBayes(x = X[train_idx, , drop = FALSE], y = y[train_idx], laplace = lap)
        probs <- e1071:::predict.naiveBayes(mdl, X[test_idx, , drop = FALSE], type = "raw")
        compute_auc_manual(y[test_idx], probs[, pos_level], pos_level)
      }, error = function(e) NA_real_)
    })
    sc <- mean(fold_scores, na.rm = TRUE)
    if (!is.na(sc) && sc > best_score) {
      best_score  <- sc
      best_params <- list(laplace = lap)
    }
  }
  cat(sprintf("  Best: laplace=%.2f, AUC=%.4f\n", best_params$laplace, best_score))
  list(best_params = best_params, best_score = best_score)
}

#' Stratified Grid Search for KNN
tune_knn_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("auc", "accuracy")) {
  folds <- create_stratified_folds(y, k = n_folds)
  cat(sprintf("KNN stratified grid search (%d-fold)...\n", n_folds))
  
  if (is.null(param_grid)) {
    max_k <- min(floor(sqrt(length(y))), 30)
    param_grid <- list(n_neighbors = seq(3, max_k, by = 2))
  }
  
  pos_level   <- levels(y)[1]
  best_score  <- -Inf
  best_params <- NULL
  
  for (k_val in param_grid$n_neighbors) {
    fold_scores <- sapply(folds, function(test_idx) {
      train_idx <- setdiff(1:length(y), test_idx)
      tryCatch({
        pred      <- class::knn(train = X[train_idx, , drop = FALSE],
                                test  = X[test_idx, , drop = FALSE],
                                cl = y[train_idx], k = k_val, prob = TRUE)
        prob_attr <- attr(pred, "prob")
        probs     <- ifelse(pred == pos_level, prob_attr, 1 - prob_attr)
        compute_auc_manual(y[test_idx], probs, pos_level)
      }, error = function(e) NA_real_)
    })
    sc <- mean(fold_scores, na.rm = TRUE)
    if (!is.na(sc) && sc > best_score) {
      best_score  <- sc
      best_params <- list(n_neighbors = k_val)
    }
  }
  cat(sprintf("  Best: k=%d, AUC=%.4f\n", best_params$n_neighbors, best_score))
  list(best_params = best_params, best_score = best_score)
}

#' Stratified Grid Search for ElasticNet
tune_elasticnet_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("auc", "accuracy")) {
  folds <- create_stratified_folds(y, k = n_folds)
  cat(sprintf("ElasticNet stratified grid search (%d-fold)...\n", n_folds))
  
  if (is.null(param_grid)) {
    param_grid <- list(
      alpha  = c(0, 0.25, 0.5, 0.75, 1.0),
      lambda = c(0.001, 0.01, 0.1, 1.0)
    )
  }
  
  if (is.factor(y)) {
    pos_level <- levels(y)[1]
    y_num     <- ifelse(y == pos_level, 1, 0)
  } else {
    y_num <- y
  }
  X_mat <- as.matrix(X)
  
  combos <- expand.grid(param_grid, stringsAsFactors = FALSE)
  best_score  <- -Inf
  best_params <- NULL
  
  for (i in 1:nrow(combos)) {
    a <- combos$alpha[i]
    l <- combos$lambda[i]
    fold_scores <- sapply(folds, function(test_idx) {
      train_idx <- setdiff(1:nrow(X_mat), test_idx)
      tryCatch({
        fit   <- glmnet::glmnet(X_mat[train_idx, , drop = FALSE], y_num[train_idx],
                                family = "binomial", alpha = a, lambda = l)
        preds <- as.vector(glmnet:::predict.glmnet(fit, newx = X_mat[test_idx, , drop = FALSE], s = l, type = "response"))
        compute_auc_manual(y_num[test_idx], preds)
      }, error = function(e) NA_real_)
    })
    sc <- mean(fold_scores, na.rm = TRUE)
    if (!is.na(sc) && sc > best_score) {
      best_score  <- sc
      best_params <- list(alpha = a, lambda = l)
    }
  }
  cat(sprintf("  Best: alpha=%.3f, lambda=%.4f, AUC=%.4f\n", best_params$alpha, best_params$lambda, best_score))
  list(best_params = best_params, best_score = best_score)
}

####

tune_svm_stratified <- function(data, gamma_range = 10^(-5:2),
                                cost_range   = 10^(-3:2),
                                kernel       = "radial",
                                n_folds      = 5,
                                scoring      = "accuracy",
                                seed         = 20011203) {
  set.seed(seed)
  scoring <- match.arg(scoring, c("auc", "accuracy"))
  
  # Folds stratifiés (équilibre les classes dans chaque fold)
  folds <- caret::createFolds(data$group, k = n_folds,
                              list = TRUE, returnTrain = TRUE)
  
  grid <- expand.grid(gamma = gamma_range, cost = cost_range)
  
  grid$mean_score <- sapply(seq_len(nrow(grid)), function(i) {
    g <- grid$gamma[i]
    c <- grid$cost[i]
    
    fold_scores <- sapply(folds, function(train_idx) {
      train <- data[ train_idx, ]
      test  <- data[-train_idx, ]
      
      model <- tryCatch(
        svm(group ~ ., data = train,
            kernel      = kernel,
            gamma       = g,
            cost        = c,
            type        = "C-classification"),
        error = function(e) NULL
      )
      if (is.null(model)) return(NA)
      
      if (scoring == "accuracy") {
        preds <- e1071:::predict.svm(model, test)
        mean(preds == test$group)
      } else {
        dv <- attr(e1071:::predict.svm(model, test, decision.values = TRUE), "decision.values")
        lev   <- levels(data$group)
        scores  <- as.vector(dv)
        true_bin <- as.integer(test$group == lev[1])
        r        <- rank(scores)
        n_pos    <- sum(true_bin)
        n_neg    <- length(true_bin) - n_pos
        if (n_pos == 0 || n_neg == 0) return(NA)
        (sum(r[true_bin == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
      }
    })
    mean(fold_scores, na.rm = TRUE)
  })
  
  best_idx <- which.max(grid$mean_score)
  cat(sprintf("Best SVM params → gamma: %g | cost: %g | %s: %.4f\n",
              grid$gamma[best_idx], grid$cost[best_idx],
              scoring, grid$mean_score[best_idx]))
  
  list(
    best.parameters = list(gamma = grid$gamma[best_idx],
                           cost  = grid$cost[best_idx]),
    best.score      = grid$mean_score[best_idx],
    scoring         = scoring,
    all.results     = grid
  )
}


accuracy_error_fun <- function(true, pred) {
  
  if (is.matrix(pred)) {
    predicted_class <- colnames(pred)[apply(pred, 1, which.max)]
  } else {
    predicted_class <- as.character(pred)
  }
  accuracy <- mean(predicted_class == as.character(true))
  return(1 - accuracy)  #  minimise function
}
# Fonction custom AUC pour tune.svm 
auc_error_fun <- function(true, pred) {
  if (is.matrix(pred)) {
    scores <- pred[, 2]
  } else {
    scores <- as.numeric(pred)
  }
  true_bin <- as.integer(true == levels(true)[2])
  r        <- rank(scores)
  n_pos    <- sum(true_bin)
  n_neg    <- length(true_bin) - n_pos
  if (n_pos == 0 || n_neg == 0) return(0.5)
  auc <- (sum(r[true_bin == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  return(1 - auc)  # tune minimise 
}

modelfunction_V2 <- function(learningmodel,
                             validation = NULL,
                             modelparameters,
                             transformdataparameters,
                             datastructuresfeatures = NULL,
                             learningselect) {
  
  if (modelparameters$modeltype != "nomodel") {
    
    colnames(learningmodel)[1] <- "group"
    
    if (modelparameters$invers) {
      learningmodel[, 1] <- factor(learningmodel[, 1],
                                   levels = rev(levels(learningmodel[, 1])),
                                   ordered = TRUE)
    }
    lev <- levels(x = learningmodel[, 1])
    names(lev) <- c("positif", "negatif")
    
    # ── Random Forest ──────────────────────────────────────────────────────────
    if (modelparameters$modeltype == "randomforest") {
      learningmodel <- as.data.frame(learningmodel[sort(rownames(learningmodel)), ])
      x <- as.data.frame(learningmodel[, -1])
      colnames(x) <- colnames(learningmodel)[-1]
      #x <- as.data.frame(x[, sort(colnames(x))])
      set.seed(20011203)
      ntree_param <- ifelse(is.null(modelparameters$ntree), 1000, modelparameters$ntree)
      
      # Default values for all RF hyperparameters
      nodesize_param  <- 1
      maxnodes_param  <- NULL
      sampsize_param  <- NULL
      replace_param   <- if (!is.null(modelparameters$replace)) modelparameters$replace else TRUE
      sampsize_frac   <- if (!is.null(modelparameters$sampsize_frac)) modelparameters$sampsize_frac else 1.0
      
      if (is.null(modelparameters$autotunerf) || modelparameters$autotunerf) {
        if (!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch) {
          # ── GridSearchCV tuning ──
          cat("Using GridSearchCV for Random Forest hyperparameter tuning...\n")
          param_grid <- list(
            n_estimators    = if (!is.null(modelparameters$rf_grid_ntree))    modelparameters$rf_grid_ntree    else c(100, 500, 1000),
            max_features    = if (!is.null(modelparameters$rf_grid_mtry))     modelparameters$rf_grid_mtry     else c("sqrt", "log2"),
            min_samples_split = if (!is.null(modelparameters$rf_grid_nodesize)) modelparameters$rf_grid_nodesize else c(1, 5, 10)
          )
          grid_result <- tryCatch({
            tune_rf_gridsearch(X = x, y = learningmodel[, 1],
                               param_grid = param_grid, n_folds = 5,
                               scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to tuneRF:", e$message, "\n"); NULL
          })
          if (!is.null(grid_result)) {
            best_params <- grid_result$best_params
            optimal_mtry <- if (!is.null(best_params$max_features)) {
              if (best_params$max_features == "sqrt")  floor(sqrt(ncol(x)))
              else if (best_params$max_features == "log2") floor(log2(ncol(x)))
              else as.numeric(best_params$max_features)
            } else floor(sqrt(ncol(x)))
            ntree_param    <- if (!is.null(best_params$n_estimators))      best_params$n_estimators      else ntree_param
            nodesize_param <- if (!is.null(best_params$min_samples_split)) best_params$min_samples_split else 1
            cat(sprintf("GridSearchCV best params: ntree=%d, mtry=%d, nodesize=%d, score=%.4f\n",
                        ntree_param, optimal_mtry, nodesize_param, grid_result$best_score))
          } else {
            cat("GridSearchCV failed, falling back to stratified CV for mtry...\n")
            p <- ncol(x)
            mtry_candidates <- unique(c(max(1, floor(sqrt(p))), max(1, floor(log2(p))), max(1, floor(p/3))))
            fb_folds <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(x) - 1))
            pos_lev  <- levels(learningmodel[, 1])[1]
            best_fb  <- -Inf
            for (mt in mtry_candidates) {
              sc <- mean(sapply(fb_folds, function(ti) {
                tri <- setdiff(1:nrow(x), ti)
                tryCatch({
                  mdl <- randomForest(x = x[tri,,drop=FALSE], y = learningmodel[tri,1],
                                      ntree = ntree_param, mtry = mt, importance = FALSE)
                  prbs <- randomForest:::predict.randomForest(mdl, x[ti,,drop=FALSE], type = "prob")
                  compute_auc_manual(learningmodel[ti,1], prbs[, pos_lev], pos_lev)
                }, error = function(e) NA_real_)
              }), na.rm = TRUE)
              if (!is.na(sc) && sc > best_fb) { best_fb <- sc; optimal_mtry <- mt }
            }
            nodesize_param <- 1
          }
          # maxnodes tuning: pick best from grid via OOB after fixing ntree/mtry/nodesize
          maxnodes_candidates <- if (!is.null(modelparameters$rf_grid_maxnodes)) modelparameters$rf_grid_maxnodes else c(0)
          maxnodes_candidates <- as.numeric(maxnodes_candidates)
          if (length(maxnodes_candidates) > 1 || any(maxnodes_candidates != 0)) {
            best_oob <- Inf
            best_maxnodes <- NULL
            n_train <- nrow(x)
            samp_sz <- if (sampsize_frac < 1.0) round(n_train * sampsize_frac) else n_train
            for (mn in maxnodes_candidates) {
              mn_val <- if (mn == 0) NULL else mn
              tmp_model <- randomForest(x = x, y = learningmodel[, 1],
                                        ntree = ntree_param, mtry = optimal_mtry,
                                        nodesize = nodesize_param, maxnodes = mn_val,
                                        sampsize = if (sampsize_frac < 1.0) samp_sz else nrow(x),
                                        replace = replace_param, importance = FALSE)
              oob_err <- mean(tmp_model$err.rate[, "OOB"])
              cat(sprintf("  maxnodes=%s -> OOB=%.4f\n", ifelse(is.null(mn_val), "NULL", as.character(mn_val)), oob_err))
              if (oob_err < best_oob) { best_oob <- oob_err; best_maxnodes <- mn_val }
            }
            maxnodes_param <- best_maxnodes
            cat(sprintf("Best maxnodes: %s\n", ifelse(is.null(maxnodes_param), "NULL (unlimited)", as.character(maxnodes_param))))
          }
          # sampsize tuning: pick best fraction from grid
          sampsize_candidates <- if (!is.null(modelparameters$rf_grid_sampsize)) as.numeric(modelparameters$rf_grid_sampsize) else c(1.0)
          if (length(sampsize_candidates) > 1) {
            best_oob <- Inf
            best_frac <- 1.0
            for (sf in sampsize_candidates) {
              samp_sz <- round(nrow(x) * sf)
              tmp_model <- randomForest(x = x, y = learningmodel[, 1],
                                        ntree = ntree_param, mtry = optimal_mtry,
                                        nodesize = nodesize_param, maxnodes = maxnodes_param,
                                        sampsize = samp_sz, replace = replace_param, importance = FALSE)
              oob_err <- mean(tmp_model$err.rate[, "OOB"])
              cat(sprintf("  sampsize_frac=%.3f -> OOB=%.4f\n", sf, oob_err))
              if (oob_err < best_oob) { best_oob <- oob_err; best_frac <- sf }
            }
            sampsize_frac <- best_frac
            cat(sprintf("Best sampsize fraction: %.3f\n", sampsize_frac))
          }
        } else {
          # ── Traditional mode: stratified CV over ntree, mtry, nodesize ──
          ntree_values    <- if (!is.null(modelparameters$rf_ntree_range)) modelparameters$rf_ntree_range else c(500, 1000)
          nodesize_values <- if (!is.null(modelparameters$rf_nodesize_range)) modelparameters$rf_nodesize_range else c(1)
          p <- ncol(x)
          mtry_values <- unique(c(max(1, floor(sqrt(p))), max(1, floor(log2(p))), max(1, floor(p / 3))))
          
          folds_rf   <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(x) - 1))
          pos_level  <- levels(learningmodel[, 1])[1]
          best_score <- -Inf
          optimal_mtry <- floor(sqrt(p))
          
          cat("Traditional stratified CV: searching over ntree, mtry and nodesize...\n")
          for (nt in ntree_values) {
            for (ns in nodesize_values) {
              for (mt in mtry_values) {
                fold_scores <- sapply(folds_rf, function(test_idx) {
                  train_idx <- setdiff(1:nrow(x), test_idx)
                  tryCatch({
                    mdl   <- randomForest(x = x[train_idx, , drop = FALSE], y = learningmodel[train_idx, 1],
                                          ntree = nt, mtry = mt, nodesize = ns, importance = FALSE)
                    probs <- randomForest:::predict.randomForest(mdl, x[test_idx, , drop = FALSE], type = "prob")
                    compute_auc_manual(learningmodel[test_idx, 1], probs[, pos_level], pos_level)
                  }, error = function(e) NA_real_)
                })
                sc <- mean(fold_scores, na.rm = TRUE)
                cat(sprintf("  ntree=%d, mtry=%d, nodesize=%d -> AUC=%.4f\n", nt, mt, ns, sc))
                if (!is.na(sc) && sc > best_score) {
                  best_score     <- sc
                  ntree_param    <- nt
                  optimal_mtry   <- mt
                  nodesize_param <- ns
                }
              }
            }
          }
          cat(sprintf("Best stratified CV params: ntree=%d, mtry=%d, nodesize=%d, AUC=%.4f\n",
                      ntree_param, optimal_mtry, nodesize_param, best_score))
        }
      } else {
        # ── Manual mode ──
        optimal_mtry   <- ifelse(is.null(modelparameters$mtry), floor(sqrt(ncol(x))), modelparameters$mtry)
        nodesize_param <- if (!is.null(modelparameters$nodesize)) modelparameters$nodesize else 1
        maxnodes_param <- if (!is.null(modelparameters$maxnodes)) modelparameters$maxnodes else NULL
        sampsize_frac  <- if (!is.null(modelparameters$sampsize_frac)) modelparameters$sampsize_frac else 1.0
        replace_param  <- if (!is.null(modelparameters$replace)) modelparameters$replace else TRUE
      }
      
      # Compute actual sampsize from fraction
      sampsize_param <- if (!is.null(sampsize_frac) && sampsize_frac < 1.0) round(nrow(x) * sampsize_frac) else nrow(x)
      
      model <- randomForest(x = x, y = learningmodel[, 1],
                            ntree = ntree_param, mtry = optimal_mtry,
                            nodesize = nodesize_param, maxnodes = maxnodes_param,
                            sampsize = sampsize_param, replace = replace_param,
                            importance = TRUE)
      model$optimal_mtry    <- optimal_mtry
      model$ntree_used      <- ntree_param
      model$nodesize_used   <- nodesize_param
      model$maxnodes_used   <- maxnodes_param
      model$sampsize_used   <- sampsize_param
      model$replace_used    <- replace_param
      
      if (modelparameters$fs) {
        featureselect  <- selectedfeature(model = model, modeltype = "randomforest",
                                          tab = learningmodel, criterionimportance = "fscore",
                                          criterionmodel = "auc")
        model          <- featureselect$model
        learningmodel  <- featureselect$dataset
      }
      
      # ← Scores bruts seulement ; PAS de predictclasslearning ici
      scorelearning <- data.frame(model$votes[, lev["positif"]])
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
    }
    
    # ── SVM ────────────────────────────────────────────────────────────────────
    if (modelparameters$modeltype == "svm") {
      if (is.null(modelparameters$autotunesvm) || modelparameters$autotunesvm) {
        kernel_param <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)
        svm_scoring <- if (!is.null(modelparameters$svm_scoring)) modelparameters$svm_scoring else "auc"
        svm_gamma_range <- if (!is.null(modelparameters$svm_gamma_range)) modelparameters$svm_gamma_range else 10^(-5:2)
        svm_cost_range  <- if (!is.null(modelparameters$svm_cost_range))  modelparameters$svm_cost_range  else 10^(-3:2)
        tune_result <- tune_svm_stratified(data = learningmodel,
                                           gamma_range = svm_gamma_range,
                                           cost_range  = svm_cost_range,
                                           kernel      = kernel_param,
                                           n_folds     = min(5, nrow(learningmodel) - 1),
                                           scoring     = svm_scoring,
                                           seed        = 123)
        cat("Stratified SVM tuning results:\n"); print(tune_result$best.parameters)
        cost_param   <- tune_result$best.parameters$cost
        gamma_param  <- tune_result$best.parameters$gamma
      } else {
        cat("define svm parameters manually \n")
        cost_param   <- ifelse(is.null(modelparameters$cost),   1,       modelparameters$cost)
        gamma_param  <- ifelse(is.null(modelparameters$gamma),  0.1,     modelparameters$gamma)
        kernel_param <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)
        # epsilon_param <- ifelse(is.null(modelparameters$epsilon), 0.1, modelparameters$epsilon)
      }
      
      model <- svm(group ~ ., data = learningmodel,
                   kernel = kernel_param, cost = cost_param, gamma = gamma_param,
                   # epsilon = epsilon_param, 
                   type = "C-classification", probability = TRUE)
      model$cost  <- cost_param
      model$gamma <- gamma_param
      
      if (modelparameters$fs) {
        featureselect <- selectedfeature(model = model, modeltype = "svm",
                                         tab = learningmodel, criterionimportance = "fscore",
                                         criterionmodel = "auc")
        model         <- featureselect$model
        learningmodel <- featureselect$dataset
      }
      
      # ← Scores bruts seulement
      scorelearning <- model$decision.values
      if (sum(lev == (strsplit(colnames(scorelearning), split = "/")[[1]])) == 0) {
        scorelearning <- scorelearning * (-1)
        colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
      }
    }
    
    # ── LightGBM ───────────────────────────────────────────────────────────────
    if (modelparameters$modeltype == "lightgbm") {
      x      <- as.matrix(learningmodel[, -1])
      y      <- ifelse(learningmodel[, 1] == lev["positif"], 1, 0)
      dtrain <- lgb.Dataset(data = x, label = y)
      
      if (is.null(modelparameters$autotunelgb) || modelparameters$autotunelgb) {
        set.seed(20011203)
        best_params <- list(objective = "binary", metric = "auc",
                            num_leaves = 31, learning_rate = 0.05,
                            feature_fraction = 0.9, bagging_fraction = 0.8,
                            bagging_freq = 5, verbose = -1)
        lgb_folds <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(learningmodel) - 1))
        cv_results <- lgb.cv(params = best_params, data = dtrain, nrounds = 200,
                             folds = lgb_folds,
                             early_stopping_rounds = 10, verbose = -1)
        optimal_nrounds <- cv_results$best_iter
        model <- lgb.train(params = best_params, data = dtrain,
                           nrounds = optimal_nrounds, verbose = -1)
        model$optimal_nrounds      <- optimal_nrounds
        model$optimal_num_leaves   <- best_params$num_leaves
        model$optimal_learning_rate <- best_params$learning_rate
      } else {
        nrounds_param      <- ifelse(is.null(modelparameters$nrounds_lgb),    100,  modelparameters$nrounds_lgb)
        num_leaves_param   <- ifelse(is.null(modelparameters$num_leaves),      31,  modelparameters$num_leaves)
        learning_rate_param <- ifelse(is.null(modelparameters$learning_rate_lgb), 0.05, modelparameters$learning_rate_lgb)
        params <- list(objective = "binary", metric = "auc",
                       num_leaves = num_leaves_param, learning_rate = learning_rate_param,
                       feature_fraction = 0.9, bagging_fraction = 0.8,
                       bagging_freq = 5, verbose = -1)
        model <- lgb.train(params = params, data = dtrain, nrounds = nrounds_param, verbose = -1)
        model$optimal_nrounds       <- nrounds_param
        model$optimal_num_leaves    <- num_leaves_param
        model$optimal_learning_rate <- learning_rate_param
      }
      
      # ← Scores bruts seulement
      scorelearning <- data.frame(predict(model, x))
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
    }
    
    # ── Naive Bayes ────────────────────────────────────────────────────────────
    if (modelparameters$modeltype == "naivebayes") {
      optimal_laplace <- 0
      
      if (!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch) {
        cat("Using GridSearchCV for Naive Bayes hyperparameter tuning...\n")
        param_grid <- list(
          laplace = if (!is.null(modelparameters$nb_grid_laplace)) modelparameters$nb_grid_laplace else c(0, 0.5, 1, 2, 5)
        )
        grid_result <- tryCatch({
          tune_nb_gridsearch(X = as.data.frame(learningmodel[, -1]), y = learningmodel[, 1],
                             param_grid = param_grid, n_folds = 5,
                             scoring = c("auc", "accuracy"))
        }, error = function(e) {
          cat("GridSearchCV failed, using default laplace=0:", e$message, "\n"); NULL
        })
        if (!is.null(grid_result)) {
          best_params     <- grid_result$best_params
          optimal_laplace <- if (!is.null(best_params$laplace)) best_params$laplace else 0
          cat(sprintf("GridSearchCV best params: laplace=%.2f, score=%.4f\n",
                      optimal_laplace, grid_result$best_score))
        }
      }
      
      model <- naiveBayes(x = learningmodel[, -1], y = learningmodel[, 1], laplace = optimal_laplace)
      model$model_type      <- "naivebayes"
      model$optimal_laplace <- optimal_laplace
      
      # ← Scores bruts seulement
      pred_probs    <- e1071:::predict.naiveBayes(model, learningmodel[, -1], type = "raw")
      scorelearning <- data.frame(pred_probs[, lev["positif"]])
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
    }
    
    # ── KNN ────────────────────────────────────────────────────────────────────
    if (modelparameters$modeltype == "knn") {
      if (is.null(modelparameters$autotuneknn) || modelparameters$autotuneknn) {
        if (!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch) {
          cat("Using GridSearchCV for KNN hyperparameter tuning...\n")
          max_k      <- min(floor(sqrt(nrow(learningmodel))), 30)
          param_grid <- list(
            n_neighbors = if (!is.null(modelparameters$knn_grid_k)) modelparameters$knn_grid_k else seq(3, max_k, by = 2)
          )
          grid_result <- tryCatch({
            tune_knn_gridsearch(X = as.data.frame(learningmodel[, -1]), y = learningmodel[, 1],
                                param_grid = param_grid, n_folds = 5,
                                scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to manual CV:", e$message, "\n"); NULL
          })
          if (!is.null(grid_result)) {
            best_params <- grid_result$best_params
            optimal_k   <- if (!is.null(best_params$n_neighbors)) best_params$n_neighbors else 5
            cat(sprintf("GridSearchCV best params: k=%d, score=%.4f\n", optimal_k, grid_result$best_score))
          } else {
            set.seed(20011203)
            max_k    <- min(floor(sqrt(nrow(learningmodel))), 20)
            k_values <- seq(3, max_k, by = 2)
            best_k   <- 3; best_acc <- 0
            knn_folds <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(learningmodel) - 1))
            for (k_test in k_values) {
              accuracies <- sapply(knn_folds, function(test_idx) {
                train_idx <- setdiff(1:nrow(learningmodel), test_idx)
                pred <- knn(train = learningmodel[train_idx, -1], test = learningmodel[test_idx, -1],
                            cl = learningmodel[train_idx, 1], k = k_test)
                mean(pred == learningmodel[test_idx, 1])
              })
              avg_acc <- mean(accuracies)
              if (avg_acc > best_acc) { best_acc <- avg_acc; best_k <- k_test }
            }
            optimal_k <- best_k
          }
        } else {
          set.seed(20011203)
          max_k    <- min(floor(sqrt(nrow(learningmodel))), 20)
          k_values <- seq(3, max_k, by = 2)
          best_k   <- 3; best_acc <- 0
          knn_folds <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(learningmodel) - 1))
          for (k_test in k_values) {
            accuracies <- sapply(knn_folds, function(test_idx) {
              train_idx <- setdiff(1:nrow(learningmodel), test_idx)
              pred <- class::knn(train = learningmodel[train_idx, -1], test = learningmodel[test_idx, -1],
                          cl = learningmodel[train_idx, 1], k = k_test)
              mean(pred == learningmodel[test_idx, 1])
            })
            avg_acc <- mean(accuracies)
            if (avg_acc > best_acc) { best_acc <- avg_acc; best_k <- k_test }
          }
          optimal_k <- best_k
        }
      } else {
        optimal_k <- ifelse(is.null(modelparameters$k_neighbors), 5, modelparameters$k_neighbors)
      }
      
      model <- list(train_data   = learningmodel[, -1],
                    train_labels = learningmodel[, 1],
                    optimal_k    = optimal_k,
                    model_type   = "knn")
      
      # ← Scores bruts seulement (leave-one-out pour le training)
      scorelearning_vec <- numeric(nrow(learningmodel))
      for (i in 1:nrow(learningmodel)) {
        train_idx         <- setdiff(1:nrow(learningmodel), i)
        distances         <- apply(learningmodel[train_idx, -1], 1, function(row) {
          sqrt(sum((as.numeric(learningmodel[i, -1]) - as.numeric(row))^2))
        })
        k_nearest_idx     <- order(distances)[1:optimal_k]
        k_nearest_labels  <- learningmodel[train_idx, 1][k_nearest_idx]
        scorelearning_vec[i] <- sum(k_nearest_labels == lev["positif"]) / optimal_k
      }
      scorelearning <- data.frame(scorelearning_vec)
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
    }
    
    # ── ElasticNet ────────────────────────────────────────────────────────────
    if (modelparameters$modeltype == "elasticnet") {
      x           <- as.matrix(learningmodel[, -1])
      y           <- ifelse(learningmodel[, 1] == lev["positif"], 1, 0)
      alpha_param <- ifelse(is.null(modelparameters$alpha), 0.5, modelparameters$alpha)
      lambda_param <- modelparameters$lambda
      
      if (!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch && is.null(lambda_param)) {
        cat("Using GridSearchCV for ElasticNet hyperparameter tuning...\n")
        param_grid <- list(
          alpha  = if (!is.null(modelparameters$en_grid_alpha))  modelparameters$en_grid_alpha  else c(0, 0.25, 0.5, 0.75, 1.0),
          lambda = if (!is.null(modelparameters$en_grid_lambda)) modelparameters$en_grid_lambda else c(0.001, 0.01, 0.1, 1.0)
        )
        grid_result <- tryCatch({
          tune_elasticnet_gridsearch(X = as.data.frame(x), y = learningmodel[, 1],
                                     param_grid = param_grid, n_folds = 5,
                                     scoring = c("auc", "accuracy"))
        }, error = function(e) {
          cat("GridSearchCV failed, falling back to cv.glmnet:", e$message, "\n"); NULL
        })
        if (!is.null(grid_result)) {
          best_params  <- grid_result$best_params
          alpha_param  <- if (!is.null(best_params$alpha))  best_params$alpha  else 0.5
          lambda_param <- if (!is.null(best_params$lambda)) best_params$lambda else NULL
          cat(sprintf("GridSearchCV best params: alpha=%.3f, lambda=%.4f, score=%.4f\n",
                      alpha_param, lambda_param, grid_result$best_score))
          set.seed(20011203)
          en_k     <- min(5, nrow(learningmodel) - 1)
          en_folds <- create_stratified_folds(learningmodel[, 1], k = en_k)
          en_foldid <- folds_to_foldid(en_folds, nrow(learningmodel))
          cvfit        <- cv.glmnet(x, y, family = "binomial", alpha = alpha_param,
                                    type.measure = "auc", foldid = en_foldid)
          lambda_param <- cvfit$lambda.min
          model        <- list(glmnet_model = cvfit, lambda = lambda_param, alpha = alpha_param,
                               cvfit = cvfit, optimal_lambda = lambda_param, lambda_1se = cvfit$lambda.1se)
        } else {
          set.seed(20011203)
          en_k     <- min(10, nrow(learningmodel) - 1)
          en_folds <- create_stratified_folds(learningmodel[, 1], k = en_k)
          en_foldid <- folds_to_foldid(en_folds, nrow(learningmodel))
          cvfit        <- cv.glmnet(x, y, family = "binomial", alpha = alpha_param,
                                    type.measure = "auc", foldid = en_foldid)
          lambda_param <- cvfit$lambda.min
          model        <- list(glmnet_model = cvfit, lambda = lambda_param, alpha = alpha_param,
                               cvfit = cvfit, optimal_lambda = lambda_param, lambda_1se = cvfit$lambda.1se)
        }
      } else if (is.null(lambda_param)) {
        set.seed(20011203)
        en_k     <- min(10, nrow(learningmodel) - 1)
        en_folds <- create_stratified_folds(learningmodel[, 1], k = en_k)
        en_foldid <- folds_to_foldid(en_folds, nrow(learningmodel))
        cvfit        <- cv.glmnet(x, y, family = "binomial", alpha = alpha_param,
                                  type.measure = "auc", foldid = en_foldid)
        lambda_param <- cvfit$lambda.min
        model        <- list(glmnet_model = cvfit, lambda = lambda_param, alpha = alpha_param,
                             cvfit = cvfit, optimal_lambda = lambda_param, lambda_1se = cvfit$lambda.1se)
      } else {
        cat("Creating ElasticNet model with manual parameters: alpha=", alpha_param, ", lambda=", lambda_param, "\n")
        fit   <- glmnet(x, y, family = "binomial", alpha = alpha_param, lambda = lambda_param)
        model <- list(glmnet_model = fit, lambda = lambda_param, alpha = alpha_param,
                      cvfit = NULL, optimal_lambda = lambda_param, lambda_1se = NULL)
      }
      
      cat("la classe de model$glmnet_model est : ", class(model$glmnet_model), "\n")
      
      if (modelparameters$fs) {
        coef_values       <- as.matrix(coef(model$glmnet_model, s = lambda_param))
        selected_features <- rownames(coef_values)[which(coef_values[-1, 1] != 0)]
        if (length(selected_features) > 0) {
          learningmodel <- learningmodel[, c("group", selected_features)]
          x             <- as.matrix(learningmodel[, -1])
          if (is.null(modelparameters$lambda)) {
            fs_k <- min(10, nrow(learningmodel) - 1)
            fs_folds <- create_stratified_folds(learningmodel[, 1], k = fs_k)
            fs_foldid <- folds_to_foldid(fs_folds, nrow(learningmodel))
            cvfit        <- cv.glmnet(x, y, family = "binomial", alpha = alpha_param,
                                      type.measure = "auc", foldid = fs_foldid)
            lambda_param <- cvfit$lambda.min
            fit          <- glmnet(x, y, family = "binomial", alpha = alpha_param, lambda = lambda_param)
            cat("class of fitted model : ", class(fit))
            model        <- list(glmnet_model = fit, lambda = lambda_param, alpha = alpha_param,
                                 cvfit = cvfit, optimal_lambda = lambda_param, lambda_1se = cvfit$lambda.1se)
          } else {
            fit   <- glmnet(x, y, family = "binomial", alpha = alpha_param, lambda = lambda_param)
            model <- list(glmnet_model = fit, lambda = lambda_param, alpha = alpha_param,
                          cvfit = NULL, optimal_lambda = lambda_param, lambda_1se = NULL)
          }
        }
      }
      
      # ← Scores bruts seulement
      if (inherits(model$glmnet_model, "cv.glmnet")) {
        scorelearning <- as.vector(glmnet:::predict.cv.glmnet(model$glmnet_model, newx = x, s = lambda_param, type = "response"))
      } else {
        scorelearning <- as.vector(glmnet::predict.glmnet(model$glmnet_model, newx = x, s = lambda_param, type = "response"))
      }
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
    }
    
    # ── XGBoost ────────────────────────────────────────────────────────────────
    if (modelparameters$modeltype == "xgboost") {
      x      <- as.matrix(learningmodel[, -1])
      y      <- ifelse(learningmodel[, 1] == lev["positif"], 1, 0)
      dtrain <- xgb.DMatrix(data = x, label = y)
      
      if (is.null(modelparameters$autotunexgb) || modelparameters$autotunexgb) {
        if (!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch) {
          cat("Using GridSearchCV for XGBoost hyperparameter tuning...\n")
          
          alpha_param <- ifelse(is.null(modelparameters$alpha_xgb), 0, modelparameters$alpha_xgb)
          lambda_param <- ifelse(is.null(modelparameters$lambda_xgb), 0, modelparameters$lambda_xgb)
          param_grid <- list(
            n_estimators = if (!is.null(modelparameters$xgb_grid_nrounds))   modelparameters$xgb_grid_nrounds   else c(50, 100, 200),
            max_depth    = if (!is.null(modelparameters$xgb_grid_maxdepth))  modelparameters$xgb_grid_maxdepth  else c(3, 6, 9),
            learning_rate = if (!is.null(modelparameters$xgb_grid_eta))      modelparameters$xgb_grid_eta       else c(0.01, 0.1, 0.3),
            gamma        = if (!is.null(modelparameters$xgb_grid_gamma))     modelparameters$xgb_grid_gamma     else c(0, 0.1, 0.5),
            subsample    = if (!is.null(modelparameters$xgb_grid_subsample)) modelparameters$xgb_grid_subsample else c(0.8, 1.0)
          )
          grid_result <- tryCatch({
            tune_xgb_gridsearch(X = as.data.frame(x), y = learningmodel[, 1],
                                param_grid = param_grid, n_folds = 5,
                                scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to xgb.cv:", e$message, "\n"); NULL
          })
          if (!is.null(grid_result)) {
            best_params              <- grid_result$best_params
            optimal_nrounds          <- if (!is.null(best_params$n_estimators)) best_params$n_estimators else 100
            optimal_max_depth        <- if (!is.null(best_params$max_depth))    best_params$max_depth    else 6
            optimal_eta              <- if (!is.null(best_params$learning_rate)) best_params$learning_rate else 0.3
            optimal_gamma            <- if (!is.null(best_params$gamma))        best_params$gamma        else 0
            optimal_subsample        <- if (!is.null(best_params$subsample))    best_params$subsample    else 1.0
            optimal_min_child_weight <- if (!is.null(best_params$min_child_weight)) best_params$min_child_weight else 1
            optimal_alpha =  if (!is.null(best_params$alpha)) best_params$alpha else 0
            optimal_lambda = if (!is.null(best_params$lambda)) best_params$lambda else 0
            
            cat(sprintf("GridSearchCV best params: nrounds=%d, max_depth=%d, eta=%.3f, gamma=%.3f, score=%.4f\n",
                        optimal_nrounds, optimal_max_depth, optimal_eta, optimal_gamma, grid_result$best_score))
            final_params <- list(objective = "binary:logistic", eval_metric = "auc",
                                 max_depth = optimal_max_depth, eta = optimal_eta,
                                 gamma = optimal_gamma, subsample = optimal_subsample,
                                 lambda = optimal_lambda, alpha = optimal_alpha,
                                 min_child_weight = optimal_min_child_weight)
            model <- xgb.train(params = final_params, data = dtrain, nrounds = optimal_nrounds, verbose = 0)
            model$optimal_nrounds          <- optimal_nrounds
            model$optimal_max_depth        <- optimal_max_depth
            model$optimal_eta              <- optimal_eta
            model$optimal_gamma            <- optimal_gamma
            model$optimal_subsample        <- optimal_subsample
            model$optimal_min_child_weight <- optimal_min_child_weight
          } else {
            alpha_param   <- ifelse(is.null(modelparameters$alpha_xgb),   0, modelparameters$alpha_xgb)
            lambda_param  <- ifelse(is.null(modelparameters$lambda_xgb),  0, modelparameters$lambda_xgb)
            gamma_param   <- ifelse(is.null(modelparameters$gamma_xgb),   0, modelparameters$gamma_xgb)
            subsample_param <- ifelse(is.null(modelparameters$subsample_xgb), 1.0, modelparameters$subsample_xgb)
            set.seed(20011203)
            best_params <- list(objective = "binary:logistic", eval_metric = "auc",
                                alpha = alpha_param, lambda = lambda_param,
                                gamma = gamma_param, subsample = subsample_param,
                                max_depth = 6, eta = 0.3, min_child_weight = 1)
            xgb_folds <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(learningmodel) - 1))
            tryCatch({
              cv_results      <- xgb.cv(params = best_params, data = dtrain, nrounds = 200,
                                        folds = xgb_folds,
                                        early_stopping_rounds = 10, verbose = 0)
              optimal_nrounds <- cv_results$best_iteration
            }, error = function(e) { optimal_nrounds <<- 20 })
            model <- xgb.train(params = best_params, data = dtrain, nrounds = optimal_nrounds, verbose = 0)
            model$optimal_nrounds          <- optimal_nrounds
            model$optimal_max_depth        <- best_params$max_depth
            model$optimal_eta              <- best_params$eta
            model$optimal_min_child_weight <- best_params$min_child_weight
            model$optimal_gamma            <- best_params$gamma
            model$optimal_subsample        <- best_params$subsample
          }
        } else {
          set.seed(20011203)
          alpha_param   <- ifelse(is.null(modelparameters$alpha_xgb),   0, modelparameters$alpha_xgb)
          lambda_param  <- ifelse(is.null(modelparameters$lambda_xgb),  0, modelparameters$lambda_xgb)
          gamma_param   <- ifelse(is.null(modelparameters$gamma_xgb),   0, modelparameters$gamma_xgb)
          subsample_param <- ifelse(is.null(modelparameters$subsample_xgb), 1.0, modelparameters$subsample_xgb)
          best_params <- list(objective = "binary:logistic", eval_metric = "auc",
                              alpha = alpha_param, lambda = lambda_param,
                              gamma = gamma_param, subsample = subsample_param,
                              max_depth = 6, eta = 0.3, min_child_weight = 1)
          xgb_folds <- create_stratified_folds(learningmodel[, 1], k = min(5, nrow(learningmodel) - 1))
          cv_results      <- xgb.cv(params = best_params, data = dtrain, nrounds = 200,
                                    folds = xgb_folds,
                                    early_stopping_rounds = 10, verbose = 0)
          optimal_nrounds <- cv_results$best_iteration
          cat("best parameters\n"); print(best_params)
          cat("optimal rounds : ", optimal_nrounds, "\n")
          model <- xgb.train(params = best_params, data = dtrain, nrounds = optimal_nrounds, verbose = 0)
          model$optimal_nrounds          <- optimal_nrounds
          model$optimal_max_depth        <- best_params$max_depth
          model$optimal_eta              <- best_params$eta
          model$optimal_min_child_weight <- best_params$min_child_weight
          model$optimal_gamma            <- best_params$gamma
          model$optimal_subsample        <- best_params$subsample
        }
      } else {
        
        cat("Creating XGBoost model with manual parameters \n")
        nrounds_param   <- ifelse(is.null(modelparameters$nrounds),    100, modelparameters$nrounds)
        max_depth_param <- ifelse(is.null(modelparameters$max_depth),    6,  modelparameters$max_depth)
        eta_param       <- ifelse(is.null(modelparameters$eta),         0.3, modelparameters$eta)
        
        gamma_param <- ifelse(is.null(modelparameters$gamma_xgb), 0, modelparameters$gamma_xgb)
        subsample_param <- ifelse(is.null(modelparameters$subsample_xgb), 1.0, modelparameters$subsample_xgb)
        min_child_weight_param <- ifelse(is.null(modelparameters$min_child_weight), 1, modelparameters$min_child_weight)
        alpha_param <- ifelse(is.null(modelparameters$alpha_xgb), 0, modelparameters$alpha_xgb)
        lambda_param <- ifelse(is.null(modelparameters$lambda_xgb), 0, modelparameters$lambda_xgb)
        
        
        params <- list(objective = "binary:logistic", eval_metric = "auc",
                       alpha = alpha_param, lambda = lambda_param, subsample = subsample_param,
                       gamma =  gamma_param,min_child_weight = min_child_weight_param,
                       max_depth = max_depth_param, eta = eta_param, min_child_weight = 1)
        model <- xgb.train(params = params, data = dtrain, nrounds = nrounds_param, verbose = 0)
        model$optimal_nrounds          <- nrounds_param
        model$optimal_max_depth        <- max_depth_param
        model$optimal_eta              <- eta_param
        
        model$optimal_min_child_weight <- min_child_weight_param
        model$optimal_gamma <- gamma_param
        model$optimal_subsample <- subsample_param
        model$optimal_alpha <- alpha_param
        model$optimal_lambda <- lambda_param
        model$optimal_subsample        <- subsample_param
        model$optimal_min_child_weight <- min_child_weight_param
        cat("optimal_nrounds :  ", nrounds_param, "\n")
        cat("optimal_max_depth :  ", max_depth_param, "\n")
        cat("optimal_eta :  ", eta_param, "\n")
        cat("optimal alpha : ", alpha_param, "\n")
        cat("optimal lambda : ", lambda_param, "\n")
        cat("optimal_subsample :  ", subsample_param, "\n")
        cat("optimal_gamma :  ", gamma_param, "\n")
      }
      
      # ← Scores bruts seulement
      scorelearning <- data.frame(xgboost:::predict.xgb.Booster(model, x))
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
    }
    
    # ── Assemblage du résultat learning ────────────────────────────────────────
    # NOTE : predictclasslearning intentionnellement absent ici ;
    # il sera ajouté par apply_threshold() dans le reactive MODEL.
    classlearning     <- learningmodel[, 1]
    reslearningmodel  <- data.frame(classlearning, scorelearning)
    colnames(reslearningmodel) <- c("classlearning", "scorelearning")
    datalearningmodel <- list("learningmodel"    = learningmodel,
                              "reslearningmodel" = reslearningmodel)
    
    # ── Validation ─────────────────────────────────────────────────────────────
    if (modelparameters$adjustval) {
      colnames(validation)[1] <- "group"
      validationdiff <- validation[, which(colnames(validation) %in% colnames(learningmodel))]
      learningselect2 <- learningselect
      
      if (transformdataparameters$log) {
        validationdiff[, -1]  <- transformationlog(x = validationdiff[, -1] + 1, logtype = transformdataparameters$logtype)
        learningselect2[, -1] <- transformationlog(x = learningselect2[, -1] + 1, logtype = transformdataparameters$logtype)
      }
      if (transformdataparameters$arcsin) {
        maxlearn <- apply(X = learningselect[, -1], MARGIN = 2, FUN = max, na.rm = TRUE)
        minlearn <- apply(X = learningselect[, -1], MARGIN = 2, FUN = min, na.rm = TRUE)
        for (i in 2:dim(validationdiff)[2]) {
          validationdiff[, i] <- (validationdiff[, i] - minlearn[i - 1]) / (maxlearn[i - 1] - minlearn[i - 1])
          validationdiff[which(validationdiff[, i] > 1), i] <- 1
          validationdiff[which(validationdiff[, i] < 0), i] <- 0
          validationdiff[, i] <- asin(sqrt(validationdiff[, i]))
        }
        learningselect2[, -1] <- apply(X = learningselect2[, -1], MARGIN = 2,
                                       FUN = function(x) { (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) })
        learningselect2[, -1] <- asin(sqrt(learningselect2[, -1]))
      }
      if (transformdataparameters$standardization) {
        learningselectval <<- learningselect2
        # Calculer sdselect uniquement sur les colonnes de features (excluant "group")
        feat_cols_learning <- colnames(learningselect2)[colnames(learningselect2) %in% colnames(validationdiff) & colnames(learningselect2) != "group"]
        sdselect <- apply(learningselect2[, feat_cols_learning, drop = FALSE], 2, sd, na.rm = TRUE)
        print("sdselect"); print(sdselect)
        # Aligner avec les colonnes de features de validationdiff (col 1 est "group")
        feat_cols       <- colnames(validationdiff)[-1]
        sdselect_aligned <- sdselect[feat_cols]
        if (length(sdselect_aligned) != length(feat_cols) || any(is.na(sdselect_aligned))) {
          warning("sdselect_aligned contient des NA ou longueur incorrecte — vérifier la correspondance des colonnes entre learningselect2 et validationdiff")
        }
        validationdiff[, -1] <- scale(as.matrix(validationdiff[, -1]), center = FALSE, scale = sdselect_aligned)
      }
      if (!is.null(datastructuresfeatures)) {
        validationdiff[which(is.na(validationdiff), arr.ind = TRUE)[
          which(which(is.na(validationdiff), arr.ind = TRUE)[, 2] %in%
                  which(colnames(validationdiff) %in% datastructuresfeatures$names)), ]] <- 0
      }
      validationmodel <<- replaceNAvalidation(as.data.frame(validationdiff[, -1]),
                                              toto   = as.data.frame(learningmodel[, -1]),
                                              rempNA = transformdataparameters$rempNA)
      colnames(validationmodel) <- colnames(validationdiff)[-1]
      rownames(validationmodel) <- rownames(validationdiff)
      
      # ── Scores bruts validation selon le type de modèle ──────────────────────
      if (modelparameters$modeltype == "randomforest") {
        cat("colnames of validationmodel : \n")
        print(colnames(validationmodel))
        scoreval <- randomForest:::predict.randomForest(object = model, type = "prob",
                                                        newdata = validationmodel)[, lev["positif"]]
      }
      if (modelparameters$modeltype == "svm") {
        if (!is.null(model)) {
          scoreval <- attr(e1071:::predict.svm(model, newdata = validationmodel,
                                               decision.values = TRUE), "decision.values")
          cat("score val dans model function :  \n")
          print(scoreval)
          if (sum(lev == (strsplit(colnames(scoreval), split = "/")[[1]])) == 0) {
            scoreval <- scoreval * (-1)
          }
        }
      }
      if (modelparameters$modeltype == "elasticnet") {
        req(model$glmnet_model)
        x_val <- as.matrix(validationmodel)
        if (inherits(model$glmnet_model, "cv.glmnet")) {
          scoreval <- as.vector(glmnet:::predict.cv.glmnet(model$glmnet_model,
                                                           newx = x_val, s = model$lambda, type = "response"))
        } else {
          scoreval <- as.vector(glmnet::predict.glmnet(model$glmnet_model,
                                                       newx = x_val, s = model$lambda, type = "response"))
        }
      }
      if (modelparameters$modeltype == "xgboost") {
        x_val    <- as.matrix(validationmodel)
        dval     <- xgb.DMatrix(data = x_val)
        scoreval <- xgboost:::predict.xgb.Booster(model, dval)
      }
      if (modelparameters$modeltype == "lightgbm") {
        x_val    <- as.matrix(validationmodel)
        scoreval <- predict(model, x_val)
      }
      if (modelparameters$modeltype == "naivebayes") {
        pred_probs <- e1071:::predict.naiveBayes(model, validationmodel, type = "raw")
        scoreval   <- pred_probs[, lev["positif"]]
      }
      if (modelparameters$modeltype == "knn") {
        scoreval_vec <- numeric(nrow(validationmodel))
        for (i in 1:nrow(validationmodel)) {
          distances        <- apply(model$train_data, 1, function(row) {
            sqrt(sum((as.numeric(validationmodel[i, ]) - as.numeric(row))^2))
          })
          k_nearest_idx    <- order(distances)[1:model$optimal_k]
          k_nearest_labels <- model$train_labels[k_nearest_idx]
          scoreval_vec[i]  <- sum(k_nearest_labels == lev["positif"]) / model$optimal_k
        }
        scoreval <- scoreval_vec
      }
      
      classval <- validation[, 1]
      if (sum(lev == (levels(classval))) == 0) {
        classval <- factor(classval, levels = rev(levels(classval)), ordered = TRUE)
      }
      
      # NOTE : predictclassval intentionnellement absent ici ;
      # il sera ajouté par apply_threshold() dans le reactive MODEL.
      resvalidationmodel <- data.frame(classval, scoreval)
      colnames(resvalidationmodel) <- c("classval", "scoreval")
      
      # AUC ne dépend pas du seuil → calculé ici une seule fois
      auc_val <- pROC::auc(pROC::roc(as.vector(classval), as.vector(scoreval), quiet = TRUE))
      
      datavalidationmodel <- list("validationdiff"      = validationdiff,
                                  "validationmodel"     = validationmodel,
                                  "resvalidationmodel"  = resvalidationmodel,
                                  "auc"                 = auc_val)
    } else {
      datavalidationmodel <- list()
    }
    
    
    if (modelparameters$modeltype == "randomforest") {
      modelparameters$mtry     <- model$optimal_mtry
      modelparameters$ntree    <- model$ntree_used
      modelparameters$nodesize <- model$nodesize_used
    } else if (modelparameters$modeltype == "svm") {
      modelparameters$cost  <- model$cost
      modelparameters$gamma <- model$gamma
    } else if (modelparameters$modeltype == "knn") {
      modelparameters$k_neighbors <- model$optimal_k
    } else if (modelparameters$modeltype == "elasticnet") {
      modelparameters$alpha  <- model$alpha
      modelparameters$lambda <- model$lambda
    } else if (modelparameters$modeltype == "xgboost") {
      modelparameters$nrounds          <- model$optimal_nrounds
      modelparameters$max_depth        <- model$optimal_max_depth
      modelparameters$eta              <- model$optimal_eta
      modelparameters$gamma_xgb        <- model$optimal_gamma
      modelparameters$alpha_xgb        <- model$optimal_alpha
      modelparameters$lambda_xgb       <- model$optimal_lambda
      modelparameters$subsample_xgb    <- model$optimal_subsample
      modelparameters$min_child_weight <- model$optimal_min_child_weight
    }
    
    # ── Retour ─────────────────────────────────────────────────────────────────
    # Clé "modelparameters" (cohérence avec apply_threshold et MODEL_TRAIN)
    list("datalearningmodel"   = datalearningmodel,
         "model"               = model,
         "datavalidationmodel" = datavalidationmodel,
         "groups"              = lev,
         "modelparameters"     = modelparameters)
  }
}



modelfunction <- function(learningmodel,
                          validation=NULL,
                          modelparameters,
                          transformdataparameters,
                          datastructuresfeatures=NULL,
                          learningselect){
  if(modelparameters$modeltype!="nomodel"){
    colnames(learningmodel)[1]<-"group"
    
    if(modelparameters$invers){
      learningmodel[,1]<-factor(learningmodel[,1],levels = rev(levels(learningmodel[,1])),ordered = TRUE)
    }
    lev<-levels(x = learningmodel[,1])
    names(lev)<-c("positif","negatif")
    
    #Build model
    if (modelparameters$modeltype=="randomforest"){
      learningmodel<-as.data.frame(learningmodel[sort(rownames(learningmodel)),])
      
      x<-as.data.frame(learningmodel[,-1])
      colnames(x)<-colnames(learningmodel)[-1]
      x<-as.data.frame(x[,sort(colnames(x))])
      set.seed(20011203)
      ntree_param <- ifelse(is.null(modelparameters$ntree), 1000, modelparameters$ntree)
      
      # Default values for all RF hyperparameters
      nodesize_param  <- 1
      maxnodes_param  <- NULL
      sampsize_param  <- NULL
      replace_param   <- if (!is.null(modelparameters$replace)) modelparameters$replace else TRUE
      sampsize_frac   <- if (!is.null(modelparameters$sampsize_frac)) modelparameters$sampsize_frac else 1.0
      
      if(is.null(modelparameters$autotunerf) || modelparameters$autotunerf){
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          # ── GridSearchCV tuning ──
          cat("Using GridSearchCV for Random Forest hyperparameter tuning...\n")
          param_grid <- list(
            n_estimators    = if(!is.null(modelparameters$rf_grid_ntree))    modelparameters$rf_grid_ntree    else c(100, 500, 1000),
            max_features    = if(!is.null(modelparameters$rf_grid_mtry))     modelparameters$rf_grid_mtry     else c("sqrt", "log2"),
            min_samples_split = if(!is.null(modelparameters$rf_grid_nodesize)) modelparameters$rf_grid_nodesize else c(1, 5, 10)
          )
          grid_result <- tryCatch({
            tune_rf_gridsearch(X = x, y = learningmodel[,1],
                               param_grid = param_grid, n_folds = 5,
                               scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to tuneRF:", e$message, "\n"); NULL
          })
          if(!is.null(grid_result)) {
            best_params <- grid_result$best_params
            optimal_mtry <- if(!is.null(best_params$max_features)) {
              if(best_params$max_features == "sqrt") floor(sqrt(ncol(x)))
              else if(best_params$max_features == "log2") floor(log2(ncol(x)))
              else as.numeric(best_params$max_features)
            } else floor(sqrt(ncol(x)))
            ntree_param    <- if(!is.null(best_params$n_estimators))      best_params$n_estimators      else ntree_param
            nodesize_param <- if(!is.null(best_params$min_samples_split)) best_params$min_samples_split else 1
            cat(sprintf("GridSearchCV best params: ntree=%d, mtry=%d, nodesize=%d, score=%.4f\n",
                        ntree_param, optimal_mtry, nodesize_param, grid_result$best_score))
          } else {
            cat("GridSearchCV failed, falling back to stratified CV for mtry...\n")
            p <- ncol(x)
            mtry_candidates <- unique(c(max(1, floor(sqrt(p))), max(1, floor(log2(p))), max(1, floor(p/3))))
            fb_folds <- create_stratified_folds(learningmodel[,1], k = min(5, nrow(x) - 1))
            pos_lev  <- levels(learningmodel[,1])[1]
            best_fb  <- -Inf
            for (mt in mtry_candidates) {
              sc <- mean(sapply(fb_folds, function(ti) {
                tri <- setdiff(1:nrow(x), ti)
                tryCatch({
                  mdl <- randomForest(x = x[tri,,drop=FALSE], y = learningmodel[tri,1],
                                      ntree = ntree_param, mtry = mt, importance = FALSE)
                  prbs <- randomForest:::predict.randomForest(mdl, x[ti,,drop=FALSE], type = "prob")
                  compute_auc_manual(learningmodel[ti,1], prbs[, pos_lev], pos_lev)
                }, error = function(e) NA_real_)
              }), na.rm = TRUE)
              if (!is.na(sc) && sc > best_fb) { best_fb <- sc; optimal_mtry <- mt }
            }
            nodesize_param <- 1
          }
          # maxnodes tuning: pick best from grid via OOB
          maxnodes_candidates <- if (!is.null(modelparameters$rf_grid_maxnodes)) modelparameters$rf_grid_maxnodes else c(0)
          maxnodes_candidates <- as.numeric(maxnodes_candidates)
          if (length(maxnodes_candidates) > 1 || any(maxnodes_candidates != 0)) {
            best_oob <- Inf; best_maxnodes <- NULL
            samp_sz <- if (sampsize_frac < 1.0) round(nrow(x) * sampsize_frac) else nrow(x)
            for (mn in maxnodes_candidates) {
              mn_val <- if (mn == 0) NULL else mn
              tmp_model <- randomForest(x = x, y = learningmodel[,1],
                                        ntree = ntree_param, mtry = optimal_mtry,
                                        nodesize = nodesize_param, maxnodes = mn_val,
                                        sampsize = samp_sz, replace = replace_param, importance = FALSE)
              oob_err <- mean(tmp_model$err.rate[, "OOB"])
              cat(sprintf("  maxnodes=%s -> OOB=%.4f\n", ifelse(is.null(mn_val), "NULL", as.character(mn_val)), oob_err))
              if (oob_err < best_oob) { best_oob <- oob_err; best_maxnodes <- mn_val }
            }
            maxnodes_param <- best_maxnodes
          }
          # sampsize tuning: pick best fraction from grid
          sampsize_candidates <- if (!is.null(modelparameters$rf_grid_sampsize)) as.numeric(modelparameters$rf_grid_sampsize) else c(1.0)
          if (length(sampsize_candidates) > 1) {
            best_oob <- Inf; best_frac <- 1.0
            for (sf in sampsize_candidates) {
              samp_sz <- round(nrow(x) * sf)
              tmp_model <- randomForest(x = x, y = learningmodel[,1],
                                        ntree = ntree_param, mtry = optimal_mtry,
                                        nodesize = nodesize_param, maxnodes = maxnodes_param,
                                        sampsize = samp_sz, replace = replace_param, importance = FALSE)
              oob_err <- mean(tmp_model$err.rate[, "OOB"])
              if (oob_err < best_oob) { best_oob <- oob_err; best_frac <- sf }
            }
            sampsize_frac <- best_frac
          }
        } else {
          # ── Traditional mode: stratified CV over ntree, mtry, nodesize ──
          ntree_values    <- if (!is.null(modelparameters$rf_ntree_range)) modelparameters$rf_ntree_range else c(500, 1000)
          nodesize_values <- if (!is.null(modelparameters$rf_nodesize_range)) modelparameters$rf_nodesize_range else c(1)
          p <- ncol(x)
          mtry_values <- unique(c(max(1, floor(sqrt(p))), max(1, floor(log2(p))), max(1, floor(p / 3))))
          
          folds_rf   <- create_stratified_folds(learningmodel[,1], k = min(5, nrow(x) - 1))
          pos_level  <- levels(learningmodel[,1])[1]
          best_score <- -Inf
          optimal_mtry <- floor(sqrt(p))
          
          cat("Traditional stratified CV: searching over ntree, mtry and nodesize...\n")
          for (nt in ntree_values) {
            for (ns in nodesize_values) {
              for (mt in mtry_values) {
                fold_scores <- sapply(folds_rf, function(test_idx) {
                  train_idx <- setdiff(1:nrow(x), test_idx)
                  tryCatch({
                    mdl   <- randomForest(x = x[train_idx, , drop = FALSE], y = learningmodel[train_idx, 1],
                                          ntree = nt, mtry = mt, nodesize = ns, importance = FALSE)
                    probs <- randomForest:::predict.randomForest(mdl, x[test_idx, , drop = FALSE], type = "prob")
                    compute_auc_manual(learningmodel[test_idx, 1], probs[, pos_level], pos_level)
                  }, error = function(e) NA_real_)
                })
                sc <- mean(fold_scores, na.rm = TRUE)
                cat(sprintf("  ntree=%d, mtry=%d, nodesize=%d -> AUC=%.4f\n", nt, mt, ns, sc))
                if (!is.na(sc) && sc > best_score) {
                  best_score     <- sc
                  ntree_param    <- nt
                  optimal_mtry   <- mt
                  nodesize_param <- ns
                }
              }
            }
          }
          cat(sprintf("Best stratified CV params: ntree=%d, mtry=%d, nodesize=%d, AUC=%.4f\n",
                      ntree_param, optimal_mtry, nodesize_param, best_score))
        }
      } else {
        # ── Manual mode ──
        optimal_mtry   <- ifelse(is.null(modelparameters$mtry), floor(sqrt(ncol(x))), modelparameters$mtry)
        nodesize_param <- if (!is.null(modelparameters$nodesize)) modelparameters$nodesize else 1
        maxnodes_param <- if (!is.null(modelparameters$maxnodes)) modelparameters$maxnodes else NULL
        sampsize_frac  <- if (!is.null(modelparameters$sampsize_frac)) modelparameters$sampsize_frac else 1.0
        replace_param  <- if (!is.null(modelparameters$replace)) modelparameters$replace else TRUE
      }
      
      # Compute actual sampsize from fraction
      sampsize_param <- if (!is.null(sampsize_frac) && sampsize_frac < 1.0) round(nrow(x) * sampsize_frac) else nrow(x)
      
      model <- randomForest(x = x, y = learningmodel[,1],
                            ntree = ntree_param, mtry = optimal_mtry,
                            nodesize = nodesize_param, maxnodes = maxnodes_param,
                            sampsize = sampsize_param, replace = replace_param,
                            importance = TRUE)
      model$optimal_mtry    <- optimal_mtry
      model$ntree_used      <- ntree_param
      model$nodesize_used   <- nodesize_param
      model$maxnodes_used   <- maxnodes_param
      model$sampsize_used   <- sampsize_param
      model$replace_used    <- replace_param
      if(modelparameters$fs){
        featureselect<-selectedfeature(model=model,modeltype = "randomforest",tab=learningmodel,
                                       criterionimportance = "fscore",criterionmodel = "auc")
        model<-featureselect$model
        learningmodel<-featureselect$dataset
      }
      
      scorelearning =data.frame(model$votes[,lev["positif"]])
      colnames(scorelearning)<-paste(lev[1],"/",lev[2],sep="")
      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
      #predictclasslearning==model$predicted
    }
    
    if(modelparameters$modeltype=="svm"){
      # Determine hyperparameters
      if(is.null(modelparameters$autotunesvm) || modelparameters$autotunesvm){
        kernel_param <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)
        svm_scoring <- if (!is.null(modelparameters$svm_scoring)) modelparameters$svm_scoring else "auc"
        tune_result <- tune_svm_stratified(data = learningmodel,
                                           gamma_range = 10^(-5:2),
                                           cost_range  = 10^(-3:2),
                                           kernel      = kernel_param,
                                           n_folds     = min(5, nrow(learningmodel) - 1),
                                           scoring     = svm_scoring,
                                           seed        = 20011203)
        cat('Stratified SVM tuning results:\n')
        print(tune_result$best.parameters)
        cost_param <- tune_result$best.parameters$cost
        gamma_param <- tune_result$best.parameters$gamma
        
      } else {
        # Use manual hyperparameters
        cat("define svm parameters manually \n")
        # epsilon_param <- ifelse(is.null(modelparameters$epsilon), 0.1, modelparameters$epsilon)
        cost_param <- ifelse(is.null(modelparameters$cost), 1, modelparameters$cost)
        gamma_param <- ifelse(is.null(modelparameters$gamma), 0.1, modelparameters$gamma)
        kernel_param <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)
        
        # model <- svm(group ~ ., data = learningmodel,
        #             kernel= kernel_param ,
        #             cost=cost_param, gamma=gamma_param,
        #             probability=FALSE)
        # model$cost <- cost_param
        # model$gamma <- gamma_param
        # model$kernel <- kernel_param
      }
      
      model <- svm(group ~ ., data = learningmodel,
                   kernel= kernel_param ,
                   # epsilon  = epsilon_param,
                   cost=cost_param, 
                   gamma=gamma_param,
                   type = "C-classification",
                   probability=TRUE)
      model$cost <- cost_param
      model$gamma <- gamma_param
      # model$epsilon <- epsilon_param
      model$kernel <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)
      
      if(modelparameters$fs){
        
        featureselect<-selectedfeature(model=model,modeltype = "svm",tab=learningmodel,
                                       criterionimportance = "fscore",criterionmodel = "auc")
        model<-featureselect$model
        learningmodel<-featureselect$dataset
      }
      
      # calculate the decision values for the learning set
      scorelearning <-model$decision.values
      #scorelearning = attr(e1071:::predict.svm(model, learningmodel[,-1], probability  = TRUE), "probabilities")
      if(sum(lev==(strsplit(colnames(scorelearning),split = "/")[[1]]))==0){
        scorelearning<-scorelearning*(-1)
        colnames(scorelearning)<-paste(lev[1],"/",lev[2],sep="")
      }
      
      # Obtenir les probabilités au lieu des decision values
      # pred_probs <- attr(predict(model, learningmodel[,-1], probability=TRUE), "probabilities")
      # scorelearning <- data.frame(pred_probs[, lev["positif"]])
      # colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
      
      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
    }
    
    if(modelparameters$modeltype=="lightgbm"){
      
      # LightGBM gradient boosting
      x <- as.matrix(learningmodel[,-1])
      # IMPORTANT: Encode y so that 1 = lev["positif"] (first level), 0 = lev["negatif"] (second level)
      y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
      # Create LightGBM dataset
      dtrain <- lgb.Dataset(data = x, label = y)
      # Determine hyperparameters
      if(is.null(modelparameters$autotunelgb) || modelparameters$autotunelgb){
        # Perform hyperparameter tuning using cross-validation
        set.seed(20011203)
        # Parameter grid search
        best_params <- list(
          objective = "binary",
          metric = "auc",
          num_leaves = 31,
          learning_rate = 0.05,
          feature_fraction = 0.9,
          bagging_fraction = 0.8,
          bagging_freq = 5,
          verbose = -1
        )
        
        # Stratified cross-validation to find optimal nrounds
        lgb_folds <- create_stratified_folds(learningmodel[,1], k = min(5, nrow(learningmodel)-1))
        cv_results <- lgb.cv(
          params = best_params,
          data = dtrain,
          nrounds = 200,
          folds = lgb_folds,
          early_stopping_rounds = 10,
          verbose = -1
        )
        
        optimal_nrounds <- cv_results$best_iter
        # Train final model with optimal parameters
        model <- lgb.train(
          params = best_params,
          data = dtrain,
          nrounds = optimal_nrounds,
          verbose = -1
        )
        
        # Store optimal parameters
        model$optimal_nrounds <- optimal_nrounds
        model$optimal_num_leaves <- best_params$num_leaves
        model$optimal_learning_rate <- best_params$learning_rate
      } else {
        # Use manual hyperparameters
        nrounds_param <- ifelse(is.null(modelparameters$nrounds_lgb), 100, modelparameters$nrounds_lgb)
        num_leaves_param <- ifelse(is.null(modelparameters$num_leaves), 31, modelparameters$num_leaves)
        learning_rate_param <- ifelse(is.null(modelparameters$learning_rate_lgb), 0.05, modelparameters$learning_rate_lgb)
        params <- list(
          objective = "binary",
          metric = "auc",
          num_leaves = num_leaves_param,
          learning_rate = learning_rate_param,
          feature_fraction = 0.9,
          bagging_fraction = 0.8,
          bagging_freq = 5,
          verbose = -1
        )
        
        
        
        model <- lgb.train(
          params = params,
          data = dtrain,
          nrounds = nrounds_param,
          verbose = -1
        )
        
        
        
        # Store parameters
        model$optimal_nrounds <- nrounds_param
        model$optimal_num_leaves <- num_leaves_param
        model$optimal_learning_rate <- learning_rate_param
      }
      
      # Make predictions (probabilities)
      scorelearning <- predict(model, x)
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
    }
    
    if(modelparameters$modeltype=="naivebayes"){
      # Naive Bayes classifier
      # Check if GridSearchCV should be used
      optimal_laplace <- 0  # Default value
      
      if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
        # Use GridSearchCV from superml for hyperparameter tuning
        cat("Using GridSearchCV for Naive Bayes hyperparameter tuning...\n")
        
        # Prepare parameter grid
        param_grid <- list(
          laplace = if(!is.null(modelparameters$nb_grid_laplace)) modelparameters$nb_grid_laplace else c(0, 0.5, 1, 2, 5)
        )
        
        # Run GridSearchCV
        grid_result <- tryCatch({
          X_df <- as.data.frame(learningmodel[,-1])
          tune_nb_gridsearch(X = X_df, y = learningmodel[,1],
                             param_grid = param_grid,
                             n_folds = 5,
                             scoring = c("auc", "accuracy"))
        }, error = function(e) {
          cat("GridSearchCV failed, using default laplace=0:", e$message, "\n")
          NULL
        })
        
        if(!is.null(grid_result)) {
          best_params <- grid_result$best_params
          optimal_laplace <- if(!is.null(best_params$laplace)) best_params$laplace else 0
          cat(sprintf("GridSearchCV best params: laplace=%.2f, score=%.4f\n",
                      optimal_laplace, grid_result$best_score))
        }
      }
      
      # Build model with optimal or default laplace parameter
      model <- naiveBayes(x = learningmodel[,-1], y = learningmodel[,1], laplace = optimal_laplace)
      
      # Store model type and optimal parameter
      model$model_type <- "naivebayes"
      model$optimal_laplace <- optimal_laplace
      
      # Make predictions (probabilities)
      pred_probs <- e1071:::predict.naiveBayes(model, learningmodel[,-1], type="raw")
      scorelearning <- data.frame(pred_probs[, lev["positif"]])
      colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
    }
    
    if(modelparameters$modeltype=="knn"){
      # K-Nearest Neighbors
      # Determine k parameter
      if(is.null(modelparameters$autotuneknn) || modelparameters$autotuneknn){
        # Check if GridSearchCV should be used
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          # Use GridSearchCV from superml for comprehensive hyperparameter tuning
          cat("Using GridSearchCV for KNN hyperparameter tuning...\n")
          
          # Prepare parameter grid
          max_k <- min(floor(sqrt(nrow(learningmodel))), 30)
          param_grid <- list(
            n_neighbors = if(!is.null(modelparameters$knn_grid_k)) modelparameters$knn_grid_k else seq(3, max_k, by=2)
          )
          
          # Run GridSearchCV
          grid_result <- tryCatch({
            X_df <- as.data.frame(learningmodel[,-1])
            tune_knn_gridsearch(X = X_df, y = learningmodel[,1],
                                param_grid = param_grid,
                                n_folds = 5,
                                scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to manual CV:", e$message, "\n")
            NULL
          })
          
          if(!is.null(grid_result)) {
            best_params <- grid_result$best_params
            optimal_k <- if(!is.null(best_params$n_neighbors)) best_params$n_neighbors else 5
            cat(sprintf("GridSearchCV best params: k=%d, score=%.4f\n",
                        optimal_k, grid_result$best_score))
          } else {
            set.seed(20011203)
            max_k <- min(floor(sqrt(nrow(learningmodel))), 20)
            k_values <- seq(3, max_k, by=2)
            best_k <- 3; best_acc <- 0
            knn_folds <- create_stratified_folds(learningmodel[,1], k = min(5, nrow(learningmodel) - 1))
            for(k_test in k_values){
              accuracies <- sapply(knn_folds, function(test_idx) {
                train_idx <- setdiff(1:nrow(learningmodel), test_idx)
                pred <- knn(train = learningmodel[train_idx, -1],
                            test = learningmodel[test_idx, -1],
                            cl = learningmodel[train_idx, 1], k = k_test)
                mean(pred == learningmodel[test_idx, 1])
              })
              avg_acc <- mean(accuracies)
              if(avg_acc > best_acc){ best_acc <- avg_acc; best_k <- k_test }
            }
            optimal_k <- best_k
          }
        } else {
          set.seed(20011203)
          max_k <- min(floor(sqrt(nrow(learningmodel))), 20)
          k_values <- seq(3, max_k, by=2)
          best_k <- 3; best_acc <- 0
          knn_folds <- create_stratified_folds(learningmodel[,1], k = min(5, nrow(learningmodel) - 1))
          for(k_test in k_values){
            accuracies <- sapply(knn_folds, function(test_idx) {
              train_idx <- setdiff(1:nrow(learningmodel), test_idx)
              pred <- knn(train = learningmodel[train_idx, -1],
                          test = learningmodel[test_idx, -1],
                          cl = learningmodel[train_idx, 1], k = k_test)
              mean(pred == learningmodel[test_idx, 1])
            })
            avg_acc <- mean(accuracies)
            if(avg_acc > best_acc){ best_acc <- avg_acc; best_k <- k_test }
          }
          optimal_k <- best_k
        }
        
      } else {
        # Use manual k parameter
        optimal_k <- ifelse(is.null(modelparameters$k_neighbors), 5, modelparameters$k_neighbors)
      }
      
      
      
      # KNN doesn't have a traditional "model" object, store parameters
      model <- list(
        train_data = learningmodel[,-1],
        train_labels = learningmodel[,1],
        optimal_k = optimal_k,
        model_type = "knn"
        
      )
      
      # Make predictions using knn with probability estimation
      # For probability, we'll use the proportion of k neighbors in each class
      
      scorelearning_vec <- numeric(nrow(learningmodel))
      
      for(i in 1:nrow(learningmodel)){
        # Leave-one-out prediction for training set
        train_idx <- setdiff(1:nrow(learningmodel), i)
        
        # Get k nearest neighbors
        distances <- apply(learningmodel[train_idx, -1], 1, function(row) {
          sqrt(sum((as.numeric(learningmodel[i, -1]) - as.numeric(row))^2))
          
        })
        
        k_nearest_idx <- order(distances)[1:optimal_k]
        k_nearest_labels <- learningmodel[train_idx, 1][k_nearest_idx]
        
        # Calculate probability as proportion of positif class
        
        scorelearning_vec[i] <- sum(k_nearest_labels == lev["positif"]) / optimal_k
      }
      
      
      
      scorelearning <- data.frame(scorelearning_vec)
      colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
      
      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
      
    }
    
    if(modelparameters$modeltype=="elasticnet"){
      # Penalized Logistic Regression (ElasticNet)
      x <- as.matrix(learningmodel[,-1])
      # IMPORTANT: Encode y so that 1 = lev["positif"] (first level), 0 = lev["negatif"] (second level)
      # This ensures that predict returns P(lev["positif"])
      y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
      
      # Get hyperparameters (use defaults if not provided)
      alpha_param <- ifelse(is.null(modelparameters$alpha), 0.5, modelparameters$alpha)
      lambda_param <- modelparameters$lambda  # NULL for CV selection
      
      # Check if GridSearchCV should be used
      if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch && is.null(lambda_param)){
        # Use GridSearchCV from superml for comprehensive hyperparameter tuning
        cat("Using GridSearchCV for ElasticNet hyperparameter tuning...\n")
        
        # Prepare parameter grid
        param_grid <- list(
          alpha = if(!is.null(modelparameters$en_grid_alpha)) modelparameters$en_grid_alpha else c(0, 0.25, 0.5, 0.75, 1.0),
          lambda = if(!is.null(modelparameters$en_grid_lambda)) modelparameters$en_grid_lambda else c(0.001, 0.01, 0.1, 1.0)
        )
        
        # Run GridSearchCV
        grid_result <- tryCatch({
          X_df <- as.data.frame(x)
          tune_elasticnet_gridsearch(X = X_df, y = learningmodel[,1],
                                     param_grid = param_grid,
                                     n_folds = 5,
                                     scoring = c("auc", "accuracy"))
        }, error = function(e) {
          cat("GridSearchCV failed, falling back to cv.glmnet:", e$message, "\n")
          NULL
        })
        
        if(!is.null(grid_result)) {
          best_params <- grid_result$best_params
          alpha_param <- if(!is.null(best_params$alpha)) best_params$alpha else 0.5
          lambda_param <- if(!is.null(best_params$lambda)) best_params$lambda else NULL
          
          cat(sprintf("GridSearchCV best params: alpha=%.3f, lambda=%.4f, score=%.4f\n",
                      alpha_param, lambda_param, grid_result$best_score))
          
          # Use the best parameters to fit with cv.glmnet for consistency
          set.seed(20011203)
          en_k <- min(10, nrow(learningmodel)-1)
          en_folds <- create_stratified_folds(learningmodel[,1], k = en_k)
          en_foldid <- folds_to_foldid(en_folds, nrow(learningmodel))
          cvfit <- cv.glmnet(x, y, family="binomial", alpha=alpha_param,
                             type.measure="auc", foldid=en_foldid)
          lambda_param <- cvfit$lambda.min
          model <- list(glmnet_model=cvfit, lambda=lambda_param, alpha=alpha_param,
                        cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
        } else {
          # Fallback to traditional cv.glmnet if GridSearchCV fails
          set.seed(20011203)
          en_k <- min(10, nrow(learningmodel)-1)
          en_folds <- create_stratified_folds(learningmodel[,1], k = en_k)
          en_foldid <- folds_to_foldid(en_folds, nrow(learningmodel))
          cvfit <- cv.glmnet(x, y, family="binomial", alpha=alpha_param,
                             type.measure="auc", foldid=en_foldid)
          lambda_param <- cvfit$lambda.min
          model <- list(glmnet_model=cvfit, lambda=lambda_param, alpha=alpha_param,
                        cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
        }
      } else if(is.null(lambda_param)){
        # Perform stratified cross-validation to find optimal lambda if not provided
        set.seed(20011203)
        en_k <- min(10, nrow(learningmodel)-1)
        en_folds <- create_stratified_folds(learningmodel[,1], k = en_k)
        en_foldid <- folds_to_foldid(en_folds, nrow(learningmodel))
        cvfit <- cv.glmnet(x, y, family="binomial", alpha=alpha_param,
                           type.measure="auc", foldid=en_foldid)
        lambda_param <- cvfit$lambda.min
        # if(!is.null(cvfit$glmnet.fit)){
        #   fit <- cvfit$glmnet.fit
        # } else {
        #   fit <- glmnet(x, y, family="binomial", alpha=alpha_param)
        # }
        model <- list(glmnet_model=cvfit, lambda=lambda_param, alpha=alpha_param,
                      cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
      } else {
        # Manual mode: use specified lambda and alpha parameters
        cat("Creating ElasticNet model with manual parameters: alpha=", alpha_param, ", lambda=", lambda_param, "\n")
        fit <- glmnet(x, y, family="binomial", alpha=alpha_param, lambda=lambda_param)
        model <- list(glmnet_model=fit, lambda=lambda_param, alpha=alpha_param,
                      cvfit=NULL, optimal_lambda=lambda_param, lambda_1se=NULL)
      }
      
      cat("la classe de model$glmnet_model est : ", class(model$glmnet_model), "\n" )
      
      # Feature selection based on non-zero coefficients
      if(modelparameters$fs){
        coef_values <- as.matrix(coef(model$glmnet_model, s=lambda_param))
        selected_features <- rownames(coef_values)[which(coef_values[-1,1] != 0)]
        if(length(selected_features) > 0){
          learningmodel <- learningmodel[, c("group", selected_features)]
          x <- as.matrix(learningmodel[,-1])
          # Refit model with selected features
          if(is.null(modelparameters$lambda)){
            fs_k <- min(10, nrow(learningmodel)-1)
            fs_folds <- create_stratified_folds(learningmodel[,1], k = fs_k)
            fs_foldid <- folds_to_foldid(fs_folds, nrow(learningmodel))
            cvfit <- cv.glmnet(x, y, family="binomial", alpha=alpha_param,
                               type.measure="auc", foldid=fs_foldid)
            lambda_param <- cvfit$lambda.min
            # Refit model with optimal lambda to ensure we have a valid glmnet object
            fit <- glmnet(x, y, family="binomial", alpha=alpha_param, lambda=lambda_param)
            cat("class of fitted modele :  ", class(fit))
            model <- list(glmnet_model=fit, lambda=lambda_param, alpha=alpha_param,
                          cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
          } else {
            fit <- glmnet(x, y, family="binomial", alpha=alpha_param, lambda=lambda_param)
            model <- list(glmnet_model=fit, lambda=lambda_param, alpha=alpha_param,
                          cvfit=NULL, optimal_lambda=lambda_param, lambda_1se=NULL)
          }
        }
      }
      
      # Make predictions (probabilities)
      # Use appropriate predict method based on model class
      if(inherits(model$glmnet_model, "cv.glmnet")){
        scorelearning <- as.vector(glmnet:::predict.cv.glmnet(model$glmnet_model, newx=x, s=lambda_param, type="response"))
      } else {
        scorelearning <- as.vector(glmnet::predict.glmnet(model$glmnet_model, newx=x, s=lambda_param, type="response"))
      }
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
      
      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
    }
    
    if(modelparameters$modeltype=="xgboost"){
      x <- as.matrix(learningmodel[,-1])
      y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
      
      dtrain <- xgb.DMatrix(data = x, label = y)
      
      # Determine hyperparameters
      if(is.null(modelparameters$autotunexgb) || modelparameters$autotunexgb){
        # Check if GridSearchCV should be used
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          # Use GridSearchCV from superml for comprehensive hyperparameter tuning
          cat("Using GridSearchCV for XGBoost hyperparameter tuning...\n")
          
          # Prepare parameter grid
          param_grid <- list(
            n_estimators = if(!is.null(modelparameters$xgb_grid_nrounds)) modelparameters$xgb_grid_nrounds else c(50, 100, 200),
            max_depth = if(!is.null(modelparameters$xgb_grid_maxdepth)) modelparameters$xgb_grid_maxdepth else c(3, 6, 9),
            learning_rate = if(!is.null(modelparameters$xgb_grid_eta)) modelparameters$xgb_grid_eta else c(0.01, 0.1, 0.3),
            gamma = if(!is.null(modelparameters$xgb_grid_gamma)) modelparameters$xgb_grid_gamma else c(0, 0.1, 0.5),
            subsample = if(!is.null(modelparameters$xgb_grid_subsample)) modelparameters$xgb_grid_subsample else c(0.8, 1.0)
          )
          
          # Run GridSearchCV
          grid_result <- tryCatch({
            # Convert data for superml
            X_df <- as.data.frame(x)
            tune_xgb_gridsearch(X = X_df, y = learningmodel[,1],
                                param_grid = param_grid,
                                n_folds = 5,
                                scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to xgb.cv:", e$message, "\n")
            NULL
          })
          
          if(!is.null(grid_result)) {
            # Extract best parameters from GridSearchCV
            best_params <- grid_result$best_params
            
            optimal_nrounds <- if(!is.null(best_params$n_estimators)) best_params$n_estimators else 100
            optimal_max_depth <- if(!is.null(best_params$max_depth)) best_params$max_depth else 6
            optimal_eta <- if(!is.null(best_params$learning_rate)) best_params$learning_rate else 0.3
            optimal_gamma <- if(!is.null(best_params$gamma)) best_params$gamma else 0
            optimal_subsample <- if(!is.null(best_params$subsample)) best_params$subsample else 1.0
            optimal_min_child_weight <- if(!is.null(best_params$min_child_weight)) best_params$min_child_weight else 1
            
            cat(sprintf("GridSearchCV best params: nrounds=%d, max_depth=%d, eta=%.3f, gamma=%.3f, score=%.4f\n",
                        optimal_nrounds, optimal_max_depth, optimal_eta, optimal_gamma, grid_result$best_score))
            
            # Create final parameters list
            final_params <- list(
              objective = "binary:logistic",
              eval_metric = "auc",
              max_depth = optimal_max_depth,
              eta = optimal_eta,
              gamma = optimal_gamma,
              subsample = optimal_subsample,
              min_child_weight = optimal_min_child_weight
            )
            
            # Train final model with optimal parameters
            model <- xgb.train(
              params = final_params,
              data = dtrain,
              nrounds = optimal_nrounds,
              verbose = 0
            )
            
            # Store optimal parameters
            model$optimal_nrounds <- optimal_nrounds
            model$optimal_max_depth <- optimal_max_depth
            model$optimal_eta <- optimal_eta
            model$optimal_gamma <- optimal_gamma
            model$optimal_subsample <- optimal_subsample
            cat("optimal_subsample :  ", optimal_subsample, "\n")
            model$optimal_min_child_weight <- optimal_min_child_weight
          } else {
            # Fallback to traditional xgb.cv if GridSearchCV fails
            # Perform hyperparameter tuning using cross-validation
            set.seed(20011203)
            
            alpha_param   <- ifelse(is.null(modelparameters$alpha_xgb),   0, modelparameters$alpha_xgb)
            lambda_param  <- ifelse(is.null(modelparameters$lambda_xgb),  0, modelparameters$lambda_xgb)
            gamma_param   <- ifelse(is.null(modelparameters$gamma_xgb),   0, modelparameters$gamma_xgb)
            subsample_param <- ifelse(is.null(modelparameters$subsample_xgb), 1.0, modelparameters$subsample_xgb)
            
            # Parameter grid search
            best_params <- list(
              objective = "binary:logistic",
              eval_metric = "auc",
              alpha = alpha_param,
              lambda = lambda_param,
              gamma = gamma_param,
              subsample = subsample_param,
              max_depth = 6,
              eta = 0.3,
              min_child_weight = 1
            )
            
            # Stratified cross-validation to find optimal nround
            xgb_folds <- create_stratified_folds(learningmodel[,1], k = min(5, nrow(learningmodel)-1))
            tryCatch({
              cv_results <- xgb.cv(
                params = best_params,
                data = dtrain,
                nrounds = 200,
                folds = xgb_folds,
                early_stopping_rounds = 10,
                verbose = 0
              )
              
              optimal_nrounds <- cv_results$best_iteration
            }, error =  function(e){
              e$message
              showNotification(
                "Something wrong where running!, Try GridSearchCV (superml) option ",
                type = 'error',
                
              )
              
              optimal_nrounds <- 20
            })
            
            
            # Train final model with optimal parameters
            model <- xgb.train(
              params = best_params,
              data = dtrain,
              nrounds = optimal_nrounds,
              verbose = 0
            )
            
            # Store optimal parameters
            model$optimal_nrounds <- optimal_nrounds
            model$optimal_max_depth <- best_params$max_depth
            model$optimal_eta <- best_params$eta
            model$optimal_min_child_weight <- best_params$min_child_weight
            model$optimal_gamma <- 0
            model$optimal_subsample <- 1.0
          }
        } else {
          # Use traditional xgb.cv for hyperparameter tuning
          # Perform hyperparameter tuning using cross-validation
          set.seed(20011203)
          
          gamma_param   <- ifelse(is.null(modelparameters$gamma_xgb),   0, modelparameters$gamma_xgb)
          subsample_param <- ifelse(is.null(modelparameters$subsample_xgb), 1.0, modelparameters$subsample_xgb)
          
          # Parameter grid search
          best_params <- list(
            objective = "binary:logistic",
            eval_metric = "auc",
            max_depth = 6,
            eta = 0.3,
            min_child_weight = 1,
            gamma = gamma_param,
            subsample = subsample_param
          )
          
          # Stratified cross-validation to find optimal nrounds
          xgb_folds <- create_stratified_folds(learningmodel[,1], k = min(5, nrow(learningmodel)-1))
          cv_results <- xgb.cv(
            params = best_params,
            data = dtrain,
            nrounds = 200,
            folds = xgb_folds,
            early_stopping_rounds = 10,
            verbose = 0
          )
          
          optimal_nrounds <- cv_results$best_iteration
          
          # Train final model with optimal parameters
          cat("best parmeters \n")
          print(best_params)
          
          cat("optimal rounds  : ", optimal_nrounds, "\n")
          model <- xgb.train(
            params = best_params,
            data = dtrain,
            nrounds = optimal_nrounds,
            verbose = 0
          )
          
          # Store optimal parameters
          model$optimal_nrounds <- optimal_nrounds
          model$optimal_max_depth <- best_params$max_depth
          model$optimal_eta <- best_params$eta
          model$optimal_min_child_weight <- best_params$min_child_weight
          model$optimal_gamma <- best_params$gamma
          model$optimal_subsample <- best_params$subsample
          cat("optimal_subsample :  ", best_params$subsample, "\n")
        }
        
      } else {
        # Use manual hyperparameters
        nrounds_param <- ifelse(is.null(modelparameters$nrounds), 100, modelparameters$nrounds)
        max_depth_param <- ifelse(is.null(modelparameters$max_depth), 6, modelparameters$max_depth)
        eta_param <- ifelse(is.null(modelparameters$eta), 0.3, modelparameters$eta)
        gamma_param <- ifelse(is.null(modelparameters$gamma_xgb), 0, modelparameters$gamma_xgb)
        subsample_param <- ifelse(is.null(modelparameters$subsample_xgb), 1.0, modelparameters$subsample_xgb)
        min_child_weight_param <- ifelse(is.null(modelparameters$min_child_weight), 1, modelparameters$min_child_weight)
        
        params <- list(
          objective = "binary:logistic",
          eval_metric = "auc",
          max_depth = max_depth_param,
          eta = eta_param,
          min_child_weight = 1
        )
        
        model <- xgb.train(
          params = params,
          data = dtrain,
          nrounds = nrounds_param,
          verbose = 0
        )
        
        # Store parameters
        model$optimal_nrounds <- nrounds_param
        model$optimal_max_depth <- max_depth_param
        model$optimal_eta <- eta_param
        model$optimal_min_child_weight <- min_child_weight_param
        model$optimal_gamma <- gamma_param
        model$optimal_subsample <- subsample_param
        cat("optimal_subsample :  ", subsample_param, "\n")
        cat("optimal_gamma :  ", gamma_param, "\n")
      }
      
      # Make predictions (probabilities)
      scorelearning <- xgboost:::predict.xgb.Booster(model, x)
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
      
      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
    }
    
    #levels(predictclassval)<-paste("test",levels(predictclasslearning),sep="")
    levels(predictclasslearning)<-paste("test ",lev,sep="")
    classlearning<-learningmodel[,1]
    
    ##########
    # # Calculate Youden threshold from training data
    # youden_result <- younden(classlearning, scorelearning[,1])
    # youden_threshold <- youden_result[4]  # 4th element is the threshold
    #
    # # Update model parameters with Youden threshold
    # modelparameters$thresholdmodel <- youden_threshold
    #
    # # Recalculate predictions using Youden threshold instead of fixed 0.5
    # predictclasslearning <- factor(levels = lev)
    # predictclasslearning[which(scorelearning[,1] >= youden_threshold)] <- lev["positif"]
    # predictclasslearning[which(scorelearning[,1] < youden_threshold)] <- lev["negatif"]
    # predictclasslearning <- as.factor(predictclasslearning)
    # levels(predictclasslearning)<-paste("test",lev,sep="")
    
    ########
    
    reslearningmodel<-data.frame(classlearning,scorelearning,predictclasslearning)
    colnames(reslearningmodel) <-c("classlearning","scorelearning","predictclasslearning")
    datalearningmodel<-list("learningmodel"=learningmodel,"reslearningmodel"=reslearningmodel)
    
    if (modelparameters$adjustval){
      #Validation
      colnames(validation)[1]<-"group"
      validationdiff<-validation[,which(colnames(validation)%in%colnames(learningmodel))]
      learningselect2<-learningselect
      if(transformdataparameters$log) {
        validationdiff[,-1]<-transformationlog(x = validationdiff[,-1]+1,logtype =transformdataparameters$logtype )
        learningselect2[,-1]<-transformationlog(x = learningselect2[,-1]+1,logtype=transformdataparameters$logtype)}
      if(transformdataparameters$arcsin){
        maxlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = max,na.rm=T)
        minlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = min,na.rm=T)
        for (i in 2:dim(validationdiff)[2]){
          validationdiff[,i]<-(validationdiff[,i]-minlearn[i-1])/(maxlearn[i-1]-minlearn[i-1])
          #validationdiff[,-1]<-apply(X = as.data.frame(validationdiff[,-1]),MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
          validationdiff[which(validationdiff[,i]>1),i]<-1
          validationdiff[which(validationdiff[,i]<0),i]<-0
          validationdiff[,i]<-asin(sqrt(validationdiff[,i]))
        }
        learningselect2[,-1]<-apply(X = learningselect2[,-1],MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
        learningselect2[,-1]<-asin(sqrt(learningselect2[,-1]))
      }
      if(transformdataparameters$standardization){
        learningselectval<<-learningselect2
        # Calculer sdselect uniquement sur les colonnes de features (excluant "group")
        feat_cols_learning <- colnames(learningselect2)[colnames(learningselect2) %in% colnames(validationdiff) & colnames(learningselect2) != "group"]
        sdselect<-apply(learningselect2[, feat_cols_learning, drop = FALSE], 2, sd,na.rm=T)
        print('sdselect')
        print(sdselect)
        # Aligner avec les colonnes de features de validationdiff
        feat_cols <- colnames(validationdiff)[-1]
        sdselect_aligned <- sdselect[feat_cols]
        validationdiff[,-1]<-scale(as.matrix(validationdiff[,-1]),center=F,scale=sdselect_aligned)
      }
      
      #NAstructure if NA ->0
      if(!is.null(datastructuresfeatures)){
        validationdiff[which(is.na(validationdiff),arr.ind = T)[which(which(is.na(validationdiff),arr.ind = T)[,2]%in%which(colnames(validationdiff)%in%datastructuresfeatures$names)),]]<-0
      }
      #
      validationmodel<<- replaceNAvalidation(as.data.frame(validationdiff[,-1]),toto=as.data.frame(learningmodel[,-1]),rempNA=transformdataparameters$rempNA)
      colnames(validationmodel)<-colnames(validationdiff)[-1]
      rownames(validationmodel)<-rownames(validationdiff)
      
      #prediction a partir du model
      
      #prediction a partir du model
      if(modelparameters$modeltype=="randomforest"){
        scoreval <- randomForest:::predict.randomForest(object=model,type="prob",newdata = validationmodel)[,lev["positif"]]
        predictclassval<-vector(length = length(scoreval) )
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
        
      }
      
      if(modelparameters$modeltype=="svm"){
        if(!is.null(model)){
          # SVM validation predictions
          cat("=== [modelfunction OLD] SVM VALIDATION DIAGNOSTIC ===\n")
          cat("colnames(learningmodel):", paste(colnames(learningmodel), collapse=", "), "\n")
          cat("colnames(validationmodel):", paste(colnames(validationmodel), collapse=", "), "\n")
          cat("dim(learningmodel):", dim(learningmodel), "\n")
          cat("dim(validationmodel):", dim(validationmodel), "\n")
          cat("column types of validationmodel:\n"); print(sapply(validationmodel, class))
          missing_from_val <- setdiff(colnames(learningmodel)[-1], colnames(validationmodel))
          extra_in_val     <- setdiff(colnames(validationmodel), colnames(learningmodel)[-1])
          cat("Missing in validationmodel:", paste(missing_from_val, collapse=", "), "\n")
          cat("Extra in validationmodel:", paste(extra_in_val, collapse=", "), "\n")
          print(e1071:::predict.svm(model,newdata =  validationmodel,decision.values=T))
          
          #calculate decision values for the validation set
          scoreval =attr(e1071:::predict.svm(model,newdata =  validationmodel,decision.values=T),"decision.values")
          cat("scoreval :  \n")
          print(scoreval)
          if(sum(lev==(strsplit(colnames(scoreval),split = "/")[[1]]))==0){scoreval<-scoreval*(-1)}
          
          # Utiliser les probabilités pour la validation
          # pred_probs_val <- attr(e1071:::predict.svm(model, newdata = validationmodel, probability=TRUE), "probabilities")
          # scoreval <- pred_probs_val[, lev["positif"]]
          
          predictclassval<-vector(length = length(scoreval) )
          predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
          predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
          predictclassval<-as.factor(predictclassval)
        }
        
      }
      
      if(modelparameters$modeltype=="elasticnet"){
        req(model$glmnet_model)
        # ElasticNet validation predictions
        x_val <- as.matrix(validationmodel)
        # scoreval <- as.vector(glmnet:::predict.cv.glmnet(model$glmnet_model, newx=x_val, s=model$lambda, type="response"))
        if(inherits(model$glmnet_model, "cv.glmnet")){
          scoreval <- as.vector(glmnet:::predict.cv.glmnet(model$glmnet_model,
                                                           newx=x_val, s=model$lambda, type="response"))
        } else {
          scoreval <- as.vector(glmnet::predict.glmnet(model$glmnet_model, newx=x_val, s=model$lambda, type="response"))
        }
        # scoreval <- as.vector(glmnet::predict.glmnet(model$glmnet_model, newx=x_val, s=model$lambda, type="response"))
        predictclassval<-vector(length = length(scoreval) )
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
      }
      
      if(modelparameters$modeltype=="xgboost"){
        # XGBoost validation predictions
        x_val <- as.matrix(validationmodel)
        dval <- xgb.DMatrix(data = x_val)
        scoreval <- xgboost:::predict.xgb.Booster(model, dval)
        predictclassval<-vector(length = length(scoreval) )
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
      }
      
      if(modelparameters$modeltype=="lightgbm"){
        # LightGBM validation predictions
        x_val <- as.matrix(validationmodel)
        scoreval <- predict(model, x_val)
        predictclassval<-vector(length = length(scoreval) )
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
      }
      
      if(modelparameters$modeltype=="naivebayes"){
        # Naive Bayes validation predictions
        pred_probs <- e1071:::predict.naiveBayes(model, validationmodel, type="raw")
        scoreval <- pred_probs[, lev["positif"]]
        predictclassval<-vector(length = length(scoreval) )
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
      }
      
      if(modelparameters$modeltype=="knn"){
        # KNN validation predictions
        # Get k nearest neighbors for probability estimation
        scoreval_vec <- numeric(nrow(validationmodel))
        for(i in 1:nrow(validationmodel)){
          # Calculate distances to all training points
          distances <- apply(model$train_data, 1, function(row) {
            sqrt(sum((as.numeric(validationmodel[i, ]) - as.numeric(row))^2))
          })
          # Get k nearest neighbors
          k_nearest_idx <- order(distances)[1:model$optimal_k]
          k_nearest_labels <- model$train_labels[k_nearest_idx]
          # Calculate probability as proportion of positif class
          scoreval_vec[i] <- sum(k_nearest_labels == lev["positif"]) / model$optimal_k
        }
        scoreval <- scoreval_vec
        predictclassval<-vector(length = length(scoreval) )
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
      }
      
      if(sum(lev==(levels(predictclassval)))==0){
        predictclassval<-factor(predictclassval,levels = rev(levels(predictclassval)),ordered = TRUE)
      }
      classval<- validation[,1]
      if(sum(lev==(levels(classval)))==0){
        classval<-factor(classval,levels = rev(levels(classval)),ordered = TRUE)
      }
      
      #levels(predictclassval)<-paste("test",levels(predictclassval),sep="")
      levels(predictclassval)<-paste("test ",lev,sep="")
      resvalidationmodel<-data.frame(classval,scoreval,predictclassval)
      colnames(resvalidationmodel) <-c("classval","scoreval","predictclassval")
      auc <- tryCatch(
        pROC::auc(pROC::roc(as.vector(classval), as.vector(scoreval), quiet=T)),
        error = function(e) {
          cat("[modelfunction OLD] pROC::roc failed:", e$message, "\n")
          cat("  sum(is.na(scoreval)):", sum(is.na(as.vector(scoreval))), "/ length:", length(as.vector(scoreval)), "\n")
          NA
        }
      )
      datavalidationmodel<-list("validationdiff"=validationdiff,
                                "validationmodel"=validationmodel,
                                "resvalidationmodel"=resvalidationmodel,"auc"=auc)
      
    }
    else{datavalidationmodel<-list()}
    res<-list("datalearningmodel"= datalearningmodel,
              "model"= model,
              "datavalidationmodel"= datavalidationmodel,
              "groups"= lev,
              "parameters" = modelparameters)
  }
}




replaceNAvalidation<-function(validationdiff,toto,rempNA){
  validationdiffssNA<-validationdiff
  for(i in 1:nrow(validationdiff)){
    validationdiffssNA[i,]<-replaceNAoneline(lineNA = validationdiff[i,],toto = toto,rempNA =rempNA)
  }
  return(validationdiffssNA)
}

replaceNAoneline<-function(lineNA,toto,rempNA){
  alldata<-rbind(lineNA,toto)
  if(rempNA=="moygr"){ 
    #print("impossible de remplacer les NA par la moyenne par group pour la validation")
    linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA ="moy")[1,-1]       
     }
  
  else{linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA =rempNA)[1,-1]}
  
  return(linessNA)
}

ROCcurve<-function(validation,decisionvalues,maintitle="Roc curve",graph=T,ggplot=T){
  
  validation<-factor(validation,levels = rev(levels(validation)),ordered = TRUE)
  
  #argument : validation, vector of appartenance,
  #            decisionvalues, vector of scores
  #fulldata<-rocdata(grp = validation, pred = as.vector(decisionvalues))
  data<-pROC::roc(validation,decisionvalues)
  if(!graph){return(data.frame("sensitivity"=data$sensitivities,"specificity"=data$specificities,"thresholds"=data$thresholds))}
  if(!ggplot){plot(data)}
  if(ggplot){
    y<-rev(data$sensitivities)
    x<-rev(data$specificities)
    roc<-data.frame(x,y)
    auc<-as.numeric(pROC::auc(data))
    
    col<-gg_color_hue(3)
    roccol<-col[1]
    bin = 0.01
    diag = data.frame(x = seq(0, 1, by = bin), y = rev(seq(0, 1, by = bin)))
    p <- ggplot(data = roc, aes(x = x, y = y)) + 
      geom_point(color = roccol) +
      geom_line(color = roccol) + 
      geom_line(data = diag, aes(x = x, y = y), color =col[3])
    sp = 19
    f <- p + geom_point(data = diag, aes(x = x, y = y), color = "lightgrey", shape = sp) + 
      theme(axis.text = element_text(size = 16), 
            title = element_text(size = 18, face = 'bold') , 
            axis.text.x = element_text(size = 15 ,  face = 'bold' ) ,
            axis.text.y =  element_text(size = 15 , face =  'bold'),
            axis.title.x = element_text(size = 18 , face = 'bold'), 
            axis.title.y =  element_text(size = 18 , face = 'bold')
      ) + 
      labs(y = "Sensitivity", x = "1 - Specificity", title = maintitle) +
      annotate("text",x=0.2,y=0.1,label=paste("AUC = ",as.character(round(auc,digits = 3))),size=7,colour= roccol)+
      scale_x_reverse()
    
    f
  }
}

scoremodelplot<-function(class,score,names,threshold,type,graph,printnames,jitter, maintitle = "Score representation train"){
  class<-factor(class,levels =rev(levels(class)))
  
  if(type=="boxplot"){
    boxplotggplot(class = class,
                  score = score, 
                  names = names,
                  threshold=threshold,
                  maintitle = maintitle,
                  graph = graph, jitter = jitter)
  }
  else if(type=="points"){
    plot_pred_type_distribution(class = class, 
                                score = score
                                ,names=names,
                                threshold=threshold,
                                graph=graph,
                                maintitle = maintitle,
                                printnames=printnames  )
  } 
}

boxplotggplot<-function(class,score,names,threshold,maintitle="Score representation ",graph=T, jitter = TRUE){
  data<-data.frame("names"=names,"class"= class,"score"=as.vector(score))
  if(!graph){return(data)}
  p<-ggplot(data, aes(x=class, y=score)) +
    scale_fill_manual( values = c("#00BFC4","#F8766D") ) +
    geom_boxplot(aes(fill=class),  outlier.colour = "gray") +
    geom_hline(yintercept = threshold, color='red', alpha=0.6) +
    ggtitle(maintitle) + 
    theme(plot.title=element_text( size=15), 
          axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
          axis.text.y =  element_text(size = 12 , face =  'bold'),
          axis.title.x = element_text(size = 15 , face = 'bold'), 
          axis.title.y =  element_text(size = 15 , face = 'bold'),
          legend.text = element_text( size = 12 , face = 'bold'),
          legend.title = element_text(size = 14 , face =  'bold'))
  
  if(jitter) {
    p = p + geom_jitter()
  }
  
  p
}

plot_pred_type_distribution <- function(class,score,names, threshold,
                                        maintitle="Score representation",printnames=F,graph=T) {
  #in this function the levels of the class is inverted in order to have the control group on the left side of the graph
  df<-data.frame(names,class,score)
  colnames(df)<-c("names","class","score")
  v <-rep(NA, nrow(df))
  v <- ifelse(df$score >= threshold & df$class == levels(class)[2], "TruePositiv", v)
  v <- ifelse(df$score >= threshold & df$class == levels(class)[1], "FalsePositiv", v)
  v <- ifelse(df$score < threshold & df$class ==  levels(class)[2], "FalseNegativ", v)
  v <- ifelse(df$score < threshold & df$class == levels(class)[1], "TrueNegativ", v)
  
  df$predtype <-factor(v,levels = c("FalseNegativ","FalsePositiv","TrueNegativ","TruePositiv"),ordered = T)
  if(!graph){return(df)}
  set.seed(20011203)
  if(printnames){
    g<-ggplot(data=df, aes(x=class, y=score)) + 
      #geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
      # geom_boxplot(fill=rgb(1,1,1,alpha=0.6), color=NA) +
      geom_text(label=names,colour=palet(df$predtype,multiple = TRUE))+
      geom_jitter(aes(color=predtype), alpha=0.6) +
      geom_hline(yintercept=threshold, color="red", alpha=0.6) +
      scale_color_manual(values=palet(predtype = df$predtype),name="") +
      ggtitle(maintitle) + 
      theme(plot.title=element_text( size=15),
            axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
            axis.text.y =  element_text(size = 12 , face =  'bold'),
            axis.title.x = element_text(size = 15 , face = 'bold'), 
            axis.title.y =  element_text(size = 15 , face = 'bold'),
            legend.text = element_text( size = 12 , face = 'bold'),
            legend.title = element_text(size = 14 , face =  'bold'),
            legend.position ="bottom")
  }
  else{
    g<-ggplot(data=df, aes(x=class, y=score)) + 
      #geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
      # geom_boxplot(fill=rgb(1,1,1,alpha=0.6), color=NA) +
      geom_jitter(aes(color=predtype), alpha=0.6) +
      geom_hline(yintercept=threshold, color="red", alpha=0.6) +
      scale_color_manual(values=palet(predtype = df$predtype),name="") +
      ggtitle(maintitle) + 
      theme(plot.title=element_text( size=15), 
            axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
            axis.text.y =  element_text(size = 12 , face =  'bold'),
            axis.title.x = element_text(size = 15 , face = 'bold'), 
            axis.title.y =  element_text(size = 15 , face = 'bold'),
            legend.text = element_text( size = 12 , face = 'bold'),
            legend.title = element_text(size = 14 , face =  'bold'),
            legend.position ="bottom")
  }
  g
  
}


# fonction wiht boxplot with jitter $

jitter_boxlpot =  function(class, score , names , threshold, maintitle =  "score plot with boxplot", graph = T){
  data <-data.frame("names"=names,"class"= class,"score"=as.vector(score))
  
  if(!graph){return(data)}
  p<-ggplot(data, aes(x=class, y=score)) +
    scale_fill_manual( values = c("#00BFC4","#F8766D") ) +
    geom_boxplot(aes(fill=class)) +
    geom_jitter(aes(color=class), alpha=0.6) +
    geom_hline(yintercept = threshold, color='red', alpha=0.6) +
    ggtitle(maintitle) + 
    theme(plot.title=element_text( size=15), 
          axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
          axis.text.y =  element_text(size = 12 , face =  'bold'),
          axis.title.x = element_text(size = 15 , face = 'bold'), 
          axis.title.y =  element_text(size = 15 , face = 'bold'),
          legend.text = element_text( size = 12 , face = 'bold'),
          legend.title = element_text(size = 14 , face =  'bold'))
  
  p
}
palet<-function(predtype,multiple=FALSE){
  if(multiple){col<-as.character(predtype)}
  else{col<-sort(unique(as.character(predtype)))}
  col[which(col=="FalseNegativ")]<-"#C77CFF"
  col[which(col=="FalsePositiv")]<-"#00BA38"
  col[which(col=="TrueNegativ")]<-"#00BFC4"
  col[which(col=="TruePositiv")]<-"#F8766D"
  return(col)
}


selectedfeature<-function(model,modeltype,tab,validation,criterionimportance,criterionmodel,fstype="learn"){
  rmvar<-testmodel(model=model,modeltype = modeltype,tab=tab,validation=validation,
                   criterionimportance = criterionimportance,criterionmodel = criterionmodel,fstype=fstype)
  i=0
  tabdiff2<-tab
  while(rmvar!=0){
    i<-i+1
    print(paste(i,"eliminates features"))
    tabdiff2<-tabdiff2[,-rmvar]
    if(modeltype=="svm"){
      colnames(tabdiff2)[1] <- "group"
      tune_result <- tune_svm_stratified(data = tabdiff2,
                                         gamma_range = 10^(-5:2), cost_range = 10^(-3:2),
                                         kernel = "radial",
                                         n_folds = min(5, nrow(tabdiff2) - 1))
      model <- svm(group ~ ., data = tabdiff2, kernel = "radial",
                   cost = tune_result$best.parameters$cost,
                   gamma = tune_result$best.parameters$gamma,
                   type = "C-classification")
      model$cost <- tune_result$best.parameters$cost
      model$gamma <- tune_result$best.parameters$gamma
    }
    if (modeltype=="randomforest"){      
      tabdiff2<-as.data.frame(tabdiff2[,c(colnames(tabdiff2)[1],sort(colnames(tabdiff2[,-1])))])
      tabdiff2<-as.data.frame(tabdiff2[sort(rownames(tabdiff2)),])
      
      set.seed(20011203)
      model <- randomForest(tabdiff2[,-1],tabdiff2[,1],ntree=1000,importance=T,keep.forest=T)
    }
    rmvar<-testmodel(model=model,modeltype = modeltype,tab=tabdiff2,validation=validation,
                     criterionimportance = criterionimportance,criterionmodel = criterionmodel,fstype=fstype)
  }
  res<-list("dataset"=tabdiff2,"model"=model)
  return(res)
}


testmodel<-function(model,modeltype,tab,validation,criterionimportance,criterionmodel,fstype){
  #retourn la variable a enlever
  importancevar<-importancemodelsvm(model = model,modeltype=modeltype,tabdiff=tab,criterion = criterionimportance)
  lessimportantevar<-which(importancevar==min(importancevar,na.rm =T) )
  test<-vector()
  if(modeltype=="svm"){
    if(criterionmodel=="BER"){bermod<-BER(class = tab[,1],classpredict = model$fitted)}
    if(criterionmodel=="auc"){
      if (fstype=='learn'){aucmod <- pROC::auc(pROC::roc(tab[,1], as.vector(model$decision.values),quiet=T))}
      if (fstype=='val'){
        print("")
        #predict sur la validation
        #mais pour ca validation doit etre = a validationmodel, avec toute les transformation
      }}
    for(i in 1:length(lessimportantevar)){
      tabdiff2<-tab[,-lessimportantevar[i]]
      colnames(tabdiff2)[1] <- "group"
      tune_result_diff <- tune_svm_stratified(data = tabdiff2,
                                              gamma_range = 10^(-5:2), cost_range = 10^(-3:2),
                                              kernel = "radial",
                                              n_folds = min(5, nrow(tabdiff2) - 1))
      resmodeldiff <- svm(group ~ ., data = tabdiff2, kernel = "radial",
                          cost = tune_result_diff$best.parameters$cost,
                          gamma = tune_result_diff$best.parameters$gamma,
                          type = "C-classification")
      if(criterionmodel=="accuracy"){test[i]<-resmodeldiff$tot.accuracy-model$tot.accuracy}
      if(criterionmodel=="BER"){
        #print(paste("Ber test :",BER(class = tabdiff2[,1],classpredict = resmodeldiff$fitted) ))
        test[i]<-bermod-BER(class = tabdiff2[,1],classpredict = resmodeldiff$fitted)}
      if(criterionmodel=="auc"){
        test[i]<-pROC::auc(pROC::roc(tabdiff2[,1], as.vector(resmodeldiff$decision.values),quiet=T))-aucmod}
    }}
  if(modeltype=="randomforest"){
    if(criterionmodel=="BER"){bermod<-BER(class = tab[,1],classpredict = model$predicted)}
    if(criterionmodel=="auc"){aucmod<-pROC::auc(pROC::roc(tab[,1], as.vector(model$votes[,1]),quiet=T))}
    for(i in 1:length(lessimportantevar)){
      tabdiff2<-tab[,-lessimportantevar[i]]
      tabdiff2<-as.data.frame(tabdiff2[,c(colnames(tabdiff2)[1],sort(colnames(tabdiff2[,-1])))])
      tabdiff2<-as.data.frame(tabdiff2[sort(rownames(tabdiff2)),])
      
      set.seed(20011203)
      resmodeldiff <-randomForest(tabdiff2[,-1],tabdiff2[,1],ntree=1000,importance=T,keep.forest=T,trace=T)
      if(criterionmodel=="accuracy"){test[i]<-mean(resmodeldiff$confusion[,3])-mean(model$confusion[,3])}
      if(criterionmodel=="BER"){
        test[i]<-bermod-BER(class = tabdiff2[,1],classpredict = resmodeldiff$predicted)}
      if(criterionmodel=="auc"){
        test[i]<- pROC::auc(pROC::roc(tabdiff2[,1], as.vector(resmodeldiff$votes[,1]),quiet=T))-aucmod}
    }
  }
  #print(paste("test :",max(test)))
  if(max(test)>=0){num<-lessimportantevar[which(test==max(test))[1]]}
  else(num<-0)
  #print(paste( "num", num))
  return(num)
} 

importancemodelsvm<-function(model,modeltype,tabdiff,criterion){
  #function calculate the importance of each variable of the model
  #first column of tabdiff is the group
  importancevar<-vector()
  if(criterion=="accuracy"){
    if(modeltype=="svm"){
      for (i in 2:ncol(tabdiff)){
        vec<-vector()
        tabdiffmodif<-tabdiff
        for( j in 1:20){
          tabdiffmodif[,i]<-tabdiffmodif[sample(1:nrow(tabdiff)),i]
          #tabdiffmodif<-tabdiffmodif[,-i]
          
          resmodeldiff<-svm(y =tabdiffmodif[,1],x=tabdiffmodif[,-1],cross=10,
                            type ="C-classification",
                            kernel= ifelse(is.null(model$kernel),"radial",model$kernel),
                            cost=model$cost,
                            gamma=model$gamma)
          vec[j]<-abs(resmodeldiff$tot.accuracy-model$tot.accuracy)
        }
        importancevar[i]<-mean(vec)}
      
    }
    if(modeltype=="randomforest"){
      
      tabdiff<-as.data.frame(tabdiff[,c(colnames(tabdiff)[1],sort(colnames(tabdiff[,-1])))])
      tabdiff<-as.data.frame(tabdiff2[sort(rownames(tabdiff)),])
      
      set.seed(20011203)
      model <- randomForest(tabdiff[,-1],tabdiff[,1],ntree=1000,importance=T,keep.forest=T)
      importancevar<-model$importance[,4]
      importancevar<-c(NA,importancevar)
    }
  }
  if(criterion=="fscore"){
    importancevar<-Fscore(tab = as.data.frame(tabdiff[,-1]),class=tabdiff[,1])
  }
  return(importancevar)
}
# fonction qui calcule le fscore de chaque variable d'un tableau en fonction de la classe, 
# qui represente la distance entre les classes en fonction de la variable et la variance intra classe, 
#plus le fscore est eleve plus la variable est importante pour differencier les classes
Fscore<-function(tab,class){
  tabpos<-as.data.frame(tab[which(class==levels(class)[1]),])
  npos<-nrow(tabpos)
  tabneg<-as.data.frame(tab[which(class==levels(class)[2]),])
  nneg<-nrow(tabneg)
  fscore<-vector()
  for(i in 1:ncol(tab)){
    moypos<-mean(tabpos[,i])
    moyneg<-mean(tabneg[,i])
    moy<-mean(tab[,i])
    numerateur<-(moypos-moy)^2+(moyneg-moy)^2
    denominateur<-(sum((tabpos[,i]-moypos)^2)*(1/(npos-1)))+(sum((tabneg[,i]-moyneg)^2)*(1/(nneg-1)))
    fscore[i]<-numerateur/denominateur
  }
  return(c(NA,fscore))
}

BER<-function(class,classpredict){
  pos<-which(class==levels(class)[1])
  neg<-which(class==levels(class)[2])
  (1/2)*( sum(class[pos]!=classpredict[pos])/length(pos)+ sum(class[neg]!=classpredict[neg])/length(neg)  )
}

nll<-function(element){
  if(is.null(element)){return("")}
  else{return(element)}
}

sensibility<-function(predict,class){
  data<-table(predict,class)
  sensi<-round(data[1,1]/(data[1,1]+data[2,1]),digits = 3)
  return(sensi)
}

specificity<-function(predict,class){
  data<-table(predict,class )
  round(data[2,2]/(data[1,2]+data[2,2]),digit=3)
}

# cette fonction construit un tableau de parametres a tester a partir d'une liste de parametres
# chaque element de la liste est un vecteur de valeurs a tester pour le parametre correspondant
constructparameters<-function(listparameters){
  resparameters<-data.frame(listparameters[[1]])
  namescol<-names(listparameters)
  
  for(i in 2:length(listparameters)){
    tt<-rep(listparameters[[i]],each=nrow(resparameters))
    res<-resparameters
    if(length(listparameters[[i]])>1){
      for (j in 1:(length(listparameters[[i]])-1)){
        res<-rbind(res,resparameters)
      }
    }
    resparameters<-cbind(res,tt)
  }
  colnames(resparameters)<-namescol
  return(resparameters)
}

testparametersfunction<-function(learning,validation,tabparameters){
  set.seed(20011203)
  # results<-matrix(data = NA,nrow =nrow(tabparameters), ncol=9 )
  # colnames(results)<-c("auc validation","sensibility validation","specificityvalidation",
  #                      "auc learning","sensibility learning","specificity learning",
  #                      "number of features in model","number of differented features",
  #                      "number of features selected")
  
  results<-matrix(data = NA,nrow =nrow(tabparameters), ncol=10)
  colnames(results)<-c("auc validation","sensibility validation","specificity validation",
                       "auc learning","sensibility learning","specificity learning",
                       "threshold used","number of features in model",
                       "number of differented features","number of features selected")
  print(paste(nrow(tabparameters),"parameters "))
  for (i in 1:nrow(tabparameters)){
    print(i)
    parameters<-tabparameters[i,]
    if(!parameters$NAstructure){tabparameters[i,c("thresholdNAstructure","structdata","maxvaluesgroupmin","minvaluesgroupmax")]<-rep(x = NA,4)    }
    #selectdataparameterst<-parameters[1:7]
    selectdataparameters<<-list("prctvalues"=parameters$prctvalues,
                                "selectmethod"=parameters$selectmethod,
                                "NAstructure"=parameters$NAstructure,
                                "structdata"=parameters$structdata,
                                "thresholdNAstructure"=parameters$thresholdNAstructure,
                                "maxvaluesgroupmin"=parameters$maxvaluesgroupmin,"minvaluesgroupmax"=parameters$minvaluesgroupmax)
    resselectdata<<-selectdatafunction(learning = learning,selectdataparameters = selectdataparameters)
    
    #transformdataparameters<<-parameters[8:11]
    if(!parameters$log){tabparameters[i,"logtype"]<-NA}
    transformdataparameters<<-list("log"=parameters$log,"logtype"=parameters$logtype,"standardization"=parameters$standardization,"arcsin"=parameters$arcsin,"rempNA"=parameters$rempNA)
    
    learningtransform<-transformdatafunctionBinairy(learningselect = resselectdata$learningselect,structuredfeatures = resselectdata$structuredfeatures,
                                             datastructuresfeatures =   resselectdata$datastructuresfeatures,transformdataparameters = transformdataparameters)
    
    testparameters<<-list("SFtest"=FALSE,"test"=parameters$test,"adjustpval"=as.logical(parameters$adjustpv),"thresholdpv"=parameters$thresholdpv,"thresholdFC"=parameters$thresholdFC)
    restest<<-testfunction(tabtransform = learningtransform,testparameters = testparameters)
    
    if(parameters$test=="notest"){
      learningmodel<-learningtransform
      tabparameters[i,c("adjustpv","thresholdpv","thresholdFC")]<-rep(x = NA,3)
    }
    else{learningmodel<-restest$tabdiff}
    
    if(ncol(learningmodel)!=0){
      
      # Determine if automatic tuning should be used based on tuning_method parameter
      use_autotuning <- (!is.null(parameters$tuning_method) && parameters$tuning_method == "automatic")
      
      # Set autotuning flags for each model type
      autotunerf_flag <- use_autotuning
      autotunesvm_flag <- use_autotuning
      autotunexgb_flag <- use_autotuning
      autotunelgb_flag <- use_autotuning
      autotuneknn_flag <- use_autotuning
      
      modelparameters<<-list("modeltype"=parameters$model,
                             "invers"=FALSE,
                             "thresholdmodel"=parameters$thresholdmodel,
                             "fs"=as.logical(parameters$fs),
                             "adjustval"=!is.null(validation),
                             "autotunerf"=autotunerf_flag,
                             "autotunesvm"=autotunesvm_flag,
                             "autotunexgb"=autotunexgb_flag,
                             "autotunelgb"=autotunelgb_flag,
                             "autotuneknn"=autotuneknn_flag
      )
      validate(need(ncol(learning)!=0,"No select dataset"))
      
      
      #resmodel<<-modelfunction(learningmodel = learningmodel,validation = validation,modelparameters = modelparameters,
      #                         transformdataparameters = transformdataparameters,datastructuresfeatures =  datastructuresfeatures)
      out<- tryCatch(modelfunction(learningmodel = learningmodel,
                                   validation = validation,
                                   modelparameters = modelparameters,
                                   transformdataparameters = transformdataparameters,
                                   datastructuresfeatures =  datastructuresfeatures,
                                   learningselect = resselectdata$learningselect), 
                     error = function(e) e)
      if(any(class(out)=="error"))parameters$model<-"nomodel"
      else{
        
        resmodel<-out
        
        # Apply threshold optimization if requested
        if(!is.null(parameters$threshold_method) && parameters$threshold_method != "fixed" && parameters$model != "nomodel"){
          tryCatch({
            # Calculate optimal threshold from ROC curve on learning data
            classlearning <- resmodel$datalearningmodel$reslearningmodel$classlearning
            scorelearning <- resmodel$datalearningmodel$reslearningmodel$scorelearning
            
            # Create ROC object
            roc_obj <- pROC::roc(classlearning, scorelearning, quiet=TRUE)
            
            # Find optimal threshold based on selected method
            if(parameters$threshold_method == "youden"){
              # Youden method: maximizes sensitivity + specificity - 1
              optimal_coords <- coords(roc_obj, "best", best.method="youden", ret=c("threshold", "sensitivity", "specificity"))
              optimal_threshold <- optimal_coords$threshold
              
              # Display optimization results
              cat(sprintf("    ✓ Youden optimization (iter %d): threshold=%.4f (sens=%.3f, spec=%.3f, Youden=%.3f)\n",
                          i, optimal_threshold,
                          optimal_coords$sensitivity,
                          optimal_coords$specificity,
                          optimal_coords$sensitivity + optimal_coords$specificity - 1))
              
            } else if(parameters$threshold_method == "equiprob"){
              # Equiprobability method: closest point to diagonal (minimizes |FP-FN|)
              optimal_coords <- coords(roc_obj, "best", best.method="closest.topleft", ret=c("threshold", "sensitivity", "specificity"))
              optimal_threshold <- optimal_coords$threshold
              
              # Calculate false positive and false negative rates for display
              fp_rate <- 1 - optimal_coords$specificity
              fn_rate <- 1 - optimal_coords$sensitivity
              
              # Display optimization results
              cat(sprintf("    ✓ Equiprobability optimization (iter %d): threshold=%.4f (sens=%.3f, spec=%.3f, FPR=%.3f, FNR=%.3f)\n",
                          i, optimal_threshold,
                          optimal_coords$sensitivity,
                          optimal_coords$specificity,
                          fp_rate, fn_rate))
            }
            
            # Recalculate predicted classes using optimal threshold for learning data
            # IMPORTANT: In this application, levels(classlearning)[1] = "positif" (case)
            # Score represents probability of being positive, so high score → predict positive
            # Therefore: score >= threshold → levels[1] (positif), score < threshold → levels[2] (negatif)
            resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- ifelse(scorelearning >= optimal_threshold, levels(classlearning)[1], levels(classlearning)[2])
            resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- factor(resmodel$datalearningmodel$reslearningmodel$predictclasslearning, levels = levels(classlearning))
            
            # If validation data exists, apply optimal threshold to validation predictions as well
            # Same logic: high score → predict positive (level 1)
            if(!is.null(validation)){
              classval <- resmodel$datavalidationmodel$resvalidationmodel$classval
              scoreval <- resmodel$datavalidationmodel$resvalidationmodel$scoreval
              resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- ifelse(scoreval >= optimal_threshold, levels(classval)[1], levels(classval)[2])
              resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- factor(resmodel$datavalidationmodel$resvalidationmodel$predictclassval, levels = levels(classval))
            }
            
            # Update threshold in parameters for record
            parameters$thresholdmodel <- optimal_threshold
          }, error = function(e){
            # If threshold optimization fails, continue with original threshold
            cat(sprintf("    ✗ Threshold optimization FAILED (iteration %d): %s\n", i, e$message))
            cat(sprintf("      → Keeping initial threshold: %.4f\n", parameters$thresholdmodel))
            warning(paste("Threshold optimization failed:", e$message))
          })
        } else {
          # For "fixed" threshold method, use the threshold from parameters (already set to 0.5 for proba, 0 for SVM)
          # The classes are already predicted in modelfunction with this threshold
          # No need to recalculate, just ensure threshold is recorded
          if(parameters$model != "nomodel" && parameters$model != "svm"){
            # For probabilistic models, threshold should be 0.5 (already set)
            # For SVM, threshold is 0 (handled in modelfunction)
            # Just ensure the threshold is recorded correctly
            if(is.null(parameters$thresholdmodel) || is.na(parameters$thresholdmodel)){
              parameters$thresholdmodel <- 0.5
            }
          }
        }
        
        
        # # Apply Youden threshold optimization if requested
        # if(!is.null(parameters$optimize_threshold) && parameters$optimize_threshold && parameters$model != "nomodel"){
        #   tryCatch({
        #     # Calculate optimal threshold using Youden method from ROC curve on learning data
        #     classlearning <- resmodel$datalearningmodel$reslearningmodel$classlearning
        #     scorelearning <- resmodel$datalearningmodel$reslearningmodel$scorelearning
        #     
        #     # Create ROC object
        #     roc_obj <- roc(classlearning, scorelearning, quiet=TRUE)
        #     
        #     # Find optimal threshold using Youden method (maximizes sensitivity + specificity - 1)
        #     optimal_coords <- coords(roc_obj, "best", best.method="youden", ret=c("threshold", "sensitivity", "specificity"))
        #     optimal_threshold <- optimal_coords$threshold
        #     
        #     # Recalculate predicted classes using optimal threshold for learning data
        #     #resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- ifelse(scorelearning >= optimal_threshold, levels(classlearning)[2], levels(classlearning)[1])
        #     # IMPORTANT: In this application, levels(classlearning)[1] = "positif" (case)
        #     # Score represents probability of being positive, so high score → predict positive
        #     # Therefore: score >= threshold → levels[1] (positif), score < threshold → levels[2] (negatif)
        #     resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- ifelse(scorelearning >= optimal_threshold, levels(classlearning)[1], levels(classlearning)[2])
        #     resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- factor(resmodel$datalearningmodel$reslearningmodel$predictclasslearning, levels = levels(classlearning))
        #     
        #     # If validation data exists, apply optimal threshold to validation predictions as well
        #     if(!is.null(validation)){
        #       classval <- resmodel$datavalidationmodel$resvalidationmodel$classval
        #       scoreval <- resmodel$datavalidationmodel$resvalidationmodel$scoreval
        #       # resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- ifelse(scoreval >= optimal_threshold, levels(classval)[2], levels(classval)[1])
        #       resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- ifelse(scoreval >= optimal_threshold, levels(classval)[1], levels(classval)[2])
        #       resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- factor(resmodel$datavalidationmodel$resvalidationmodel$predictclassval, levels = levels(classval))
        #     }
        #     
        #     # Update threshold in parameters for record
        #     parameters$thresholdmodel <- optimal_threshold
        #   }, error = function(e){
        #     # If threshold optimization fails, continue with original threshold
        #     cat(sprintf("    ✗ Youden optimization FAILED (iteration %d): %s\n", i, e$message))
        #     cat(sprintf("      → Keeping initial threshold: %.4f\n", parameters$thresholdmodel))
        #     warning(paste("Threshold optimization failed:", e$message))
        #   })
        # }
        
      }
    }
    else{parameters$model<-"nomodel"}
    #numberfeaturesselected
    # results[i,9]<-positive(dim(resselectdata$learningselect)[2]-1)
    #numberfeaturesdiff
    #numberfeaturesselected (shifted from 9 to 10)
    results[i,10]<-positive(dim(resselectdata$learningselect)[2]-1)
    #numberfeaturesdiff (shifted from 8 to 9)
    if(parameters$test!="notest"){
      results[i,8]<-positive(dim(restest$tabdiff)[2]-1)
    }
    #numberfeaturesmodel
    if(parameters$model!="nomodel"){
      # results[i,7]<-dim(resmodel$datalearningmodel$learningmodel)[2]-1
      results[i,8]<-dim(resmodel$datalearningmodel$learningmodel)[2]-1
      #thresholdused (NEW: index 7)
      results[i,7]<-round(parameters$thresholdmodel, digits = 4)
      #auclearning
      results[i,4]<-round(as.numeric(pROC::auc(pROC::roc(resmodel$datalearningmodel$reslearningmodel$classlearning,resmodel$datalearningmodel$reslearningmodel$scorelearning,quiet=T))),digits = 3)
      #sensibilitylearning
      results[i,5]<-sensibility(resmodel$datalearningmodel$reslearningmodel$predictclasslearning,resmodel$datalearningmodel$reslearningmodel$classlearning)
      #specificitylearning
      results[i,6]<-specificity(resmodel$datalearningmodel$reslearningmodel$predictclasslearning,resmodel$datalearningmodel$reslearningmodel$classlearning)
      if(!is.null(validation)){
        #aucvalidation
        results[i,1]<-round(as.numeric(pROC::auc(pROC::roc(resmodel$datavalidationmodel$resvalidationmodel$classval,resmodel$datavalidationmodel$resvalidationmodel$scoreval,quiet=T))),digits = 3)
        #sensibilityvalidation
        results[i,2]<-sensibility(resmodel$datavalidationmodel$resvalidationmodel$predictclassval,resmodel$datavalidationmodel$resvalidationmodel$classval)
        #specificityvalidation
        results[i,3]<-specificity(resmodel$datavalidationmodel$resvalidationmodel$predictclassval,resmodel$datavalidationmodel$resvalidationmodel$classval)
      }
    }
  }
  return(cbind(results,tabparameters))
}

##
importanceplot<-function(model,learningmodel,modeltype,graph=T){
  shiny::validate(need(!is.null(model),"No model"))
  shiny::validate(need(ncol(learningmodel)>2,"only one feature"))
  if(modeltype=="randomforest"){
    var_importance<- data.frame(variables=rownames(model$importance),
                                importance=as.vector(model$importance[,4]))
    
    varo<-var_importance[order(var_importance$importance,decreasing = T),1]
    var_importance$variables<-as.character(var_importance$variables)
    var_importance$variables<-factor(x =var_importance$variables,levels =varo  )
    
    p <- ggplot(var_importance, aes(x=variables, weight=importance,fill=variables))
    g<-p + geom_bar()+coord_flip()+ylab("Variable Importance (Mean Decrease in Gini Index)")+
      theme(legend.position="none",plot.title=element_text( size=15))+ggtitle("Importance of variables in the model")+scale_fill_grey()
  }
  if(modeltype=="svm"){
    # fscore represente la distance entre les classes en fonction de la variable et la variance intra classe, 
    #plus le fscore est eleve plus la variable est importante pour differencier les classes
    importancevar<-importancemodelsvm(model = model,modeltype="svm",tabdiff=learningmodel,criterion = "fscore")
    
    var_importance<-as.data.frame(cbind(colnames(learningmodel),importancevar)[-1,])
    var_importance[,1]<-as.character(var_importance[,1])
    var_importance[,2]<-as.numeric(as.character(var_importance[,2]))
    colnames(var_importance)<-c("variables","importance")
    varo<-var_importance[order(var_importance$importance,decreasing = T),1]
    var_importance$variables<-as.character(var_importance$variables)
    var_importance$variables<-factor(x =var_importance$variables,levels =varo  )
    
    p <- ggplot(var_importance, aes(x=variables, weight=importance,fill=variables))
    g<-p + geom_bar()+
      coord_flip()+
      ylab("Variable Importance (fscore)") + 
      theme(legend.position="none",
            plot.title = element_text(size = 15, face = "bold"),
            axis.title = element_text(size = 15, face = "bold"),
            axis.text = element_text(size = 15, face = "bold"),
            legend.text = element_text(size = 10, face = "bold"),
            legend.title = element_text(size = 15, face = "bold"),
            axis.title.x = element_text(size = 15, face = "bold"),
            axis.title.y = element_text(size = 15, face = "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) + 
      ggtitle("Importance of variables in the model")+
      scale_fill_grey()
    

  }
  if(modeltype=="elasticnet"){
    # Extract coefficients from elasticnet model
    coef_matrix <- as.matrix(coef(model$glmnet_model, s=model$lambda))
    coef_values <- coef_matrix[-1, 1]  # Remove intercept
    names(coef_values) <- colnames(learningmodel)[-1]
    
    # Keep only non-zero coefficients
    nonzero_coefs <- coef_values[coef_values != 0]
    
    if(length(nonzero_coefs) > 0){
      var_importance <- data.frame(
        variables = names(nonzero_coefs),
        importance = abs(nonzero_coefs),
        stringsAsFactors = FALSE
      )
      
      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)
      
      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar()+
        coord_flip()+
        ylab("Variable Importance (Absolute Coefficient)")+
        theme(legend.position="none",plot.title=element_text( size=15))+
        ggtitle("Importance of variables in the model")+
        scale_fill_grey()
    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No variables with non-zero coefficients")
    }
  }
  if(modeltype=="xgboost"){
    # Extract feature importance from XGBoost model
    importance_matrix <- xgb.importance(model = model)
    
    if(nrow(importance_matrix) > 0){
      var_importance <- data.frame(
        variables = importance_matrix$Feature,
        importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      )
      
      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)
      
      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar()+coord_flip()+ylab("Variable Importance (Gain)")+
        theme(legend.position="none",plot.title=element_text( size=15))+
        ggtitle("Importance of variables in the model")+
        scale_fill_grey()
    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No feature importance available")
    }
  }
  if(modeltype=="lightgbm"){
    # Extract feature importance from LightGBM model
    importance_matrix <- lgb.importance(model = model)
    
    
    
    if(nrow(importance_matrix) > 0){
      var_importance <- data.frame(
        variables = importance_matrix$Feature,
        importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      )
      
      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)
      
      
      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar()+coord_flip()+ylab("Variable Importance (Gain)")+
        theme(legend.position="none",plot.title=element_text( size=15))+
        ggtitle("Importance of variables in the model")+scale_fill_grey()
      
    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No feature importance available")
      
    }
    
  }
  if(modeltype=="naivebayes"){
    # Naive Bayes doesn't have traditional feature importance
    # We can compute conditional probabilities per class
    var_importance <- data.frame()
    g <- errorplot(text = "Naive Bayes: Feature importance not available\nModel uses probabilistic independence assumptions")
    
  }
  if(modeltype=="knn"){
    # KNN doesn't have traditional feature importance
    # Could compute based on feature scaling but not meaningful
    var_importance <- data.frame()
    g <- errorplot(text = "KNN: Feature importance not available\nModel uses distance-based classification")
    
  }
  if(!graph){return(var_importance)}
  if(graph){
    g
  }
}


positive<-function(x){
  if(x<0){x<-0}
  else{x}
  return(x)
}


# Fonction pour créer une visualisation PCA 2D interactive avec plotly
PlotPca2D_interactive <- function(data, y, title = "PCA of selected variables") {
  # Effectuer la PCA
  pca_result <- prcomp(data, center = TRUE, scale. = TRUE)
  
  # Calculer la variance expliquée
  var_explained <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)
  
  # Créer le dataframe pour plotly
  pca_data <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    Group = as.factor(y),
    Sample = rownames(data)
  )
  
  # Créer le graphique interactif avec plotly
  plot_ly(pca_data, 
          x = ~PC1, 
          y = ~PC2, 
          color = ~Group,
          colors = c("#F8766D", "#00BFC4"),
          type = 'scatter',
          mode = 'markers',
          marker = list(size = 10, opacity = 0.7),
          text = ~paste("Sample:", Sample, "<br>Group:", Group),
          hoverinfo = 'text') %>%
    layout(
      title = list(text = title, font = list(size = 16, face = "bold")),
      xaxis = list(title = paste0("PC1 (", var_explained[1], "% variance)"),
                   titlefont = list(size = 14, face = "bold")),
      yaxis = list(title = paste0("PC2 (", var_explained[2], "% variance)"),
                   titlefont = list(size = 14, face = "bold")),
      legend = list(title = list(text = "Groups"))
    )
}


# Fonction pour créer une visualisation PCA 3D interactive avec plotly
PlotPca3D_interactive <- function(data, y, title = "PCA of selected variables") {
  # Effectuer la PCA
  pca_result <- prcomp(data, center = TRUE, scale. = TRUE)
  
  # Calculer la variance expliquée
  var_explained <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)
  
  # Vérifier qu'il y a au moins 3 composantes principales
  if(ncol(pca_result$x) < 3) {
    stop("Not enough main components for 3D visualisation")
  }
  
  # Créer le dataframe pour plotly
  pca_data <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    PC3 = pca_result$x[, 3],
    Group = as.factor(y),
    Sample = rownames(data)
  )
  
  # Créer le graphique 3D interactif avec plotly
  plot_ly(pca_data, 
          x = ~PC1, 
          y = ~PC2, 
          z = ~PC3,
          color = ~Group,
          colors = c("#F8766D", "#00BFC4"),
          type = 'scatter3d',
          mode = 'markers',
          marker = list(size = 6, opacity = 0.7),
          text = ~paste("Sample:", Sample, "<br>Group:", Group),
          hoverinfo = 'text') %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      scene = list(
        xaxis = list(title = paste0("PC1 (", var_explained[1], "%)")),
        yaxis = list(title = paste0("PC2 (", var_explained[2], "%)")),
        zaxis = list(title = paste0("PC3 (", var_explained[3], "%)"))
      ),
      legend = list(title = list(text = "Groups"))
    )
}


# Fonction combinée qui crée les deux visualisations (2D et 3D)
PlotPca_Combined <- function(data, y, title_prefix = "PCA") {
  list(
    pca_2d = PlotPca2D_interactive(data, y, paste(title_prefix, "- Vue 2D")),
    pca_3d = PlotPca3D_interactive(data, y, paste(title_prefix, "- Vue 3D"))
  )
}

####
# Function to apply a new threshold without retraining the model
# This separates threshold adjustment from hyperparameter tuning

# ── 2. apply_threshold ────────────────────────────────────────────────────────
# CHANGEMENTS vs version précédente :
#   - Ajout de levels(predictclasslearning) <- paste("test", lev, sep="")
#     pour cohérence avec le nommage historique de modelfunction
#   - Ajout de levels(predictclassval) <- paste("test", lev, sep="")
#   - Préservation de validationdiff et auc dans datavalidationmodel
#     (ces champs existaient dans modelfunction mais étaient perdus)
# ══════════════════════════════════════════════════════════════════════════════
apply_threshold <- function(model_result, new_threshold, groups = NULL) {
  
  # ── Groupes ──────────────────────────────────────────────────────────────────
  if (is.null(groups)) {
    groups <- model_result$groups   # lev avec names c("positif","negatif")
  }
  lev <- groups
  
  # ── Application du seuil sur les données d'apprentissage ─────────────────────
  res_lrn <- model_result$datalearningmodel$reslearningmodel
  
  # Diagnostic : vérifier que le data.frame source contient bien les colonnes attendues
  if (is.null(res_lrn) || !is.data.frame(res_lrn)) {
    stop("apply_threshold : 'reslearningmodel' est NULL ou n'est pas un data.frame. ",
         "Vérifiez que modelfunction retourne bien ce champ.")
  }
  
  cols_required <- c("classlearning", "scorelearning")
  cols_missing  <- setdiff(cols_required, colnames(res_lrn))
  if (length(cols_missing) > 0) {
    stop("apply_threshold : colonnes manquantes dans reslearningmodel : ",
         paste(cols_missing, collapse = ", "),
         ". Colonnes disponibles : ", paste(colnames(res_lrn), collapse = ", "))
  }
  
  scorelearning <- res_lrn$scorelearning
  classlearning <- res_lrn$classlearning
  
  if (is.data.frame(scorelearning)) scorelearning <- scorelearning[, 1]
  
  predictclasslearning <- factor(levels = lev)
  predictclasslearning[which(scorelearning >= new_threshold)] <- lev["positif"]
  predictclasslearning[which(scorelearning <  new_threshold)] <- lev["negatif"]
  predictclasslearning <- as.factor(predictclasslearning)
  
  # Renommage des niveaux (cohérence avec le reste de l'application)
  levels(predictclasslearning) <- paste("test ", lev, sep = "")
  
  reslearningmodel <- data.frame(
    classlearning        = classlearning,
    scorelearning        = scorelearning,
    predictclasslearning = predictclasslearning
  )
  
  datalearningmodel <- list(
    "learningmodel"    = model_result$datalearningmodel$learningmodel,
    "reslearningmodel" = reslearningmodel
  )
  
  # ── Application du seuil sur les données de validation (si présentes) ────────
  datavalidationmodel <- NULL
  
  if (!is.null(model_result$datavalidationmodel) &&
      length(model_result$datavalidationmodel) > 0) {
    
    scoreval  <- model_result$datavalidationmodel$resvalidationmodel$scoreval
    classval  <- model_result$datavalidationmodel$resvalidationmodel$classval
    
    if (is.data.frame(scoreval)) scoreval <- scoreval[, 1]
    
    predictclassval <- factor(levels = lev)
    predictclassval[which(scoreval >= new_threshold)] <- lev["positif"]
    predictclassval[which(scoreval <  new_threshold)] <- lev["negatif"]
    predictclassval <- as.factor(predictclassval)
    
    # Renommage des niveaux (cohérence avec le reste de l'application)
    levels(predictclassval) <- paste("test ", lev, sep = "")
    
    resvalidationmodel <- data.frame(classval, scoreval, predictclassval)
    colnames(resvalidationmodel) <- c("classval", "scoreval", "predictclassval")
    
    # Préservation de validationdiff, validationmodel et auc (calculé dans modelfunction)
    datavalidationmodel <- list(
      "validationdiff"     = model_result$datavalidationmodel$validationdiff,
      "validationmodel"    = model_result$datavalidationmodel$validationmodel,
      "resvalidationmodel" = resvalidationmodel,
      "auc"                = model_result$datavalidationmodel$auc
    )
  }
  
  # ── Mise à jour du seuil dans modelparameters ─────────────────────────────────
  modelparameters               <- model_result$modelparameters
  modelparameters$thresholdmodel <- new_threshold
  
  list(
    "datalearningmodel"   = datalearningmodel,
    "model"               = model_result$model,
    "datavalidationmodel" = datavalidationmodel,
    "groups"              = lev,
    "modelparameters"     = modelparameters
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# cv_model : Cross-validation k-fold sur le jeu d'apprentissage
#
# Paramètres :
#   learningmodel    : data.frame (col1 = facteur groupe, col2:n = features)
#   trained_model    : objet modèle retourné par MODEL_TRAIN()$model
#                      (contient les hyperparamètres optimaux déjà tunés)
#   modelparameters  : liste de paramètres (modeltype, fs, …)
#   threshold        : seuil de classification
#   k                : nombre de folds (défaut 5)
#
# Retour : data.frame avec colonnes Fold | AUC | Sensibilité | Spécificité
#          + lignes résumé Mean et SD
# ══════════════════════════════════════════════════════════════════════════════
cv_model <- function(learningmodel, trained_model, modelparameters, threshold = 0.5, k = 5) {
  
  set.seed(42)
  colnames(learningmodel)[1] <- "group"
  y   <- learningmodel[, 1]
  lev <- levels(y)
  names(lev) <- c("positif", "negatif")
  n   <- nrow(learningmodel)
  
  # ── Création des folds (stratifiés par classe) ──────────────────────────────
  idx_pos <- which(y == lev["positif"])
  idx_neg <- which(y == lev["negatif"])
  
  folds_pos <- split(sample(idx_pos), cut(seq_along(idx_pos), k, labels = FALSE))
  folds_neg <- split(sample(idx_neg), cut(seq_along(idx_neg), k, labels = FALSE))
  folds     <- lapply(seq_len(k), function(i) c(folds_pos[[i]], folds_neg[[i]]))
  
  # ── Extraction des hyperparamètres déjà tunés ───────────────────────────────
  mt <- modelparameters$modeltype
  
  # randomforest
  rf_mtry    <- if (!is.null(trained_model$optimal_mtry))  trained_model$optimal_mtry  else floor(sqrt(ncol(learningmodel) - 1))
  rf_ntree   <- if (!is.null(trained_model$ntree_used))    trained_model$ntree_used    else 500
  
  # svm
  svm_cost   <- if (!is.null(trained_model$cost))   trained_model$cost   else 1
  svm_gamma  <- if (!is.null(trained_model$gamma))  trained_model$gamma  else 0.1
  svm_kernel <- if (!is.null(modelparameters$kernel)) modelparameters$kernel else "radial"
  
  cat("type of svm  kernel  : ", svm_kernel, "\n")
  
  # elasticnet
  en_alpha   <- if (!is.null(trained_model$alpha))           trained_model$alpha           else
    if (!is.null(modelparameters$alpha))          modelparameters$alpha          else 0.5
  en_lambda  <- if (!is.null(trained_model$lambda))          trained_model$lambda          else
    if (!is.null(trained_model$optimal_lambda))   trained_model$optimal_lambda  else NULL
  
  # xgboost
  xgb_nrounds <- if (!is.null(trained_model$optimal_nrounds))           trained_model$optimal_nrounds           else 100
  xgb_depth   <- if (!is.null(trained_model$optimal_max_depth))         trained_model$optimal_max_depth         else 6
  xgb_eta     <- if (!is.null(trained_model$optimal_eta))               trained_model$optimal_eta               else 0.1
  xgb_gamma   <- if (!is.null(trained_model$optimal_gamma))             trained_model$optimal_gamma             else 0
  xgb_sub     <- if (!is.null(trained_model$optimal_subsample))         trained_model$optimal_subsample         else 0.8
  xgb_child   <- if (!is.null(trained_model$optimal_min_child_weight))  trained_model$optimal_min_child_weight  else 1
  
  # lightgbm
  lgb_nrounds <- if (!is.null(trained_model$optimal_nrounds))           trained_model$optimal_nrounds           else 100
  lgb_leaves  <- if (!is.null(trained_model$optimal_num_leaves))        trained_model$optimal_num_leaves        else 31
  lgb_lr      <- if (!is.null(trained_model$optimal_learning_rate))     trained_model$optimal_learning_rate     else 0.1
  
  # knn
  knn_k <- if (!is.null(trained_model$optimal_k)) trained_model$optimal_k else 5
  
  # ── Boucle sur les folds ────────────────────────────────────────────────────
  results <- lapply(seq_len(k), function(i) {
    
    test_idx  <- folds[[i]]
    train_idx <- setdiff(seq_len(n), test_idx)
    
    train_data <- learningmodel[train_idx, ]
    test_data  <- learningmodel[test_idx,  ]
    
    y_train <- train_data[, 1]
    y_test  <- test_data[,  1]
    X_train <- train_data[, -1, drop = FALSE]
    X_test  <- test_data[,  -1, drop = FALSE]
    
    score <- tryCatch({
      
      if (mt == "randomforest") {
        mod <- randomForest(x = X_train, y = y_train,
                            ntree = rf_ntree, mtry = rf_mtry,
                            nodesize = 1, importance = FALSE)
       randomForest:::predict.randomForest(mod, X_test, type = "prob")[, lev["positif"]]
        
      } else if (mt == "svm") {
        mod <- svm(x = X_train, y = y_train,
                   # probability = TRUE,
                   cost = svm_cost, gamma = svm_gamma, kernel = svm_kernel)
        attr(e1071:::predict.svm(mod, X_test, decision.values = TRUE), "decision.values")
        
      } else if (mt == "elasticnet") {
        x_mat  <- as.matrix(X_train)
        x_test_mat <- as.matrix(X_test)
        y_bin  <- ifelse(y_train == lev["positif"], 1, 0)
        if (is.null(en_lambda)) {
          inner_folds <- create_stratified_folds(y_train, k = min(3, length(y_train) - 1))
          inner_foldid <- folds_to_foldid(inner_folds, length(y_train))
          cv_fit <- cv.glmnet(x_mat, y_bin, alpha = en_alpha, family = "binomial", foldid = inner_foldid)
          lam <- cv_fit$lambda.min
        } else { lam <- en_lambda }
        mod <- glmnet(x_mat, y_bin, alpha = en_alpha, lambda = lam, family = "binomial")
        as.numeric(glmnet:::predict.glmnet(mod, x_test_mat, s = lam, type = "response"))
        
      } else if (mt == "xgboost") {
        y_bin  <- ifelse(y_train == lev["positif"], 1, 0)
        dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_bin)
        dtest  <- xgb.DMatrix(data = as.matrix(X_test))
        params <- list(objective = "binary:logistic", eval_metric = "auc",
                       max_depth = xgb_depth, eta = xgb_eta,
                       gamma = xgb_gamma, subsample = xgb_sub,
                       min_child_weight = xgb_child)
        mod <- xgb.train(params = params, data = dtrain,
                         nrounds = xgb_nrounds, verbose = 0)
        as.numeric(xgboost:::predict.xgb.Booster(mod, dtest))
        
      } else if (mt == "lightgbm") {
        y_bin  <- ifelse(y_train == lev["positif"], 1, 0)
        dtrain <- lgb.Dataset(data = as.matrix(X_train), label = y_bin)
        params <- list(objective = "binary", metric = "auc",
                       num_leaves = lgb_leaves, learning_rate = lgb_lr,
                       verbose = -1)
        mod <- lgb.train(params = params, data = dtrain,
                         nrounds = lgb_nrounds, verbose = -1)
        as.numeric(predict(mod, as.matrix(X_test)))
        
      } else if (mt == "naivebayes") {
        mod <- naiveBayes(x = X_train, y = y_train)
        e1071:::predict.naiveBayes(mod, X_test, type = "raw")[, lev["positif"]]
        
      } else if (mt == "knn") {
        # kNN : score = proportion des voisins dans la classe positive
        k_use <- min(knn_k, nrow(X_train) - 1)
        scores_fold <- numeric(nrow(X_test))
        for (j in seq_len(nrow(X_test))) {
          d   <- apply(X_train, 1, function(r) sqrt(sum((as.numeric(X_test[j, ]) - as.numeric(r))^2)))
          nn  <- order(d)[seq_len(k_use)]
          scores_fold[j] <- sum(y_train[nn] == lev["positif"]) / k_use
        }
        scores_fold
        
      } else if (mt == "logistic") {
        df_train <- cbind(group = y_train, X_train)
        df_test  <- X_test
        mod <- glm(group ~ ., data = df_train, family = binomial())
        stats::predict.glm(mod, newdata = df_test, type = "response")
        
      } else { rep(NA_real_, nrow(X_test)) }
      
    }, error = function(e) {
      warning(sprintf("CV fold %d failed (%s): %s", i, mt, e$message))
      rep(NA_real_, nrow(X_test))
    })
    
    # ── Métriques du fold ──────────────────────────────────────────────────────
    auc_val <- tryCatch(
      as.numeric(pROC::auc(pROC::roc(as.vector(y_test), as.vector(score), quiet = TRUE))),
      error = function(e) NA_real_
    )
    
    cat(" threshold used  :  ", threshold, "\n")
    # pred_class <- factor(ifelse(score >= threshold, lev["positif"], lev["negatif"]),
    #                      levels = lev)
    # pred_class <- factor(paste("test", pred_class), levels = paste("test", lev))
    y_test_lbl <- y_test
    
    pred_class <- factor(levels = lev)
    pred_class[which(score >= threshold)] <- lev["positif"]
    pred_class[which(score <  threshold)] <- lev["negatif"]
    pred_class <- as.factor(pred_class)
    
    sensibility_cv<-function(predict,class){
      sensibility(predict = predict, class = class)
    }
    
    specificity_cv  <-function(predict,class){
      specificity(predict = predict, class = class)
    }
    
    # sen <- tryCatch({
    #   tbl <- table(Predicted = pred_class, Actual = y_test_lbl)
    #   round(tbl[1, 1] / (tbl[1, 1] + tbl[2, 1]), 3)
    # }, error = function(e) NA_real_)
    
    sen = sensibility_cv(pred_class, y_test_lbl)
    spe = specificity_cv(pred_class, y_test_lbl)
    
    # spe <- tryCatch({
    #   tbl <- table(Predicted = pred_class, Actual = y_test_lbl)
    #   round(tbl[2, 2] / (tbl[1, 2] + tbl[2, 2]), 3)
    # }, error = function(e) NA_real_)
    
    data.frame(Fold = paste0("Fold ", i),
               AUC          = round(auc_val, 3),
               Sensibilité  = round(sen, 3),
               Spécificité  = round(spe, 3),
               stringsAsFactors = FALSE)
  })
  
  res_df <- do.call(rbind, results)
  
  # ── Lignes résumé ────────────────────────────────────────────────────────────
  mean_row <- data.frame(
    Fold        = "Moyenne",
    AUC         = round(mean(res_df$AUC,         na.rm = TRUE), 3),
    Sensibilité = round(mean(res_df$Sensibilité, na.rm = TRUE), 3),
    Spécificité = round(mean(res_df$Spécificité, na.rm = TRUE), 3),
    stringsAsFactors = FALSE
  )
  sd_row <- data.frame(
    Fold        = "Écart-type",
    AUC         = round(sd(res_df$AUC,         na.rm = TRUE), 3),
    Sensibilité = round(sd(res_df$Sensibilité, na.rm = TRUE), 3),
    Spécificité = round(sd(res_df$Spécificité, na.rm = TRUE), 3),
    stringsAsFactors = FALSE
  )
  
  rbind(res_df, mean_row, sd_row)
}


# ==========================================================================
# t-SNE AND UMAP VISUALIZATION
# ==========================================================================
plot_tsne <- function(data, y, perplexity = 30, title = "t-SNE Visualization") {
  set.seed(20011203)
  X <- as.matrix(data)
  
  perplexity <- min(perplexity, floor((nrow(X) - 1) / 3))
  if(perplexity < 1) perplexity <- 1
  
  tsne_result <- Rtsne(X, dims = 2, perplexity = perplexity, 
                       verbose = FALSE, max_iter = 1000, check_duplicates = FALSE)
  
  tsne_data <- data.frame(
    Dim1 = tsne_result$Y[, 1],
    Dim2 = tsne_result$Y[, 2],
    Group = as.factor(y),
    Sample = rownames(data)
  )
  
  plot_ly(tsne_data, x = ~Dim1, y = ~Dim2, color = ~Group,
          colors = c("#F8766D", "#00BFC4"),
          type = 'scatter', mode = 'markers',
          marker = list(size = 10, opacity = 0.7),
          text = ~paste("Sample:", Sample, "<br>Group:", Group),
          hoverinfo = 'text') %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      xaxis = list(title = "t-SNE 1"),
      yaxis = list(title = "t-SNE 2"),
      legend = list(title = list(text = "Group"))
    )
}

plot_umap <- function(data, y, n_neighbors = 15, title = "UMAP Visualization") {
  set.seed(20011203)
  X <- as.matrix(data)
  
  n_neighbors <- min(n_neighbors, nrow(X) - 1)
  if(n_neighbors < 2) n_neighbors <- 2
  
  umap_config <- umap.defaults
  umap_config$n_neighbors <- n_neighbors
  umap_config$random_state <- 20011203
  
  umap_result <- umap(X, config = umap_config)
  
  umap_data <- data.frame(
    Dim1 = umap_result$layout[, 1],
    Dim2 = umap_result$layout[, 2],
    Group = as.factor(y),
    Sample = rownames(data)
  )
  
  plot_ly(umap_data, x = ~Dim1, y = ~Dim2, color = ~Group,
          colors = c("#F8766D", "#00BFC4"),
          type = 'scatter', mode = 'markers',
          marker = list(size = 10, opacity = 0.7),
          text = ~paste("Sample:", Sample, "<br>Group:", Group),
          hoverinfo = 'text') %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      xaxis = list(title = "UMAP 1"),
      yaxis = list(title = "UMAP 2"),
      legend = list(title = list(text = "Group"))
    )
}

# ==========================================================================
# CLUSTERED HEATMAP OF TOP FEATURES
# ==========================================================================
library(grid)
plot_clustered_heatmap <- function(data, y, n_top = 30, title = "Clustered Heatmap - Top Features") {
  X <- as.matrix(data)
  grid.newpage()
  
  if(ncol(X) > n_top) {
    var_importance <- apply(X, 2, function(col) {
      tryCatch({
        kruskal.test(col ~ as.factor(y))$p.value
      }, error = function(e) 1)
    })
    top_idx <- order(var_importance)[1:n_top]
    X <- X[, top_idx]
  }
  
  X_scaled <- scale(X)
  
  annotation_row <- data.frame(Group = as.factor(y))
  rownames(annotation_row) <- rownames(data)[1:nrow(X)]
  
  n_groups <- length(unique(y))
  group_colors <- setNames(
    # c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")[1:n_groups],
    c("#F8766D", "#00BFC4"),
    levels(as.factor(y))
  )
  ann_colors <- list(Group = group_colors)
  
  tryCatch({
    p =  pheatmap(t(X_scaled),
             annotation_col = annotation_row,
             annotation_colors = ann_colors,
             clustering_method = "ward.D2",
             show_colnames = FALSE,
             main = title,
             fontsize_row = 10,
             width = 12,
             height = 8,
             fontweight_row = "bold",
             color = colorRampPalette(c("#2166AC", "white", "#B2182B"))(100))
    
    gt <- p$gtable
    
    library(gridExtra)
    for (i in seq_along(gt$grobs)) {
      if ("row_names" %in% gt$grobs[[i]]$name) {
        gt$grobs[[i]]$gp <- gpar(fontface = "bold", fontsize = 10)
      }
    }
    
    # print(grid.draw(gt))
    # print(gt)
    print(p)
    
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Heatmap error:", e$message), cex = 1.2, col = "red")
  })
}

# ==========================================================================
# CORRELATION NETWORK
# ==========================================================================
# plot_correlation_network <- function(data, cor_threshold = 0.6, title = "Correlation Network") {
#   X <- data
#   if(ncol(X) < 2) {
#     return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Need at least 2 variables", size = 6) + theme_void())
#   }
#   
#   cor_matrix <- cor(X, use = "pairwise.complete.obs")
#   diag(cor_matrix) <- 0
#   
#   cor_matrix[abs(cor_matrix) < cor_threshold] <- 0
#   
#   if(sum(cor_matrix != 0) == 0) {
#     return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
#                                label = paste("No correlations above threshold", cor_threshold), size = 5) + theme_void())
#   }
#   
#   graph <- graph_from_adjacency_matrix(abs(cor_matrix), 
#                                        mode = "undirected", 
#                                        weighted = TRUE, 
#                                        diag = FALSE)
#   graph <- delete_vertices(graph, which(degree(graph) == 0))
#   
#   if(vcount(graph) == 0) {
#     return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
#                                label = "No connected variables", size = 5) + theme_void())
#   }
#   
#   tg <- as_tbl_graph(graph)
#   
#   p <- ggraph(tg, layout = "fr") +
#     geom_edge_link(aes(width = weight, alpha = weight), color = "steelblue") +
#     geom_node_point(size = 5, color = "#E74C3C") +
#     geom_node_text(aes(label = name), repel = TRUE, size = 3, fontface = "bold") +
#     scale_edge_width(range = c(0.5, 3)) +
#     scale_edge_alpha(range = c(0.3, 1)) +
#     labs(title = title, subtitle = paste("Correlation threshold:", cor_threshold)) +
#     theme_void() +
#     theme(
#       plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#       plot.subtitle = element_text(size = 11, hjust = 0.5),
#       legend.position = "none"
#     )
#   
#   return(p)
# }


plot_correlation_network <- function(data, cor_threshold = 0.6, title = "Correlation Network") {
  X <- data
  if(ncol(X) < 2) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Need at least 2 variables", size = 6) + theme_void())
  }
  
  cor_matrix <- cor(X, use = "pairwise.complete.obs")
  diag(cor_matrix) <- 0
  cor_matrix[abs(cor_matrix) < cor_threshold] <- 0
  
  if(sum(cor_matrix != 0) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                               label = paste("No correlations above threshold", cor_threshold), size = 5) + theme_void())
  }
  
  graph <- igraph::graph_from_adjacency_matrix(
    abs(cor_matrix), mode = "undirected", weighted = TRUE, diag = FALSE
  )
  
  # Qualification explicite pour éviter le conflit de namespace
  isolated <- which(igraph::degree(graph) == 0)
  if(length(isolated) > 0) {
    graph <- igraph::delete_vertices(graph, isolated)
  }
  
  if(igraph::vcount(graph) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                               label = "No connected variables", size = 5) + theme_void())
  }
  
  tg <- tidygraph::as_tbl_graph(graph)
  
  p <- ggraph(tg, layout = "fr") +
    geom_edge_link(aes(width = weight, alpha = weight), color = "steelblue") +
    geom_node_point(size = 5, color = "#E74C3C") +
    geom_node_text(aes(label = name), repel = TRUE, size = 5, fontface = "bold") +
    scale_edge_width(range = c(0.5, 3)) +
    scale_edge_alpha(range = c(0.3, 1)) +
    labs(title = title, subtitle = paste("Correlation threshold:", cor_threshold)) +
    theme_void() +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "none"
    )
  
  return(p)
}
# ==========================================================================
# CALIBRATION PLOT
# ==========================================================================

plot_calibration <- function(actual, predicted_probs, n_bins = 10, title = "Calibration Plot") {
  if(is.matrix(predicted_probs) || is.data.frame(predicted_probs)) {
    n_classes <- ncol(predicted_probs)
    class_names <- if(!is.null(colnames(predicted_probs))) colnames(predicted_probs) else paste("Class", 1:n_classes)
    
    all_data <- data.frame()
    for(i in 1:n_classes) {
      binary_actual <- as.numeric(actual == levels(actual)[i])
      probs <- predicted_probs[, i]
      
      bins <- cut(probs, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
      bin_data <- data.frame(probs = probs, actual = binary_actual, bin = bins)
      
      bin_summary <- bin_data %>%
        group_by(bin) %>%
        summarise(
          mean_predicted = mean(probs, na.rm = TRUE),
          mean_actual = mean(actual, na.rm = TRUE),
          count = n(),
          .groups = 'drop'
        ) %>%
        mutate(Class = class_names[i])
      
      all_data <- rbind(all_data, bin_summary)
    }
    
    p <- ggplot(all_data, aes(x = mean_predicted, y = mean_actual, color = Class)) +
      geom_point(aes(size = count), alpha = 0.7) +
      geom_line() +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      scale_size_continuous(range = c(2, 8)) +
      labs(title = title, x = "Mean Predicted Probability", y = "Observed Proportion",
           size = "N samples") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10, face = "bold")
      ) +
      coord_fixed()
    
  } else {
    binary_actual <- as.numeric(actual == levels(actual)[1])
    probs <- predicted_probs
    bins <- cut(probs, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
    bin_data <- data.frame(probs = probs, actual = binary_actual, bin = bins)
    
    bin_summary <- bin_data %>%
      group_by(bin) %>%
      summarise(
        mean_predicted = mean(probs, na.rm = TRUE),
        mean_actual = mean(actual, na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      )
    
    p <- ggplot(bin_summary, aes(x = mean_predicted, y = mean_actual)) +
      geom_point(aes(size = count), color = "#3498DB", alpha = 0.7) +
      geom_line(color = "#3498DB") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      scale_size_continuous(range = c(2, 8)) +
      labs(title = title, x = "Mean Predicted Probability", y = "Observed Proportion",
           size = "N samples") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      coord_fixed()
  }
  
  return(p)
}

# -----------------------------
# BINAIRE
# -----------------------------
plot_calibration_binary <- function(actual, predicted_probs,
                             n_bins = 10,
                             title = "Calibration Plot") {
  
  binary_actual <- as.numeric(actual == levels(actual)[1])
  
  probs <- predicted_probs
  
  bins <- cut(
    probs,
    breaks = seq(0, 1, length.out = n_bins + 1),
    include.lowest = TRUE
  )
  
  bin_data <- data.frame(
    probs = probs,
    actual = binary_actual,
    bin = bins
  )
  
  bin_summary <- bin_data %>%
    group_by(bin) %>%
    summarise(
      mean_predicted = mean(probs, na.rm = TRUE),
      mean_actual = mean(actual, na.rm = TRUE),
      count = n(),
      .groups = "drop"
    )
  
  p <- ggplot(
    bin_summary,
    aes(x = mean_predicted, y = mean_actual)
  ) +
    geom_point(
      aes(size = count),
      color = "#3498DB",
      alpha = 0.7
    ) +
    geom_line(color = "#3498DB") +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "gray50"
    ) +
    scale_size_continuous(range = c(2, 8)) +
    labs(
      title = title,
      x = "Mean Predicted Probability",
      y = "Observed Proportion",
      size = "N samples"
    ) +
    theme_minimal() +
    coord_fixed() +
    xlim(0, 1) +
    ylim(0, 1)
  
  
  return(p)
}

# ==========================================================================
# MODEL COMPARISON: RUN ALL MODELS
# --------------------------------------------------------------------------
# Entraîne chaque modèle sélectionné via modelfunction_V2 (CV stratifiée)
# puis applique apply_threshold pour obtenir les classes prédites.
# Les métriques sont calculées avec les fonctions existantes de l'application
# (sensibility, specificity, pROC::roc) pour le cas binaire, et par
# macro-moyenne one-vs-rest pour le cas multi-classes.
# ==========================================================================
run_all_models <- function(learningmodel, validation, transformdataparameters,
                           datastructuresfeatures, learningselect,
                           models_to_run = NULL, threshold = 0.5) {
  
  if (is.null(models_to_run)) {
    models_to_run <- c("randomforest", "svm", "elasticnet",
                       "xgboost", "naivebayes", "knn")
  }
  
  results_list    <- list()
  metrics_summary <- data.frame()
  n_classes       <- length(levels(as.factor(learningmodel[, 1])))
  
  for (model_type in models_to_run) {
    cat(sprintf("\n=== Training: %s ===\n", model_type))
    
    # ── Réinitialisation de la graine avant chaque modèle ──────────────────
    set.seed(20011203)
    
    # ── Paramètres par défaut alignés sur l'interface (ui.R) ───────────────
    
    if (model_type == "randomforest") {
      modelparameters <- list(
        modeltype         = "randomforest",
        invers            = FALSE,
        thresholdmodel    = threshold,
        fs                = FALSE,
        adjustval         = !is.null(validation),
        use_gridsearch    = FALSE,
        autotunerf        = TRUE,          
        ntree             = 1000,          
        rf_ntree_range    = c(100, 500, 1000),
        rf_nodesize_range = c(1, 5, 10),
        sampsize_frac     = 1.0,
        replace           = TRUE,
        # paramètres manuels non utilisés (autotunerf = TRUE)
        mtry              = NULL,
        nodesize          = NULL,
        maxnodes          = NULL
      )
      
    } else if (model_type == "svm") {
      modelparameters <- list(
        modeltype      = "svm",
        invers         = FALSE,
        thresholdmodel = threshold,
        fs             = FALSE,
        adjustval      = !is.null(validation),
        autotunesvm    = TRUE,
        svm_scoring    = "accuracy",           
        svm_gamma_range = 10^(-5:2),       
        svm_cost_range  = 10^(-3:2),       
        # paramètres manuels non utilisés (autotunesvm = TRUE)
        cost           = NULL,
        gamma          = NULL,
        kernel         = "radial"
      )
      
    } else if (model_type == "elasticnet") {
      
      modelparameters <- list(
        modeltype      = "elasticnet",
        invers         = FALSE,
        thresholdmodel = threshold,
        fs             = FALSE,
        adjustval      = !is.null(validation),
        use_gridsearch = FALSE,            
        alpha          = 0.5,             
        lambda         = NULL             
      )
      
    } else if (model_type == "xgboost") {
      modelparameters <- list(
        modeltype      = "xgboost",
        invers         = FALSE,
        thresholdmodel = threshold,
        fs             = FALSE,
        adjustval      = !is.null(validation),
        use_gridsearch = FALSE,
        autotunexgb    = TRUE,            
        gamma_xgb      = 0,
        subsample_xgb  = 1.0,
        lambda_xgb     = 0,
        alpha_xgb      = 0,
        nrounds        = NULL,
        max_depth      = NULL,
        eta            = NULL
      )
      
    } else if (model_type == "lightgbm") {
      modelparameters <- list(
        modeltype      = "lightgbm",
        invers         = FALSE,
        thresholdmodel = threshold,
        fs             = FALSE,
        adjustval      = !is.null(validation),
        autotunelgb    = TRUE,
        nrounds_lgb    = NULL,
        num_leaves     = NULL,
        learning_rate_lgb = NULL
      )
      
    } else if (model_type == "naivebayes") {
      modelparameters <- list(
        modeltype      = "naivebayes",
        invers         = FALSE,
        thresholdmodel = threshold,
        fs             = FALSE,
        adjustval      = !is.null(validation),
        use_gridsearch = TRUE,            
        nb_grid_laplace = c(0, 0.5, 1, 2, 5)
      )
      
    } else if (model_type == "knn") {
      modelparameters <- list(
        modeltype      = "knn",
        invers         = FALSE,
        thresholdmodel = threshold,
        fs             = FALSE,
        adjustval      = !is.null(validation),
        use_gridsearch = FALSE,
        autotuneknn    = TRUE,            
        k_neighbors    = NULL
      )
      
    } else {
      cat(sprintf("  ✗ Unknown model type: %s — skipping\n", model_type))
      next
    }
    
    # ── 1. Entraînement ─────────────────────────────────────────────────────
    raw <- tryCatch(
      modelfunction_V2(
        learningmodel           = learningmodel,
        validation              = validation,
        modelparameters         = modelparameters,
        transformdataparameters = transformdataparameters,
        datastructuresfeatures  = datastructuresfeatures,
        learningselect          = learningselect
      ),
      error = function(e) {
        cat(sprintf("  ✗ Error training %s: %s\n", model_type, e$message))
        return(NULL)
      }
    )
    if (is.null(raw)) next
    
    # ── 2. Application du seuil ─────────────────────────────────────────────
    out <- tryCatch(
      apply_threshold(raw, new_threshold = threshold),
      error = function(e) {
        cat(sprintf("  ✗ apply_threshold error for %s: %s\n",
                    model_type, e$message))
        return(NULL)
      }
    )
    if (is.null(out)) next
    
    results_list[[model_type]] <- out
    
    # ── 3. Métriques Training ────────────────────────────────────────────────
    res_lrn  <- out$datalearningmodel$reslearningmodel
    row_data <- extract_metrics(
      res_lrn$predictclasslearning,
      res_lrn$classlearning,
      res_lrn$scorelearning,
      n_classes, prefix = "Train"
    )
    row_data$Model <- model_type
    
    # ── 4. Métriques Validation ─────────────────────────────────────────────
    if (!is.null(validation) &&
        length(out$datavalidationmodel) > 0 &&
        !is.null(out$datavalidationmodel$resvalidationmodel)) {
      res_val  <- out$datavalidationmodel$resvalidationmodel
      val_data <- extract_metrics(
        res_val$predictclassval,
        res_val$classval,
        res_val$scoreval,
        n_classes, prefix = "Val"
      )
      row_data <- cbind(row_data, val_data)
    }
    
    metrics_summary <- rbind(metrics_summary, row_data)
  }
  
  if (nrow(metrics_summary) > 0) {
    metrics_summary <- metrics_summary[,
                                       c("Model", setdiff(names(metrics_summary), "Model"))]
  }
  
  return(list(
    results = results_list,
    metrics = metrics_summary
  ))
}
# run_all_models <- function(learningmodel, validation, transformdataparameters,
#                            datastructuresfeatures, learningselect,
#                            models_to_run = NULL, threshold = 0.5) {
#   
#   if(is.null(models_to_run)) {
#     models_to_run <- c("randomforest", "svm", "elasticnet", "xgboost", "naivebayes", "knn")
#   }
#   
#   results_list    <- list()
#   metrics_summary <- data.frame()
#   n_classes       <- length(levels(as.factor(learningmodel[, 1])))
#   
#   for(model_type in models_to_run) {
#     cat(sprintf("\n=== Training: %s ===\n", model_type))
#     
#     modelparameters <- list(
#       modeltype      = model_type,
#       invers         = FALSE,
#       thresholdmodel = threshold,
#       fs             = FALSE,
#       adjustval      = !is.null(validation),
#       
#       # ── Activation du GridSearchCV pour tous les modèles qui le supportent ──
#       use_gridsearch = TRUE,
#       
#       # ── Random Forest ────────────────────────────────────────────────────────
#       autotunerf       = TRUE,
#       rf_grid_ntree    = c(100, 300, 500, 1000),
#       rf_grid_mtry     = c("sqrt", "log2"),
#       rf_grid_nodesize = c(1, 3, 5),
#       ntree            = 500,
#       
#       # ── SVM ──────────────────────────────────────────────────────────────────
#       autotunesvm     = TRUE,
#       svm_scoring     = "accuracy",
#       svm_gamma_range = 10^seq(-6, 2, by = 1),
#       svm_cost_range  = 10^seq(-3, 3, by = 1),
#       
#       # ── XGBoost ──────────────────────────────────────────────────────────────
#       autotunexgb          = TRUE,
#       # use_gridsearch       = FALSE,
#       xgb_grid_nrounds     = c(50, 100, 200, 300),
#       xgb_grid_maxdepth    = c(3, 6, 9),
#       xgb_grid_eta         = c(0.01, 0.05, 0.1, 0.3),
#       xgb_grid_gamma       = c(0, 0.1, 0.5),
#       xgb_grid_subsample   = c(0.8, 1.0),
#       
#       # ── LightGBM ─────────────────────────────────────────────────────────────
#       autotunelgb = TRUE,
#       
#       # ── KNN ──────────────────────────────────────────────────────────────────
#       autotuneknn = TRUE,
#       k_neighbors = 5,
#       
#       # ── Naive Bayes ──────────────────────────────────────────────────────────
#       nb_grid_laplace = c(0, 0.5, 1, 2, 5)
#     )
#     
#     # ── 1. Entraînement (modelfunction_V2 = version avec CV stratifiée) ──────
#     raw <- tryCatch(
#       modelfunction_V2(learningmodel       = learningmodel,
#                        validation           = validation,
#                        modelparameters      = modelparameters,
#                        transformdataparameters = transformdataparameters,
#                        datastructuresfeatures = datastructuresfeatures,
#                        learningselect       = learningselect),
#       error = function(e) {
#         cat(sprintf("  ✗ Error training %s: %s\n", model_type, e$message))
#         return(NULL)
#       }
#     )
#     if(is.null(raw)) next
#     
#     # ── 2. Application du seuil → obtient predictclasslearning / predictclassval
#     out <- tryCatch(
#       apply_threshold(raw, new_threshold = threshold),
#       error = function(e) {
#         cat(sprintf("  ✗ apply_threshold error for %s: %s\n", model_type, e$message))
#         return(NULL)
#       }
#     )
#     if(is.null(out)) next
#     
#     results_list[[model_type]] <- out
#     
#     # ── 3. Métriques Training ────────────────────────────────────────────────
#     res_lrn   <- out$datalearningmodel$reslearningmodel
#     row_data  <- extract_metrics(res_lrn$predictclasslearning,
#                                  res_lrn$classlearning,
#                                  res_lrn$scorelearning,
#                                  n_classes, prefix = "Train")
#     row_data$Model <- model_type
#     
#     # ── 4. Métriques Validation (si disponible) ──────────────────────────────
#     if(!is.null(validation) && length(out$datavalidationmodel) > 0 &&
#        !is.null(out$datavalidationmodel$resvalidationmodel)) {
#       res_val  <- out$datavalidationmodel$resvalidationmodel
#       val_data <- extract_metrics(res_val$predictclassval,
#                                   res_val$classval,
#                                   res_val$scoreval,
#                                   n_classes, prefix = "Val")
#       row_data <- cbind(row_data, val_data)
#     }
#     
#     metrics_summary <- rbind(metrics_summary, row_data)
#   }
#   
#   # Remettre Model en première colonne
#   if(nrow(metrics_summary) > 0) {
#     metrics_summary <- metrics_summary[, c("Model", setdiff(names(metrics_summary), "Model"))]
#   }
#   
#   return(list(
#     results = results_list,
#     metrics = metrics_summary
#   ))
# }

# --------------------------------------------------------------------------
# extract_metrics : calcule Accuracy, Sensitivity, Specificity, AUC
#   - Binaire  : utilise sensibility() / specificity() existantes + pROC::roc
#   - Multi-cl.: macro-moyenne one-vs-rest + pROC::multiclass.roc
# --------------------------------------------------------------------------
extract_metrics <- function(predicted, actual, scores, n_classes, prefix = "Train") {
  
  # Retirer le préfixe "test " ajouté par apply_threshold pour l'accuracy
  pred_clean <- sub("^test *", "", as.character(predicted))
  
  # ── Accuracy ─────────────────────────────────────────────────────────────
  acc <- tryCatch(
    round(mean(pred_clean == as.character(actual), na.rm = TRUE), 3),
    error = function(e) NA
  )
  
  # ── Sensitivity / Specificity ────────────────────────────────────────────
  if(n_classes == 2) {
    # Binaire → fonctions existantes de l'application
    sen <- tryCatch(sensibility(predicted, actual), error = function(e) NA)
    spe <- tryCatch(specificity(predicted, actual), error = function(e) NA)
  } else {
    # Multi-classes → macro-moyenne one-vs-rest
    pred_factor <- factor(pred_clean, levels = levels(actual))
    cm <- table(Predicted = pred_factor, Actual = actual)
    classes <- levels(actual)
    
    sens_per_class <- sapply(classes, function(cls) {
      tp <- if(cls %in% rownames(cm)) cm[cls, cls] else 0
      fn <- sum(cm[, cls]) - tp
      if((tp + fn) == 0) return(NA)
      tp / (tp + fn)
    })
    spec_per_class <- sapply(classes, function(cls) {
      tp <- if(cls %in% rownames(cm)) cm[cls, cls] else 0
      fp <- if(cls %in% rownames(cm)) sum(cm[cls, ]) - tp else 0
      tn <- sum(cm) - sum(cm[, cls]) - fp
      if((tn + fp) == 0) return(NA)
      tn / (tn + fp)
    })
    
    sen <- round(mean(sens_per_class, na.rm = TRUE), 3)
    spe <- round(mean(spec_per_class, na.rm = TRUE), 3)
  }
  
  # ── AUC ──────────────────────────────────────────────────────────────────
  auc_val <- tryCatch({
    if(n_classes == 2) {
      round(as.numeric(pROC::auc(pROC::roc(actual, scores, quiet = TRUE))), digits = 3)
    } else {
      round(as.numeric(pROC::multiclass.roc(actual, scores, quiet = TRUE)$auc), digits = 3)
    }
  }, error = function(e) NA)
  
  result <- data.frame(
    x_Accuracy    = acc,
    x_Sensitivity = sen,
    x_Specificity = spe,
    x_AUC         = auc_val,
    stringsAsFactors = FALSE
  )
  colnames(result) <- paste0(prefix, "_", c("Accuracy", "Sensitivity", "Specificity", "AUC"))
  result
}

# ==========================================================================
# DELONG TEST FOR AUC COMPARISON
# ==========================================================================
delong_compare_models <- function(comparison_results) {
  models   <- names(comparison_results$results)
  n_models <- length(models)
  
  if(n_models < 2) {
    return(data.frame(message = "Need at least 2 models to compare"))
  }
  
  pval_matrix <- matrix(NA, nrow = n_models, ncol = n_models)
  rownames(pval_matrix) <- models
  colnames(pval_matrix) <- models
  
  # Préférer la validation si disponible, sinon utiliser les données d'apprentissage
  use_val <- all(sapply(models, function(m) {
    res <- comparison_results$results[[m]]
    !is.null(res$datavalidationmodel) && length(res$datavalidationmodel) > 0 &&
      !is.null(res$datavalidationmodel$resvalidationmodel)
  }))
  
  for(i in 1:(n_models - 1)) {
    for(j in (i + 1):n_models) {
      tryCatch({
        res_i <- comparison_results$results[[models[i]]]
        res_j <- comparison_results$results[[models[j]]]
        
        if(use_val) {
          actual   <- res_i$datavalidationmodel$resvalidationmodel$classval
          scores_i <- res_i$datavalidationmodel$resvalidationmodel$scoreval
          scores_j <- res_j$datavalidationmodel$resvalidationmodel$scoreval
        } else {
          actual   <- res_i$datalearningmodel$reslearningmodel$classlearning
          scores_i <- res_i$datalearningmodel$reslearningmodel$scorelearning
          scores_j <- res_j$datalearningmodel$reslearningmodel$scorelearning
        }
        
        if(is.null(actual) || is.null(scores_i) || is.null(scores_j)) next
        scores_i <- as.numeric(scores_i)
        scores_j <- as.numeric(scores_j)
        
        roc_i <- pROC::roc(actual, scores_i, quiet = TRUE)
        roc_j <- pROC::roc(actual, scores_j, quiet = TRUE)
        test_result <- pROC::roc.test(roc_i, roc_j, method = "delong")
        pval_matrix[i, j] <- round(test_result$p.value, 4)
        pval_matrix[j, i] <- round(test_result$p.value, 4)
        
      }, error = function(e) {
        cat(sprintf("DeLong test error (%s vs %s): %s\n", models[i], models[j], e$message))
      })
    }
  }
  
  result_df <- as.data.frame(pval_matrix)
  for(k in 1:n_models) result_df[k, k] <- "-"
  return(result_df)
}

# ==========================================================================
# RADAR/SPIDER PLOT FOR MODEL COMPARISON
# ==========================================================================
plot_radar_comparison <- function(metrics_summary, type = "validation") {
  if(nrow(metrics_summary) < 1) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No models to compare", size = 6) + theme_void())
  }
  
  if(type == "validation" && "Val_AUC" %in% colnames(metrics_summary)) {
    radar_data <- metrics_summary %>%
      dplyr::select(Model, Val_Accuracy, Val_Sensitivity, Val_Specificity, Val_AUC) %>%
      dplyr::rename(Accuracy = Val_Accuracy, Sensitivity = Val_Sensitivity,
                    Specificity = Val_Specificity, AUC = Val_AUC)
  } else {
    radar_data <- metrics_summary %>%
      dplyr::select(Model, Train_Accuracy, Train_Sensitivity, Train_Specificity, Train_AUC) %>%
      dplyr::rename(Accuracy = Train_Accuracy, Sensitivity = Train_Sensitivity,
                    Specificity = Train_Specificity, AUC = Train_AUC)
  }
  
  models <- radar_data$Model
  radar_values <- radar_data[, -1]
  radar_values[is.na(radar_values)] <- 0
  
  radar_df <- rbind(rep(1, ncol(radar_values)), rep(0, ncol(radar_values)), radar_values)
  rownames(radar_df) <- c("Max", "Min", models)
  
  n_models <- length(models)
  colors_fill <- c("#E74C3C44", "#3498DB44", "#2ECC7144", "#F39C1244", "#9B59B644", "#1ABC9C44")[1:n_models]
  colors_line <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6", "#1ABC9C")[1:n_models]
  
  par(mar = c(1, 1, 2, 1))
  radarchart(radar_df,
             axistype = 1,
             pcol = colors_line,
             pfcol = adjustcolor(topo.colors(n_models), 0.15),
             #colors_fill,
             plwd = 2,
             plty = 1,
             cglcol = "grey",
             cglty = 1,
             axislabcol = "grey40",
             vlcex = 1.1,
             palcex = 1.1,
             title = paste("Model Comparison -", tools::toTitleCase(type)))
  
  legend("topright", legend = models, col = colors_line, 
         lty = 1, lwd = 2, bty = "n", cex = 1.5)
}

# ==========================================================================
# SHAP VALUES (via iml package)
# ==========================================================================
compute_shap_values <- function(model, learningmodel, modeltype, n_samples = 50) {
  X <- learningmodel[, -1]
  y <- learningmodel[, 1]
  
  predict_fn <- function(model, newdata) {
    newdata <- as.data.frame(newdata)
    
    # iml peut passer newdata avec des noms de colonnes entiers ("1","2",...) au lieu
    # des noms de features → renommer si le nombre de colonnes correspond
    if(ncol(newdata) == ncol(X)) colnames(newdata) <- colnames(X)
    
    for(i in seq_len(ncol(newdata))) {
      newdata[, i] <- suppressWarnings(as.numeric(as.character(newdata[, i])))
    }
    
    tryCatch({
      if(modeltype == "randomforest") {
        preds <- randomForest:::predict.randomForest(model, newdata, type = "prob")
        return(as.data.frame(preds))
      } else if(modeltype == "svm") {
        dv <- attr(e1071:::predict.svm(model, newdata, decision.values = TRUE), "decision.values")
        return(data.frame(score = as.numeric(dv)))
      } else if(modeltype == "elasticnet") {
        newdata_matrix <- as.matrix(newdata)
        if(inherits(model$glmnet_model, "cv.glmnet")) {
          preds <- glmnet:::predict.cv.glmnet(model$glmnet_model, newdata_matrix, s = model$lambda, type = "response")
        } else {
          preds <- glmnet::predict.glmnet(model$glmnet_model, newdata_matrix, s = model$lambda, type = "response")
        }
        if(length(dim(preds)) == 3) preds <- preds[, , 1]
        return(as.data.frame(preds))
      } else if(modeltype == "xgboost") {
        dmat <- xgb.DMatrix(data = as.matrix(newdata))
        preds <- xgboost:::predict.xgb.Booster(model, dmat, reshape = TRUE)
        return(as.data.frame(preds))
      } else if(modeltype == "naivebayes") {
        preds <- e1071:::predict.naiveBayes(model, newdata, type = "raw")
        return(as.data.frame(preds))
      } else if(modeltype == "knn") {
        return(data.frame(pred = rep(0.5, nrow(newdata))))
      }
    }, error = function(e) {
      data.frame(pred = rep(NA_real_, nrow(newdata)))
    })
  }
  
  predictor <- Predictor$new(
    model = model,
    data = X,
    y = y,
    predict.function = function(model, newdata) predict_fn(model, newdata)
  )
  
  sample_idx <- sample(1:nrow(X), min(n_samples, nrow(X)))
  
  shapley_results <- list()
  for(idx in sample_idx) {
    tryCatch({
      shap <- Shapley$new(predictor, x.interest = X[idx, , drop = FALSE])
      shapley_results[[as.character(idx)]] <- shap$results
    }, error = function(e) {
      cat(sprintf("SHAP error for sample %d: %s\n", idx, e$message))
    })
  }
  
  if(length(shapley_results) == 0) return(NULL)
  
  all_shap <- do.call(rbind, shapley_results)
  
  shap_importance <- all_shap %>%
    group_by(feature) %>%
    summarise(mean_abs_shap = mean(abs(phi), na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(mean_abs_shap))
  
  return(list(
    shap_details = all_shap,
    shap_importance = shap_importance,
    predictor = predictor
  ))
}

plot_shap_importance <- function(shap_result, n_top = 20, title = "SHAP Feature Importance") {
  if(is.null(shap_result)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "SHAP computation failed", size = 6) + theme_void())
  }
  
  df <- head(shap_result$shap_importance, n_top)
  df$feature <- factor(df$feature, levels = rev(df$feature))
  
  p <- ggplot(df, aes(x = feature, y = mean_abs_shap, fill = mean_abs_shap)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_gradient(low = "#3498DB", high = "#E74C3C") +
    labs(title = title, x = "", y = "Mean |SHAP value|") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "none"
    )
  
  return(p)
}

# ==========================================================================
# PARTIAL DEPENDENCE PLOTS (PDP)
# ==========================================================================
plot_pdp <- function(model, learningmodel, modeltype, feature_name, title = NULL) {
  X <- learningmodel[, -1, drop = FALSE]
  y <- learningmodel[, 1]
  
  if(!(feature_name %in% colnames(X))) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                               label = paste("Feature not found:", feature_name), size = 5) + theme_void())
  }
  
  # Guard: need ≥2 unique non-NA values to build any grid at all
  feat_vals <- X[[feature_name]]
  n_unique   <- length(unique(na.omit(feat_vals)))
  if(n_unique < 2) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                               label = paste("Feature has no variance:", feature_name), size = 5) + theme_void())
  }
  
  # Use a small grid capped to actual unique values — this is what fixes
  # the approxfun 'x and y lengths differ' crash for low-variance features.
  # iml's approxfun is called with length = grid.size, so the prediction
  # vector MUST also have exactly that many rows × nrow(X) entries.
  grid_size <- min(20L, n_unique)
  
  # Pre-compute column medians once, used as NA-safe fallback in predict_fn
  col_medians <- sapply(seq_len(ncol(X)), function(i) {
    v <- suppressWarnings(as.numeric(as.character(X[, i])))
    m <- median(v, na.rm = TRUE)
    if(is.na(m)) 0 else m
  })
  names(col_medians) <- colnames(X)
  
  predict_fn <- function(model, newdata) {
    newdata      <- as.data.frame(newdata)
    expected_n   <- nrow(newdata)
    
    # iml sometimes passes integer column names → restore original names
    if(ncol(newdata) == ncol(X)) colnames(newdata) <- colnames(X)
    
    # Coerce to numeric and impute NAs with column median (never leave NAs —
    # models like randomForest silently drop NA rows, breaking vector length)
    for(i in seq_len(ncol(newdata))) {
      col     <- suppressWarnings(as.numeric(as.character(newdata[, i])))
      na_idx  <- is.na(col)
      if(any(na_idx)) col[na_idx] <- col_medians[colnames(newdata)[i]]
      newdata[, i] <- col
    }
    
    preds <- tryCatch({
      raw <- if(modeltype == "randomforest") {
        p <- randomForest:::predict.randomForest(model, newdata, type = "prob")
        as.numeric(p[, 1])
      } else if(modeltype == "svm") {
        dv <- attr(e1071:::predict.svm(model, newdata, decision.values = TRUE),
                   "decision.values")
        as.numeric(dv)
      } else if(modeltype == "xgboost") {
        dmat <- xgb.DMatrix(data = as.matrix(newdata))
        as.numeric(xgboost:::predict.xgb.Booster(model, dmat))
      } else if(modeltype == "naivebayes") {
        p <- e1071:::predict.naiveBayes(model, newdata, type = "raw")
        as.numeric(p[, 1])
      } else {
        rep(0.5, expected_n)
      }
      raw
    }, error = function(e) {
      rep(0.5, expected_n)
    })
    
    # Hard length guarantee — this is the direct fix for approxfun's crash
    if(length(preds) != expected_n) preds <- rep(0.5, expected_n)
    
    # iml's predict.function contract requires a data.frame, not a vector
    data.frame(score = preds)
  }
  
  predictor <- Predictor$new(
    model            = model,
    data             = X,
    y                = y,
    predict.function = function(model, newdata) predict_fn(model, newdata)
  )
  
  pdp_result <- tryCatch(
    suppressWarnings(
      FeatureEffect$new(predictor, feature = feature_name,
                        method = "pdp", grid.size = grid_size)
    ),
    error = function(e) {
      cat(sprintf("[PDP] FeatureEffect failed for '%s': %s\n", feature_name, e$message))
      NULL
    }
  )
  
  if(is.null(pdp_result)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                               label = paste("PDP could not be computed for:", feature_name), size = 5) + theme_void())
  }
  
  if(is.null(title)) title <- paste("Partial Dependence Plot -", feature_name)
  
  plot(pdp_result) +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.text  = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 14, face = "bold")
    )
}

# ==========================================================================
# LIME LOCAL EXPLANATIONS
# ==========================================================================

# Wrapper class for LIME compatibility
model_type.lime_wrapper <- function(x, ...) "classification"

predict_model.lime_wrapper <- function(x, newdata, type = "raw", ...) {
  newdata <- as.data.frame(newdata)
  for(i in 1:ncol(newdata)) {
    newdata[, i] <- as.numeric(as.character(newdata[, i]))
  }
  
  mt <- x$modeltype
  m  <- x$raw_model
  yt <- x$y_levels
  
  if(mt == "randomforest") {
    preds <- randomForest:::predict.randomForest(m, newdata, type = "prob")
    return(as.data.frame(preds))
  } else if(mt == "svm") {
    preds <- e1071:::predict.svm(m, newdata, probability = TRUE)
    probs <- attr(preds, "probabilities")
    if(!is.null(probs)) {
      probs <- as.data.frame(probs)
      probs <- probs[, yt, drop = FALSE]
      return(probs)
    }
    return(data.frame(pred = as.numeric(preds)))
  } else if(mt == "elasticnet") {
    x_mat <- as.matrix(newdata)
    if(inherits(m$glmnet_model, "cv.glmnet")) {
      preds <- glmnet:::predict.cv.glmnet(m$glmnet_model, newx = x_mat,
                                          s = m$lambda, type = "response")
    } else {
      preds <- glmnet::predict.glmnet(m$glmnet_model, newx = x_mat,
                                      s = m$lambda, type = "response")
    }
    if(length(dim(preds)) == 3) preds <- preds[, , 1]
    preds <- as.data.frame(preds)
    if(ncol(preds) == 1) {
      preds <- data.frame(p1 = 1 - preds[, 1], p2 = preds[, 1])
      colnames(preds) <- yt
    }
    return(preds)
  } else if(mt == "xgboost") {
    dmat <- xgb.DMatrix(data = as.matrix(newdata))
    preds <- xgboost:::predict.xgb.Booster(m, dmat, reshape = TRUE)
    preds_df <- as.data.frame(preds)
    if(ncol(preds_df) == length(yt)) colnames(preds_df) <- yt
    return(preds_df)
  } else if(mt == "naivebayes") {
    preds <- e1071:::predict.naiveBayes(m, newdata, type = "raw")
    return(as.data.frame(preds))
  } else {
    n_classes <- length(yt)
    return(as.data.frame(matrix(1/n_classes, nrow = nrow(newdata), ncol = n_classes,
                                dimnames = list(NULL, yt))))
  }
}

# lime function 
explain_lime <- function(model, learningmodel, modeltype, sample_indices = 1:3, n_features = 10) {
  X_train <- learningmodel[, -1]
  y_train <- learningmodel[, 1]
  
  for(i in 1:ncol(X_train)) {
    X_train[, i] <- as.numeric(as.character(X_train[, i]))
  }
  
  # Create wrapper with lime_wrapper class
  wrapper <- list(raw_model = model, modeltype = modeltype, y_levels = levels(y_train))
  class(wrapper) <- "lime_wrapper"
  
  explainer <- tryCatch({
    lime::lime(X_train, wrapper, bin_continuous = TRUE)
  }, error = function(e) {
    cat(sprintf("LIME explainer error: %s\n", e$message))
    return(NULL)
  })
  
  if(is.null(explainer)) return(NULL)
  
  sample_idx <- sample_indices[sample_indices <= nrow(X_train)]
  if(length(sample_idx) == 0) sample_idx <- 1
  
  explanations <- tryCatch({
    lime::explain(X_train[sample_idx, , drop = FALSE], explainer, 
                  n_labels = min(length(levels(y_train)), 3),
                  n_features = n_features,
                  n_permutations = 1000)
  }, error = function(e) {
    cat(sprintf("LIME explanation error: %s\n", e$message))
    return(NULL)
  })
  
  return(explanations)
}

plot_lime_explanation <- function(lime_result, title = "LIME Explanation") {
  if(is.null(lime_result)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                               label = "LIME explanation not available", size = 6) + theme_void())
  }
  
  p <- plot_features(lime_result) +
    labs(title = title) +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.title = element_text(size = 15 , face =  "bold"),
      axis.text.y = element_text(size = 15 , face = "bold"),
      axis.text.x  = element_text(size = 12, face = "bold"),
      legend.title = element_text(size  = 15 , face = "bold"),
      legend.text = element_text(size  = 15 , face = "bold")
    )
  
  return(p)
}

# ==========================================================================
# LEARNING CURVE (binary classification)
# ==========================================================================

learning_curve_binary <- function(learningmodel,
                                  modelparameters,
                                  train_sizes = seq(0.1, 1.0, by = 0.1),
                                  n_folds     = 5) {
  
  colnames(learningmodel)[1] <- "group"
  y   <- learningmodel[, 1]
  X   <- learningmodel[, -1, drop = FALSE]
  lev <- levels(y)               # c(positif, negatif) — same convention as modelfunction_V2
  n   <- nrow(learningmodel)
  
  # Helper: train one model on X_tr / y_tr, return prob score on X_te
  train_predict <- function(X_tr, y_tr, X_te) {
    mt  <- modelparameters$modeltype
    dat <- cbind(group = y_tr, X_tr)
    
    tryCatch({
      if (mt == "randomforest") {
        ntree_p    <- if (!is.null(modelparameters$ntree))    modelparameters$ntree    else 500
        mtry_p     <- if (!is.null(modelparameters$mtry))     modelparameters$mtry     else floor(sqrt(ncol(X_tr)))
        nodesize_p <- if (!is.null(modelparameters$nodesize)) modelparameters$nodesize else 1
        m <- randomForest::randomForest(
          x        = X_tr,
          y        = y_tr,
          ntree    = ntree_p,
          mtry     = mtry_p,
          nodesize = nodesize_p
        )
        as.numeric(randomForest:::predict.randomForest(m, X_te, type = "prob")[, lev[1]])
        
      } else if (mt == "svm") {
        cost_p   <- if (!is.null(modelparameters$cost))   modelparameters$cost   else 1
        gamma_p  <- if (!is.null(modelparameters$gamma))  modelparameters$gamma  else 1 / ncol(X_tr)
        kernel_p <- if (!is.null(modelparameters$kernel)) modelparameters$kernel else "radial"
        m <- e1071::svm(
          x               = X_tr,
          y               = y_tr,
          cost            = cost_p,
          gamma           = gamma_p,
          kernel          = kernel_p,
          decision.values = TRUE,
          probability     = FALSE
        )
        dv <- attr(e1071:::predict.svm(m, X_te, decision.values = TRUE), "decision.values")
        score <- as.numeric(dv)
        # Flip sign if positive class is in wrong position
        if (!is.null(colnames(dv)) &&
            strsplit(colnames(dv)[1], "/")[[1]][1] != lev[1]) score <- -score
        score
        
      } else if (mt == "xgboost") {
        y_bin  <- as.numeric(y_tr == lev[1])
        dtr    <- xgb.DMatrix(data = as.matrix(X_tr), label = y_bin)
        dte    <- xgb.DMatrix(data = as.matrix(X_te))
        nrounds_p        <- if (!is.null(modelparameters$nrounds))          modelparameters$nrounds          else 100
        max_depth_p      <- if (!is.null(modelparameters$max_depth))        modelparameters$max_depth        else 6
        eta_p            <- if (!is.null(modelparameters$eta))              modelparameters$eta              else 0.3
        gamma_p          <- if (!is.null(modelparameters$gamma_xgb))        modelparameters$gamma_xgb        else 0    
        subsample_p      <- if (!is.null(modelparameters$subsample_xgb))    modelparameters$subsample_xgb    else 1.0  
        min_child_p      <- if (!is.null(modelparameters$min_child_weight)) modelparameters$min_child_weight else 1    
        alpha_p          <- if (!is.null(modelparameters$alpha_xgb))        modelparameters$alpha_xgb        else 0    
        lambda_p         <- if (!is.null(modelparameters$lambda_xgb))       modelparameters$lambda_xgb       else 1    
        m <- xgboost::xgboost(
          data             = dtr,
          nrounds          = nrounds_p,
          max_depth        = max_depth_p,
          eta              = eta_p,
          gamma            = gamma_p,
          subsample        = subsample_p,
          min_child_weight = min_child_p,
          alpha            = alpha_p,
          lambda           = lambda_p,
          objective        = "binary:logistic",
          verbose          = 0
        )
        as.numeric(xgboost:::predict.xgb.Booster(m, dte))
      } else if (mt == "naivebayes") {
        m <- e1071::naiveBayes(group ~ ., data = dat,
                               laplace = if (!is.null(modelparameters$laplace)) modelparameters$laplace else 0)
        as.numeric(e1071:::predict.naiveBayes(m, X_te, type = "raw")[, lev[1]])
        
      } else if (mt == "elasticnet") {
        alpha_p     <- if (!is.null(modelparameters$alpha)) modelparameters$alpha else 0.5
        nfolds_safe <- max(3, min(5, floor(length(y_tr) / 2)))   # bug ElasticNet petits n
        m <- glmnet::cv.glmnet(as.matrix(X_tr), y_tr,
                               family  = "binomial",
                               alpha   = alpha_p,
                               nfolds  = nfolds_safe)
        prob <- as.numeric(glmnet:::predict.cv.glmnet(m, as.matrix(X_te),
                                                      s = "lambda.min",
                                                      type = "response"))
        # glmnet retourne P(dernier niveau facteur), pas nécessairement lev[1]
        glmnet_pos <- levels(y_tr)[2]   # classe encodée "1" par glmnet
        if (glmnet_pos != lev[1]) prob <- 1 - prob   #  inversion
        prob
      } else if (mt == "knn") {
        k_p <- if (!is.null(modelparameters$k_neighbors)) modelparameters$k_neighbors else 5
        sapply(seq_len(nrow(X_te)), function(i) {
          dists <- apply(X_tr, 1, function(row) sqrt(sum((as.numeric(X_te[i, ]) - as.numeric(row))^2)))
          idx   <- order(dists)[seq_len(k_p)]
          mean(y_tr[idx] == lev[1])
        })
        
      } else {
        rep(0.5, nrow(X_te))
      }
    }, error = function(e) {
      cat(sprintf("[LC] train_predict error (%s): %s\n", mt, e$message))
      rep(NA_real_, nrow(X_te))
    })
  }
  
  # ── Cross-validated learning curve ─────────────────────────────────────────
  # Build stratified folds once
  set.seed(42)
  fold_ids <- numeric(n)
  for (cls in lev) {
    idx      <- which(y == cls)
    shuffled <- sample(idx)
    fold_ids[shuffled] <- (seq_along(shuffled) - 1) %% n_folds + 1
  }
  
  results <- list()
  for (size in train_sizes) {
    for (fold in seq_len(n_folds)) {
      test_idx  <- which(fold_ids == fold)
      train_all <- which(fold_ids != fold)
      
      # Sub-sample train_all to match the requested size fraction
      n_take <- max(2 * length(lev),            # at least one of each class
                    round(length(train_all) * size))
      n_take <- min(n_take, length(train_all))
      
      # Stratified sub-sampling within the training fold
      sub_idx <- unlist(lapply(lev, function(cls) {
        cls_idx <- train_all[y[train_all] == cls]
        k       <- max(1, round(length(cls_idx) * size))
        sample(cls_idx, min(k, length(cls_idx)))
      }))
      
      X_tr <- X[sub_idx,  , drop = FALSE]
      y_tr <- y[sub_idx]
      X_te <- X[test_idx, , drop = FALSE]
      y_te <- y[test_idx]
      
      score_te <- train_predict(X_tr, y_tr, X_te)
      #score_tr <- train_predict(X_tr, y_tr, X_tr)  
      if (modelparameters$modeltype == "randomforest") {
        ntree_p    <- if (!is.null(modelparameters$ntree))    modelparameters$ntree    else 500
        mtry_p     <- if (!is.null(modelparameters$mtry))     modelparameters$mtry     else floor(sqrt(ncol(X_tr)))
        nodesize_p <- if (!is.null(modelparameters$nodesize)) modelparameters$nodesize else 1
        m_oob <- randomForest::randomForest(
          x        = X_tr,
          y        = y_tr,
          ntree    = ntree_p,
          mtry     = mtry_p,
          nodesize = nodesize_p
        )
        score_tr <- as.numeric(m_oob$votes[, lev[1]])
      } else {
        score_tr <- train_predict(X_tr, y_tr, X_tr)
      }
      
      safe_auc <- function(truth, score) {
        tryCatch(
          as.numeric(pROC::auc(pROC::roc(as.vector(truth), as.vector(score), quiet = TRUE))),
          error = function(e) NA_real_
        )
      }
      safe_acc <- function(truth, score, modeltype = "other") {
        threshold <- if (modeltype == "svm") 0 else 0.5
        pred <- ifelse(score >= threshold, lev[1], lev[2])
        mean(pred == as.character(truth), na.rm = TRUE)
      }
      
      results[[length(results) + 1]] <- data.frame(
        train_size    = size,
        n_train       = length(sub_idx),
        fold          = fold,
        train_auc     = safe_auc(y_tr, score_tr),
        cv_auc        = safe_auc(y_te, score_te),
        train_acc = safe_acc(y_tr, score_tr, modelparameters$modeltype),
        cv_acc    = safe_acc(y_te, score_te, modelparameters$modeltype)
      )
    }
  }
  
  do.call(rbind, results)
}


plot_learning_curve_binary <- function(lc_data, metric = "auc", title = NULL) {
  
  stopifnot(metric %in% c("auc", "accuracy"))
  
  train_col <- if (metric == "auc") "train_auc" else "train_acc"
  cv_col    <- if (metric == "auc") "cv_auc"    else "cv_acc"
  y_label   <- if (metric == "auc") "AUC"       else "Accuracy"
  if (is.null(title)) title <- paste("Learning Curve —", y_label)
  
  # Agréger par train_size uniquement (pas n_train)
  # → un seul point par taille, moyenne sur les folds
  agg <- lc_data %>%
    dplyr::group_by(train_size) %>%
    dplyr::summarise(
      mean_train = mean(.data[[train_col]], na.rm = TRUE),
      sd_train   = sd(.data[[train_col]],   na.rm = TRUE),
      mean_cv    = mean(.data[[cv_col]],    na.rm = TRUE),
      sd_cv      = sd(.data[[cv_col]],      na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(
      # Convertir train_size (0.1 → 1.0) en pourcentage lisible sur l'axe
      size_pct = paste0(round(train_size * 100), "%")
    )
  
  # Fixer l'ordre des labels % sur l'axe X
  agg$size_pct <- factor(agg$size_pct, levels = agg$size_pct)
  
  ggplot(agg, aes(x = size_pct)) +
    geom_ribbon(aes(ymin = mean_train - sd_train, ymax = mean_train + sd_train,
                    group = 1),
                fill = "#2980B9", alpha = 0.15) +
    geom_ribbon(aes(ymin = mean_cv - sd_cv, ymax = mean_cv + sd_cv,
                    group = 1),
                fill = "#E74C3C", alpha = 0.15) +
    geom_line(aes(y = mean_train, colour = "Training",  group = 1), linewidth = 1.1) +
    geom_point(aes(y = mean_train, colour = "Training"), size = 2.5) +
    geom_line(aes(y = mean_cv,    colour = "Cross-val", group = 1), linewidth = 1.1) +
    geom_point(aes(y = mean_cv,   colour = "Cross-val"), size = 2.5) +
    scale_colour_manual(values = c("Training" = "#2980B9", "Cross-val" = "#E74C3C")) +
    labs(title = title,
         x     = "Training set size",
         y     = y_label,
         colour = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title      = element_text(size = 15, face = "bold"),
      axis.text       = element_text(size = 11, face = "bold"),
      axis.text.x     = element_text(angle = 0),
      axis.title      = element_text(size = 13, face = "bold"),
      legend.position = "top"
    )
}

