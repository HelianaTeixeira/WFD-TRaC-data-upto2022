#File name: Functions_BLM.R
#Functions used to fit BLM models, compute confusion matrix and produce plots used for boundary setting
#Geoff Phillips
#11 February 2023

#' Function from tool kit, used to estimate predicted values from logistic model
#'
#' @param model 
#' @param logarithm 
#' @param Prob 
#'
#' @return  value of x for given probability

#'
#' @examples
#' 
getpVar <- function(model=NULL, logarithm=TRUE,Prob) {
  Fact<-log(Prob/(1-Prob))
  if (logarithm) {
    return (10^((Fact-model$coefficients[1])/model$coefficients[2]))
  } else {
    return ((Fact-model$coefficients[1])/model$coefficients[2])
  }
}
#==================================================================================================
#' Function to fit binary logistic model
#'
#' @param df        name dataframe
#' @param x         x values
#' @param y         y values
#' @param logarithm TRUE to use log10 transform (default)
#'
#' @return          model object

MyBLM <- function(df,x,y,logarithm=TRUE){
  if (logarithm) {
    return(glm(eval(as.name(y))~log10(eval(as.name(x))), data=df, family=binomial(logit)))
  } else {
    return (glm(eval(as.name(y))~(eval(as.name(x))), data=df, family=binomial(logit)))
  }
}


#==================================================================================================
#' Fit binary logistic model, calculate thresholds and boundary values
#'
#' @param df        the data frame used 
#' @param plt       plots within optiThresh (default to FALSE) 
#' @param x         the independent variable as text (default to "TP") 
#' @param y         the dependent binary variable (default to "BinClass") 
#' @param min_new.x min value of x for model predictions (funct. uses min x or value, default=1) 
#' @param max_new.x max value of x for model predictions (funct. uses max x or value, default=1)
#' @param logarithm TRUE to use log10 transform (default) for model
#'
#' @return          a list with the following components
#' mod              a list with the model details
#' mod.pred         a data frame with sequence of independent variable values with predicted values
#' pseudo.r2        pseudo.r2 value
#' Auc              AuC for the  model
#' Prev             Prevalence value for the data
#' allMeasures      a data frame with the confusion matrix measure values by probability
#' optEach          a data frame with the optimal values for each measure, including additional measures not used by modEva
#' 
#' @examples
#' 
#' Fit model to log10TP data
#' BLM <- modBLM(subset(datSoE.pp.u,!is.na(TP)),FALSE)
#' 
#' Fit model to log10TN data
#' BLM <- modBLM(subset(datSoE.pp.u,!is.na(TN)),plt=FALSE,x="TN")
#' 
#' Fit model to log10TN data and increase range of predicted values
#' BLM <- modBLM(subset(datSoE.pp.u,!is.na(TN)),plt=FALSE,x="TN_Mn", max_new.x=20)
#' 
#' #' Fit model to BOD5 without log10 trasform data
#' BLM <- modBLM(subset(datSoE.pp.u,!is.na(TN)),plt=FALSE,x="BOD5_mn",logarithm=FALSE)
#'  
modBLM <- function (df,plt = FALSE,x="TP",y="BinClass",min_new.x=1,max_new.x=1,logarithm=TRUE) {
  dframe <- deparse(substitute(df))
  df <- df %>% filter(!is.na(NEQR)) %>%  # remove missing NEQR
        filter(eval(as.name(SE))>0)  #remove 0 values for SE to avoid error when using log transform
  NRec <- dim(df)[1]
  # call function to fit model  
  mod <- MyBLM(df,x,y,logarithm)
  # predict from model 
  # in some cases need to create new df with bigger range to allow for extrapolation  of error lines
  # if this the case decrease/increase default min_new.x or max_new.x passed to function 
  min.x <- min(min(df[[x]]),min_new.x) # select min of x in data set or value passed to function
  max.x <- max(max(df[[x]]),max_new.x) # select max of x in data set or value passed to function
  new.dat <- data.frame(var=seq(from=min.x ,to=max.x,length.out=500))
  names(new.dat) <- x
  mod.pred <- cbind(new.dat,predict(mod, type="link",newdata=new.dat, se.fit=TRUE ))
  mod.pred <- mod.pred %>%  mutate(lower = mod$family$linkinv(fit - 1.96*se.fit), upper= mod$family$linkinv(fit + 1.96*se.fit),point.estimate = mod$family$linkinv(fit) )
  # calculate pseudo r2  
  mod.logit <- lrm(eval(as.name(y))~log10(eval(as.name(x))), data=df)
  pseudo.r2 = mod.logit$stats[10]
  # get AuC & prevalence of data used
  AuC_all <- AUC(model = mod, plot = plt) 
  Auc <- AuC_all$AUC
  Prev <- AuC_all$prevalence
  # get all measures  provided by modEva
  OThresh <- optiThresh(model = mod, pch = 20,plot = plt)
  # get dataframe of measures by probability
  allMeasures <- OThresh$all.thresholds
  allMeasures <- allMeasures %>% 
    mutate(p=as.numeric(rownames(OThresh$all.thresholds)))# add probability values
  # create dataframe and calculate classification errors from confusion matrix for each probability 
  # to enable calculation of mismatch used in toolkit
  Errors <- data.frame()
  N <- dim(df)[1]
  for(n in 1:100){
    p <- n/100
    CMatrix <- threshMeasures(mod,thresh = p,plot = FALSE,verbosity=0)$ConfusionMatrix
    Tp <- CMatrix[1,1]
    Fp <- CMatrix[1,2]
    Fn <- CMatrix[2,1]
    Tn <- CMatrix[2,2]
    out <- data.frame(p,Tp,Fp,Fn,Tn)
    Errors <- rbind(Errors,out)
  }
  # add these errors to dataframe of measures by probability
  suppressMessages(allMeasures <- left_join(allMeasures,Errors))
  allMeasures <-  allMeasures %>% 
    mutate(FPR=Fp/N,FNR=Fn/N,FPR_FNRdiff=abs(FPR-FNR))
  # Create data frame of optimum threshold values using output from optiThresh
  optEach <- OThresh$optimals.each
  # get thresholds based on criteria not provided by modEva
  # identify the threshold p value that provides critical Commission value of 0.1 
  critVal <- 0.1
  p_critCom <- allMeasures[which.min(abs(allMeasures$Commission-critVal)),"p"] 
  # identify the threshold p value that provides critical Omission value of 0.1
  p_critOmi <- allMeasures[which.min(abs(allMeasures$Omission-critVal)),"p"] 
  # identify the threshold p value that provides critical Sensitivity value of 0.9
  critVal2 <- 0.9
  p_critSens <- allMeasures[which.min(abs(allMeasures$Sensitivity-critVal2)),"p"] 
  # identify the threshold p value that provides critical Specificity value of 0.9
  p_critSpec <- allMeasures[which.min(abs(allMeasures$Specificity-critVal2)),"p"] 
  # Get threshold p value at crossover of Omission/Commission using optiPair function
  OmisComis <- optiPair(model = mod, measures = c("Omission","Commission"),plot = plt)
  p_OmisComis <- OmisComis$ThreshDiff
  valOmi <- om_OmisComis <- OmisComis$measures.values[OmisComis$measures.values$Threshold==p_OmisComis,1]# Omission value at crossover
  # Get threshold p value at crossover of Over/Under prediction rates
  OPRUPR <- optiPair(model = mod, measures = c("OPR","UPR"),plot = plt)
  p_OPRUPR <- OPRUPR$ThreshDiff
  valOPR <- om_OPRUPR <- OPRUPR$measures.values[OPRUPR$measures.values$Threshold==p_OPRUPR,1]#OPR value at crossover
  # Get threshold p value at cross over of FPR/FNR (toolkit mismatch)
  p_FPR_FNR <- allMeasures[which.min(allMeasures$FPR_FNRdiff),20]
  valFPR <- allMeasures[which.min(allMeasures$FPR_FNRdiff),25]
  
  # identify the threshold p value that provides critical Commission value of 0.2 
  critVal3 <- 0.2
  p_critCom3 <- allMeasures[which.min(abs(allMeasures$Commission-critVal3)),"p"] 
  # identify the threshold p value that provides critical Omission value of 0.2
  
  # Add each of these to the dataframe optEach
  optEach <- rbind(optEach,list("OmisComm",p_OmisComis,valOmi,"Cross-over Omis/Commis"))
  optEach <- rbind(optEach,list("OPR_UPR",p_OPRUPR,valOPR,"Cross-over OPR/UPR"))
  optEach <- rbind(optEach,list("TKitMM",p_FPR_FNR,valFPR,"Cross-over FPR/NPR"))
  optEach <- rbind(optEach,list("Com0.1",p_critCom,critVal,"equal 0.1"))
  optEach <- rbind(optEach,list("Omi0.1",p_critOmi,critVal,"equal 0.1"))
  optEach <- rbind(optEach,list("Sens0.9",p_critSens,critVal2,"equal 0.9"))
  optEach <- rbind(optEach,list("Spec0.9",p_critSpec,critVal2,"equal 0.9"))
  optEach <- rbind(optEach,list("default",0.5,NA,NA))
  optEach <- rbind(optEach,list("Com0.2",p_critCom3,critVal3,"equal 0.2"))
  # Calculate Boundary values for each of these
  optEach <- optEach %>% mutate(Bound=getpVar(mod,"True",threshold)) 
  return (list(mod=mod,mod.pred=mod.pred,pseudo.r2=pseudo.r2,Auc=Auc,Prev=Prev,allMeasures=allMeasures,optEach=optEach,NRec=NRec,df=dframe))
}
#--------------------------------------------------------------------------------------------------
#functions used to add values of metrics for different thresholds used in summary tables
GetCom <- function(p){allMeasures[allMeasures$p==p,"Commission"]}
GetOmi <- function(p){allMeasures[allMeasures$p==p,"Omission"]}
GetKap <- function(p){allMeasures[allMeasures$p==p,"kappa"]}
GetLCL<- function(p){lcl <- round(mod.pred[which.min(abs(mod.pred$lower-p)),SE],dec)}
GetUCL <- function(p){ucl <- round(mod.pred[which.min(abs(mod.pred$upper-p)),SE],dec)}

#--------------------------------------------------------------------------------------------------

#function sort optima and identify criteria meeting proposed rule
#NEEDS FURTHER MODIFICATION TO ADD THE KAPPA TEST
IdentifyThreshold <- function(df=optEach) {df %>% arrange(df,threshold) %>% 
    mutate(TpLT90=ifelse(threshold <=0.9,1,0)) %>%            #test1 for threshold <=0.9
    mutate(TomiLT2com=ifelse(omission <2*commission,1,0)) %>% #test 2 omis < 2x comm
    mutate(Tb=paste0(TpLT90,TomiLT2com)) %>%                  #combine tests
    # select the last threshold that meet the criteria Tb=="11"
    mutate(Select=ifelse(Tb!=lead(Tb,n=1) & Tb=="11" & df==lead(df,n=1),1,0)) %>% 
    mutate(Fl=ifelse(df==lag(df,n=1),Select+lag(Select,n=1),0)) %>% 
    mutate(Select=ifelse(Select==0 & df!=lead(df,n=1)& lag(Fl,n=1)==0 & Tb=="11",1,Select)) %>% 
    mutate(Select=ifelse(is.na(df==lead(df,n=1)),ifelse(lag(Fl,n=1)<1 & Tb=="11",1,0),Select))
}
#----------------------------------------------------------------

#' Function to plot selected measures v threshold probability and mark potential critical thresholds
#'
#' @param df      data frame used (default to allMeasures)
#'
#' @return        a plot
#' @examples
#' pMeas()
#' 
pMeas <- function(df=allMeasures){
  a <- optEach$threshold[optEach$measure==meas[1]] # identify thresholds to mark
  b <- optEach$threshold[optEach$measure==meas[2]]
  c <- optEach$threshold[optEach$measure==meas[3]]
  d <- optEach$threshold[optEach$measure==meas[4]]
  e <- optEach$threshold[optEach$measure==meas[5]]
  f <- optEach$threshold[optEach$measure==meas[6]]
  wrap <- 20
  breaks=seq(0,1,.1)
  TextAuC <- paste0("AUC:",round(out$Auc,3))
  TextPrev <- paste0("Prevalence:",round(out$Prev,2))
  p1 <- ggplot(df,aes(y=kappa,x=p,colour=str_wrap("kappa",wrap)),lwd=1)+
    geom_line(lwd=1)+
    geom_line(aes(y=CCR,x=p,colour=str_wrap("Correct Classification Rate",wrap)),lwd=1)+
    labs(y="proportion",colour="measure",x="threshold prob")+
    scale_x_continuous(breaks = breaks)+
    scale_color_manual(values=c("green","blue"))+
    geom_vline(xintercept = c(a,b),lty=2)+
    annotate(geom = "text",x=c(a,b),y=1.0,label=c(" a"," b"),hjust=0)+
    annotate(geom = "text",x=0,y=1.0,label=TextAuC,size=3,hjust=0)+
    annotate(geom = "text",x=0,y=0.8,label=TextPrev,size=3,hjust=0)+
    geom_hline(yintercept = 0.21,lty=2)+
    theme_classic()+
    theme(legend.justification = "left")
  
  p2 <- ggplot(df,aes(y=FNR,x=p,colour=str_wrap("False negative (FNR)",wrap)),lwd=1)+
   geom_line(lwd=1)+
   geom_line(aes(y=FPR,x=p,colour=str_wrap("False positive (FPR)",wrap)),lwd=1)+
   labs(y="proportion",colour="measure",x="threshold prob")+
   scale_x_continuous(breaks = breaks)+
   scale_color_manual(values=c("orange","red"))+
   geom_vline(xintercept = c(c),lty=2)+
   annotate(geom = "text",x=c(c),y=1.0,label=c(" c"),hjust=0)+
   theme_classic()+
   theme(legend.justification = "left")
  
  p3 <- ggplot(df,aes(y=Omission,x=p,colour=str_wrap("False negative (omission)",wrap)),lwd=1)+
    geom_line(lwd=1)+
    geom_line(aes(y=Commission,x=p,colour=str_wrap("False positive (commission)",wrap)),lwd=1)+
    labs(y="proportion",colour="measure",x="threshold prob")+
    scale_x_continuous(breaks = breaks)+
    scale_color_manual(values=c("orange","red"))+
    geom_vline(xintercept = c(d,e,f),lty=2)+
    geom_hline(yintercept = c(0.1,0.2),lty=2)+
    annotate(geom = "text",x=c(d,e,f),y=1.0,label=c(" d", " e", " f"),hjust=0)+
    theme_classic()+
    theme(legend.justification = "left")
  
  p1+p2+p3+plot_layout(ncol = 1)
  
}

#----------------------------------------------------------------------------
# Functions to draw blank confusion matrix
pCM <- function(colName){
  Tp <- 2
  dat.bl <- data.frame(x=c(0,1),y=c(0,1))
  ggplot(dat.bl,aes(x=x,y=y))+
    geom_point(alpha=0)+
    scale_x_continuous(breaks =c(0.25,0.75),labels = str_wrap(c("Good or better","Moderate or worse"),10))+
    scale_y_continuous(breaks = c(0.25,0.75),labels = str_wrap(c("Moderate or worse","Good or better"),10),guide=guide_axis(angle = 90))+
    xlab(NULL)+
    labs(x=paste(colName,"class"),y="biota class")+
    geom_hline(yintercept = 0.6)+
    geom_vline(xintercept= 0.5)+
    theme_classic()+
  annotate("rect",xmin=0,xmax=0.5,ymin=0,ymax=0.6,fill=alpha("red",0.25))+
    annotate("rect",xmin=0,xmax=0.5,ymin=0.6,ymax=1,fill=alpha("green",0.25))+
    annotate("rect",xmin=0.5,xmax=1,ymin=0.6,ymax=1,fill=alpha("orange",0.25))+
    annotate("rect",xmin=0.5,xmax=1,ymin=0,ymax=0.6,fill=alpha("blue",0.25))
}
# with reversed x axis labels
pCMRev <- function(colName){
  Tp <- 2
  dat.bl <- data.frame(x=c(0,1),y=c(0,1))
  ggplot(dat.bl,aes(x=x,y=y))+
    geom_point(alpha=0)+
    scale_x_continuous(breaks =c(0.75,0.25),labels = str_wrap(c("Good or better","Moderate or worse"),10))+
    scale_y_continuous(breaks = c(0.25,0.75),labels = str_wrap(c("Moderate or worse","Good or better"),10),guide=guide_axis(angle = 90))+
    xlab(NULL)+
    labs(x=paste(colName,"class"),y="biota class")+
    geom_hline(yintercept = 0.6)+
    geom_vline(xintercept= 0.5)+
    theme_classic()+
    annotate("rect",xmin=0,xmax=0.5,ymin=0,ymax=0.6,fill=alpha("blue",0.25))+
    annotate("rect",xmin=0,xmax=0.5,ymin=0.6,ymax=1,fill=alpha("orange",0.25))+
    annotate("rect",xmin=0.5,xmax=1,ymin=0.6,ymax=1,fill=alpha("green",0.25))+
    annotate("rect",xmin=0.5,xmax=1,ymin=0,ymax=0.6,fill=alpha("red",0.25))
}

#------------------------------------------------------------------------
#Function draw box plot for any supporting element, called by function GetFig1c

#' Title
#'
#' @param df         data frame used
#' @param bound      boundry value
#' @param colName    name of column with values to plot
#' @param ylabel     axis label
#'
#' @return           a box plot
#'
bp.SE <- function(df,bound,colName,ylabel="biota class"){
  column <- sym(colName)
  ggplot(df,aes(x=!!column,y=factor(eval(as.name(BiotaClass)))))+
    geom_boxplot(varwidth = TRUE)+
    scale_y_discrete(labels = str_wrap(c("Moderate or worse","Good or better"),10),guide=guide_axis(angle = 90))+
    scale_x_log10()+
    geom_vline(xintercept = bound,lty=2)+
    geom_hline(yintercept = 1.5)+
    labs(y=ylabel,x=xlabel)+
    theme_classic()
}

#------------------------------------------------------------------------
#Functions draw box plots for EQR, called by function GetFig1d
#' Title
#'
#' @param df        data frame used
#' @param cat       categorical x variable
#' @param DepVar    dependent variable
#' @param colName   name of column (used for label of axis)
#'
#' @return          a plot

bp.EQR <- function(df,cat,DepVar,colName){
  ggplot(df,aes(y=!!DepVar,x=factor(cat)))+
    geom_boxplot(varwidth = TRUE)+
    scale_x_discrete(limits=rev,labels = str_wrap(c("Good or better","Moderate or worse"),10))+
    #geom_hline(yintercept = 0.6,lty=2)+
    geom_vline(xintercept = 1.5)+
    labs(y="biota EQR",x=paste(colName,"class"))+
    theme_classic()
}
# with reversed x axis for negative pressure response
bp.EQRRev <- function(df,cat,DepVar,colName){
  ggplot(df,aes(y=!!DepVar,x=factor(cat)))+
    geom_boxplot(varwidth = TRUE)+
    scale_x_discrete(labels = str_wrap(c("Moderate or worse","Good or better"),10))+
    #geom_hline(yintercept = 0.6,lty=2)+
    geom_vline(xintercept = 1.5)+
    labs(y="biota EQR",x=paste(colName,"class"))+
    theme_classic()
}
#----------------------------------------------------------------------------

#' Title  Logistic Regression Plot
#'
#' @param df        data frame with data
#' @param meas      measure used to determine boundary
#' @param colName   name of SE used
#' @param EQRVar    name of EQR variable used (default "NEQR")
#' @param adj       
#' @param dec       decimals for text on plot (default  0)
#' @param negResp   Pressure variable has negative response e.g. transparency (default = FALSE)
#' @param jith      jitter value for points (default = 0)
#'
#' @return
#' 
GetFig1a <- function(df,meas,colName,EQRVar="NEQR",adj=0,dec=2,negResp=FALSE,jith=0){
  # get critical probability threshold (p_u) specified by meas from data frame optEach 
  p_u <- optEach[optEach$measure==meas,"threshold"]
  bound_u <- round(optEach[optEach$measure==meas,"Bound"],dec)
  print(bound_u)
  LCLBound_u <- round(mod.pred[which.min(abs(mod.pred$lower-p_u)),colName],dec)
  UCLBound_u <- round(mod.pred[which.min(abs(mod.pred$upper-p_u)),colName],dec)
  column <- sym(colName)# convert text defining supporting element to symbol for plotting
  TextAuC <- paste0("AUC:",round(out$Auc,3))
  TextPrev <- paste0("Prevalence:",round(out$Prev,2))
  # Create logistic regression plot
  df <- df %>% filter(!is.na(NEQR)) %>%  # remove missing NEQR
    filter(eval(as.name(SE))>0)  #remove 0 values for SE to avoid error when using log transform
  p1 <- ggplot(df,aes(y=eval(as.name(BiotaClass)),x=!!column))+
    geom_point(position=position_jitter(height=jith,width=NULL))+
    stat_smooth(method = "glm", method.args = list(family = "binomial"),formula=y~x)+
    scale_x_log10()+
    labs(y="probability biota good", x=xlabel)+
    theme_classic()+
    geom_hline(yintercept = p_u,lty=2)+
    annotate(geom = "text",x=min(df[[colName]]),y=p_u+0.05+adj,label=paste("p=",p_u,meas),size=3,hjust=0,vjust=0)+
    geom_vline(xintercept = bound_u)+
    geom_vline(xintercept = LCLBound_u,lty=2)+
    geom_vline(xintercept = UCLBound_u,lty=2)+
    annotate(geom = "text",x=bound_u,y=0.05,label=bound_u,size=3)+
    annotate(geom = "text",x=max(df[[colName]]),y=0.70,label = TextPrev,colour = "black",size=3,hjust=1)+
    annotate(geom = "text",x=max(df[[colName]]),y=0.75,label = TextAuC,colour = "black",size=3,hjust=1)
  # add box colours
    if(negResp==FALSE){p1+
      annotate(geom = "text",x=LCLBound_u,y=0.1,label=LCLBound_u,size=3,hjust=1)+
      annotate(geom = "text",x=UCLBound_u,y=0.1,label=UCLBound_u,size=3,hjust=0)+        
      annotate("rect",xmin=min(df[[colName]]),xmax=bound_u,ymin=0,ymax=p_u,fill=alpha("red",0.2))+
      annotate("rect",xmin=min(df[[colName]]),xmax=bound_u,ymin=p_u,ymax=1,fill=alpha("green",0.2))+
      annotate("rect",xmin=bound_u,xmax=max(df[[colName]]),ymin=p_u,ymax=1,fill=alpha("orange",0.2))+
      annotate("rect",xmin=bound_u,xmax=max(df[[colName]]),ymin=0,ymax=p_u,fill=alpha("blue",0.2))
    } else {p1+
      annotate(geom = "text",x=LCLBound_u,y=0.1,label=LCLBound_u,size=3,hjust=0)+
      annotate(geom = "text",x=UCLBound_u,y=0.1,label=UCLBound_u,size=3,hjust=1)+         
      annotate("rect",xmin=min(df[[colName]]),xmax=bound_u,ymin=0,ymax=p_u,fill=alpha("blue",0.2))+
      annotate("rect",xmin=min(df[[colName]]),xmax=bound_u,ymin=p_u,ymax=1,fill=alpha("orange",0.2))+
      annotate("rect",xmin=bound_u,xmax=max(df[[colName]]),ymin=p_u,ymax=1,fill=alpha("green",0.2))+
      annotate("rect",xmin=bound_u,xmax=max(df[[colName]]),ymin=0,ymax=p_u,fill=alpha("red",0.2))  
    }
}

#' Title Confusion Matrix
#'
#' @param df        data frame with data
#' @param meas      measure used to determine boundary
#' @param colName   name of SE used
#' @param negResp   Pressure variable has negative response e.g. transparency (default = FALSE)
#'
#' @return

GetFig1b <- function(df,meas,colName,negResp=FALSE){
  # get critical probability threshold (p_u) specified by meas from data frame optEach 
  p_u <- optEach[optEach$measure==meas,"threshold"]
  # get the values of the measures for this probability  from data frame allMeasures
  Tp_u <- round(allMeasures$Tp[allMeasures$p==p_u],2) # number of true +ve values
  Fn_u <- round(allMeasures$Fn[allMeasures$p==p_u],2) # number of false -ve values
  Fp_u <- round(allMeasures$Fp[allMeasures$p==p_u],2) # number of false +ve values
  Tn_u <- round(allMeasures$Tn[allMeasures$p==p_u],2) # number of true -ve values
  FNR_u <-  round(allMeasures$FNR[allMeasures$p==p_u],2)# FNR rate
  FPR_u <-  round(allMeasures$FPR[allMeasures$p==p_u],2)# FPR rate
  Comm_u <- round(allMeasures$Commission[allMeasures$p==p_u],2)# Commission value
  Omis_u <- round(allMeasures$Omission[allMeasures$p==p_u],2)# Omission value
  UPR_u <-  round(allMeasures$UPR[allMeasures$p==p_u],2)# UPR rate
  OPR_u <-  round(allMeasures$OPR[allMeasures$p==p_u],2)# OPR rate
  column <- sym(colName)# convert text defining supporting element to symbol for plotting
  #TextAuC <- paste0("AUC:",round(out$Auc,3))
  #TextPrev <- paste0("Prevalence:",round(out$Prev,2))
  # Create annotate confusion matrix
  if(negResp==FALSE){
    pCM(colName)+
      annotate(geom="text",x=c(0,0.55,0,0.55),y=c(0.95,0.95,0.55,0.55),label=(c(paste("Tp=",Tp_u),paste("Fn=",Fn_u),paste("Fp=",Fp_u),paste("Tn=",Tn_u))),hjust=0)+
      annotate(geom="text",x=c(0.55,0.),y=c(0.85,0.45),label=(c(paste("FNR Fn/N=",FNR_u),paste("FPR Fp/N=",FPR_u))),hjust=0,size=tsize)+
      annotate(geom="text",x=c(0.55,0),y=c(0.75,0.35),label=(c(paste("Omis Fn/(Tp+Fn)=",Omis_u),paste("Comm(Fp/(Fp+Tn)=",Comm_u))),hjust=0,size=tsize)
      #annotate(geom="text",x=c(0.55,0),y=c(0.65,0.25),label=(c(paste("UPR Fn/(Tn+Fn)=",UPR_u),paste("OPR Fp/(Tp+Fp)=",OPR_u))),hjust=0,size=tsize)
  } else {
    pCMRev(colName)+
      annotate(geom="text",x=c(0.55,0,0.55,0),y=c(0.95,0.95,0.55,0.55),label=(c(paste("Tp=",Tp_u),paste("Fn=",Fn_u),paste("Fp=",Fp_u),paste("Tn=",Tn_u))),hjust=0)+
      annotate(geom="text",x=c(0,0.55),y=c(0.85,0.45),label=(c(paste("FNR Fn/N=",FNR_u),paste("FPR Fp/N=",FPR_u))),hjust=0,size=tsize)+
      annotate(geom="text",x=c(0,0.55),y=c(0.75,0.35),label=(c(paste("Omis Fn/(Tp+Fn)=",Omis_u),paste("Comm(Fp/(Fp+Tn)=",Comm_u))),hjust=0,size=tsize)
     # annotate(geom="text",x=c(0,0.55),y=c(0.65,0.25),label=(c(paste("UPR Fn/(Tn+Fn)=",UPR_u),paste("OPR Fp/(Tp+Fp)=",OPR_u))),hjust=0,size=tsize)
  }
}

#' Title   Box plot of SE showing boundary value
#' @param df        data frame with data
#' @param meas      measure used to determine boundary
#' @param colName   name of SE used
#' @param EQRVar    name of EQR variable used (default "NEQR")
#' @param adj       
#' @param dec       decimals for text on plot (default  0)
#'
#' @return

GetFig1c <- function(df,meas,colName,EQRVar="NEQR",adj=0,dec=2){
  p_u <- optEach[optEach$measure==meas,"threshold"]
  bound_u <- round(optEach[optEach$measure==meas,"Bound"],dec)
  df <- df %>% filter(!is.na(NEQR)) %>%  # remove missing NEQR
    filter(eval(as.name(SE))>0)  #remove 0 values for SE to avoid error when using log transform
  bp.SE(df,bound_u,colName)
}

#' Title  Box plot of EQR values for binary SE categories
#'
#' @param df        data frame with data
#' @param meas      measure used to determine boundary
#' @param colName   name of SE used
#' @param EQRVar    name of EQR variable used (default "NEQR")
#' @param adj       
#' @param dec       decimals for text on plot (default  0)
#' @param negResp   Pressure variable has negative response e.g. transparency (default = FALSE) 
#'
#' @return
#'
#' @examples
GetFig1d <- function(df,meas,colName,EQRVar="NEQR",adj=0,dec=2,negResp=FALSE){
  p_u <- optEach[optEach$measure==meas,"threshold"]
  bound_u <- round(optEach[optEach$measure==meas,"Bound"],dec)
  DepVar <- sym(EQRVar)# 
  # using the boundary value classify the supporting element into 2 classes 
  # and add to data frame containing the supporting element
  df <- df %>% filter(!is.na(NEQR)) %>%  # remove missing NEQR
    filter(eval(as.name(SE))>0)  #remove 0 values for SE to avoid error when using log transform
  if(negResp==FALSE){
    df <- df  %>% 
      mutate(Class_u=ifelse(df[[colName]]<bound_u,1,0)) #classify using predicted bound
    bp.EQR(df,df$Class_u,DepVar,colName)
  } else {
    df <- df  %>%  
      mutate(Class_u=ifelse(df[[colName]]<bound_u,0,1)) #classify using predicted bound
    bp.EQRRev(df,df$Class_u,DepVar,colName)
  } 
}  

# function to produce scatter plot classified by any categorical variable, example being "countryCode"
pGLM_z <- function(df,x=colName,y="NEQR",lab=xlabel,z="countryCode"){
  df <- df %>% filter(!is.na(NEQR)) %>%  # remove missing NEQR
    filter(eval(as.name(SE))>0)  #remove 0 values for SE to avoid error when using log transform
  ggplot(df,aes(y=eval(as.name(y)),x=eval(as.name(x)),colour=eval(as.name(z))))+
    geom_point()+
    scale_x_log10()+
    #geom_hline(yintercept = 0.6,lty=2)+ #EDIT re-activate if NEQR
    theme_classic(base_size = 12)+
    labs(y="biota EQR", x=lab,colour=z)
}


# function to produce scatter plot classified by any categorical variable, example being "countryCode" and fit a GLM to all data (i.e. not split by category)
pGLM_z_Alls <- function(df,x=colName,y="NEQR",lab=xlabel,z=NULL){
  df <- df %>% filter(!is.na(NEQR)) %>%  # remove missing NEQR
    filter(eval(as.name(SE))>0)  #remove 0 values for SE to avoid error when using log transform
  ggplot(df,aes(y=eval(as.name(y)),x=eval(as.name(x)),colour=eval(as.name(z))))+
    geom_point()+
    scale_x_log10()+
    geom_smooth(data=df,aes(y=eval(as.name(y)),x=eval(as.name(x))),inherit.aes = FALSE,method = "glm", method.args = list(family = "binomial"),formula=y~x)+
    #geom_hline(yintercept = 0.6,lty=2)+ #EDIT re-activate if NEQR
    theme_classic(base_size = 12)+
    labs(y="biota EQR", x=lab,colour=z)
}

# HT: function to produce scatter plot biota EQR vs SE
GetScatterPLot0 <- function(df,colName,EQRVar, dec=1){
  df <- df %>% 
    filter(!is.na(NEQR)) %>%  # remove missing NEQR
    filter(eval(as.name(SE))>0) #remove 0 values for SE to avoid error when using log transform
  
  ggplot(df,aes(y=eval(as.name(EQRVar)),x=eval(as.name(colName)), colour=Country))+
    geom_point()+
    theme_classic(base_size = 12)+
    labs(y="biota EQR", x=xlabel)
}

#HT: density plot
GetDensityPLot0 <- function(df,colName,EQSVar, dec=1){
  df <- df %>% filter(!is.na(NEQR)) #%>%  # remove missing NEQR
    #filter(eval(as.name(SE))>0)  #remove 0 values for SE to avoid error when using log transform
  
  densP<-ggplot(df,aes(x=eval(as.name(colName)), group=eval(as.name(EQSVar))))+
    geom_density(aes(fill=eval(as.name(EQSVar))),alpha=0.4)+ 
    theme_classic(base_size = 12)+
    #scale_y_continuous (limits=c(0,1))+
    labs(y="proportion sites", x=xlabel)+
    scale_fill_discrete(name='Biota Status',labels=c("Moderate or worse", "Good or better"))+
    scale_color_discrete(name='Biota Status',labels=c("Moderate or worse", "Good or better"))
    
  
  # build the data displayed on the plot.
  densP.data <- ggplot_build(densP)$data[[1]]
  
  # Note that column 'scaled' is used for plotting
  # so we extract the max density row for each group
  p.text <- lapply(split(densP.data, f = densP.data$group), function(df){
    df[which.max(df$scaled), ]
  })
  p.text <- do.call(rbind, p.text)  # we can also get p.text with dplyr.

  # now add the text layer to the plot
  densP + annotate('text', x = p.text$x, y = p.text$y,
               label = sprintf('n = %d', p.text$n), vjust = 0)
}

# HT: function to produce scatter plot biota EQR vs SE predicted boundaries
GetScatterPLot <- function(df,meas,colName,EQRVar, dec=1){
  df <- df %>% filter(!is.na(NEQR)) %>%  # remove missing NEQR
    filter(eval(as.name(SE))>0)  #remove 0 values for SE to avoid error when using log transform
  
  #get predicted SE boundaries 
  #get critical probability threshold (p_u) specified by meas from data frame optEach 
  p_u <- optEach[optEach$measure==meas,"threshold"]
  bound_u <- round(optEach[optEach$measure==meas,"Bound"],dec)
  LCLBound_u <- round(mod.pred[which.min(abs(mod.pred$lower-p_u)),colName],dec)
  UCLBound_u <- round(mod.pred[which.min(abs(mod.pred$upper-p_u)),colName],dec)

  ggplot(df,aes(y=eval(as.name(EQRVar)),x=eval(as.name(colName)), colour=Country))+
    geom_point()+
    geom_vline(xintercept = bound_u)+
    geom_vline(xintercept = LCLBound_u,lty=2)+
    geom_vline(xintercept = UCLBound_u,lty=2)+
    theme_classic(base_size = 12)+
    labs(y="biota EQR", x=xlabel)+
    annotate(geom = "text",x=bound_u,y=0.05,label=bound_u,size=3)+
    annotate(geom = "text",x=LCLBound_u,y=0.1,label=LCLBound_u,size=3,hjust=1)+
    annotate(geom = "text",x=UCLBound_u,y=0.1,label=UCLBound_u,size=3,hjust=0)
}
