#generates plots from count data for manuscript.
#note the change from previous verions! Only showing Hybrids vs BT and DV instead of hybrid classes

setwd("C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/")

require("ggplot2")
require("reshape")
require("doBy")
require("plyr")
require("cowplot")
require("gridExtra")

#standard error function
se <- function(x) sd(x)/sqrt(length(x))

#create list of files
files<-sapply(c(1:10,18:24),function(x) paste0("BTDV_model_",x,"_ALL_counts.csv"))

#specify model labels (i.e. "scenario" is different from # in manuscript)
modelLabels<-c("M1:IntraMate,NoSelec","M2:IntraMate,LowSelec","M3:IntraMate,HighSelec","M4:RandomMate,NoSelec",
          "M5:RandomMate,LowSelec,HighHyFit","M6:RandomMate,HighSelec,HighHyFit","M7:RandomMate,LowSelec,MedHyFit",
          "M8:RandomMate,HighSelec,MedHyFit","M9:RandomMate,LowSelec,LowHyFit","M10:RandomMate,HighSelec,LowHyFit",
          "M11:SelfMate,NoSelec","M12:SelfMate,LowSelec,HighHyFit","M13:SelfMate,HighSelec,HighHyFit",
          "M14:SelfMate,LowSelec,MedHyFit","M15:SelfMate,HighSelec,MedHyFit","M16:SelfMate,LowSelec,LowHyFit",
          "M17:SelfMate,HighSelec,LowHyFit")

figures<-list()
pies<-list()
for(i in 1:length(files)){
  data<-data.frame(read.csv(files[i],sep=",",header=T))
  data$Mcrun<-as.factor(data$Mcrun)
  data$Patch<-as.factor(data$Patch)
  
  #print how many unique MCs
  MCcount<-length(unique(data$runMC))
  print(paste0("Model:",files[i]," MCs: ",MCcount))
  
  # #####patch analyses by species presence#####
  # spData<-data[which(data$Species=='BT'|data$Species=='DV'|data$Species=='hybrids'), ]
  # castedSp<-cast(spData,Patch+runMC+Year~Species,value="Value")
  # castedSp$BT[castedSp$BT>0]<-1
  # castedSp$DV[castedSp$DV>0]<-1
  # castedSp$onlyBT[castedSp$BT>0&castedSp$DV<1&castedSp$hybrids<1]<-1
  # castedSp$onlyDV[castedSp$DV>0&castedSp$BT<1&castedSp$hybrids<1]<-1
  # castedSp$overlap[castedSp$BT>0&castedSp$DV>0]<-1
  # castedSp$hybrids[castedSp$hybrids>0]<-1
  # castedSp[is.na(castedSp)]<-0
  # spPatch<-summaryBy(BT+DV+onlyBT+onlyDV+overlap+hybrids~Year+runMC,castedSp,FUN=sum)
  # avgSpData<-summaryBy(BT.sum+DV.sum+onlyBT.sum+onlyDV.sum+overlap.sum+hybrids.sum~Year,spPatch,FUN=c(mean,se))
  # avgSpData$model<-modelLabels[i]
  # names(avgSpData)<-c("Year","BT","DV","BTonly","DVonly","BTDV","Hybrids",
  #                     "BT.SE","DV.SE","BTonly.SE","DVonly.SE","BTDV.SE","Hybrids.SE","Model")
  # if(i==1){
  #   patchDat=avgSpData
  # }else{
  #   patchDat=rbind(patchDat,avgSpData)
  # }
  
  ######line graphs counts averaged over all MCs#######

  ###choose what type of lines to plot
  # ##only species counts
  # data<-data[which(data$Species=='BT'|data$Species=='hybrids'|data$Species=='DV'), ]
  # ##only hybrids
  # data<-data[which(data$Species!='BT'&data$Species!='hybrids'&data$Species!='DV'&data$Species!='meanhindex'), ]
  # ##only DV
  # data<-data[which(data$Species=='DV'), ]
  # ##BT,DV, hindex
  # data<-data[which(data$Species!='hybrids'&data$Species!='meanhindex'), ]
  ##BT,DV, hybrids
  data<-data[which(data$Species%in%c("hybrids","BT","DV")),]
  
  sumdata<-summaryBy(Value~Species+Year+runMC,data,FUN=sum)
  avgdata<-summaryBy(Value.sum~Species+Year,sumdata,FUN=c(length,mean,sd))
  # Rename column change.length to just N
  names(avgdata)<- c("Species","Year","N","Mean","SD")
  # Calculate standard error of the mean
  avgdata$SE <- avgdata$SD / sqrt(avgdata$N)
  #avgdata$Value.sum.sd[is.na(avgdata$Value.sum.sd)]<-0
  avgdata$lwr<-avgdata$Mean-(1.96*avgdata$SE)
  avgdata$upr<-avgdata$Mean+(1.96*avgdata$SE)
  avgdata$lwr[avgdata$lwr<0] <- 0
  figure<-ggplot(avgdata,aes(x=Year,y=Mean,group=Species,color=Species))+
    geom_smooth(data=avgdata,aes(ymin=lwr,ymax=upr),stat="identity")+
    geom_vline(xintercept = 50,linetype=2)+geom_vline(xintercept = 80,linetype=2)+
    theme(plot.title = element_text(hjust = 0.5))+ylim(0,75000)+
    scale_color_manual(breaks=c("BT","hybrids","DV"),
                       values=c("blue","red","yellow"),
                       labels=c("BT","Hybrid","DV"))+
    #ggtitle(labels[i])+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())
  figures[[i]]<-figure

  #####individual MCs#####
  # #only species counts
  # #data<-data[which(data$Species=='BT'|data$Species=='hybrids'|data$Species=='DV'), ]
  # #only hybrids
  # #data<-data[which(data$Species!='BT'&data$Species!='hybrids'&data$Species!='DV'&data$Species!='meanhindex'), ]
  # #only DV
  # #data<-data[which(data$Species=='DV'), ]
  # #BT,DV, hindex
  # data<-data[which(data$Species!='hybrids'&data$Species!='meanhindex'), ]
  # # #plot all MCs indivdiually
  # sumdata<-summaryBy(Value~Species+Year+runMC,data,FUN=sum)
  # #DVdat<-sumdata[which(sumdata$Species=='DV'), ]
  # colnames(sumdata)<-c("Species","Year","runMC","Value")
  # # if (i==3){
  # #   if(!is.na(DV_extinct_year)){
  # #     figure<-ggplot(sumdata,aes(x=Year,y=Value,shape=runMC,color=Species))+geom_point()+geom_line()+
  # #   geom_vline(xintercept = 50,linetype=2)+
  # #   geom_vline(xintercept = 80,linetype=2)+
  # #   annotate("text", x = 150, y = 70000, label = labels[i],size=5)+theme(legend.position="bottom")+
  # #     guides(shape=FALSE)+ylim(0,70000)+annotate("text", x = DV_extinct_year, y = 0,label="*",size=10)
  # #   #+annotate("text", x = 65, y = max(sumdata$Value), label = "Selec/Gene")
  # #   #+annotate("text", x = 95, y = max(sumdata$Value), label = "Hybrids")
  # #
  # #   }else{figure<-ggplot(sumdata,aes(x=Year,y=Value,shape=runMC,color=Species))+geom_point()+geom_line()+
  # #     geom_vline(xintercept = 50,linetype=2)+
  # #     geom_vline(xintercept = 80,linetype=2)+
  # #     annotate("text", x = 150, y = 70000, label = labels[i],size=5)+theme(legend.position="bottom")+
  # #     guides(shape=FALSE)+ylim(0,70000)
  # #   }
  # #   figures[[i]]<-figure
  # # }
}

#save patchDat file
write.csv(patchDat,"../FINAL_BTDV_COUNTS/patchData_bySpecies.csv")

#print out values from a given year (need to manually run a single loop)
# for null model (i=1)
avgdata[which(avgdata$Year==280&avgdata$Species%in%c("BT","DV")),] 
# Species Year  N    Mean       SD        SE      lwr      upr
# 38      BT  280 10 23240.2 1470.503  465.0139 22328.77 24151.63
# 76      DV  280 10 31150.0 4783.423 1512.6513 28185.20 34114.80
# for low vs high temp, random mating and med hybrid fitness (i=7 and 8)
avgdata[which(avgdata$Year==150&avgdata$Species%in%c("hybrids")),] 
#i=7
#Species Year  N    Mean       SD       SE      lwr      upr
#hybrids  150 10 30014.7 12414.59 3925.837 22320.06 37709.34
#i=8
#Species Year  N   Mean       SD       SE      lwr      upr
#hybrids  150 10 8725.8 6834.477 2161.252 4489.747 12961.85

####create plots for figures 3,4, and 5 in manuscript####
#use this code to make plots with shared legend and axis titles
setwd("C:/Users/Lucas/Desktop/BTDV/manuscript/EcoMod_2nd_submission/Figures/")

require(grid)

#Extract the legend as a separate grob using a small helper function:
# Function to extract legend
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend) }

# Extract legend as a grob
leg = g_legend(figures[[1]])

###FIGURE 3###
figure3_plots<-list(figures[[1]]+ggtitle("a) Intraspecific Mating")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[4]]+ggtitle("b) Random Mating")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[11]]+ggtitle("c) Mate Preference")+theme(plot.title = element_text(hjust = 0,face="plain")))
figure3<-grid.arrange(
  arrangeGrob(grobs=lapply(figure3_plots, function(p) p + guides(colour=FALSE)), ncol=1, 
              bottom=textGrob("Year", gp=gpar(fontsize=15)), 
              left=textGrob("Count", gp=gpar(fontsize=15), rot=90)),
  leg, 
  widths=c(9,1)
)
save_plot("../../../../manuscript/2019_01_submission_EcoMod_R2/Figures/figure3.tiff",figure3,ncol=1,nrow=3,base_aspect_ratio = 2.2,compression="lzw+p")

###FIGURE 4###
figure4_plots<-list(figures[[5]]+ggtitle("a) High Hybrid Fitness, Low Selection")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[6]]+ggtitle("b) High Hybrid Fitness, High Selection")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[7]]+ggtitle("c) Med Hybrid Fitness, Low Selection")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[8]]+ggtitle("d) Med Hybrid Fitness, High Selection")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[9]]+ggtitle("e) Low Hybrid Fitness, Low Selection")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[10]]+ggtitle("f) Low Hybrid Fitness, High Selection")+theme(plot.title = element_text(hjust = 0,face="plain")))
figure4<-grid.arrange(
  arrangeGrob(grobs=lapply(figure4_plots, function(p) p + guides(colour=FALSE)), ncol=2, 
              bottom=textGrob("Year", gp=gpar(fontsize=15)), 
              left=textGrob("Count", gp=gpar(fontsize=15), rot=90)),
  leg, 
  widths=c(9,1)
)
save_plot("../../../../manuscript/2019_01_submission_EcoMod_R2/Figures/figure4.tiff",figure4,ncol=2,nrow=3,base_aspect_ratio = 1.3,compression="lzw+p")

###FIGURE 5###
figure5_plots<-list(figures[[12]]+ggtitle("a) High Hybrid Fitness, Low Selection")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[13]]+ggtitle("b) High Hybrid Fitness, High Selection")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[14]]+ggtitle("c) Med Hybrid Fitness, Low Selection")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[15]]+ggtitle("d) Med Hybrid Fitness, High Selection")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[16]]+ggtitle("e) Low Hybrid Fitness, Low Selection")+theme(plot.title = element_text(hjust = 0,face="plain")),
                    figures[[17]]+ggtitle("f) Low Hybrid Fitness, High Selection")+theme(plot.title = element_text(hjust = 0,face="plain")))
figure5<-grid.arrange(
  arrangeGrob(grobs=lapply(figure5_plots, function(p) p + guides(colour=FALSE)), ncol=2, 
              bottom=textGrob("Year", gp=gpar(fontsize=15)), 
              left=textGrob("Count", gp=gpar(fontsize=15), rot=90)),
  leg, 
  widths=c(9,1)
)
save_plot("../../../../manuscript/2019_01_submission_EcoMod_R2/Figures/figure5.tiff",figure5,ncol=2,nrow=3,base_aspect_ratio = 1.3,compression="lzw+p")

#####figure 6: combined hybrid plot, random mating-- removed from manuscript during revisions#####
#compares total hybrids across six sceanrios with random mating
files<-c("C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_5_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_6_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_7_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_8_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_9_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_10_ALL_counts.csv")

labels<-c("Model 5","Model 6","Model 7","Model 8","Model 9","Model 10")
hyFit<-c("High","High","Med","Med","Low","Low")
selec<-c("Low","High","Low","High","Low","High")
for(i in 1:length(files)){
  data<-data.frame(read.csv(files[i],sep=",",header=T))
  data$Mcrun<-as.factor(data$Mcrun)
  data$Patch<-as.factor(data$Patch)
  ##only hybrids
  data<-data[which(data$Species=='hybrids'), ]
  data$Model<-labels[i]
  data$HybridFit<-hyFit[i]
  data$Selection<-selec[i]
  if(i==1){
    hybData<-data
  }else{
    hybData<-rbind(hybData,data)
  }
}
sumHybData<-summaryBy(Value~Year+runMC+Model+HybridFit+Selection,hybData,FUN=sum)
avgHybData<-summaryBy(Value.sum~Year+Model+HybridFit+Selection,sumHybData,FUN=c(length,mean,sd))
# Rename column change.length to just N
names(avgHybData)<- c("Year","Model","HybridFit","Selection","N","Mean","SD")
# Calculate standard error of the mean
avgHybData$SE <- avgHybData$SD / sqrt(avgHybData$N)
#avgHybData$Value.sum.sd[is.na(avgHybData$Value.sum.sd)]<-0
avgHybData$lwr<-avgHybData$Mean-(1.96*avgHybData$SE)
avgHybData$upr<-avgHybData$Mean+(1.96*avgHybData$SE)
avgHybData$lwr[avgHybData$lwr<0] <- 0
avgHybData$HybridFit<-as.factor(avgHybData$HybridFit)
avgHybData$HybridFit <- relevel(avgHybData$HybridFit, "Low")
avgHybData$HybridFit <- relevel(avgHybData$HybridFit, "Med")
avgHybData$HybridFit <- relevel(avgHybData$HybridFit, "High")
# #original figure (Sep 2018 submission)
# figure6<-ggplot(avgHybData,aes(x=Year,y=Mean))+
#   geom_smooth(data=avgHybData,aes(ymin=lwr,ymax=upr,group=Model,color=Selection,lty=HybridFit),stat="identity")+
#   geom_vline(xintercept = 50,linetype=2)+geom_vline(xintercept = 80,linetype=2)+
#   theme(plot.title = element_text(hjust = 0.5))+ylim(0,52000)+ylab("Count")+
#   scale_color_manual(values=c("blue", "black"))+scale_linetype_manual(values=c("solid","dashed","dotted"))+
#   theme(plot.title = element_text(hjust = 0,face="plain"))

#add label that uses both variables
avgHybData$Label<-as.factor(paste0(avgHybData$HybridFit," Hybrid Fitness, ",avgHybData$Selection," Selection"))
levels(avgHybData$Label)
avgHybData$Label<-factor(avgHybData$Label,levels(avgHybData$Label)[c(1,2,5,6,3,4)])
avgHybData$Label<-factor(avgHybData$Label,levels=c("High Selection","High HybFit, High Selec","Med HybFit, High Selec","Low HybFit, High Selec",
                                                   "Low Selection","High HybFit, Low Selec","Med HybFit, Low Selec","Low HybFit, Low Selec"))
figure6<-ggplot(avgHybData,aes(x=Year,y=Mean))+
  geom_smooth(data=avgHybData,aes(ymin=lwr,ymax=upr,group=Model,color=Label,lty=Label),alpha=0.2,stat="identity")+
  geom_vline(xintercept = 50,linetype=2)+geom_vline(xintercept = 80,linetype=2)+
  theme(plot.title = element_text(hjust = 0.5))+ylim(0,52000)+ylab("Count")+
  #scale_color_manual(values=c("blue", "black","blue","black","blue", "black"))+
  scale_color_manual(values=c("black","dark grey", "black","dark grey","black","dark grey"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed","dotted","dotted"))+
  theme(legend.title = element_blank(),
        legend.key.size = unit(0.5,"cm"),
        legend.position = "bottom",
        legend.spacing.x = unit(0.05, 'cm'),
        legend.text=element_text(size=8),
        legend.background = element_rect(colour = "white", fill = "white"))+
  guides(color=guide_legend(override.aes=list(fill=NA),ncol=3),
         linetype=guide_legend(override.aes=list(size=0.5)))
  
save_plot("../../../../manuscript/2019_01_submission_EcoMod_R2/Figures/figure6.tiff",figure6,base_aspect_ratio = 1.8,compression="lzw+p")






#####new figure: combined hybrid plot, self preference mating-- removed from manuscript during revisions#####
#compares total hybrids across six sceanrios with self preference mating
files<-c("C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_19_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_20_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_21_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_22_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_23_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_24_ALL_counts.csv")

labels<-c("Model 19","Model 20","Model 21","Model 22","Model 23","Model 24")
hyFit<-c("High","High","Med","Med","Low","Low")
selec<-c("Low","High","Low","High","Low","High")
for(i in 1:length(files)){
  data<-data.frame(read.csv(files[i],sep=",",header=T))
  data$Mcrun<-as.factor(data$Mcrun)
  data$Patch<-as.factor(data$Patch)
  ##only hybrids
  data<-data[which(data$Species=='hybrids'), ]
  data$Model<-labels[i]
  data$HybridFit<-hyFit[i]
  data$Selection<-selec[i]
  if(i==1){
    hybData<-data
  }else{
    hybData<-rbind(hybData,data)
  }
}
sumHybData<-summaryBy(Value~Year+runMC+Model+HybridFit+Selection,hybData,FUN=sum)
avgHybData<-summaryBy(Value.sum~Year+Model+HybridFit+Selection,sumHybData,FUN=c(length,mean,sd))
# Rename column change.length to just N
names(avgHybData)<- c("Year","Model","HybridFit","Selection","N","Mean","SD")
# Calculate standard error of the mean
avgHybData$SE <- avgHybData$SD / sqrt(avgHybData$N)
#avgHybData$Value.sum.sd[is.na(avgHybData$Value.sum.sd)]<-0
avgHybData$lwr<-avgHybData$Mean-(1.96*avgHybData$SE)
avgHybData$upr<-avgHybData$Mean+(1.96*avgHybData$SE)
avgHybData$lwr[avgHybData$lwr<0] <- 0
avgHybData$HybridFit<-as.factor(avgHybData$HybridFit)
avgHybData$HybridFit <- relevel(avgHybData$HybridFit, "Low")
avgHybData$HybridFit <- relevel(avgHybData$HybridFit, "Med")
avgHybData$HybridFit <- relevel(avgHybData$HybridFit, "High")

#add label that uses both variables
avgHybData$Label<-as.factor(paste0(avgHybData$HybridFit," Hybrid Fitness, ",avgHybData$Selection," Selection"))
levels(avgHybData$Label)
avgHybData$Label<-factor(avgHybData$Label,levels(avgHybData$Label)[c(1,2,5,6,3,4)])
newFigure<-ggplot(avgHybData,aes(x=Year,y=Mean))+
  geom_smooth(data=avgHybData,aes(ymin=lwr,ymax=upr,group=Model,color=Label,lty=Label),alpha=0.2,stat="identity")+
  geom_vline(xintercept = 50,linetype=2)+geom_vline(xintercept = 80,linetype=2)+
  theme(plot.title = element_text(hjust = 0.5))+ylim(0,55000)+ylab("Count")+
  #scale_color_manual(values=c("blue", "black","blue","black","blue", "black"))+
  scale_color_manual(values=c("black","dark grey", "black","dark grey","black","dark grey"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed","dotted","dotted"))+
  theme(legend.title = element_blank(),
        legend.key.size = unit(0.5,"cm"),
        legend.position = "bottom",
        legend.spacing.x = unit(0.05, 'cm'),
        legend.text=element_text(size=8),
        legend.background = element_rect(colour = "white", fill = "white"))+
  guides(color=guide_legend(override.aes=list(fill=NA),ncol=3),
         linetype=guide_legend(override.aes=list(size=0.5)))

save_plot("../../../../manuscript/2019_01_submission_EcoMod_R2/Figures/newFigure.tiff",newFigure,base_aspect_ratio = 1.8,compression="lzw+p")

#####new figure2: combined hybrid plot#####
#compares total hybrids across six sceanrios with random mating and six scenarios with mate preference
files<-c("C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_5_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_6_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_7_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_8_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_9_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_10_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_19_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_20_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_21_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_22_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_23_ALL_counts.csv",
         "C:/Users/Lucas/Desktop/BTDV/simulations/outputs/counts/FINAL_BTDV_COUNTS/BTDV_model_24_ALL_counts.csv")

labels<-c("Model 5","Model 6","Model 7","Model 8","Model 9","Model 10",
          "Model 12","Model 13","Model 14","Model 15","Model 16","Model 17")
hyFit<-c("High","High","Med","Med","Low","Low","High","High","Med","Med","Low","Low")
selec<-c("Low","High","Low","High","Low","High","Low","High","Low","High","Low","High")
mating<-c("Random","Random","Random","Random","Random","Random",
          "SelfPref","SelfPref","SelfPref","SelfPref","SelfPref","SelfPref")
for(i in 1:length(files)){
  data<-data.frame(read.csv(files[i],sep=",",header=T))
  data$Mcrun<-as.factor(data$Mcrun)
  data$Patch<-as.factor(data$Patch)
  ##only hybrids
  data<-data[which(data$Species=='hybrids.5'), ]
  data$Model<-labels[i]
  data$HybridFit<-hyFit[i]
  data$Selection<-selec[i]
  data$Mating<-mating[i]
  if(i==1){
    hybData<-data
  }else{
    hybData<-rbind(hybData,data)
  }
}
sumHybData<-summaryBy(Value~Year+runMC+Model+HybridFit+Selection+Mating,hybData,FUN=sum)
avgHybData<-summaryBy(Value.sum~Year+Model+HybridFit+Selection+Mating,sumHybData,FUN=c(length,mean,sd))
# Rename column change.length to just N
names(avgHybData)<- c("Year","Model","HybridFit","Selection","Mating","N","Mean","SD")
# Calculate standard error of the mean
avgHybData$SE <- avgHybData$SD / sqrt(avgHybData$N)
#avgHybData$Value.sum.sd[is.na(avgHybData$Value.sum.sd)]<-0
avgHybData$lwr<-avgHybData$Mean-(1.96*avgHybData$SE)
avgHybData$upr<-avgHybData$Mean+(1.96*avgHybData$SE)
avgHybData$lwr[avgHybData$lwr<0] <- 0
avgHybData$HybridFit<-as.factor(avgHybData$HybridFit)
avgHybData$HybridFit <- relevel(avgHybData$HybridFit, "Low")
avgHybData$HybridFit <- relevel(avgHybData$HybridFit, "Med")
avgHybData$HybridFit <- relevel(avgHybData$HybridFit, "High")
newFigure2<-ggplot(avgHybData,aes(x=Year,y=Mean))+
  geom_smooth(data=avgHybData,aes(ymin=lwr,ymax=upr,group=Model,color=Mating,lty=HybridFit),stat="identity")+
  geom_vline(xintercept = 50,linetype=2)+geom_vline(xintercept = 80,linetype=2)+
  facet_wrap(~Selection)+
  theme(plot.title = element_text(hjust = 0.5))+ylim(0,55000)+ylab("Count")+
  scale_color_manual(values=c("blue", "black"))+scale_linetype_manual(values=c("solid","dashed","dotted"))+
  theme(plot.title = element_text(hjust = 0,face="plain"))

save_plot("allF1Hybrids_models5-10_12-17.tiff",newFigure2,base_aspect_ratio = 1.8,compression="lzw+p")


#####print out patch overlap statistics for manuscript
patchDat<-read.csv("patchData_bySpecies.csv")

head(patchDat)
models<-c("M2:IntraMate,LowSelec","M3:IntraMate,HighSelec")
patchDat[which(patchDat$Year==280&patchDat$Model%in%models),c("Model","BTDV","BTDV.SE")]
models<-c("M5:RandomMate,LowSelec,HighHyFit","M7:RandomMate,LowSelec,MedHyFit","M9:RandomMate,LowSelec,LowHyFit")
patchDat[which(patchDat$Year==280&patchDat$Model%in%models),c("Model","BT","BT.SE","DV","DV.SE")]
