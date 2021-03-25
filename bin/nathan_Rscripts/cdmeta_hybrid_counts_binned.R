#extracts count data from raw CDMetaPOP outputs


require(plyr)
require(data.table)

##############################

#inputs
direc<-paste0(getwd(),"/")
#run for all directories in cwd
runs<-list.dirs(full.names=F,recursive=F)
#specify certain runs
#runs<-c("BTDV_model4_1535805784","BTDV_model4_1535964766","BTDV_model4_1535980901",
#"BTDV_model18_1535806045","BTDV_model18_1535980901","BTDV_model18_1535981103")
#specify number of runs or allMCs=T
allMCs=T
#mcruns=c(1,1,1,1,1,1)
#specify years to process
years=c(0,20,30,40,50,51,52,53,54,55,60,70,80,81,82,83,84,85,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280)

#########################################
for (f in 1:length(runs)){
	num.extinct.MCS=0
	species.out<-list()
	runnum.out<-list()
	year.out<-list()
	subpop.out<-list()
	BT.out<-list()
	hybrids.out<-list()
	count.out<-list()
	#mature.count.out<-list()
	#age.count.out<-matrix(,nrow=24*length(mcruns[f])*length(years),ncol=5)
	rownum<-1
	loc<-paste(direc,runs[f],sep="")
	#specify all MCs
	if(allMCs==T){
	  mcruns<-length(list.dirs(loc,full.names=F,recursive=F))
	  print(paste0("Number of MCS: ",mcruns))
	}
	for(mcrun in (0:(mcruns[f]-1))){
		year_extinct=""
		#run<-paste(direc,runs[f],"/batchrun0mcrun",mcrun,sep="")
		#years<-list.files(path=loc,pattern="^ind")[-1]
		for(y in 1:length(years)){
			#year<-as.integer(strsplit(strsplit(years[y],"ind")[[1]][2],".csv")[[1]])
			#path=paste(run,"/",years[y],sep="")
			path=paste(loc,"/batchrun0mcrun",mcrun,"/ind",years[y],".csv",sep="")
			if(file.exists(path)){
				data <-fread(path,select=c("Subpopulation","mature","age","L0A0","Hindex")) # load file
				#add column for hindex bin, round to nearest 0.1
				#data$Hindex.bin<-round(data$Hindex,1)
				# count number of BT, DV, and hybrids
				# counts<-ddply(data, .(Subpopulation), summarise, BT=sum(Hindex>=0.9),Hybrids=sum(Hindex>0.1&Hindex<0.9),DV=sum(Hindex<=0.1),
				# Hybrid.1=sum(Hindex>0.1&Hindex<0.2),Hybrid.2=sum(Hindex>=0.2&Hindex<0.3),Hybrid.3=sum(Hindex>=0.3&Hindex<0.4),Hybrid.4=sum(Hindex>=0.4&Hindex<0.5),
				# Hybrid.5=sum(Hindex>=0.5&Hindex<0.6),Hybrid.6=sum(Hindex>=0.6&Hindex<0.7),Hybrid.7=sum(Hindex>=0.7&Hindex<0.8),Hybrid.8=sum(Hindex>=0.8&Hindex<0.9),Mean.Hindex=mean(Hindex))
				counts<-ddply(data, .(Subpopulation), summarise, BT=sum(Hindex>=0.9),Hybrids=sum(Hindex>0.1&Hindex<0.9),DV=sum(Hindex<=0.1),
					HybridBT=sum(Hindex>0.6&Hindex<0.9),Hybrid.5=sum(Hindex>=0.4&Hindex<=0.6),HybridDV=sum(Hindex>0.1&Hindex<=0.4),Mean.Hindex=mean(Hindex))
				}else{
				if(year_extinct==""){
					year_extinct<-years[y]
					print(paste("Pop extinct in model",runs[f],"mcrun",mcrun,"year",years[y]))
					num.extinct.MCS=num.extinct.MCS+1}
				counts<-matrix(0,nrow=276,ncol=8)
				counts[,1]<-1:276
				}
			#all.counts<-ddply(data, .(Subpopulation), summarise, BT=sum(L0A0==2),Hybrid=sum(L0A0==1),DV=sum(L0A0==0))
			#hindex.counts<-ddply(data, .(Subpopulation,Hindex.bin), summarise, length(Hindex.bin))
			#mature.data<-data[which(mature=='1'),]
			#mature.counts<-ddply(mature.data, .(Subpopulation), summarise, BT=sum(Hindex==1),Hybrids=sum(Hindex>0&Hindex<1),DV=sum(Hindex==0))
			#counts<-merge(data.frame(all.counts),data.frame(hindex.counts), by = "Subpopulation",all=TRUE)
			subpops<-counts[,1]
			counts.BT<-counts[,2]
			counts.hybrids<-counts[,3]
			counts.DV<-counts[,4]
			# counts.hybrids.1<-counts[,5]
			# counts.hybrids.2<-counts[,6]
			# counts.hybrids.3<-counts[,7]
			# counts.hybrids.4<-counts[,8]
			# counts.hybrids.5<-counts[,9]
			# counts.hybrids.6<-counts[,10]
			# counts.hybrids.7<-counts[,11]
			# counts.hybrids.8<-counts[,12]
			counts.hybridBT<-counts[,5]
			counts.hybrid.5<-counts[,6]
			counts.hybridDV<-counts[,7]
			meanhindex<-counts[,8]
			#counts.BT.mature<-counts[,5]
			#counts.hybrids.mature<-counts[,6]
			#counts.DV.mature<-counts[,7]
			total.counts<-length(subpops)*7
			subpop.out[rownum:(rownum+total.counts-1)]<-rep(subpops,7)
			#species.out[rownum:(rownum+total.counts-1)]<-c(rep("BT",length(subpops)),rep("hybrids",length(subpops)),rep("DV",length(subpops)))
			#count.out[rownum:(rownum+total.counts-1)]<-c(counts.BT,counts.hybrids,counts.DV)
			# species.out[rownum:(rownum+total.counts-1)]<-c(rep("BT",length(subpops)),rep("hybrids",length(subpops)),rep("DV",length(subpops)),
			# rep("hybrids.1",length(subpops)),rep("hybrids.2",length(subpops)),rep("hybrids.3",length(subpops)),
			# rep("hybrids.4",length(subpops)),rep("hybrids.5",length(subpops)),rep("hybrids.6",length(subpops)),
			# rep("hybrids.7",length(subpops)),rep("hybrids.8",length(subpops)),rep("meanhindex",length(subpops)))
			species.out[rownum:(rownum+total.counts-1)]<-c(rep("BT",length(subpops)),rep("hybrids",length(subpops)),rep("DV",length(subpops)),
			rep("hybridsBT",length(subpops)),rep("hybrids.5",length(subpops)),rep("hybridsDV",length(subpops)),rep("meanhindex",length(subpops)))
			count.out[rownum:(rownum+total.counts-1)]<-c(counts.BT,counts.hybrids,counts.DV,counts.hybridBT,counts.hybrid.5,counts.hybridDV,meanhindex)
			#mature.count.out[rownum:(rownum+total.counts-1)]<-c(counts.BT.mature,counts.hybrids.mature,counts.DV.mature)
			runnum.out[rownum:(rownum+total.counts-1)]<-rep(mcrun,total.counts)
			year.out[rownum:(rownum+total.counts-1)]<-rep(years[y],total.counts)
			#year.out[rownum:(rownum+total.counts-1)]<-rep(years[y],total.counts)
			#if(length(species.out)==length(subpop.out)&&length(species.out)==length(count.out)&&length(species.out)==length(mature.count.out)&&length(species.out)==length(runnum.out)&&length(species.out)==length(year.out)){
			#	rownum=rownum+(length(subpops)*3)
			#	}else{print("ERROR: output lists different sizes")}
			rownum=rownum+(length(subpops)*7)
		}
	}
	output<-data.frame(cbind(subpop.out,species.out,count.out,runnum.out,year.out))
	output<-as.matrix(output)
	colnames(output)<-c("Patch","Species","Value","Mcrun","Year")
	write.table(output, paste(loc,"/",runs[f],"_count_outputs.csv",sep=""), sep=",",row.names=F, col.names=T)
	print(paste(runs[f]," done. ",num.extinct.MCS," MCs extinct before year 280"))
}

# #extract age and size data from first MCrun
# data <-fread(paste(direc,runs[1],"/batchrun0mcrun0/","ind200.csv",sep=""),select=c("age","size","L0A0")) # load file
# write.table(data,paste(loc,"/",runs,"_growthyr200.csv",sep=""),sep=",")

