library(vegan)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(pheatmap)

map_p=read.csv(file="/Users/shansun/Google Drive/China/wgs/metadata_wgs.csv",row.names=1)
bins_info2=read.csv(file="/Users/shansun/Google\ Drive/China/wgs/bins/bins_info_wtax.csv",row.names=1)
bins_info2$t1[which(is.na(bins_info2$t1))]=52

map_p$t1c2=paste(map_p$t1,map_p$urban_c2,sep="_")
map_p$t1c2=factor(map_p$t1c2,levels=c("43_0", "43_1", "52_0", "52_1"))
urban_i=na.omit(data.frame(cbind(map_p$index15,c("Rural","Urban")[factor(map_p$urban_c2)])))
urban_i[,1]=as.numeric(as.character(urban_i[,1]))
colnames(urban_i)=c("UrbanizationIndex","Group")
urban_i[,2]=factor(urban_i[,2],levels=c("Rural","Urban"))
wilcox.test(urban_i[,1]~urban_i[,2])
"W = 0, p-value < 2.2e-16"
#figure1
colr1=adjustcolor(c("tomato","steelblue"),alpha=1)
colr2=adjustcolor(c("tomato","steelblue"),alpha=0.5)

mash_d=na.omit(data.frame(cbind(bins_info2$dis.x,bins_info2$Completeness,bins_info2$Contamination,c("Rural","Urban")[factor(bins_info2$c2)])))
mash_d[,1]=as.numeric(as.character(mash_d[,1]))
mash_d[,2]=as.numeric(as.character(mash_d[,2]))
mash_d[,3]=as.numeric(as.character(mash_d[,3]))
mash_d[,4]=factor(mash_d[,4],levels=c("Rural","Urban"))
colnames(mash_d)=c("Distance","Completeness","Contamination","Group")
wilcox.test(mash_d[,1]~mash_d[,4])
"	Wilcoxon rank sum test with continuity correction

data:  mash_d[, 1] by mash_d[, 4]
W = 914721, p-value = 3.814e-13
alternative hypothesis: true location shift is not equal to 0"
wilcox.test(mash_d[,2]~mash_d[,4])
"W = 751184, p-value = 0.07662"
wilcox.test(mash_d[,3]~mash_d[,4])
"W = 803593, p-value = 0.2585"

pdf("/Users/Shansun/Google\ Drive/MAG_manu/mash_cc_t1c2.pdf",height=3,width=8)
par(mfrow=c(1,4),mar=c(5,5,2,2))
boxplot(urban_i[,1]~urban_i[,2],border=colr1,ylab="Urbanization index",las=1,cex.axis=1,main="P < 2.2e-16")
stripchart(urban_i[,1]~urban_i[,2], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)

boxplot(mash_d[,1]~mash_d[,4],border=colr1,ylab="Mash distance",las=1,cex.axis=1,main="P = 3.81e-13")
stripchart(mash_d[,1]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)

boxplot(mash_d[,2]~mash_d[,4],border=colr1,ylab="Completeness",las=1,cex.axis=1,main="P = 0.07")
stripchart(mash_d[,2]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)

boxplot(mash_d[,3]~mash_d[,4],border=colr1,ylab="Contamination",las=1,cex.axis=1,main="P = 0.26")
stripchart(mash_d[,3]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
dev.off()


mash_d=na.omit(data.frame(cbind(bins_info2$size,bins_info2$num_orfs,bins_info2$num_annots,bins_info2$annot_ratio,c("Rural","Urban")[factor(bins_info2$c2)])))
mash_d[,1]=log10(as.numeric(as.character(mash_d[,1])))
mash_d[,2]=log10(as.numeric(as.character(mash_d[,2])))
mash_d[,3]=log10(as.numeric(as.character(mash_d[,3])))
mash_d[,4]=as.numeric(as.character(mash_d[,4]))
mash_d[,5]=factor(mash_d[,5],levels=c("Rural","Urban"))
colnames(mash_d)=c("size","num_orfs","num_annotations","annotation_ratio","Group")
wilcox.test(mash_d[,1]~mash_d[,5])
"W = 135075, p-value = 0.002833"
wilcox.test(mash_d[,2]~mash_d[,5])
"W = 138708, p-value = 0.02169"
wilcox.test(mash_d[,3]~mash_d[,5])
"W = 133452, p-value = 0.0009893"
wilcox.test(mash_d[,4]~mash_d[,5])
"W = 142307, p-value = 0.1069"


pdf("/Users/Shansun/Google\ Drive/MAG_manu/orfs_t1c2.pdf",height=3,width=8)
par(mfrow=c(1,4),mar=c(5,5,2,2))
boxplot(mash_d[,1]~mash_d[,5],border=colr1,ylab="log10(genome size)",las=1,cex.axis=1,main="P = 0.0028")
stripchart(mash_d[,1]~mash_d[,5], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
boxplot(mash_d[,2]~mash_d[,5],border=colr1,ylab="log10(num_orfs)",las=1,cex.axis=1,main="P = 0.022")
stripchart(mash_d[,2]~mash_d[,5], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
boxplot(mash_d[,3]~mash_d[,5],border=colr1,ylab="log10(num_annotations)",las=1,cex.axis=1,main="P = 0.00099")
stripchart(mash_d[,3]~mash_d[,5], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
boxplot(mash_d[,4]~mash_d[,5],border=colr1,ylab="annotation_ratio",las=1,cex.axis=1,main="P = 0.11")
stripchart(mash_d[,4]~mash_d[,5], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
dev.off()

#pangenome prevalence and abudance
pangenome=read.table(file="/Users/shansun/Google\ Drive/China/wgs/bins/map_batch/pangenome/all.tab",sep="\t",header=T)
pangenome_relabd1=pangenome[,seq(3,ncol(pangenome),8)]
pangenome_relabd=apply(pangenome_relabd1,1,as.character)
pangenome_relabd=apply(pangenome_relabd,1,as.numeric)
rownames(pangenome_relabd)=pangenome[,1]
colnames(pangenome_relabd)=sapply(strsplit(colnames(pangenome_relabd1),"\\_"), "[[", 1)
colnames(pangenome_relabd)=sapply(strsplit(colnames(pangenome_relabd),"\\."), "[[", 1)

pan_shannon=vegan::diversity(t(pangenome_relabd),index="shannon")
wilcox.test(pan_shannon~map_p$urban_c2)
t.test(pan_shannon~map_p$urban_c2)
boxplot(pan_shannon~map_p$urban_c2)

pangenome_un=pangenome_relabd[grep("group",rownames(pangenome_relabd)),]
apply(pangenome_un,2,function(i){length(which(i!=0))})
sort(colSums(pangenome_un))

100-mean(pangenome_relabd[1,])#65.26643
mean(colSums(pangenome_un))#17.67079
100-mean(colSums(pangenome_un))-mean(pangenome_relabd[1,])#47.59565

pangenome_relabd2=pangenome[,seq(2,ncol(pangenome),8)]
pangenome_un=pangenome_relabd2[grep("group",rownames(pangenome_relabd)),]
sort(colSums(pangenome_un))

#prevalence
pangenome_prev_all=apply(pangenome_relabd[-1,],1, function(i){length(which(i!=0))})/214
pangenome_prev0=apply(pangenome_relabd[-1,map_p$urban_c2==0],1, function(i){length(which(i!=0))})/113
pangenome_prev1=apply(pangenome_relabd[-1,map_p$urban_c2==1],1, function(i){length(which(i!=0))})/101
pangenome_prev=data.frame(cbind(pangenome_prev0,pangenome_prev1))
colnames(pangenome_prev)=c("rural","urban")
pangenome_prev$tax=sapply(strsplit(sapply(strsplit(rownames(pangenome_prev),"_f__"),"[",1),"o__"),"[",2)
pangenome_prev$tax[pangenome_prev$tax%in%names(which(table(pangenome_prev$tax)<5))]="other"

pdf("/Users/Shansun/Google\ Drive/MAG_manu/Pangenome_revalence_novel_cor.pdf",height=6,width=6)
plot(pangenome_prev[,1],pangenome_prev[,2],col="lightgrey",pch=16,xlab="Rural prevalence",ylab="Urban prevalence")
points(pangenome_prev[grep("group",rownames(pangenome_prev)),1],pangenome_prev[grep("group",rownames(pangenome_prev)),2],col="red",pch=16)
abline(0,1)
dev.off()

pan_phylum=sapply(strsplit(sapply(strsplit(rownames(pangenome_prev),"_c__"),"[",1),"p__"),"[",2)
pan_class=sapply(strsplit(sapply(strsplit(rownames(pangenome_prev),"_o__"),"[",1),"c__"),"[",2)
pan_order=sapply(strsplit(sapply(strsplit(rownames(pangenome_prev),"_f__"),"[",1),"o__"),"[",2)
pan_family=sapply(strsplit(sapply(strsplit(rownames(pangenome_prev),"_g__"),"[",1),"f__"),"[",2)
pan_genus=sapply(strsplit(sapply(strsplit(rownames(pangenome_prev),"_s__"),"[",1),"g__"),"[",2)

pan_tax=cbind(pan_phylum,pan_class,pan_order,pan_family,pan_genus)
for (i in 1:5){
  pan_tax[,i][which(pan_tax[,i]%in%names(which(table(pan_tax[,i])<9)))]=NA
}

col11=c("lightblue","lightpink","orange","blue","hotpink","black","deeppink","darkgreen","darkcyan","gold","brown","purple","darkorchid4","darkolivegreen1","darkred","darkblue","darkorange")

for (i in 1:5){
  pan_tax1=factor(pan_tax[,i],levels=names(sort(table(pan_tax[,i]),decreasing = T)))
  pdf(paste0("/Users/Shansun/Google\ Drive/MAG_manu/Pangenome_revalence_novel_cor_L",i,".pdf"),height=6,width=6)
  plot(pangenome_prev[,1],pangenome_prev[,2],col="lightgrey",pch=16,xlab="Rural prevalence",ylab="Urban prevalence")
  points(pangenome_prev[,1],pangenome_prev[,2],col=col11[factor(pan_tax1)],pch=16)
  abline(0,1)
  legend("topleft",levels(factor(pan_tax1)),col=col11[1:length(levels(factor( pan_tax1)))],pch=16,cex=0.6)
  dev.off()
}

col11=c("red","lightblue","blue","orange","lightpink","violet","black","deeppink","darkgreen","darkcyan","yellow","brown","purple","darkorchid4","darkolivegreen1","cyan","darkblue","darkorange")
pan_tax1=factor(pan_genus,levels=names(sort(table(pan_genus),decreasing = T)[1:length(col11)]))
pdf(paste0("/Users/Shansun/Google\ Drive/MAG_manu/Pangenome_revalence_novel_cor_L5a.pdf"),height=6,width=6)
plot(pangenome_prev[,1],pangenome_prev[,2],col="lightgrey",pch=16,xlab="Rural prevalence",ylab="Urban prevalence")
points(pangenome_prev[,1],pangenome_prev[,2],col=col11[factor(pan_tax1)],pch=16)
abline(0,1)
legend("topleft",levels(factor(pan_tax1)),col=col11[1:length(levels(factor( pan_tax1)))],pch=16,cex=0.6)
dev.off()


pangenome_prev$genus=sapply(strsplit(sapply(strsplit(rownames(pangenome_prev),"_s__"),"[",1),"g__"),"[",2)
table(paste(pangenome_prev$genus,pangenome_prev$novel))

boxplot(apply(pangenome_relabd[-1,],2, function(i){length(which(i!=0))})~map_p$urban_c2)

pangenome_prev$novel=rep(0,447)
pangenome_prev$novel[grep("group",sapply(strsplit(rownames(pangenome_prev),"s__"),"[",2))]=1

pangenome_prev_m=melt(pangenome_prev,measure.vars = c("rural","urban"),id.vars=c("tax","novel"))

par(mar=c(15,5,5,5),mfrow=c(1,1))
boxplot(pangenome_prev_m$value~paste(pangenome_prev_m$tax,pangenome_prev_m$variable,pangenome_prev_m$novel),las=2)
stripchart(pangenome_prev_m$value~paste(pangenome_prev_m$tax,pangenome_prev_m$variable,pangenome_prev_m$novel), vertical=T, method="jitter",  add=TRUE, pch=16,col=c("red","darkred","blue","darkblue"))
boxplot(pangenome_prev_m$value~paste(pangenome_prev_m$tax,pangenome_prev_m$novel),las=2)
boxplot(pangenome_prev_m$value~paste(pangenome_prev_m$tax,pangenome_prev_m$variable),las=2)
boxplot(pangenome_prev_m$value~pangenome_prev_m$variable,las=2)
boxplot(pangenome_prev_m$value~pangenome_prev_m$novel,las=2)
boxplot(pangenome_prev_m$value~paste(pangenome_prev_m$variable,pangenome_prev_m$novel),las=2)

mean(colSums(pangenome_relabd[-1,which(map_p$urban_c2==0)])) #65.4
mean(colSums(pangenome_relabd[-1,which(map_p$urban_c2==1)])) #65.0
mean(colSums(pangenome_relabd[grep("group",rownames(pangenome_relabd)),which(map_p$urban_c2==0)])) #19.1
mean(colSums(pangenome_relabd[grep("group",rownames(pangenome_relabd)),which(map_p$urban_c2==1)])) #16.0

sd(colSums(pangenome_relabd[-1,which(map_p$urban_c2==0)])) #9.38
sd(colSums(pangenome_relabd[-1,which(map_p$urban_c2==1)])) #11.48
sd(colSums(pangenome_relabd[grep("group",rownames(pangenome_relabd)),which(map_p$urban_c2==0)])) #8.05
sd(colSums(pangenome_relabd[grep("group",rownames(pangenome_relabd)),which(map_p$urban_c2==1)])) #7.87

pangenome_relabd_n0=pangenome_relabd[grep("group",rownames(pangenome_relabd)),which(map_p$urban_c2==0)]
pangenome_relabd_n1=pangenome_relabd[grep("group",rownames(pangenome_relabd)),which(map_p$urban_c2==1)]
pangenome_prev_n0=apply(pangenome_relabd_n0,1,function(i){length(which(i!=0))})/113
pangenome_prev_n1=apply(pangenome_relabd_n1,1,function(i){length(which(i!=0))})/101
pangenome_relabd_k0=pangenome_relabd[!grepl("group",rownames(pangenome_relabd)),which(map_p$urban_c2==0)]
pangenome_relabd_k1=pangenome_relabd[!grepl("group",rownames(pangenome_relabd)),which(map_p$urban_c2==1)]
pangenome_prev_k0=apply(pangenome_relabd_k0,1,function(i){length(which(i!=0))})/113
pangenome_prev_k1=apply(pangenome_relabd_k1,1,function(i){length(which(i!=0))})/101
pangenome_prev_k0=pangenome_prev_k0[-1]
pangenome_prev_k1=pangenome_prev_k1[-1]

mean(pangenome_prev_k0)#0.3110038
mean(pangenome_prev_k1)#0.2630665
mean(pangenome_prev_n0)#0.2804316
mean(pangenome_prev_n1)#0.200293

nk_c2_lab=c(rep("Known_Rural",251),rep("Known_Urban",251),rep("Novel_Rural",196),rep("Novel_Urban",196))
pan_prev=data.frame(cbind(as.numeric(c(pangenome_prev_k0,pangenome_prev_k1,pangenome_prev_n0,pangenome_prev_n1)),nk_c2_lab))
pan_prev[,1]=as.numeric(as.character(pan_prev[,1]))
colnames(pan_prev)=c("Prevalence","Group")
pdf("/Users/Shansun/Google\ Drive/MAG_manu/Pan_prevalence_c2_novel.pdf",height=5,width=6)
ggboxplot(pan_prev, x = "Group", y = "Prevalence", color = "Group", palette = c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
          ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                                     panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=8),
                                     axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
dev.off()

levels1=levels(factor(pan_prev[,2]))
TukeyHSD(aov(pan_prev[,1]~factor(pan_prev[,2])))
wilcox.test(pan_prev[pan_prev[,2]==levels1[3],1],pan_prev[pan_prev[,2]==levels1[4],1])
aggregate(pan_prev[,1]~factor(pan_prev[,2]),FUN=mean)


pangenome_prev_un0=pangenome_prev[grep("group",rownames(pangenome_prev)),][which(pangenome_prev[grep("group",rownames(pangenome_prev)),1]==0),]
pangenome_prev_un1=pangenome_prev[grep("group",rownames(pangenome_prev)),][which(pangenome_prev[grep("group",rownames(pangenome_prev)),2]==0),]
dim(pangenome_prev_un0) #1
dim(pangenome_prev_un1) #26

pangenome_prev0=pangenome_prev[which(pangenome_prev[,1]==0),]
pangenome_prev1=pangenome_prev[which(pangenome_prev[,2]==0),]
dim(pangenome_prev0) #2
dim(pangenome_prev1) #42
dim(pangenome_prev[which(pangenome_prev[,1]!=0),])
dim(pangenome_prev[which(pangenome_prev[,2]!=0),])

map=read.csv(file="/Users/shansun/Google\ Drive/China/Metadata_01082019/metadata_subset_01082019.csv",row.names=1, header=T)
map_p=map[match(colnames(pangenome_relabd),rownames(map)),]

pvalues=matrix(nrow=nrow(pangenome_relabd),ncol=12)
for (n in 1:448){
  ttest1=wilcox.test(pangenome_relabd[n,which(map_p$urban_c2==0)],pangenome_relabd[n,which(map_p$urban_c2==1)])
  ttest2=t.test(pangenome_relabd[n,which(map_p$urban_c2==0)],pangenome_relabd[n,which(map_p$urban_c2==1)])
  pvalues[n,1]=ttest2$statistic
  pvalues[n,2]=ttest1$p.value
  
  ttest1=wilcox.test(pangenome_relabd[n,which(map_p$t2==1)],pangenome_relabd[n,which(map_p$t2==2)])
  ttest2=t.test(pangenome_relabd[n,which(map_p$t2==1)],pangenome_relabd[n,which(map_p$t2==2)])
  pvalues[n,4]=ttest2$statistic
  pvalues[n,5]=ttest1$p.value
  
  ttest1=wilcox.test(pangenome_relabd[n,which(map_p$t1==43)],pangenome_relabd[n,which(map_p$t1==52)])
  ttest2=t.test(pangenome_relabd[n,which(map_p$t1==43)],pangenome_relabd[n,which(map_p$t1==52)])
  pvalues[n,7]=ttest2$statistic
  pvalues[n,8]=ttest1$p.value
  
  pvalues[n,10]=summary(lm(pangenome_relabd[n,]~map_p$t1*map_p$urban_c2))$coef[4,4]
  
  pvalues[n,12]=mean(pangenome_relabd[n,which(map_p$urban_c2==0)])/mean(pangenome_relabd[n,which(map_p$urban_c2==1)])
  
}
rownames(pvalues)=pangenome[,1]
pvalues=data.frame(pvalues)
pvalues[,3]=p.adjust(pvalues[,2],method="fdr")
pvalues[,6]=p.adjust(pvalues[,5],method="fdr")
pvalues[,9]=p.adjust(pvalues[,8],method="fdr")
pvalues[,11]=p.adjust(pvalues[,10],method="fdr")
pvalues=pvalues[order(pvalues[,3]),]
head(pvalues)
length(which(apply(pvalues[,c(3,6,9)],1,min)<0.1)) #231
length(which(pvalues[,3]<0.1))#155
length(which(pvalues[,6]<0.1))#7
length(which(pvalues[,9]<0.1))#174

length(which(pvalues[,3]<0.1&pvalues[,11]<0.1))#0
length(which(pvalues[,11]<0.1))
write.csv(pvalues,file="/Users/Shansun/Google\ Drive/MAG_manu/rel_abd_test.csv")

length(which(pvalues[,3]<0.1&pvalues[,1]<0))#22
length(which(pvalues[,3]<0.1&pvalues[,1]>0))#133
length(which(grepl("group",rownames(pvalues))&pvalues[,3]<0.1&pvalues[,1]>0))#70
length(which(grepl("group",rownames(pvalues))&pvalues[,3]<0.1&pvalues[,1]<0))#6

which(pvalues[,3]<0.1&pvalues[,1]<0,T)
write.csv(pvalues[which(pvalues[,3]<0.1&pvalues[,1]<0),],file="/Users/Shansun/Google\ Drive/MAG_manu/urban_enriched_pangenomes.csv")
write.csv(pvalues[which(pvalues[,3]<0.1&pvalues[,1]>0),],file="/Users/Shansun/Google\ Drive/MAG_manu/rural_enriched_pangenomes.csv")


col11=c("red","blue","orange","violet","black","deeppink","darkgreen","darkcyan","brown","purple","darkolivegreen1","cyan","darkblue","darkorange")
pvalues$novel=rep(0,448)
pvalues$novel[grep("group",sapply(strsplit(rownames(pvalues),"s__"),"[",2))]=1
pvalues$phylum=sapply(strsplit(sapply(strsplit(rownames(pvalues),"_c__"),"[",1),"p__"),"[",2)
pvalues$class=sapply(strsplit(sapply(strsplit(rownames(pvalues),"_o__"),"[",1),"c__"),"[",2)
pvalues$order=sapply(strsplit(sapply(strsplit(rownames(pvalues),"_f__"),"[",1),"o__"),"[",2)
pvalues$family=sapply(strsplit(sapply(strsplit(rownames(pvalues),"_g__"),"[",1),"f__"),"[",2)
pvalues$genus=sapply(strsplit(sapply(strsplit(rownames(pvalues),"_s__"),"[",1),"g__"),"[",2)
pvalues$species=sapply(strsplit(rownames(pvalues),"_s__"),"[",2)

pvalues$family1=sapply(strsplit(rownames(pvalues),"_g__"),"[",1)
pvalues$genus1=sapply(strsplit(rownames(pvalues),"_s__"),"[",1)
pvalues$species1=rownames(pvalues)


pvalues$color_group=pvalues$order
pvalues$color_group[pvalues$color_group%in%names(which(table(pvalues$order)<10))]="other"
pvalues=pvalues[order(pvalues$color_group),]
pvalues$color_group=droplevels(factor(pvalues$color_group))
pvalues$color=col11[factor(pvalues$color_group)]
pvalues$color1=pvalues$color
pvalues$color1[pvalues[,3]>0.1]=adjustcolor(pvalues$color1[pvalues[,3]>0.1], alpha.f = 0.3)
pvalues$shape=c(1,16)[factor(pvalues$novel)]
pvalues$FDR_d=-log10(pvalues[,3])*sign(pvalues[,1])
sort(table(paste(pvalues$order[pvalues$FDR_d>1],pvalues$novel[pvalues$FDR_d>1])))
sort(table(paste(pvalues$order[pvalues$FDR_d<(-1)],pvalues$novel[pvalues$FDR_d<(-1)])))

#FDR_d=log2(pvalues1[,12])
#FDR_d[pvalues1[,3]>0.1]=NA

pdf("/Users/shansun/Google\ Drive/China/wgs/bins/genome_logFDR.pdf",height=15,width=10)
par(mfrow=c(3,1),mar=c(5,5,5,5))
pvalues1=pvalues[pvalues$novel==1,]
FDR_d=-log10(pvalues1[,3])*sign(pvalues1[,1])

plot(FDR_d,col=pvalues1$color1,pch=pvalues1$shape,ylab="directional log10(FDR)",cex=1.5,cex.lab=1.5)
abline(h=1,lty=2)
abline(h=0)
abline(h=-1,lty=2)

pvalues1=pvalues[pvalues$novel==0,]
FDR_d=-log10(pvalues1[,3])*sign(pvalues1[,1])

plot(FDR_d,col=pvalues1$color1,pch=pvalues1$shape,ylab="directional log10(FDR)",cex=1.5,cex.lab=1.5)
abline(h=1,lty=2)
abline(h=0)
abline(h=-1,lty=2)

plot.new()
legend("topleft",c(levels(factor(pvalues$color_group)),"novel","known"),col=c(col11[1:11],"black","black"),pch=c(rep(16,11),1),cex=1.5)
dev.off()

FDR_d=-log10(pvalues[,3])*sign(pvalues[,1])
pdf("/Users/shansun/Google\ Drive/China/wgs/bins/genome_logFDR_all.pdf",height=15,width=10)
par(mfrow=c(2,1))
plot(FDR_d,col=pvalues$color,pch=pvalues$shape,ylab="directional log10(FDR)",cex=1.2)
abline(h=1,lty=2)
abline(h=0)
abline(h=-1,lty=2)
plot.new()
legend("topleft",c(levels(factor(pvalues$color_group)),"novel","in reference"),col=c(col11[1:11],"black","black"),pch=c(rep(16,12),1))
dev.off()

bin_tab=read.csv(file="/Users/shansun/Google\ Drive/China/wgs/bins/map/all.csv",header=T)
rownames(bin_tab)=bin_tab[,1]
bin_tab=bin_tab[,-1]
bin_relabd1=bin_tab[,seq(1,ncol(bin_tab),6)]
bin_relabd=apply(bin_relabd1,1,as.character)
bin_relabd=apply(bin_relabd,1,as.numeric)

colnames(bin_relabd)=sapply(strsplit(colnames(bin_relabd1),"_"),"[[",1)
colnames(bin_relabd)=sapply(strsplit(colnames(bin_relabd),"\\."),"[[",1)
bin_relabd=bin_relabd[,match(colnames(pangenome_relabd),colnames(bin_relabd))]

plot(pangenome_relabd[1,],bin_relabd[1,])
100-mean(bin_relabd[1,])#62.48786
100-mean(pangenome_relabd[1,])#65.26643

colnames(pangenome_relabd)=sapply(strsplit(colnames(pangenome_relabd1),"\\_"), "[[", 1)
colnames(pangenome_relabd)=sapply(strsplit(colnames(pangenome_relabd),"\\."), "[[", 1)


#functions
K_tab=read.csv(file="/Users/shansun/Google Drive/China/wgs/bins/K_tab.csv",row.names=1)
K_tab[is.na(K_tab)]=0
num_gene=apply(K_tab,2,function(i){length(which(i!=0))})

K_tab1=data.frame(t(K_tab))
K_tab1$c2=bins_info2$c2[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$t2=bins_info2$t2[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$t1=bins_info2$t1[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$novel=bins_info2$novel[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$tax=bins_info2$fastani_group[match(rownames(K_tab1),bins_info2[,1])]

boxplot(num_gene~K_tab1$c2)
wilcox.test(num_gene~K_tab1$c2)
"W = 133484, p-value = 0.001011"
t.test(num_gene~K_tab1$c2)
"t = -1.4487, df = 1035.6, p-value = 0.1477"
median(num_gene[K_tab1$c2==0])
median(num_gene[K_tab1$c2==1])
mean(num_gene[K_tab1$c2==0])
mean(num_gene[K_tab1$c2==1])


K_c2_num0_list=apply(K_tab1,2,function(j){aggregate(j~K_tab1$c2,FUN=function(i){length(which(i!=0))})})
K_c2_num0=data.frame(matrix(unlist(K_c2_num0_list), nrow=length(K_c2_num0_list), byrow=T))
rownames(K_c2_num0)=names(K_c2_num0_list)
K_c2_num0=K_c2_num0[-c(7443:7446),]
K_c2_num0$c2_0=K_c2_num0[,3]/602
K_c2_num0$c2_1=K_c2_num0[,4]/501
K_c2_num0$ratio=K_c2_num0$c2_0/K_c2_num0$c2_1
K_c2_num0$diff=K_c2_num0$c2_0-K_c2_num0$c2_1
K_c2_num0=K_c2_num0[order(K_c2_num0$ratio),]
K_c2_num0=K_c2_num0[order(K_c2_num0$diff),]
head(K_c2_num0)

#fisher test for presence and c2 in KEGG
P_K_c2=c()
odds_K_c2=c()
for (i in 1:dim(K_c2_num0)[1]){
  c2_abs <-matrix(c(K_c2_num0[i,3], K_c2_num0[i,4], 602-K_c2_num0[i,3], 501-K_c2_num0[i,4]),nrow = 2,
                  dimnames = list(c2 = c("rural", "urban"),abs = c("absent", "present")))
  P_K_c2[i]=fisher.test(c2_abs,conf.int = TRUE)$p.value
  odds_K_c2[i]=fisher.test(c2_abs,conf.int = TRUE)$estimate
}
fdr_K_c2=p.adjust(P_K_c2,method="fdr")
stat_K_c2=cbind(odds_K_c2,P_K_c2,fdr_K_c2)
rownames(stat_K_c2)=rownames(K_c2_num0)
stat_K_c2=stat_K_c2[order(stat_K_c2[,3]),]
head(stat_K_c2)
rownames(stat_K_c2)[stat_K_c2[,3]<0.1]
write.csv(stat_K_c2,file="/Users/shansun/Google\ Drive/China/wgs/bins/stat_KEGG_c2_fisher.csv")

kegg_cat=read.table(file="/Users/shansun/Google\ Drive/China/wgs/bins/kegg_cat.txt",sep="\t")
#kegg_cat=kegg_cat[!grepl("09181",kegg_cat$V2),]
#kegg_cat=kegg_cat[!grepl("09182",kegg_cat$V2),]
#kegg_cat=kegg_cat[!grepl("09183",kegg_cat$V2),]

kegg_annot=read.csv(file="/Users/shansun/Google\ Drive/China/wgs/bins/KEGG_annotation.csv",header=F)

kegg_cat_sig=kegg_cat[match(rownames(stat_K_c2)[stat_K_c2[,3]<0.1],kegg_cat[,4]),]
kegg_annot_sig=kegg_annot[match(rownames(stat_K_c2)[stat_K_c2[,3]<0.1],as.character(kegg_annot[,1])),]
colnames(kegg_annot_sig)=c("KEGG","Annotation")

table(kegg_cat_sig[,2])
sort(table(kegg_cat_sig[,3]))
sort(table(kegg_cat_sig[,4]))
K_c2_num0_sig=K_c2_num0[match(rownames(stat_K_c2)[stat_K_c2[,3]<0.1],rownames(K_c2_num0)),]
stat_K_c2_sig_cat=cbind(K_c2_num0_sig,stat_K_c2[stat_K_c2[,3]<0.1,],kegg_cat_sig[,1:3],kegg_annot_sig[,2])
write.csv(stat_K_c2_sig_cat,file="/Users/shansun/Google\ Drive/China/wgs/bins/stat_KEGG_c2_fisher_sig_cat.csv")
stat_K_c2_sig_cat=read.csv(file="/Users/shansun/Google\ Drive/China/wgs/bins/stat_KEGG_c2_fisher_sig_cat.csv")

head(rownames(stat_K_c2_sig_cat)[order(stat_K_c2_sig_cat$odds_K_c2)])
tail(rownames(stat_K_c2_sig_cat)[order(stat_K_c2_sig_cat$odds_K_c2)])

head(sort(stat_K_c2_sig_cat$odds_K_c2))
tail(sort(stat_K_c2_sig_cat$odds_K_c2))

pdf("/Users/shansun/Google\ Drive/China/wgs/bins/gene_fisher_logFDR.pdf",height=12,width=10)
par(mfrow=c(2,1),mar=c(5,5,5,5))
col11=c("red","blue","green","orange","purple","pink","lightgreen","hotpink","cyan","orchid","tan","gold","darkblue","darkgreen","yellow","brown","lightblue","grey")
stat_K_c2_sig_cat$cat_major=as.character(stat_K_c2_sig_cat$V2)
stat_K_c2_sig_cat$cat_major[stat_K_c2_sig_cat$cat_major%in%names(which(table(droplevels(stat_K_c2_sig_cat$V2))<5))]="Other"
stat_K_c2_sig_cat$col1=col11[factor(stat_K_c2_sig_cat$cat_major)]
stat_K_c2_sig_cat=stat_K_c2_sig_cat[order(stat_K_c2_sig_cat$cat_major),]
plot(log2(stat_K_c2_sig_cat$odds_K_c2),col=stat_K_c2_sig_cat$col1,pch=16,ylab="log2(Odds Ratio)",cex=1.5,cex.lab=1.5,main="Differential Functional Genes")
abline(h=0)

plot.new()
legend("topleft",levels(factor(stat_K_c2_sig_cat$cat_major)),col=col11,pch=16,cex=0.8,bty="n")
dev.off()

pdf("/Users/shansun/Google\ Drive/China/wgs/bins/gene_fisher_logFDR_V3.pdf",height=16,width=10)
par(mfrow=c(2,1),mar=c(5,5,5,5))
col11=c("red","blue","green","orange","purple","pink","lightgreen","hotpink","cyan","orchid","tan","gold","darkblue","darkgreen","yellow","brown","lightblue","grey")
stat_K_c2_sig_cat$cat_major=as.character(stat_K_c2_sig_cat$V3)
stat_K_c2_sig_cat$cat_major[stat_K_c2_sig_cat$cat_major%in%names(which(table(droplevels(stat_K_c2_sig_cat$V3))<3))]="Other"
stat_K_c2_sig_cat$col1=col11[factor(stat_K_c2_sig_cat$cat_major)]
stat_K_c2_sig_cat=stat_K_c2_sig_cat[order(stat_K_c2_sig_cat$cat_major),]
plot(log2(stat_K_c2_sig_cat$odds_K_c2),col=stat_K_c2_sig_cat$col1,pch=16,ylab="log2(Odds Ratio)",cex=1.5,cex.lab=1.5,main="Differential Functional Genes")
abline(h=0)

plot.new()
legend("topleft",levels(factor(stat_K_c2_sig_cat$cat_major)),col=col11,pch=16,cex=0.8,bty="n")
dev.off()


stat_K_c2=read.csv(file="/Users/shansun/Google\ Drive/China/wgs/bins/stat_KEGG_c2_fisher.csv",row.names=1)
kegg_cat1=kegg_cat[match(rownames(stat_K_c2),kegg_cat[,4]),]
kegg_annot1=kegg_annot[match(rownames(stat_K_c2),kegg_annot[,1]),]
colnames(kegg_annot1)=c("KEGG","Annotation")
K_c2_num0=K_c2_num0[match(rownames(stat_K_c2),rownames(K_c2_num0)),]
stat_K_c2_cat=cbind(K_c2_num0,stat_K_c2,kegg_cat1,kegg_annot1[,2])
write.csv(stat_K_c2_cat,file="/Users/shansun/Google\ Drive/China/wgs/bins/stat_KEGG_c2_fisher_all_cat.csv")

stat_K_c2_sig_cat[rownames(stat_K_c2_sig_cat)%in%as.character(kegg_cat[kegg_cat$V2=="09175 Drug resistance: antimicrobial",4]),]

kegg_cat_anti=kegg_cat[kegg_cat$V2=="09175 Drug resistance: antimicrobial",]
kegg_cat_anti[kegg_cat_anti[,4]%in%rownames(stat_K_c2_sig_cat),]
stat_K_c2_sig_cat[rownames(stat_K_c2_sig_cat)%in%as.character(kegg_cat_anti[,4]),]

kegg_cat_trans=kegg_cat[kegg_cat$V3=="02000 Transporters [BR:ko02000]",]
sig_trans=as.character(kegg_cat_trans[kegg_cat_trans[,4]%in%rownames(stat_K_c2_sig_cat),4])

K_tab_trans=K_tab1[,colnames(K_tab1)%in%sig_trans]
K_tab_trans[K_tab_trans>1]=1

png("/Users/shansun/Google\ Drive/China/wgs/bins/fun/heat_transporters_c2_1.png",width=500,height=1000)
par(mfrow=c(1,1),mar=c(6,6,3,3))
K_tab_trans1=K_tab_trans[K_tab1$c2==1,]
pheatmap(K_tab_trans1)
dev.off()
png("/Users/shansun/Google\ Drive/China/wgs/bins/fun/heat_transporters_c2_0.png",width=500,height=1000)
K_tab_trans0=K_tab_trans[K_tab1$c2==0,]
pheatmap(K_tab_trans0)
dev.off()

library(ape)
anolis.tree<-read.tree(file="/Users/shansun/Google\ Drive/MAG_manu/tree_of_life_family.newick")
tip1=bins_info2$family[bins_info2$family!="f__"&bins_info2$phylum!="p__Euryarchaeota"]
tip2=names(which(table(tip1)>11))
tip=sapply(strsplit(tip2,"f__"),"[[",2)
anolis.tree1=keep.tip(anolis.tree, tip)
plot(anolis.tree1)

pdf("/Users/shansun/Google\ Drive/MAG_manu/phylo_tree_family.pdf",height=8,width=12)
par(mar=c(5,5,14,5))
plot(anolis.tree1, type = "phylogram", use.edge.length = TRUE,
     node.pos = NULL, show.tip.label = TRUE, show.node.label = FALSE,
     edge.color = "black", edge.width = 1, edge.lty = 1, font = 3,
     cex = par("cex"), adj = NULL, srt = 0, no.margin = FALSE,
     root.edge = FALSE, label.offset = 0, underscore = FALSE,
     x.lim = NULL, y.lim = NULL, direction = "upwards",
     lab4ut = NULL, tip.color = "black", plot = TRUE,
     rotate.tree = 0, open.angle = 0, node.depth = 1,
     align.tip.label = TRUE)
dev.off()

#tree
library(ape)
anolis.tree<-read.tree(file="/Users/shansun/Google\ Drive/MAG_manu/tree_of_life_order.newick")
tip1=bins_info2$order[bins_info2$order!="o__"&bins_info2$phylum!="p__Euryarchaeota"]
tip2=names(which(table(tip1)>2))
tip=sapply(strsplit(tip2,"o__"),"[[",2)
anolis.tree1=keep.tip(anolis.tree, tip)
plot(anolis.tree1)

pdf("/Users/shansun/Google\ Drive/MAG_manu/phylo_tree_order.pdf",height=8,width=12)
par(mar=c(5,5,20,5))
plot(anolis.tree1, type = "phylogram", use.edge.length = TRUE,
     node.pos = NULL, show.tip.label = TRUE, show.node.label = FALSE,
     edge.color = "black", edge.width = 1, edge.lty = 1, font = 3,
     cex = par("cex"), adj = NULL, srt = 0, no.margin = FALSE,
     root.edge = FALSE, label.offset = 0, underscore = FALSE,
     x.lim = NULL, y.lim = NULL, direction = "upwards",
     lab4ut = NULL, tip.color = "black", plot = TRUE,
     rotate.tree = 0, open.angle = 0, node.depth = 1,
     align.tip.label = TRUE)
dev.off()

pangenome_prev_all=apply(pangenome_relabd[-1,],1, function(i){length(which(i!=0))})/214
pangenome_prev0=apply(pangenome_relabd[-1,map_p$urban_c2==0],1, function(i){length(which(i!=0))})/113
pangenome_prev1=apply(pangenome_relabd[-1,map_p$urban_c2==1],1, function(i){length(which(i!=0))})/101
pangenome_prev=data.frame(cbind(pangenome_prev0,pangenome_prev1))
pangenome_prev$tax=rownames(pangenome_prev)


num_genome=vector()
perc_novel_gn=vector()
num_otu=vector()
perc_novel_otu=vector()
perv_urban=vector()
perv_rural=vector()
perv1_avg_gn=list()
perv0_avg_gn=list()
perv1_avg_otu=list()
perv0_avg_otu=list()

tstat_gn=matrix(nrow=length(anolis.tree1$tip.label),ncol=2)
tstat_otu=matrix(nrow=length(anolis.tree1$tip.label),ncol=2)
i=1
for (n in anolis.tree1$tip.label){
  num_genome[i]=length(grep(n,bins_info2$order))
  perc_novel_gn[i]=table(factor(bins_info2$novel[grep(n,bins_info2$order)],levels=c(0,1)))["1"]/length(grep(n,bins_info2$order))*100
  num_otu[i]=length(table(droplevels(bins_info2$fastani_group[grep(n,bins_info2$order)])))
  perc_novel_otu[i]=length(table(droplevels(bins_info2$fastani_group[grepl(n,bins_info2$order)&bins_info2$novel==1])))/num_otu[i]*100
  perv_urban[i]=length(which(bins_info2$bin_prev_c2_1[grep(n,bins_info2$order)]!=0))/length(grep(n,bins_info2$order))
  perv_rural[i]=length(which(bins_info2$bin_prev_c2_0[grep(n,bins_info2$order)]!=0))/length(grep(n,bins_info2$order))
  perv1_avg_gn[[i]]=bins_info2$bin_prev_c2_1[grep(n,bins_info2$order)]
  perv0_avg_gn[[i]]=bins_info2$bin_prev_c2_0[grep(n,bins_info2$order)]
  perv1_avg_otu[[i]]=pangenome_prev$pangenome_prev1[grep(n,rownames(pangenome_prev))]
  perv0_avg_otu[[i]]=pangenome_prev$pangenome_prev0[grep(n,rownames(pangenome_prev))]
  tstat_gn[i,1]=try(t.test(perv1_avg_gn[[i]],perv0_avg_gn[[i]])$statistic)
  tstat_gn[i,2]=try(wilcox.test(perv1_avg_gn[[i]],perv0_avg_gn[[i]])$p.value)
  tstat_otu[i,1]=try(t.test(perv1_avg_otu[[i]],perv0_avg_otu[[i]])$statistic)
  tstat_otu[i,2]=try(wilcox.test(perv1_avg_otu[[i]],perv0_avg_otu[[i]])$p.value)
  
  i=i+1
}
tstat_gn=data.frame(tstat_gn)
tstat_gn$FDR=p.adjust(tstat_gn[,2],method="fdr")
tstat_otu=data.frame(tstat_otu[-13,])
tstat_otu[,2]=as.numeric(as.character(tstat_otu[,2]))
tstat_otu$FDR=p.adjust(tstat_otu[,2],method="fdr")

pdf("/Users/shansun/Google\ Drive/MAG_manu/phylo_tree_order_metadata_genome.pdf",height=6,width=16)
par(mar=c(1,5,1,3),mfrow=c(4,1))
boxplot(c(rbind(perv0_avg_gn, perv1_avg_gn)),border=rep(c("tomato","steelblue1"),23),ylim=c(0,1),ylab="Prevalence",at=c(1:69)[-seq(3,69,3)],xaxt = "n",frame=F)
stripchart(c(rbind(perv0_avg_gn, perv1_avg_gn)),col=rep(c("tomato","steelblue1"),23),at=c(1:69)[-seq(3,69,3)], vertical=T, method="jitter",  add=TRUE)
barplot(perc_novel_gn,col="seagreen1",ylim=c(0,100),ylab="Novel Percentage(%)",border="seagreen1")
barplot(num_genome,col="grey",ylab="Number of genomes",border="grey")
plot.new()
legend("left",c("Rural pervalence","Urban pervalence","Percentage of novel genomes","Number of genomes"),col=c("tomato","steelblue1","seagreen1","grey"),pch=15,bty="n")
dev.off()



pdf("/Users/shansun/Google\ Drive/MAG_manu/phylo_tree_order_metadata_otu.pdf",height=6,width=16)
par(mar=c(1,5,1,3),mfrow=c(4,1))
boxplot(c(rbind(perv0_avg_otu, perv1_avg_otu)),border=rep(c("tomato","steelblue1"),23),ylim=c(0,1),ylab="Prevalence",at=c(1:69)[-seq(3,69,3)],xaxt = "n",frame=F)
stripchart(c(rbind(perv0_avg_otu, perv1_avg_otu)),col=rep(c("tomato","steelblue1"),23),at=c(1:69)[-seq(3,69,3)], vertical=T, method="jitter",  add=TRUE)
barplot(perc_novel_otu,col="seagreen1",ylim=c(0,100),ylab="Novel Percentage(%)",border="seagreen1")
barplot(num_otu,col="grey",ylab="Number of genomes",border="grey")
plot.new()
legend("left",c("Rural pervalence","Urban pervalence","Percentage of novel OTUs","Number of OTUs"),col=c("tomato","steelblue1","seagreen1","grey"),pch=15,bty="n")
dev.off()

pdf("/Users/shansun/Google\ Drive/MAG_manu/phylo_tree_order_metadata.pdf",height=6,width=16)
par(mar=c(1,5,1,3),mfrow=c(4,1))
barplot(perc_novel_gn,col="green",ylim=c(0,100),ylab="Novel genome Percentage(%)",border="green")
barplot(num_genome,col="grey",ylab="Number of genomes",border="grey")
barplot(perc_novel_otu,col="blue",ylim=c(0,100),ylab="Novel otu Percentage(%)",border="blue")
barplot(num_otu,col="darkgrey",ylab="Number of OTUs",border="darkgrey")
plot.new()
legend("left",c("Percentage of novel genomes","Number of genomes","Percentage of novel OTUs","Number of OTUs"),col=c("green","grey","blue","darkgrey"),pch=15,bty="n")
dev.off()


#urban_c2
K_tab=read.csv(file="/Users/shansun/Google Drive/China/wgs/bins/K_tab.csv",row.names=1)
K_tab[is.na(K_tab)]=0
num_gene=apply(K_tab,2,function(i){length(which(i!=0))})

K_tab1=data.frame(t(K_tab))
K_tab1$c2=bins_info2$c2[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$t2=bins_info2$t2[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$t1=bins_info2$t1[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$novel=bins_info2$novel[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$tax=bins_info2$fastani_group[match(rownames(K_tab1),bins_info2[,1])]

kegg_cat1=kegg_cat[match(rownames(K_tab),kegg_cat[,4]),]
kegg_annot1=kegg_annot[match(rownames(K_tab),kegg_annot[,1]),]

K3_tab=apply(K_tab1[,1:7442],1,function(i){aggregate(as.numeric(as.character(i))~kegg_cat1[,3],FUN=function(i){length(which(i!=0))})})
K3_tab=apply(K_tab1[,1:7442],1,function(i){aggregate(as.numeric(as.character(i))~kegg_cat1[,3],FUN=sum)})
K3_tab1=do.call(cbind, K3_tab)[,c(1,seq(2,2206,2))]
rownames(K3_tab1)=K3_tab1[,1]
K3_tab1=K3_tab1[,-1]
colnames(K3_tab1)=rownames(K_tab1)
names(table(kegg_cat[,3]))
cat_len=table(kegg_cat[,3])[match(rownames(K3_tab1),names(table(kegg_cat[,3])))]
K3_tab_cover=K3_tab1/cat_len

sapply(strsplit(colnames(K3_tab_cover),"_"),"[[",1)
K3_tab_map=bins_info2[match(colnames(K3_tab_cover),bins_info2$BinID),]

k3_sig_novel=matrix(nrow=dim(K3_tab_cover)[1],ncol=4)
for (n in 1:dim(K3_tab_cover)[1]){
  k3_sig_novel[n,4]=t.test(as.numeric(as.character(K3_tab_cover[n,]))~K3_tab_map$novel)$p.value
  k3_sig_novel[n,3]=t.test(as.numeric(as.character(K3_tab_cover[n,]))~K3_tab_map$novel)$statistic
  k3_sig_novel[n,1]=t.test(as.numeric(as.character(K3_tab_cover[n,]))~K3_tab_map$novel)$estimate[1]
  k3_sig_novel[n,2]=t.test(as.numeric(as.character(K3_tab_cover[n,]))~K3_tab_map$novel)$estimate[2]
}
k3_sig_novel=data.frame(k3_sig_novel)
k3_sig_novel$fdr=p.adjust(k3_sig_novel[,4],method="fdr")
rownames(k3_sig_novel)=rownames(K3_tab_cover)
k3_sig_novel=k3_sig_novel[order(k3_sig_novel[,4]),]
k3_sig_novel$ratio=k3_sig_novel[,2]/k3_sig_novel[,1]
head(k3_sig_novel)
k3_sig_novel[1:30,]

k3_sig_c2=matrix(nrow=dim(K3_tab_cover)[1],ncol=5)
for (n in 1:dim(K3_tab_cover)[1]){
  k3_sig_c2[n,4]=t.test(as.numeric(as.character(K3_tab_cover[n,]))~K3_tab_map$c2)$p.value
  k3_sig_c2[n,3]=t.test(as.numeric(as.character(K3_tab_cover[n,]))~K3_tab_map$c2)$statistic
  k3_sig_c2[n,1]=t.test(as.numeric(as.character(K3_tab_cover[n,]))~K3_tab_map$c2)$estimate[1]
  k3_sig_c2[n,2]=t.test(as.numeric(as.character(K3_tab_cover[n,]))~K3_tab_map$c2)$estimate[2]
  k3_sig_c2[n,5]=wilcox.test(as.numeric(as.character(K3_tab_cover[n,]))~K3_tab_map$c2)$p.value
}
k3_sig_c2=data.frame(k3_sig_c2)
k3_sig_c2$fdr=p.adjust(k3_sig_c2[,4],method="fdr")
k3_sig_c2$fdr_wilcox=p.adjust(k3_sig_c2[,5],method="fdr")
rownames(k3_sig_c2)=rownames(K3_tab_cover)
k3_sig_c2=k3_sig_c2[order(k3_sig_c2[,4]),]
k3_sig_c2$ratio=k3_sig_c2[,2]/k3_sig_c2[,1]

head(k3_sig_c2)
k3_sig_c2[1:30,]

K_info=bins_info2[match(colnames(K3_tab1),bins_info2$BinID),]
K3_tab1_c2=sapply(by(t(K3_tab1),K_info$c2,colSums),identity)
bin_l3_p=vector()
bin_l3_odd=vector()
for (n in 1:374){
  c2_abs <-matrix(c(K3_tab1_c2[n,1], K3_tab1_c2[n,2], colSums(K3_tab1_c2)[1]-K3_tab1_c2[n,1], colSums(K3_tab1_c2)[2]-K3_tab1_c2[n,2]),nrow = 2,
                  dimnames = list(c2 = c("rural", "urban"),abs = c("present", "absent")))
  bin_l3_p[n]=fisher.test(c2_abs,conf.int = TRUE)$p.value
  bin_l3_odd[n]=fisher.test(c2_abs,conf.int = TRUE)$estimate
  
}
bin_l3_fdr=p.adjust(bin_l3_p,method="fdr")
bin_l3_fisher=cbind(bin_l3_odd,bin_l3_p,bin_l3_fdr)
rownames(bin_l3_fisher)=rownames(K3_tab1_c2)
bin_l3_fisher=bin_l3_fisher[order(bin_l3_fisher[,3]),]

#K_info$order=sapply(strsplit(sapply(strsplit(as.character(K_info$fastani_group),";s__"),"[",1),"f__"),"[",2)
K_info$order=sapply(strsplit(sapply(strsplit(as.character(K_info$fastani_group),";f__"),"[",1),"o__"),"[",2)
#K_info$order=sapply(strsplit(sapply(strsplit(as.character(K_info$fastani_group),";g__"),"[",1),"f__"),"[",2)
#K_info$order=sapply(strsplit(sapply(strsplit(as.character(K_info$fastani_group),";o__"),"[",1),"c__"),"[",2)


K_info$order_c2=paste(K_info$order,K_info$c2,sep="_")
K3_tab1_c2=sapply(by(t(K3_tab1),K_info$order_c2,colSums),identity)
pan_order=names(table(K_info$order))#23
bin_l3_p=matrix(nrow=374,ncol=length(pan_order))
bin_l3_odd=matrix(nrow=374,ncol=length(pan_order))
bin_l3_names=vector()
for (n in 1:374){
  i=1
  for (m in pan_order){
    if(length(table(K_info$c2[K_info$order==m]))<2){
      next
    }else{
      order0=K3_tab1_c2[n,paste(m,0,sep="_")]
      order1=K3_tab1_c2[n,paste(m,1,sep="_")]
      all0=sum(K3_tab1_c2[-n,paste(m,0,sep="_")])
      all1=sum(K3_tab1_c2[-n,paste(m,1,sep="_")])
      c2_abs <-matrix(c(order0, order1, all0, all1),nrow = 2,
                      dimnames = list(c2 = c("rural", "urban"),abs = c("present", "absent")))
      bin_l3_p[n,i]=fisher.test(c2_abs,conf.int = TRUE)$p.value
      bin_l3_odd[n,i]=fisher.test(c2_abs,conf.int = TRUE)$estimate
      bin_l3_names[i]=m
      i=i+1
    }
  }
}
bin_l3_p=bin_l3_p[,1:length(bin_l3_names)]
rownames(bin_l3_p)=rownames(K3_tab1_c2)
colnames(bin_l3_p)=bin_l3_names
bin_l3_odd=bin_l3_odd[,1:length(bin_l3_names)]
rownames(bin_l3_odd)=rownames(K3_tab1_c2)
colnames(bin_l3_odd)=bin_l3_names

bin_l3_fdr=matrix(p.adjust(bin_l3_p,method="fdr"),nrow=374)
rownames(bin_l3_fdr)=rownames(K3_tab1_c2)
colnames(bin_l3_fdr)=bin_l3_names

i=0.1
order_fdr_sig=bin_l3_fdr[unique(which(bin_l3_fdr<i,T)[,1]),unique(which(bin_l3_fdr<i,T)[,2])]
pdf(paste0("/Users/shansun/Google\ Drive/MAG_manu/c2_order_fisher_fdr_",i,".pdf"),width=15,height=30)
pheatmap(-log10(order_fdr_sig))
dev.off()
order_odd_sig=bin_l3_odd[unique(which(bin_l3_fdr<i,T)[,1]),unique(which(bin_l3_fdr<i,T)[,2])]
order_odd_sig[order_odd_sig=="Inf"]=100
order_odd_sig[order_fdr_sig>i]=100
pdf(paste0("/Users/shansun/Google\ Drive/MAG_manu/c2_order_fisher_odd_",i,".pdf"),width=15,height=30)
#pheatmap(order_odd_sig)
pheatmap(order_odd_sig,col=c("blue",adjustcolor( "blue", alpha.f = 0.4),adjustcolor( "red", alpha.f = 0.4),"red","white"),breaks=c(0,0.5,1,3,50,100),border=F,cluster_col = F,cluster_row = F,fontsize=10,legend_breaks=c(-0.1,-0.05,0,0.05,0.1))
dev.off()
order_odd_sig_sim=order_odd_sig[rowSums(order_odd_sig)<1100,]
pdf(paste0("/Users/shansun/Google\ Drive/MAG_manu/c2_order_fisher_odd_",i,"_simplified.pdf"),width=15,height=10)
#pheatmap(order_odd_sig)
pheatmap(order_odd_sig_sim,col=c("blue",adjustcolor( "blue", alpha.f = 0.4),adjustcolor( "red", alpha.f = 0.4),"red","white"),breaks=c(0,0.5,1,3,50,100),border=F,cluster_col = F,cluster_row = F,fontsize=10,legend_breaks=c(-0.1,-0.05,0,0.05,0.1))
dev.off()

pdf(paste0("/Users/shansun/Google\ Drive/MAG_manu/c2_order_fisher_odd_",i,"_multiple.pdf"),width=8,height=3)
pheatmap(order_odd_sig_sim[c(5,7,8,10,11,13),],col=c("blue","red","white"),breaks=c(0,1,50,100),border=F,cluster_col = F,cluster_row = F,fontsize=10,legend_breaks=c(-0.1,-0.05,0,0.05,0.1))
dev.off()
