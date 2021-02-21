library(vegan)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(pheatmap)

map_p=read.csv(file="/Users/shansun/Google Drive/China/wgs/metadata_wgs.csv",row.names=1)
bins_info2=read.csv(file="/Users/shansun/Google\ Drive/China/wgs/bins/bins_info_wtax.csv",row.names=1)
bins_info2$t1[which(is.na(bins_info2$t1))]=52

write.csv(bins_info2[match(names(table(droplevels(bins_info2$bin_rep))),bins_info2$bin_rep),71:70],file="/Users/Shansun/Google\ Drive/MAG_manu/bins_rep.csv")

map_p$t1c2=paste(map_p$t1,map_p$urban_c2,sep="_")
map_p$t1c2=factor(map_p$t1c2,levels=c("43_0", "43_1", "52_0", "52_1"))
urban_i=na.omit(data.frame(cbind(map_p$index15,c("Rural_Prov1","Urban_Prov1","Rural_Prov2","Urban_Prov2")[factor(map_p$t1c2)])))
urban_i[,1]=as.numeric(as.character(urban_i[,1]))
colnames(urban_i)=c("UrbanizationIndex","Group")
urban_i[,2]=factor(urban_i[,2],levels=c("Rural_Prov1","Urban_Prov1","Rural_Prov2","Urban_Prov2"))
#figure1
colr1=adjustcolor(c("red","darkred","blue","darkblue"),alpha=1)
colr2=adjustcolor(c("red","darkred","blue","darkblue"),alpha=0.5)
pdf("/Users/Shansun/Google\ Drive/MAG_manu/index15_c2.pdf",height=5,width=4.5)
# ggboxplot(urban_i, x = "Group", y = "UrbanizationIndex", color = "Group", palette = colr, add = "jitter", lwd = 1.5,
#           ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
#              panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
#              axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
boxplot(urban_i[,1]~urban_i[,2],border=colr1,ylab="Urbanization index",las=1,cex.axis=0.7)
stripchart(urban_i[,1]~urban_i[,2], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
dev.off()

# #two levels
# urban_i=na.omit(data.frame(cbind(map_p$index15,c("Rural","Urban")[factor(map_p$urban_c2)])))
# urban_i[,1]=as.numeric(as.character(urban_i[,1]))
# colnames(urban_i)=c("UrbanizationIndex","Group")
# urban_i[,2]=factor(urban_i[,2],levels=c("Rural","Urban"))
# #figure1
# pdf("/Users/Shansun/Google\ Drive/MAG_manu/index15_c2_2levels.pdf",height=5,width=6)
# par(mar=c(6,6,3,3))
# boxplot(urban_i[,1]~urban_i[,2],border=c("red","blue"),ylab="Urbanization index")
# stripchart(urban_i[,1]~urban_i[,2], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=c("red","blue"))
# # ggboxplot(urban_i, x = "Group", y = "UrbanizationIndex", color = "Group", palette = c("red","blue"), add = "jitter", lwd = 1.5,
# #           ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
# #                                      panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
# #                                      axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
# dev.off()

bins_info2$t1_c2=paste(bins_info2$t1,bins_info2$c2,sep="_")
bins_info2$t1_c2=factor(bins_info2$t1_c2,levels=c("43_0", "43_1", "52_0", "52_1"))
mash_d=na.omit(data.frame(cbind(bins_info2$dis.x,bins_info2$Completeness,bins_info2$Contamination,c("Rural_Prov1","Urban_Prov1","Rural_Prov2","Urban_Prov2")[factor(bins_info2$t1_c2)])))
mash_d[,1]=as.numeric(as.character(mash_d[,1]))
mash_d[,2]=as.numeric(as.character(mash_d[,2]))
mash_d[,3]=as.numeric(as.character(mash_d[,3]))
mash_d[,4]=factor(mash_d[,4],levels=c("Rural_Prov1","Urban_Prov1","Rural_Prov2","Urban_Prov2"))
colnames(mash_d)=c("Distance","Completeness","Contamination","Group")

pdf("/Users/Shansun/Google\ Drive/MAG_manu/mash_t1c2.pdf",height=5,width=4.5)
boxplot(mash_d[,1]~mash_d[,4],border=colr1,ylab="Mash distance",las=1,cex.axis=0.7)
stripchart(mash_d[,1]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
# ggboxplot(mash_d, x = "Group", y = "Distance", color = "Group", palette = c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
#           ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
#                                      panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
#                                      axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
dev.off()

pdf("/Users/Shansun/Google\ Drive/MAG_manu/Completeness_t1c2.pdf",height=5,width=4.5)
boxplot(mash_d[,2]~mash_d[,4],border=colr1,ylab="Completeness",las=1,cex.axis=0.7)
stripchart(mash_d[,2]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
# ggboxplot(mash_d, x = "Group", y = "Distance", color = "Group", palette = c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
#           ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
#                                      panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
#                                      axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
dev.off()

pdf("/Users/Shansun/Google\ Drive/MAG_manu/Contamination_t1c2.pdf",height=5,width=4.5)
boxplot(mash_d[,3]~mash_d[,4],border=colr1,ylab="Contamination",las=1,cex.axis=0.7)
stripchart(mash_d[,3]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
# ggboxplot(mash_d, x = "Group", y = "Distance", color = "Group", palette = c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
#           ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
#                                      panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
#                                      axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
dev.off()

# #two levels
# mash_d=na.omit(data.frame(cbind(bins_info2$dis.x,bins_info2$Completeness,bins_info2$Contamination,c("Rural","Urban")[factor(bins_info2$c2)])))
# mash_d[,1]=as.numeric(as.character(mash_d[,1]))
# mash_d[,2]=factor(mash_d[,2],levels=c("Rural","Urban"))
# colnames(mash_d)=c("Distance","Group")
# mash_d[,4]=factor(mash_d[,4],levels=c("Rural_Prov1","Urban_Prov1","Rural_Prov2","Urban_Prov2"))
# colnames(mash_d)=c("Distance","Completeness","Contamination","Group")
# 
# pdf("/Users/Shansun/Google\ Drive/MAG_manu/mash_t1c2_2levels.pdf",height=5,width=4.5)
# boxplot(mash_d[,1]~mash_d[,4],border=colr1,ylab="Mash distance",las=1,cex.axis=0.7)
# stripchart(mash_d[,1]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
# # ggboxplot(mash_d, x = "Group", y = "Distance", color = "Group", palette = c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
# #           ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
# #                                      panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
# #                                      axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
# dev.off()
# 
# pdf("/Users/Shansun/Google\ Drive/MAG_manu/Completeness_t1c2_2levels.pdf",height=5,width=4.5)
# boxplot(mash_d[,2]~mash_d[,4],border=colr1,ylab="Completeness",las=1,cex.axis=0.7)
# stripchart(mash_d[,2]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
# # ggboxplot(mash_d, x = "Group", y = "Distance", color = "Group", palette = c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
# #           ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
# #                                      panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
# #                                      axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
# dev.off()
# 
# pdf("/Users/Shansun/Google\ Drive/MAG_manu/Contamination_t1c2_2levels.pdf",height=5,width=4.5)
# boxplot(mash_d[,3]~mash_d[,4],border=colr1,ylab="Contamination",las=1,cex.axis=0.7)
# stripchart(mash_d[,3]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
# # ggboxplot(mash_d, x = "Group", y = "Distance", color = "Group", palette = c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
# #           ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
# #                                      panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
# #                                      axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
# dev.off()


bin_size=read.csv(file="/Users/shansun/Google\ Drive/China/wgs/bins/bin_size_gc.csv",row.names=1)
bins_info2$size=bin_size[match(bins_info2[,1],bin_size[,1]),2]
bins_info2$size_log=log10(bins_info2$size)
bins_info2$gc=bin_size[match(bins_info2[,1],bin_size[,1]),3]
bins_info2$c2_f=c("rural","urban")[factor(bins_info2$c2)]
bins_info2$t1_c2=paste(bins_info2$t1,bins_info2$c2,sep="_")
bins_info2$t1_c2=factor(bins_info2$t1_c2,levels=c("43_0", "43_1", "52_0", "52_1"))
bins_info2$novel_f=c("known","novel")[factor(bins_info2$novel)]

pdf("/Users/Shansun/Google\ Drive/MAG_manu/size_c2_2levels.pdf",height=5,width=6)
ggboxplot(bins_info2, x = "c2", y = "size", color = "c2", palette = c("red","blue"), add = "jitter",  lwd = 1.5,
          ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                                     panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
                                     axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
dev.off()

pdf("/Users/Shansun/Google\ Drive/MAG_manu/gc_c2_2levels.pdf",height=5,width=6)
ggboxplot(bins_info2, x = "c2", y = "gc", color = "c2", palette = c("red","blue"), add = "jitter",  lwd = 1.5,
          ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                                     panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
                                     axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
dev.off()



pdf("/Users/Shansun/Google\ Drive/MAG_manu/size_c2_4levels.pdf",height=5,width=6)
ggboxplot(bins_info2, x = "t1_c2", y = "size", color = "t1_c2", palette =  c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
          ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                                     panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
                                     axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))+labs(x = "Group", y = "Genome size (bp)")

dev.off()

pdf("/Users/Shansun/Google\ Drive/MAG_manu/gc_c2_4levels.pdf",height=5,width=6)
ggboxplot(bins_info2, x = "t1_c2", y = "gc", color = "t1_c2", palette = c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
          ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                                     panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
                                     axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))+labs(x = "Group", y = "GC content (%)")
dev.off()




mash_d=na.omit(data.frame(cbind(c(bins_info2$bin_prev_c2_0,bins_info2$bin_prev_c2_1),c(paste0(c("Known","Novel")[factor(bins_info2$novel)],"_Rural"),paste0(c("Known","Novel")[factor(bins_info2$novel)],"_Urban")))))
mash_d[,1]=as.numeric(as.character(mash_d[,1]))
mash_d[,2]=factor(mash_d[,2])
colnames(mash_d)=c("Prevalence","Group")
TukeyHSD(aov(mash_d[,1]~factor(mash_d[,2])))
"Known_Urban-Known_Rural -0.01768220 -0.03393668 -0.001427712 0.0267073
Novel_Rural-Known_Rural -0.03326970 -0.06073389 -0.005805504 0.0100550
Novel_Urban-Known_Rural -0.11606345 -0.14352765 -0.088599263 0.0000000
Novel_Rural-Known_Urban -0.01558750 -0.04305169  0.011876695 0.4628564
Novel_Urban-Known_Urban -0.09838126 -0.12584545 -0.070917064 0.0000000
Novel_Urban-Novel_Rural -0.08279376 -0.11806918 -0.047518338 0.0000000"

"  factor(mash_d[, 2]) mash_d[, 1]
1         Known_Rural   0.2956059
2         Known_Urban   0.2779237
3         Novel_Rural   0.2623362
4         Novel_Urban   0.1795424"

pdf("/Users/Shansun/Google\ Drive/MAG_manu/Prevalence_c2_novel.pdf",height=5,width=6)
ggboxplot(mash_d, x = "Group", y = "Prevalence", color = "Group", palette = c("red","blue","darkred","darkblue"), add = "jitter",  lwd = 1.5,
          ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                                     panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=8),
                                     axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
dev.off()









bins_info2$order_maj=bins_info2$order
bins_info2$order_maj[bins_info2$order_maj%in%names(which(sort(table(bins_info2$order),decreasing = T)<30))]=NA

mash_d=na.omit(data.frame(cbind(c(bins_info2$bin_prev_c2_0,bins_info2$bin_prev_c2_1),rep(bins_info2$order_maj,2),c(paste0(c("Known","Unknown")[factor(bins_info2$novel)],"_Rural"),paste0(c("Known","Unknown")[factor(bins_info2$novel)],"_Urban")))))
mash_d[,1]=as.numeric(as.character(mash_d[,1]))
colnames(mash_d)=c("Prevalence","Order","Group")

pdf("/Users/Shansun/Google\ Drive/MAG_manu/Prevalence_c2_novel_order.pdf",height=5,width=20)
ggboxplot(mash_d, x = "Order", y = "Prevalence", color = "Group", palette = c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
          ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                                     panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=8),
                                     axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
dev.off()


bins_info2$genus=sapply(strsplit(sapply(strsplit(as.character(bins_info2$fastani_group),";s__"),"[",1),"g__"),"[",2)
genus_sig_prev=vector()
n=1
for (i in names(table(bins_info2$genus))){
  a=bins_info2[bins_info2$genus==i,]
  if(length(table(a$novel))==1){
    next
  }
  b=wilcox.test(a$bin_prev_c2_0[a$novel==1],a$bin_prev_c2_1[a$novel==1])$p.value
  print(i)
  print(b)
  if (b<0.05){
    genus_sig_prev[n]=i
    n=n+1
  }
}

mash_d=na.omit(data.frame(cbind(c(bins_info2$bin_prev_c2_0,bins_info2$bin_prev_c2_1),rep(bins_info2$genus,2),c(paste0(c("Known","Unknown")[factor(bins_info2$novel)],"_Rural"),paste0(c("Known","Unknown")[factor(bins_info2$novel)],"_Urban")))))
mash_d[,1]=as.numeric(as.character(mash_d[,1]))
colnames(mash_d)=c("Prevalence","Genus","Group")

pdf("/Users/Shansun/Google\ Drive/MAG_manu/Prevalence_c2_novel_genus.pdf",height=5,width=20)
ggboxplot(mash_d[which(mash_d[,2]%in%order_sig_prev),], x = "Genus", y = "Prevalence", color = "Group", palette = c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
          ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                                     panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=8),
                                     axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
dev.off()





bins_prev0=bins_info2$bin_prev_c2_0
bins_prev1=bins_info2$bin_prev_c2_1
bins_tax=bins_info2$genus
bins_tax[which(is.na(bins_info2$family))]=as.character(bins_info2$fastani_group[which(is.na(bins_info2$family))])
bins_tax[which(is.na(bins_info2$genus))]=as.character(bins_info2$fastani_group[which(is.na(bins_info2$genus))])

bins_tax_maj=bins_tax
bins_tax_maj[bins_tax_maj%in%names(which(sort(table(bins_tax),decreasing = T)<50))]=NA
levels_ab=names(sort(table(bins_tax_maj),decreasing = T))
#pdf("/Users/Shansun/Google\ Drive/MAG_manu/Prevalence_c2_cor.pdf",height=6,width=6)
plot(bins_prev0,bins_prev1,col="lightgrey",xlab="Rural prevalence",ylab="Urban prevalence",pch=16)
points(bins_prev0,bins_prev1,col=col11[factor(bins_tax_maj,levels=levels_ab)],pch=16)
abline(0,1)
legend("topleft",levels_ab,col=col11,pch=16,cex=0.7)
#dev.off()

pdf("/Users/Shansun/Google\ Drive/MAG_manu/Prevalence_novel_cor.pdf",height=6,width=6)
plot(bins_prev0,bins_prev1,col="lightgrey",pch=16,xlab="Rural prevalence",ylab="Urban prevalence")
points(bins_prev0[bins_info2$novel==1],bins_prev1[bins_info2$novel==1],col="red",pch=16)
abline(0,1)
dev.off()

bins_info2$bin_prev_c2_0[]




bins_info2$phylum=sapply(strsplit(sapply(strsplit(as.character(bins_info2$fastani_group),";c__"),"[",1),"p__"),"[",2)
bins_info2$class=sapply(strsplit(sapply(strsplit(as.character(bins_info2$fastani_group),";o__"),"[",1),"c__"),"[",2)
bins_info2$order=sapply(strsplit(sapply(strsplit(as.character(bins_info2$fastani_group),";f__"),"[",1),"o__"),"[",2)
bins_info2$family=sapply(strsplit(sapply(strsplit(as.character(bins_info2$fastani_group),";g__"),"[",1),"f__"),"[",2)
bins_info2$genus=sapply(strsplit(sapply(strsplit(as.character(bins_info2$fastani_group),";s__"),"[",1),"g__"),"[",2)
bins_info2$species=sapply(strsplit(as.character(bins_info2$fastani_group),";s__"),"[",2)

fam_novel1=data.frame(table(bins_info2$order[grepl("group",bins_info2$fastani_group)])/441)
fam_novel0=data.frame(table(bins_info2$order[!grepl("group",bins_info2$fastani_group)])/2077)
fam_novel=merge(fam_novel1,fam_novel0,by="Var1",all=T)
fam_novel[is.na(fam_novel)]=0
fam_noveln=apply(fam_novel[,2:3],2,as.numeric)
rownames(fam_noveln)=fam_novel[,1]
fam_novelm=fam_noveln[apply(fam_noveln,1,max)>0.03,]
Other=1-colSums(fam_novelm)
fam_novelm=rbind(fam_novelm,Other)
colnames(fam_novelm)=c("Novel","Known")
col11=c("red","blue","green","orange","purple","pink","lightgreen","hotpink","cyan","orchid","tan","grey","gold")
pdf("/Users/Shansun/Google\ Drive/MAG_manu/order_novel_perc.pdf",height=6,width=6)
par(mfrow=c(1,2))
barplot(fam_novelm,col=col11,cex.lab=2)
plot.new()
legend("left",rev(rownames(fam_novelm)),col=rev(col11[1:11]),pch=15,bty="n",cex=1)
dev.off()

bin_family=data.frame(table(bins_info2$family[bins_info2$t1_c2=="43_0"]))
for (i in c("43_1","52_0","52_1")){
  bin_family=merge(bin_family,data.frame(table(bins_info2$family[bins_info2$t1_c2==i])),by="Var1")
}
rownames(bin_family)=bin_family[,1]
bin_family=bin_family[,-1]
colnames(bin_family)=c("Rural_Prov1","Urban_Prov1","Rural_Prov2","Urban_Prov2")
bin_family1=as.matrix(t(t(bin_family)/colSums(bin_family)))
bin_family2=bin_family1[rowMeans(bin_family1)>0.02,]
Other=1-colSums(bin_family2)
bin_family2=rbind(bin_family2,Other)
pdf("/Users/Shansun/Google\ Drive/MAG_manu/bin_family_compo.pdf",height=8,width=18)
par(mfrow=c(1,2))
barplot(apply(bin_family2,2,rev),col=rev(col11[1:13]),cex.axis = 1.5,cex.names = 1.5)
plot.new()
legend("topleft",rownames(bin_family2),pch=16,col=col11[1:13],bty="n",cex=1.5)
dev.off()

bin_family_novel=merge(data.frame(table(bins_info2$family[bins_info2$novel==1])),data.frame(table(bins_info2$family)),by="Var1",all=T)
bin_family_novel[is.na(bin_family_novel)]=0
bin_family_novel$perc=bin_family_novel[,2]/bin_family_novel[,3]*100
rownames(bin_family_novel)=bin_family_novel[,1]
bin_family_novel=bin_family_novel[,-1]
bin_family_novel1=na.omit(bin_family_novel[intersect(rownames(bin_family2),rownames(bin_family_novel)),])
Other=colMeans(bin_family_novel[setdiff(rownames(bin_family_novel),rownames(bin_family2)),])
bin_family_novel2=rbind(bin_family_novel1,Other)
pdf("/Users/Shansun/Google\ Drive/MAG_manu/bin_family_novel.pdf",height=8,width=5)
barplot(rev(bin_family_novel2[,3]),pch=16,col=rev(col11[1:13]),horiz=T,xlab="Percentage (%)")
dev.off()


bin_sample_novel=merge(data.frame(table(bins_info2$sample[bins_info2$novel==1])),data.frame(table(bins_info2$sample)),by="Var1",all=T)
bin_sample_novel[is.na(bin_sample_novel)]=0
bin_sample_novel$perc=bin_sample_novel[,2]/bin_sample_novel[,3]*100
bin_sample_novel$t1c2=c("Rural_Prov1","Urban_Prov1","Rural_Prov2","Urban_Prov2")[factor(map_p$t1c2[match(bin_sample_novel[,1],rownames(map_p))])]
colnames(bin_sample_novel)[c(1,4,5)]=c("Sample","Percentage","Group")
boxplot(bin_sample_novel$Percentage~bin_sample_novel$Group)

pdf("/Users/Shansun/Google\ Drive/MAG_manu/novel_sample_t1c2.pdf",height=5,width=6)
ggboxplot(bin_sample_novel, x = "Group", y = "Percentage", color = "Group", palette = c("red","darkred","blue","darkblue"), add = "jitter",  lwd = 1.5,
          ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                                     panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
                                     axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
dev.off()

novel_t1c2=merge(data.frame(table(bins_info2$t1_c2[bins_info2$novel==1])),data.frame(table(bins_info2$t1_c2)),by="Var1")
novel_t1c2_perc=novel_t1c2[,2]/novel_t1c2[,3]*100
names(novel_t1c2_perc)=c("Rural_Prov1","Urban_Prov1","Rural_Prov2","Urban_Prov2")[factor(novel_t1c2[,1])]
pdf("/Users/Shansun/Google\ Drive/MAG_manu/novel_t1c2.pdf",height=5,width=6)
barplot(novel_t1c2_perc,col=c("red","darkred","blue","darkblue"),ylab="Percentage (%) of novel MAGs",ylim=c(0,25),cex.axis = 1,border=NA)
dev.off()


bin_genus=data.frame(table(bins_info2$genus[bins_info2$t1_c2=="43_0"]))
for (i in c("43_1","52_0","52_1")){
  bin_genus=merge(bin_genus,data.frame(table(bins_info2$genus[bins_info2$t1_c2==i])),by="Var1")
}
rownames(bin_genus)=bin_genus[,1]
bin_genus=bin_genus[,-1]
colnames(bin_genus)=c("Rural_Prov1","Urban_Prov1","Rural_Prov2","Urban_Prov2")
bin_genus1=as.matrix(t(t(bin_genus)/colSums(bin_genus)))
bin_genus2=bin_genus1[rowMeans(bin_genus1)>0.03,]
Other=1-colSums(bin_genus2)
bin_genus2=rbind(bin_genus2,Other)
pdf("/Users/Shansun/Google\ Drive/MAG_manu/bin_genus_compo.pdf",height=8,width=16)
par(mfrow=c(1,2))
barplot(-bin_genus2,col=col11)
plot.new()
legend("topright",rownames(bin_genus2),pch=16,col=col11[1:dim(bin_genus2)[1]],bty="n",cex=1)
dev.off()

plot(bins_info2$bin_prev_c2_0,bins_info2$bin_prev_c2_1,col="lightgrey")
points(bins_info2$bin_prev_c2_0[bins_info2$novel==1&bins_info2$high_quality==1],bins_info2$bin_prev_c2_1[bins_info2$novel==1&bins_info2$high_quality==1],col="red")
points(bins_info2$bin_prev_c2_0[bins_info2$novel==1],bins_info2$bin_prev_c2_1[bins_info2$novel==1],col="red")

abline(0,1)


path1=read.table(file="/Users/Shansun/Google\ Drive/China/wgs/pathabundance_unstrat_cpm.tsv",header=TRUE,sep="\t",row.names=1,quote="")
path=log10(path1+1)
colnames(path)=unlist(lapply(colnames(path),function(x)strsplit(as.character(x),".",fixed=TRUE)[[1]][1]))

map_p$t1c2=paste(map_p$t1,map_p$urban_c2,sep="_")
col11=c("red","blue","green","orange","purple","pink","black","lightblue","lightgreen","hotpink","cyan","orchid","tan","grey","gold")
col1=col11[factor(map_p$urban_c2)]

gen_pcoa=capscale(t(path)~1,distance="bray")
png("/Users/Shan/Google\ Drive/China/wgs/pathway_pcoa.png",width=700,height=700)
par(mfrow=c(1,1),mar=c(5,5,5,5))
pcoa12=ordiplot(gen_pcoa,choices=c(1,2),type="none",cex.lab=1.5,display="sites",main="Pathway")
points(pcoa12,col=adjustcolor(col1, alpha.f = 0.8),pch=16,cex=1.5)
ordiellipse(pcoa12, map_p$urban_c2, kind="se", conf=0.95, lwd=4, draw = "lines", col=col11[1],show.groups=1,label=T,font=2,cex=1) #hunan
ordiellipse(pcoa12, map_p$urban_c2, kind="se", conf=0.95, lwd=4, draw = "lines", col=col11[2],show.groups=0,label=T,font=2,cex=1) #
dev.off()

quality_novel=aggregate(bins_info2$high_quality~bins_info2$fastani_group,FUN=sum)
quality_novel1=quality_novel[grep("group",quality_novel[,1]),]
quality_novel2=quality_novel1 [which(quality_novel1[,2]>0),]

bins_info2_n=bins_info2[bins_info2$fastani_group%in%quality_novel2[,1],]
names_nq=names(table(droplevels(bins_info2_n$fastani_group)))
names_nq1=gsub(";","_",names_nq)

pan_abd_t1c2=aggregate(t(pangenome_relabd[names_nq1,])~map_p$t1c2,FUN=mean)
pan_abd_t1c2_n=apply(pan_abd_t1c2[,-1],1,as.numeric)
colnames(pan_abd_t1c2_n)=pan_abd_t1c2[,1]
pheatmap(t(scale(t(pan_abd_t1c2_n))),cluster_cols = F)

pan_abd=t(aggregate(t(pangenome_relabd)~map_p$t1c2,FUN=mean))
colnames(pan_abd)=pan_abd[1,]
pan_abd=pan_abd[-1,]
pan_abd=apply(pan_abd,2,as.numeric)
rownames(pan_abd)=rownames(pangenome_relabd)

p_phylum=sapply(strsplit(sapply(strsplit(rownames(pan_abd),"_c__"),"[",1),"p__"),"[",2)
p_class=sapply(strsplit(sapply(strsplit(rownames(pan_abd),"_o__"),"[",1),"c__"),"[",2)
p_order=sapply(strsplit(sapply(strsplit(rownames(pan_abd),"_f__"),"[",1),"o__"),"[",2)
p_family=sapply(strsplit(sapply(strsplit(rownames(pan_abd),"_g__"),"[",1),"f__"),"[",2)
p_genus=sapply(strsplit(sapply(strsplit(rownames(pan_abd),"_s__"),"[",1),"g__"),"[",2)
p_species=sapply(strsplit(rownames(pan_abd),"_s__"),"[",2)

pan_phylum1=aggregate(pan_abd~p_phylum,FUN=sum)
rownames(pan_phylum1)=pan_phylum1[,1]
pan_phylum1=pan_phylum1[,-1]
pan_phylum=apply(pan_phylum1,2,as.numeric)
Other=100-colSums(pan_phylum)
pan_phylum=rbind(pan_phylum,Other)
rownames(pan_phylum)=c(rownames(pan_phylum1),"Other")
write.csv(pan_phylum,file="/Users/Shansun/Google\ Drive/MAG_manu/pan_phylum.csv")

pan_class1=aggregate(pan_abd~p_class,FUN=sum)
rownames(pan_class1)=pan_class1[,1]
pan_class1=pan_class1[,-1]
pan_class=apply(pan_class1,2,as.numeric)
Other=100-colSums(pan_class)
pan_class=rbind(pan_class,Other)
rownames(pan_class)=c(rownames(pan_class1),"Other")
write.csv(pan_class,file="/Users/Shansun/Google\ Drive/MAG_manu/pan_class.csv")

pan_order1=aggregate(pan_abd~p_order,FUN=sum)
rownames(pan_order1)=pan_order1[,1]
pan_order1=pan_order1[,-1]
pan_order=apply(pan_order1,2,as.numeric)
Other=100-colSums(pan_order)
pan_order=rbind(pan_order,Other)
rownames(pan_order)=c(rownames(pan_order1),"Other")
write.csv(pan_order,file="/Users/Shansun/Google\ Drive/MAG_manu/pan_order.csv")

pan_family1=aggregate(pan_abd~p_family,FUN=sum)
rownames(pan_family1)=pan_family1[,1]
pan_family1=pan_family1[,-1]
pan_family=apply(pan_family1,2,as.numeric)
Other=100-colSums(pan_family)
pan_family=rbind(pan_family,Other)
rownames(pan_family)=c(rownames(pan_family1),"Other")
write.csv(pan_family,file="/Users/Shansun/Google\ Drive/MAG_manu/pan_family.csv")

pan_genus1=aggregate(pan_abd~p_genus,FUN=sum)
rownames(pan_genus1)=pan_genus1[,1]
pan_genus1=pan_genus1[,-1]
pan_genus=apply(pan_genus1,2,as.numeric)
Other=100-colSums(pan_genus)
pan_genus=rbind(pan_genus,Other)
rownames(pan_genus)=c(rownames(pan_genus1),"Other")
write.csv(pan_genus,file="/Users/Shansun/Google\ Drive/MAG_manu/pan_genus.csv")


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

pan_dis=vegdist(t(pangenome_relabd),distance="bray")
mod <- betadisper(pan_dis, map_p$urban_c2) 
boxplot(mod,cex.lab=2,cex.axis=2,ylim=c(0.12,1),border=col11)
stripchart(mod$distances~mod$group, vertical=T, method="jitter",  add=TRUE, pch=16,col=col11)
TukeyHSD(mod)#not significant


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

TukeyHSD(aov(pan_prev[,1]~factor(pan_prev[,2])))
"Known_Urban-Known_Rural -10.019841 -22.143733   2.1040502 0.1452532
Novel_Rural-Known_Rural  -6.831633 -19.792618   6.1293525 0.5270192
Novel_Urban-Known_Rural -23.454082 -36.415067 -10.4930965 0.0000218
Novel_Rural-Known_Urban   3.188209  -9.772776  16.1491937 0.9213441
Novel_Urban-Known_Urban -13.434240 -26.395225  -0.4732552 0.0388285
Novel_Urban-Novel_Rural -16.622449 -30.369650  -2.8752483 0.0103233"

pdf("/Users/Shansun/Google\ Drive/MAG_manu/Prevalence_novel_cor.pdf",height=6,width=6)
plot(bins_prev0,bins_prev1,col="lightgrey",pch=16,xlab="Rural prevalence",ylab="Urban prevalence")
points(bins_prev0[bins_info2$novel==1],bins_prev1[bins_info2$novel==1],col="red",pch=16)
abline(0,1)
dev.off()


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

FDR_d=-log10(pvalues[,9])*sign(pvalues[,7])
pdf("/Users/shansun/Google\ Drive/China/wgs/bins/genome_logFDR_allt1.pdf",height=15,width=10)
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
boxplot(num_gene~K_tab1$c2)
wilcox.test(num_gene~K_tab1$c2)
t.test(num_gene~K_tab1$c2)
median(num_gene[K_tab1$c2==0])
median(num_gene[K_tab1$c2==1])
mean(num_gene[K_tab1$c2==0])
mean(num_gene[K_tab1$c2==1])

K_tab1=data.frame(t(K_tab))
K_tab1$c2=bins_info2$c2[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$t2=bins_info2$t2[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$t1=bins_info2$t1[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$novel=bins_info2$novel[match(rownames(K_tab1),bins_info2[,1])]
K_tab1$tax=bins_info2$fastani_group[match(rownames(K_tab1),bins_info2[,1])]

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
stat_K_c2_cat=cbind(K_c2_num0,stat_K_c2,kegg_cat1[,2:5],kegg_annot1[,2])
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

K_tab1$genus=sapply(strsplit(as.character(K_tab1$tax),";s__"),"[[",1)
sort(table(paste(K_tab1$genus,K_tab1$c2)))
c2_p=matrix(nrow=124,ncol=2)
n=0
for (i in names(table(K_tab1$genus))){
  n=n+1
  if (length(table(K_tab1$c2[K_tab1$genus==i]))>1){
    K_tab1_0=K_tab1[K_tab1$genus==i,]
    c2_p[n,1]=i
    c2_p[n,2]=adonis(K_tab1_0[,1:7442]~K_tab1_0$c2)$aov.tab[1,6]
  }else{
    c2_p[n,1]=i
    c2_p[n,2]=NA
  }
}
c2_p=c2_p[order(c2_p[,2]),]
c2_p

i="d__Bacteria;p__Actinobacteriota;c__Coriobacteriia;o__Coriobacteriales;f__Coriobacteriaceae;g__Collinsella"
K_tab1_0=K_tab1[K_tab1$genus==i,]
K_tab1_0[K_tab1_0>1]=1
adonis(K_tab1_0[,1:7442]~K_tab1_0$c2)
adonis(K_tab1_0[,1:7442]~K_tab1_0$novel)
p_i=matrix(nrow=7442,ncol=2)
for (m in 1:7442){
  a=aggregate(K_tab1_0[,m]~K_tab1_0$c2,FUN=sum)
  b=matrix(c(a[1,2],a[2,2],c(table(K_tab1_0$c2)[1]-a[1,2]),(table(K_tab1_0$c2)[2]-a[2,2])),nrow=2)
  
  p_i[m,1]=colnames(K_tab1_0)[m]
  p_i[m,2]=fisher.test(b)$p.value
}
p_i=p_i[order(p_i[,2]),]
head(p_i)

c2_p=matrix(nrow=447,ncol=2)
n=0
p_i=matrix(nrow=7442,ncol=447)
for (i in names(sort(table(K_tab1$tax),decreasing = T))[29:447]){
  n=n+1
  K_tab1_0=K_tab1[K_tab1$tax==i,]
  if (length(table(K_tab1$c2[K_tab1$tax==i]))>1){
    c2_p[n,1]=i
    c2_p[n,2]=adonis(K_tab1_0[,1:7442]~K_tab1_0$c2)$aov.tab[1,6]
    # pic_path=paste0("/Users/shansun/Google\ Drive/China/wgs/bins/fun/spe/bins_pcoa_c2_",i,".png")
    # png(pic_path,width=1000,height=800)
    # par(mar=c(5,5,5,5))
    # gen_pcoa=capscale(K_tab1_0[,1:7442]~1,distance="bray")
    # var_per=round((gen_pcoa$CA$eig/sum(gen_pcoa$CA$eig))[1:6]*100,2)
    # pcoa_p=paste("PCoA",c(1:6)," (",var_per,"%)",sep="")
    # col1=c("blue","red")[factor(K_tab1_0$c2)]
    # pcoa12=ordiplot(gen_pcoa,choices=c(1,2),display="sites",type="none",cex.lab=2,xlab=pcoa_p[1],ylab=pcoa_p[2],main=n)
    # points(pcoa12,"sites",col=adjustcolor(col1, alpha.f = 0.5),pch=16,cex=2.5)
    # ordiellipse(pcoa12, K_tab1_0$c2, kind="se", conf=0.95, lwd=4, draw = "lines", col="red",show.groups=1,label=T,font=2,cex=1) 
    # ordiellipse(pcoa12, K_tab1_0$c2, kind="se", conf=0.95, lwd=4, draw = "lines", col="blue",show.groups=0,label=T,font=2,cex=1) 
    # legend("topleft",c("1","0"),col=c("red","blue"),cex=1.5,pch=16,bty = "n")
    # dev.off()
    
    for (m in 1:7442){
      K_tab1_0[K_tab1_0>1]=1
      a=aggregate(K_tab1_0[,m]~K_tab1_0$c2,FUN=sum)
      b=matrix(c(a[1,2],a[2,2],c(table(K_tab1_0$c2)[1]-a[1,2]),(table(K_tab1_0$c2)[2]-a[2,2])),nrow=2)
      if (fisher.test(b)$p.value<0.1){
        p_i[m,n]=sign(fisher.test(b)$estimate-1)
      }else{
        p_i[m,n]=0
      }
    }
  }else{
    c2_p[n,1]=i
    c2_p[n,2]=NA
    p_i[1:7442,n]=NA
  }
}

rownames(p_i)=colnames(K_tab1[,1:7442])
colnames(p_i)=names(table(K_tab1$genus))

tail(sort(apply(p_i,1,function(i){length(which(i==1))})))
p_i1=p_i
p_i1=p_i1[,apply(p_i1,2,function(i){any(!is.na(i))})]
tail(sort(apply(p_i1,2,function(i){length(which(i!=0))})))
p_i1=p_i1[,order(apply(p_i1,2,function(i){length(which(i!=0))}),decreasing = T)]
head(p_i1[,1:2])

c2_p1=c2_p[1:28,]
p.adjust(c2_p1[,2],method="fdr")


tail(sort(apply(p_i1,1,function(i){length(which(i==1))})))
tail(sort(apply(p_i1,1,function(i){length(which(i==-1))})))
tail(sort(apply(p_i1,1,function(i){length(which(i!=0))})))

p_i1_sig_num=cbind(apply(p_i1,1,function(i){length(which(i==1))}),apply(p_i1,1,function(i){length(which(i==-1))}))
p_i1_sig_num=p_i1_sig_num[order(rowSums(p_i1_sig_num),decreasing=T),]
head(p_i1_sig_num)

c2_p=matrix(nrow=447,ncol=2)
n=0
p_i=matrix(nrow=7442,ncol=124)
p_i_n1=vector()
p_i_n2=vector()
for (i in names(sort(table(K_tab1$genus),decreasing = T))){
  n=n+1
  K_tab1_0=K_tab1[K_tab1$genus==i,]
  if (length(table(K_tab1$c2[K_tab1$tax==i]))>1){
    c2_p[n,1]=i
    c2_p[n,2]=adonis(K_tab1_0[,1:7442]~K_tab1_0$c2)$aov.tab[1,6]
    # pic_path=paste0("/Users/shansun/Google\ Drive/China/wgs/bins/fun/spe/bins_pcoa_c2_",i,".png")
    # png(pic_path,width=1000,height=800)
    # par(mar=c(5,5,5,5))
    # gen_pcoa=capscale(K_tab1_0[,1:7442]~1,distance="bray")
    # var_per=round((gen_pcoa$CA$eig/sum(gen_pcoa$CA$eig))[1:6]*100,2)
    # pcoa_p=paste("PCoA",c(1:6)," (",var_per,"%)",sep="")
    # col1=c("blue","red")[factor(K_tab1_0$c2)]
    # pcoa12=ordiplot(gen_pcoa,choices=c(1,2),display="sites",type="none",cex.lab=2,xlab=pcoa_p[1],ylab=pcoa_p[2],main=n)
    # points(pcoa12,"sites",col=adjustcolor(col1, alpha.f = 0.5),pch=16,cex=2.5)
    # ordiellipse(pcoa12, K_tab1_0$c2, kind="se", conf=0.95, lwd=4, draw = "lines", col="red",show.groups=1,label=T,font=2,cex=1) 
    # ordiellipse(pcoa12, K_tab1_0$c2, kind="se", conf=0.95, lwd=4, draw = "lines", col="blue",show.groups=0,label=T,font=2,cex=1) 
    # legend("topleft",c("1","0"),col=c("red","blue"),cex=1.5,pch=16,bty = "n")
    # dev.off()
    
    for (m in 1:7442){
      K_tab1_0[K_tab1_0>1]=1
      a=aggregate(K_tab1_0[,m]~K_tab1_0$c2,FUN=sum)
      b=matrix(c(a[1,2],a[2,2],c(table(K_tab1_0$c2)[1]-a[1,2]),(table(K_tab1_0$c2)[2]-a[2,2])),nrow=2)
      p_i_n1[m]=fisher.test(b)$p.value
      p_i_n2[m]=fisher.test(b)$estimate
      
      if (fisher.test(b)$p.value<0.1){
        #p_i[m,n]=sign(fisher.test(b)$estimate-1)
        
      }else{
        #p_i[m,n]=0
      }
    }
  }else{
    c2_p[n,1]=i
    c2_p[n,2]=NA
    p_i[1:7442,n]=NA
  }
}

rownames(p_i)=colnames(K_tab1[,1:7442])
colnames(p_i)=names(table(K_tab1$genus))

tail(sort(apply(p_i,1,function(i){length(which(i==1))})))
p_i1=p_i
p_i1=p_i1[,apply(p_i1,2,function(i){any(!is.na(i))})]
tail(sort(apply(p_i1,2,function(i){length(which(i!=0))})))
p_i1=p_i1[,order(apply(p_i1,2,function(i){length(which(i!=0))}),decreasing = T)]
head(p_i1[,1:2])

c2_p1=c2_p[1:28,]
p.adjust(c2_p1[,2],method="fdr")

library(SpiecEasi) #have sparcc too
library(igraph)
cor1=sparcc(t(pangenome_relabd[,map_p$urban_c2==0]))
cor2=sparcc(t(pangenome_relabd[,map_p$urban_c2==1]))

colnames(cor2$Cor)=rownames(pangenome_relabd)
rownames(cor2$Cor)=rownames(pangenome_relabd)

colnames(cor1$Cor)=rownames(pangenome_relabd)
rownames(cor1$Cor)=rownames(pangenome_relabd)


net_por=matrix(nrow=8,ncol=18)
n=1
for (i in c(0.06,0.09,0.12,0.15,0.18,0.2)){
  fname=paste("/Users/shansun/Google\ Drive/MAG_manu/net/network_sparcc_circle_",i,".pdf")
  pdf(fname,width=15,height=7.50)
  
  par(mfrow=c(1,2),mar=c(2,5,5,5),xpd=TRUE)
  network5<- graph.adjacency(cor1$Cor, weighted=TRUE,diag=FALSE, mode="undirected") 
  network5=delete.edges(network5, which(E(network5)$weight <=i & E(network5)$weight >=-i))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1 ))
  E(network5)$color[E(network5)$weight>0]<-"green"
  E(network5)$color[E(network5)$weight<0]<-"red"
  #V(network5)$size=log2(rowMeans(pangenome_relabd[names(V(network5)),])+1)*2
  #net_label=names(V(network5))
  #net_label[rowMeans(pangenome_relabd[names(V(network5)),])<100]=NA
  plot(network5,edge.arrow.size=0.03,vertex.frame.color=NA,layout=layout_in_circle,vertex.label=NA)
  
  net_por[1,n*2-1]=length(V(network5))
  net_por[2,n*2-1]=length(E(network5))
  net_por[3,n*2-1]=length(which(E(network5)$weight>0))
  net_por[4,n*2-1]=length(which(E(network5)$weight<0))
  net_por[5,n*2-1]=transitivity(network5) #clustering coefficient0.7701
  net_por[6,n*2-1]=mean(igraph::degree(network5))#average igraph::degree7.793
  net_por[7,n*2-1]=average.path.length(network5, directed=TRUE, unconnected=TRUE)#average path length2.377
  net_por[8,n*2-1]=diameter(network5,directed = TRUE, unconnected = TRUE, weights = NA)#diameter 6
  
  network5<- graph.adjacency(cor2$Cor, weighted=TRUE,diag=FALSE, mode="undirected") 
  network5=delete.edges(network5, which(E(network5)$weight <=i & E(network5)$weight >=-i))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1 ))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1))
  E(network5)$color[E(network5)$weight>0]<-"green"
  E(network5)$color[E(network5)$weight<0]<-"red"
  #V(network5)$size=log2(rowMeans(pangenome_relabd[names(V(network5)),])+1)*2
  #net_label=names(V(network5))
  #net_label[rowMeans(pangenome_relabd[names(V(network5)),])<100]=NA
  plot(network5,edge.arrow.size=0.03,vertex.frame.color=NA,layout=layout_in_circle,vertex.label=NA)
  
  dev.off()
  
  net_por[1,n*2]=length(V(network5))
  net_por[2,n*2]=length(E(network5))
  net_por[3,n*2]=length(which(E(network5)$weight>0))
  net_por[4,n*2]=length(which(E(network5)$weight<0))
  net_por[5,n*2]=transitivity(network5) #clustering coefficient0.7701
  net_por[6,n*2]=mean(igraph::degree(network5))#average igraph::degree7.793
  net_por[7,n*2]=average.path.length(network5, directed=TRUE, unconnected=TRUE)#average path length2.377
  net_por[8,n*2]=diameter(network5,directed = TRUE, unconnected = TRUE, weights = NA)#diameter 6
  
  fname=paste("/Users/shansun/Google\ Drive/MAG_manu/net/network_sparcc_non_circle",i,".pdf")
  pdf(fname,width=20,height=10)
  
  par(mfrow=c(1,2),mar=c(2,5,5,5),xpd=TRUE)
  network5<- graph.adjacency(cor1$Cor, weighted=TRUE,diag=FALSE, mode="undirected") 
  network5=delete.edges(network5, which(E(network5)$weight <=i & E(network5)$weight >=-i))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1 ))
  E(network5)$color[E(network5)$weight>0]<-"green"
  E(network5)$color[E(network5)$weight<0]<-"red"
  #V(network5)$size=log2(colMeans(stir_norm1[,names(V(network5))])+1)*2
  #net_label=names(V(network5))
  #net_label[colMeans(stir_norm1f[,names(V(network5))])<300]=NA
  plot(network5,edge.arrow.size=0.03,vertex.frame.color=NA,vertex.label=NA)
  
  network5<- graph.adjacency(cor2$Cor, weighted=TRUE,diag=FALSE, mode="undirected") 
  network5=delete.edges(network5, which(E(network5)$weight <=i & E(network5)$weight >=-i))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1 ))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1))
  E(network5)$color[E(network5)$weight>0]<-"green"
  E(network5)$color[E(network5)$weight<0]<-"red"
  #V(network5)$size=log2(colMeans(stir_norm1[,names(V(network5))])+1)*2
  #net_label=names(V(network5))
  #net_label[colMeans(stir_norm1f[,names(V(network5))])<300]=NA
  plot(network5,edge.arrow.size=0.03,vertex.frame.color=NA,vertex.label=NA)
  
  dev.off()
  
  n=n+1
}

net_por1=net_por[,1:12]
colnames(net_por1)=paste(sort(rep(c(0.06,0.09,0.12,0.15,0.18,0.2),2)),rep(c("R","U"),6),sep="_")
rownames(net_por1)=c("num_V","num_E","PE","NE","cluster","degree","path","dia")
write.csv(net_por1,file="/Users/shansun/Google\ Drive/MAG_manu/net/sparcc_net_properties.csv")


#use spearman to confirm
cor1=cor(t(pangenome_relabd[-1,map_p$urban_c2==0]),method = "spearman")
cor2=cor(t(pangenome_relabd[-1,map_p$urban_c2==1]),method = "spearman")
cor1[is.na(cor1)]=0
cor2[is.na(cor2)]=0


net_por=matrix(nrow=8,ncol=18)
n=1
for (i in c(0.2,0.3,0.4,0.5,0.6,0.7)){
  fname=paste("/Users/shansun/Google\ Drive/MAG_manu/net/network_spearman_circle_",i,".pdf")
  pdf(fname,width=15,height=7.50)
  
  par(mfrow=c(1,2),mar=c(2,5,5,5),xpd=TRUE)
  network5<- graph.adjacency(cor1, weighted=TRUE,diag=FALSE, mode="undirected") 
  network5=delete.edges(network5, which(E(network5)$weight <=i & E(network5)$weight >=-i))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1 ))
  E(network5)$color[E(network5)$weight>0]<-"green"
  E(network5)$color[E(network5)$weight<0]<-"red"
  #V(network5)$size=log2(rowMeans(pangenome_relabd[names(V(network5)),])+1)*2
  #net_label=names(V(network5))
  #net_label[rowMeans(pangenome_relabd[names(V(network5)),])<100]=NA
  plot(network5,edge.arrow.size=0.03,vertex.frame.color=NA,layout=layout_in_circle,vertex.label=NA)
  
  net_por[1,n*2-1]=length(V(network5))
  net_por[2,n*2-1]=length(E(network5))
  net_por[3,n*2-1]=length(which(E(network5)$weight>0))
  net_por[4,n*2-1]=length(which(E(network5)$weight<0))
  net_por[5,n*2-1]=transitivity(network5) #clustering coefficient0.7701
  net_por[6,n*2-1]=mean(igraph::degree(network5))#average igraph::degree7.793
  net_por[7,n*2-1]=average.path.length(network5, directed=TRUE, unconnected=TRUE)#average path length2.377
  net_por[8,n*2-1]=diameter(network5,directed = TRUE, unconnected = TRUE, weights = NA)#diameter 6
  
  network5<- graph.adjacency(cor2, weighted=TRUE,diag=FALSE, mode="undirected") 
  network5=delete.edges(network5, which(E(network5)$weight <=i & E(network5)$weight >=-i))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1 ))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1))
  E(network5)$color[E(network5)$weight>0]<-"green"
  E(network5)$color[E(network5)$weight<0]<-"red"
  #V(network5)$size=log2(rowMeans(pangenome_relabd[names(V(network5)),])+1)*2
  #net_label=names(V(network5))
  #net_label[rowMeans(pangenome_relabd[names(V(network5)),])<100]=NA
  plot(network5,edge.arrow.size=0.03,vertex.frame.color=NA,layout=layout_in_circle,vertex.label=NA)
  
  dev.off()
  
  net_por[1,n*2]=length(V(network5))
  net_por[2,n*2]=length(E(network5))
  net_por[3,n*2]=length(which(E(network5)$weight>0))
  net_por[4,n*2]=length(which(E(network5)$weight<0))
  net_por[5,n*2]=transitivity(network5) #clustering coefficient0.7701
  net_por[6,n*2]=mean(igraph::degree(network5))#average igraph::degree7.793
  net_por[7,n*2]=average.path.length(network5, directed=TRUE, unconnected=TRUE)#average path length2.377
  net_por[8,n*2]=diameter(network5,directed = TRUE, unconnected = TRUE, weights = NA)#diameter 6
  
  fname=paste("/Users/shansun/Google\ Drive/MAG_manu/net/network_spearman_non_circle",i,".pdf")
  pdf(fname,width=20,height=10)
  
  par(mfrow=c(1,2),mar=c(2,5,5,5),xpd=TRUE)
  network5<- graph.adjacency(cor1, weighted=TRUE,diag=FALSE, mode="undirected") 
  network5=delete.edges(network5, which(E(network5)$weight <=i & E(network5)$weight >=-i))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1 ))
  E(network5)$color[E(network5)$weight>0]<-"green"
  E(network5)$color[E(network5)$weight<0]<-"red"
  #V(network5)$size=log2(colMeans(stir_norm1[,names(V(network5))])+1)*2
  #net_label=names(V(network5))
  #net_label[colMeans(stir_norm1f[,names(V(network5))])<300]=NA
  plot(network5,edge.arrow.size=0.03,vertex.frame.color=NA,vertex.label=NA)
  
  network5<- graph.adjacency(cor2, weighted=TRUE,diag=FALSE, mode="undirected") 
  network5=delete.edges(network5, which(E(network5)$weight <=i & E(network5)$weight >=-i))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1 ))
  network5=delete.vertices(network5,which(igraph::degree(network5)<1))
  E(network5)$color[E(network5)$weight>0]<-"green"
  E(network5)$color[E(network5)$weight<0]<-"red"
  #V(network5)$size=log2(colMeans(stir_norm1[,names(V(network5))])+1)*2
  #net_label=names(V(network5))
  #net_label[colMeans(stir_norm1f[,names(V(network5))])<300]=NA
  plot(network5,edge.arrow.size=0.03,vertex.frame.color=NA,vertex.label=NA)
  
  dev.off()
  
  n=n+1
}

net_por1=net_por[,1:12]
colnames(net_por1)=paste(sort(rep(c(0.2,0.3,0.4,0.5,0.6,0.7),2)),rep(c("R","U"),6),sep="_")
rownames(net_por1)=c("num_V","num_E","PE","NE","cluster","degree","path","dia")
write.csv(net_por1,file="/Users/shansun/Google\ Drive/MAG_manu/net/spearman_net_properties.csv")

library(SpiecEasi)
library(igraph)
library(sparcc)

se.mb_str1 <- spiec.easi(t(pangenome_relabd[-1,map_p$urban_c2==0]), method='mb', lambda.min.ratio=0.1,
                         nlambda=20, pulsar.params=list(rep.num=50))
se.mb_str2 <- spiec.easi(t(pangenome_relabd[-1,map_p$urban_c2==1]), method='mb', lambda.min.ratio=0.1,
                         nlambda=20, pulsar.params=list(rep.num=50))

sebeta <- symBeta(getOptBeta(se.mb_str1))
elist.mb <- summary(sebeta)
edge_matrix_SE_mbND = data.frame(as.matrix(sebeta))
length(edge_matrix_SE_mbND[edge_matrix_SE_mbND>0])/2 #2326
length(edge_matrix_SE_mbND[edge_matrix_SE_mbND<0])/2 #193
length(edge_matrix_SE_mbND[edge_matrix_SE_mbND!=0])/2 #2519


sebeta <- symBeta(getOptBeta(se.mb_str2))
elist.mb <- summary(sebeta)
edge_matrix_SE_mbND = data.frame(as.matrix(sebeta))
length(edge_matrix_SE_mbND[edge_matrix_SE_mbND>0])/2 #2139
length(edge_matrix_SE_mbND[edge_matrix_SE_mbND<0])/2 #216
length(edge_matrix_SE_mbND[edge_matrix_SE_mbND!=0])/2 #2355


cor1 <- as.matrix(symBeta(getOptBeta(se.mb_str1), mode="maxabs"))
cor2 <- as.matrix(symBeta(getOptBeta(se.mb_str2), mode="maxabs"))

network5<- graph.adjacency(cor1, weighted=TRUE,diag=FALSE, mode="undirected") 
network5=delete.vertices(network5,which(igraph::degree(network5)<1 ))

net_por_easi=matrix(nrow=8,ncol=2)
net_por_easi[1,1]=length(V(network5))
net_por_easi[2,1]=length(E(network5))
net_por_easi[3,1]=length(which(E(network5)$weight>0))
net_por_easi[4,1]=length(which(E(network5)$weight<0))
net_por_easi[5,1]=transitivity(network5) #clustering coefficient0.7701
net_por_easi[6,1]=mean(igraph::degree(network5))#average igraph::degree7.793
net_por_easi[7,1]=average.path.length(network5, directed=TRUE, unconnected=TRUE)#average path length2.377
net_por_easi[8,1]=diameter(network5,directed = TRUE, unconnected = TRUE, weights = NA)#diameter 6

network5<- graph.adjacency(cor2, weighted=TRUE,diag=FALSE, mode="undirected") 
network5=delete.vertices(network5,which(igraph::degree(network5)<1 ))

net_por_easi[1,2]=length(V(network5))
net_por_easi[2,2]=length(E(network5))
net_por_easi[3,2]=length(which(E(network5)$weight>0))
net_por_easi[4,2]=length(which(E(network5)$weight<0))
net_por_easi[5,2]=transitivity(network5) #clustering coefficient0.7701
net_por_easi[6,2]=mean(igraph::degree(network5))#average igraph::degree7.793
net_por_easi[7,2]=average.path.length(network5, directed=TRUE, unconnected=TRUE)#average path length2.377
net_por_easi[8,2]=diameter(network5,directed = TRUE, unconnected = TRUE, weights = NA)#diameter 6
write.csv(net_por_easi,file="/Users/shansun/Google\ Drive/MAG_manu/net/SpiecEasi_net_properties.csv")


"           [,1]         [,2]
[1,]  447.000000  447.0000000
[2,] 2519.000000 2355.0000000
[3,] 2326.000000 2139.0000000
[4,]  193.000000  216.0000000
[6,]   11.270694   10.5369128
[7,]    2.988042    3.0453045
[8,]    6.000000    6.0000000
"

pangenome=read.table(file="/projects/afodor_research/ssun5/china_bins/bins_map/all.tab",sep="\t",header=T)
pangenome_relabd1=pangenome[,seq(3,ncol(pangenome),8)]
pangenome_relabd=apply(pangenome_relabd1,1,as.character)
pangenome_relabd=apply(pangenome_relabd,1,as.numeric)
rownames(pangenome_relabd)=pangenome[,1]
colnames(pangenome_relabd)=sapply(strsplit(colnames(pangenome_relabd1),"\\_"), "[[", 1)
colnames(pangenome_relabd)=sapply(strsplit(colnames(pangenome_relabd),"\\."), "[[", 1)
write.csv(pangenome_relabd,file="/Users/shansun/Google\ Drive/MAG_manu/pan_relative_abundance.csv")
tabs3=pangenome_relabd
colnames(tabs3)=paste0("s",c(1:214))
tabs3=formatC(tabs3,format="e",digits=1)
write.csv(tabs3,file="/Users/shansun/Google\ Drive/MAG_manu/v1/SI/TableS3.csv")

tabs2=bins_info2
tabs2=tabs2[order(tabs2[,1]),]
sample1=sapply(strsplit(as.character(tabs2[,1]),"_"),"[[",1)
bin1=sapply(strsplit(as.character(tabs2[,1]),"_"),"[[",2)
sample2=paste0("s",c(1:214))[factor(sample1)]
bin2=sapply(strsplit(as.character(bin1),"\\."),"[[",2)
rownames(tabs2)=paste(sample2,bin2,sep="_")
tabs2=tabs2[,c(13:15,52:63,68,18:20,33,30,29,37:39,44:45,78:79,89)]
colnames(tabs2)[17:30]=c("Refseq_closest","mash_distance","mash_P.value","age","urban/rural","urbanization_index","prevalence","rural_prevalence","urban_prevalence","rural_abundance","urban_abundance","genome_size","GC_content","ARG")
tabs2[,16]=round(tabs2[,16],2)
tabs2[,20]=round(tabs2[,20],0)
for(i in c(18,19,22:29)){
  tabs2[,i]=formatC(tabs2[,i],format="e",digits=1)
}
write.csv(tabs2,file="/Users/shansun/Google\ Drive/MAG_manu/v1/SI/TableS2.csv")



library(devtools)
install_github("biobakery/banocc")

library(banocc)
compiled_banocc_model <- rstan::stan_model(model_code = banocc_model)
data(compositions_null)
abd_n=data.frame(t(pangenome_relabd[-1,]/100))
abd1=abd_n[map_p$urban_c2==0,]
abd2=abd_n[map_p$urban_c2==1,tail(order(colSums(abd_n)))]
abd1$Unclassified=1-rowSums(abd1)
b_fit     <- banocc::run_banocc(C = compositions_null, compiled_banocc_model=compiled_banocc_model)
b_output <- banocc::get_banocc_output(banoccfit=b_fit)

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
  tstat_gn[i,1]=t.test(perv1_avg_gn[[i]],perv0_avg_gn[[i]])$statistic
  tstat_gn[i,2]=t.test(perv1_avg_gn[[i]],perv0_avg_gn[[i]])$p.value
  tstat_otu[i,1]=try(t.test(perv1_avg_otu[[i]],perv0_avg_otu[[i]])$statistic)
  tstat_otu[i,2]=try(t.test(perv1_avg_otu[[i]],perv0_avg_otu[[i]])$p.value)
  
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






dim(pangenome_relabd)
dim(map_p)
cor_t=matrix(nrow=448,ncol=91)
cor_p=matrix(nrow=448,ncol=91)
m_names=vector()
for (n in 1:448){
  i=1
  for (m in 5:91){
    if (length(levels(factor(map_p[,m])))>15){
      cor1=cor.test(pangenome_relabd[n,],map_p[,m],method="spearman")
      cor_t[n,i]=cor1$estimate
      cor_p[n,i]=cor1$p.value
      m_names[i]=colnames(map_p)[m]
      i=i+1
    }
  }
}
cor_t1=cor_t[,1:68]
cor_p1=cor_p[,1:68]
colnames(cor_p1)=m_names
colnames(cor_t1)=m_names
cor_fdr=matrix(p.adjust(cor_p1,method="fdr"),ncol=68)
colnames(cor_fdr)=m_names
sig_cor=which(cor_fdr<0.05,T)
sig_cor1=cbind(rownames(pangenome_relabd)[sig_cor[,1]],m_names[sig_cor[,2]])

aov_p=matrix(nrow=448,ncol=20)
a_names=vector()
for (n in 1:448){
  i=1
  for (m in 5:91){
    if (length(levels(factor(map_p[,m])))<15&length(which(is.na(map_p[,m])))<100){
      aov_p[n,i]=summary(aov(lm(pangenome_relabd[n,]~map_p[,m])))[[1]][1,5]
      a_names[i]=colnames(map_p)[m]
      i=i+1
    }
  }
}



num_orfs=read.table(file="/Users/shansun/Google\ Drive/MAG_manu/num_orfs.txt")
num_orfs$V3=sapply(strsplit(as.character(num_orfs$V2),"_orf"),"[[",1)
bins_info2$num_orfs=num_orfs$V1[match(bins_info2$BinID,num_orfs$V3)]
bins_info2$num_orfs=bins_info2$num_orfs/2

num_annots=read.table(file="/Users/shansun/Google\ Drive/MAG_manu/annotations_num.txt")
num_annots$V3=sapply(strsplit(as.character(num_annots$V2),".emapper"),"[[",1)
bins_info2$num_annots=num_annots$V1[match(bins_info2$BinID,num_annots$V3)]
bins_info2$num_annots=bins_info2$num_annots-7

num_genes=read.table(file="/Users/shansun/Google\ Drive/MAG_manu/genes_num_prodigal.txt")
bins_info2$num_genes=num_genes$V2[match(bins_info2$BinID,num_genes$V1)]
num_prot=read.table(file="/Users/shansun/Google\ Drive/MAG_manu/prot_num_prodigal.txt")
bins_info2$num_prot=num_prot$V2[match(bins_info2$BinID,num_prot$V1)]
  
par(mfrow=c(2,2))
boxplot(bins_info2$num_genes~bins_info2$c2,main="#orfs P=0.07")
boxplot(bins_info2$num_prot~bins_info2$c2,main="#annotations P=0.003")
boxplot(bins_info2$num_annots/bins_info2$num_genes~bins_info2$c2,main="#annotations/orfs P=0.02")

wilcox.test(bins_info2$num_genes~bins_info2$c2)
wilcox.test(bins_info2$num_prot~bins_info2$c2)
wilcox.test(bins_info2$num_annots/bins_info2$num_genes~bins_info2$c2)

t.test(bins_info2$num_genes~bins_info2$c2)
t.test(bins_info2$num_prot~bins_info2$c2)
t.test(bins_info2$num_annots/bins_info2$num_prot~bins_info2$c2)
bins_info2$annot_ratio=bins_info2$num_annots/bins_info2$num_orfs

pdf("/Users/shansun/Google\ Drive/MAG_manu/orfs_c2_novel.pdf",height=8,width=8)
par(mfrow=c(2,2))
boxplot(bins_info2$num_orfs~bins_info2$c2,main="#orfs P=0.07")
boxplot(bins_info2$num_annots~bins_info2$c2,main="#annotations P=0.003")
boxplot(bins_info2$num_annots/bins_info2$num_orfs~bins_info2$c2,main="#annotations/orfs P=0.02")

par(mfrow=c(2,2))
boxplot(bins_info2$num_orfs~bins_info2$novel,main="#orfs P=0.03")
boxplot(bins_info2$num_annots~bins_info2$novel,main="#annotations P=0.63")
boxplot(bins_info2$num_annots/bins_info2$num_orfs~bins_info2$novel,main="#annotations/orfs P=0.01")
dev.off()

pdf("/Users/shansun/Google\ Drive/MAG_manu/orfs_annots_genus.pdf",height=20,width=30)
par(mfrow=c(3,1),mar=c(10,5,5,5))
boxplot(bins_info2$num_orfs~bins_info2$genus,main="#orfs",las=2)
boxplot(bins_info2$num_annots~bins_info2$genus,main="#annotations",las=2)
boxplot(bins_info2$num_annots/bins_info2$num_orfs~bins_info2$genus,main="#annotations/orfs",las=2)
dev.off()

pdf("/Users/shansun/Google\ Drive/MAG_manu/orfs_annots_order.pdf",height=20,width=15)
par(mfrow=c(3,1),mar=c(10,5,5,5))
boxplot(bins_info2$num_orfs~bins_info2$order,main="#orfs",las=2)
boxplot(bins_info2$num_annots~bins_info2$order,main="#annotations",las=2)
boxplot(bins_info2$num_annots/bins_info2$num_orfs~bins_info2$order,main="#annotations/orfs",las=2)
dev.off()


t.test(bins_info2$num_orfs~bins_info2$c2)
t.test(bins_info2$num_annots~bins_info2$c2)
t.test(bins_info2$num_annots/bins_info2$num_orfs~bins_info2$c2)

t.test(bins_info2$num_orfs~bins_info2$novel)
t.test(bins_info2$num_annots~bins_info2$novel)
t.test(bins_info2$num_annots/bins_info2$num_orfs~bins_info2$novel)

aggregate(bins_info2$size~bins_info2$genus,FUN=mean)

