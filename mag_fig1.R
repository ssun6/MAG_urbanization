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
urban_i=na.omit(data.frame(cbind(map_p$index15,c("Rural","Urban")[factor(map_p$urban_c2)])))
urban_i[,1]=as.numeric(as.character(urban_i[,1]))
colnames(urban_i)=c("UrbanizationIndex","Group")
urban_i[,2]=factor(urban_i[,2],levels=c("Rural","Urban"))
t.test(urban_i[,1]~urban_i[,2])
#figure1
colr1=adjustcolor(c("tomato","steelblue"),alpha=1)
colr2=adjustcolor(c("tomato","steelblue"),alpha=0.5)

mash_d=na.omit(data.frame(cbind(bins_info2$dis.x,bins_info2$Completeness,bins_info2$Contamination,c("Rural","Urban")[factor(bins_info2$c2)])))
mash_d[,1]=as.numeric(as.character(mash_d[,1]))
mash_d[,2]=as.numeric(as.character(mash_d[,2]))
mash_d[,3]=as.numeric(as.character(mash_d[,3]))
mash_d[,4]=factor(mash_d[,4],levels=c("Rural","Urban"))
colnames(mash_d)=c("Distance","Completeness","Contamination","Group")
t.test(mash_d[,1]~mash_d[,4])
t.test(mash_d[,2]~mash_d[,4])
t.test(mash_d[,3]~mash_d[,4])

pdf("/Users/Shansun/Google\ Drive/MAG_manu/mash_cc_t1c2.pdf",height=3,width=8)
par(mfrow=c(1,4),mar=c(5,5,2,2))
boxplot(urban_i[,1]~urban_i[,2],border=colr1,ylab="Urbanization index",las=1,cex.axis=1,main="P < 2.2e-16")
stripchart(urban_i[,1]~urban_i[,2], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)

boxplot(mash_d[,1]~mash_d[,4],border=colr1,ylab="Mash distance",las=1,cex.axis=1,main="P = 4.89e-10")
stripchart(mash_d[,1]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)

boxplot(mash_d[,2]~mash_d[,4],border=colr1,ylab="Completeness",las=1,cex.axis=1,main="P = 0.20")
stripchart(mash_d[,2]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)

boxplot(mash_d[,3]~mash_d[,4],border=colr1,ylab="Contamination",las=1,cex.axis=1,main="P = 0.63")
stripchart(mash_d[,3]~mash_d[,4], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
dev.off()


mash_d=na.omit(data.frame(cbind(bins_info2$size,bins_info2$num_orfs,bins_info2$num_annots,bins_info2$annot_ratio,c("Rural","Urban")[factor(bins_info2$c2)])))
mash_d[,1]=log10(as.numeric(as.character(mash_d[,1])))
mash_d[,2]=log10(as.numeric(as.character(mash_d[,2])))
mash_d[,3]=log10(as.numeric(as.character(mash_d[,3])))
mash_d[,4]=as.numeric(as.character(mash_d[,4]))
mash_d[,5]=factor(mash_d[,5],levels=c("Rural","Urban"))
colnames(mash_d)=c("size","num_orfs","num_annotations","annotation_ratio","Group")
t.test(mash_d[,1]~mash_d[,5])
t.test(mash_d[,2]~mash_d[,5])
t.test(mash_d[,3]~mash_d[,5])
t.test(mash_d[,4]~mash_d[,5])

pdf("/Users/Shansun/Google\ Drive/MAG_manu/orfs_t1c2.pdf",height=3,width=8)
par(mfrow=c(1,4),mar=c(5,5,2,2))
boxplot(mash_d[,1]~mash_d[,5],border=colr1,ylab="log10(genome size)",las=1,cex.axis=1,main="P = 0.014")
stripchart(mash_d[,1]~mash_d[,5], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
boxplot(mash_d[,2]~mash_d[,5],border=colr1,ylab="log10(num_orfs)",las=1,cex.axis=1,main="P = 0.054")
stripchart(mash_d[,2]~mash_d[,5], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
boxplot(mash_d[,3]~mash_d[,5],border=colr1,ylab="log10(num_annotations)",las=1,cex.axis=1,main="P = 0.0013")
stripchart(mash_d[,3]~mash_d[,5], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
boxplot(mash_d[,4]~mash_d[,5],border=colr1,ylab="annotation_ratio",las=1,cex.axis=1,main="P = 0.020")
stripchart(mash_d[,4]~mash_d[,5], vertical=T, method="jitter",  add=TRUE, pch=16,cex=0.5,col=colr2)
dev.off()

pdf("/Users/Shansun/Google\ Drive/MAG_manu/box_legend.pdf",height=5,width=5)
ggboxplot(mash_d, x = "Group", y = "num_orfs", color = "Group", palette = c("tomato","steelblue"), add = "jitter",  lwd = 1.5,
          ggtheme = theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                                     panel.background = element_blank(), axis.line = element_blank(),axis.text = element_text(size=10),
                                     axis.title=element_text(size=15),legend.text=element_text(size=10),legend.title=element_text(size=15)))
dev.off()

cor_test_rho=matrix(ncol=92,nrow=448)
cor_test_p=matrix(ncol=92,nrow=448)
for (i in 5:92){
  for (j in 1:448){
    if (all(is.na(map_p[,i]))==F){
      if (length(table(map_p[,i]))>10){
        cor_test_rho[j,i]=try(cor.test(map_p[,i],pangenome_relabd[j,],method="spearman")$estimate)
        cor_test_p[j,i]=try(cor.test(map_p[,i],pangenome_relabd[j,],method="spearman")$p.value)
      }else{
        cor_test_rho[j,i]=summary(aov(map_p[,5]~pangenome_relabd[1,]))[[1]][1,4]
        cor_test_p[j,i]=summary(aov(map_p[,5]~pangenome_relabd[1,]))[[1]][1,5]
      }
    }else{
      cor_test_rho[j,i]=NA
      cor_test_p[j,i]=NA
    }
  }
}
colnames(cor_test_rho)=colnames(map_p)
rownames(cor_test_rho)=rownames(pangenome_relabd)
colnames(cor_test_p)=colnames(map_p)
rownames(cor_test_p)=rownames(pangenome_relabd)

cor_test_p=cor_test_p[,-c(1:4,72:76)]
cor_test_rho=cor_test_rho[,-c(1:4,72:76)]
cor_test_fdr=matrix(p.adjust(cor_test_p,method="fdr"),nrow=448)
which(cor_test_p<0.001,T)
table(which(cor_test_p<0.001,T)[,2])

which(cor_test_fdr<0.1,T)

cor_fdrs=data.frame(which(cor_test_fdr<0.1,T))
cor_fdrs$pvalue=cor_test_p[which(cor_test_fdr<0.1)]
cor_fdrs$FDR=cor_test_fdr[which(cor_test_fdr<0.1)]
cor_fdrs=cor_fdrs[order(cor_fdrs[,3]),]

pdf("/Users/Shansun/Google\ Drive/MAG_manu/cors.pdf",height=10,width=10,onefile=T)
par(mfrow=c(2,2),mar=c(5,5,5,5))
for (m in 1:nrow(cor_fdrs)){
  i=cor_fdrs[m,1]
  j=cor_fdrs[m,2]
  tname=paste(rownames(cor_test_p)[i],"\n",colnames(cor_test_p)[j],"\n","rho =",cor_test_rho[i,j],"\n","FDR =",cor_test_fdr[i,j])
  plot(map_p[,-c(1:4,72:76)][,j],pangenome_relabd[i,],main=tname,cex.main=0.5)
  plot(rank(map_p[,-c(1:4,72:76)][,j]),rank(pangenome_relabd[i,]))
}
dev.off()


table(colnames(cor_test_p)[which(cor_test_fdr<0.1,T)[,2]])

cor_test_fdr_index15=p.adjust(cor_test_p[,75],method="fdr")
rownames(pangenome_relabd)[which(cor_test_fdr_index15<0.05&cor_test_rho[,75]>0)]
rownames(pangenome_relabd)[which(cor_test_fdr_index15<0.05&cor_test_rho[,75]<0)]

cor_test_fdr_div=p.adjust(cor_test_p[,82],method="fdr")
rownames(pangenome_relabd)[which(cor_test_fdr_div<0.05&cor_test_rho[,82]>0)]
rownames(pangenome_relabd)[which(cor_test_fdr_div<0.05&cor_test_rho[,82]<0)]

