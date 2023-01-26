####### SCRIPT FROM ALVES ET AL. 2022 ####
##### BY: RODRIGO ALVES, ALEX SOUZA LIRA AND LEANDRO N. EDUARDO #####

## OPEN PACKAGES AND DATA ###
rm(list=ls())
pacman::p_load(ggplot2,foreign, reshape2,readxl,ade4,vegan,TropFishR,parallel
,devtools,fishboot,ks,minpack.lm,devtools,ggplot2,ggridges,fishmethods,sizeMat,FSA,pgirmess,ade4,Rmisc)
Dados_tanaidacea <- read_excel("C:/Users/USER/Desktop/MESTRADO/Dados_tanaidacea.xlsx", sheet = "Planilha1")

tiff("Plot3.tif", width = 8, height = 8, units = 'in', res = 300)
####### ABUNDANCES X RAINFALL #####
b <- ggplot(teste) + geom_col(aes(x = ordem, y = rain), size = 1, 
color = "black", fill = "gray") +geom_line(aes(x = ordem, 
y = ab), size = 1.1, color="red", group = 1)+geom_errorbar(aes(x=ordem, ymin=ab-se, 
ymax=ab+se), width=.2,position=position_dodge(.9))+geom_point(aes(x=ordem,y=ab))+
scale_x_discrete(labels = c("jul'19","aug'19","sep'19","oct'19","nov'19","dec'19","jan'20","feb'20","apr'20","may'20","jun'20","jul'20"))+ 
scale_y_continuous(sec.axis = sec_axis(~./1,name = "Abundance (individuals per 100g of algal dry weight)"))+xlab("Month") + ylab("Cumulative rainfall (mm)")+theme_bw()


########### LINEPLOT OF LENGTHS #########
library(ggpubr)
p <- ggline(Dados_tanaidacea, x = "Month", y = "L",add = c("mean_se"),
xlab="Month",ylab="Body length (mm)",tag="B)",ylim=c(1.5,2.75),position=position_dodge(0.3),
ggtheme = theme_bw())+geom_rect(xmin=1.75,xmax=3.7,ymin=0,ymax=3,colour="gray", alpha=0.005)+
geom_rect(xmin=3.7,xmax=4.3,ymin=0,ymax=3,colour="gray",alpha=0.006)
ggarrange(b, p, ncol = 1, nrow = 2)
dev.off()


###########
tiff("Plot2.tif", width = 8, height = 8, units = 'in', res = 300)
##### STACKED BAR PLOT #################
mytable<-table(Dados_tanaidacea$Month,Dados_tanaidacea$Dev)
mytable2<-prop.table(mytable,1)
mytable2
myData2<-as.data.frame.matrix(mytable2)
#####m CHANGE VARIABLE NAME ###
myData2$ordem<-rownames(myData2)
myDataLong<-melt(myData2, id.vars=c("ordem"), value.name = "Proportion")
View(myDataLong)
names(myDataLong)[2]<-paste("Stage")
p <- ggplot() + geom_bar(aes(y = Proportion, 
                             x = ordem, fill = factor(Stage,levels=c("juvenile","non-reproductive female","male","preov female","ov female"))),
                         data = myDataLong,
                         stat="identity", colour="black") +
  theme_bw() + theme(panel.grid = element_blank())+labs(x="Month",tag="B)")+
  scale_x_discrete(limit = c("jul'19","aug'19","sep'19",
                             "oct'19","nov'19","dec'19","jan'20","feb'20","apr'20","may'20","jun'20","jul'20"))
p$labels$fill <- ""
p

######## LINE PLOT PER DEV ####
library(tidyverse)
library(ggpubr)
Dados_tanaidacea$new <- fct_recode(Dados_tanaidacea$Dev,
                                   reproductive = "ov female",
                                   reproductive = "preov female", 
                                   reproductive = "male")

b <- Dados_tanaidacea %>%
  group_by(Month,new) %>% 
  summarise(n=n()) %>% 
  ggplot()+geom_point(aes(Month,n,color=new,shape=new))+
  geom_line(aes(Month,n,group=new,color=new))+
  theme_bw()+ scale_x_discrete(limit = c("jul'19","aug'19","sep'19",
                                         "oct'19","nov'19","dec'19","jan'20","feb'20","apr'20","may'20","jun'20","jul'20"))+
  scale_y_log10()+scale_color_manual(values=c("#6a66a3","#566E3D","#a71d31"))+labs(y="Number of individuals",x=" ",tag="A)")


ggarrange(b, p, ncol = 1, nrow = 2)
dev.off()

###### VIOLIN PLOT #####
tiff("Plot1.tif", width = 8, height = 4, units = 'in', res = 300)
ggplot(Dados_tanaidacea, aes(x=Stage, y=L)) + geom_violin(fill='#A4A4A4', 
color="black")+scale_x_discrete(limits=c("MII","MIII","Neutrum","male",
"preov female","ov female"))+ geom_boxplot(width=0.1)+
stat_summary(fun = mean, geom = "point", col = "dark gray")+
labs(y="Body length (mm)")+theme_bw()
dev.off()

################ ANOVA ############
ab<-c(136,	35,	80,	200,	107,	250,	67,	181,	92,	260,	8,	325,	119,	300,	1075,	17,	50,	63,	69,	25,	65,	50,	60,	38,	33,	760,	250,	444,	200,	1225,	100,	350,	125,	322,	180,	625,	417,	279,	1400,	1030,	417,	433,	0,	367,	350,	1140,	543,	314,	800,	775,	671,	331,	0,	930,	311,	195,	117,	31,	223,	105,	100,	359,	389,	380,	575,	583,	250,	650,	280,	241,	314,	147,	128,	133,	352,	131,	227,	220,	12,	100,	217,160,	286,	188,	50,	109,	17,	94,	86,	25,	43,	13,	233,	35,	115,	136,	6,520,	40,	7,	25,	38,	100,	0,	15,	50,	27,	80,	20,	18,	145,	35,	110,	900,	430)
dadoslog<-log10(ab+1)
meses<-c(rep("jul",10),rep("ago",5),rep("sep",10),rep("oct",10),rep("nov",10),rep("dec",10),rep("jan",10),rep("feb",10),rep("apr",10),rep("may",10),rep("jun",10),rep("julh",10))
dados<-cbind(meses,dadoslog)
dados1<-as.data.frame(dados)
model <- aov(dadoslog ~ meses, data = dados1)
summary(model)
TukeyHSD(model)

######## CHI SQUARED TEST #######
#### PER MONTH #####
m<-prop.test(x = 9, n = 64, alternative = 'two.sided') #x = N OF MALES, n= TOTAL
m
m$p.value<0.05

#### PER SIZE CLASSES #####
library(FSA)
teste <- lencat(~L,data=ok, startcat = 0, w=0.5)
teste <- lencat(~L,data=ok, breaks=c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5),
                startcat = 0, w=0.5)
library(plyr);library(dplyr)
cdata <- ddply(teste, c("LCat","Sex"), summarise,
N    = length(Sex))

