####### L50 ########
##### FEMALES #####
library(FSA)
ok<-Dados_tanaidacea[!Dados_tanaidacea$Sex=='male',]
L50_sp1 <- lencat(~L,data=ok, startcat = 0, w=0.2)
head(L50_sp1)
table(L50_sp1$Maturity)
tblLen <- with(L50_sp1,table(LCat,L50_sp1$Maturity))
str(tblLen)
ptblLen <- as.data.frame(prop.table(tblLen,margin=1))
dots<-filter(ptblLen, Var2=="mat")
Cl<-as.numeric(paste(dots$LCat))
pontos<-cbind(dots,Cl)
pontos
L50_sp1= gonad_mature(ok, varNames = c("L", "Maturity"), inmName = "im",                      
matName = "mat", method = "fq", niter = 999)
print(L50_sp1)
plot(L50_sp1, xlab = "Body length (mm)", ylab = "Proportion of mature females", 
     col = c("black", "blue"), type="n")

data_plot<-L50_sp1$out
ident_l50<-round(quantile(L50_sp1$L50_boot, probs = c(0.05,0.5,0.95)), 2)

tiff("l50.tif", width = 8, height = 5, units = 'in', res = 300)
ggplot(data_plot, aes(x=x, y=fitted))+
  geom_line(size=1, color="#FF0000") +
  geom_ribbon(aes(ymin=CIlower, ymax=CIupper, fill="CI"), alpha=0.25,
              linetype = "dashed")+
  geom_line(aes(y=CIupper), color = "black", linetype = "dashed",lwd=.7)+
  geom_line(aes(y=CIlower), color = "black", linetype = "dashed",lwd=.7)+
  xlab("Body length (mm)")+ylab("Proportion of adult females")+
  scale_fill_manual(name="CI 95%",values=c("#0000FF"))+
  guides(fill="none")+
  theme_light(base_size = 12)+
  geom_point(data=pontos, aes(x = Cl, y=Freq), size=2)+
  geom_segment(aes(x=ident_l50[2], xend=ident_l50[2], y=0, yend=0.5), colour="blue",size=.85)+
  geom_segment(aes(x=min(data_plot$x), xend=ident_l50[2], y=0.5, yend=0.5), colour="blue",size=.85)+
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01))+theme_bw()+
  geom_point(aes(x=ident_l50[2], y=0.5), colour="blue", size=3)+
  theme(panel.grid = element_blank(),strip.placement = "outside",
        strip.background = element_rect(fill=NA,color="black"),panel.spacing = unit(0,"cm"),axis.title.x = element_text(vjust = -.3),axis.text.x = element_text(colour="black",size=15),
        axis.text.y = element_text(colour="black",size=15),legend.text = element_text(colour="black",size=17), legend.title = element_blank(),
        strip.text = element_text(colour="black",size=15),axis.title = element_text(size=15))+
  annotate("text", x = 1, y = .85, parse = TRUE,label = "L50==2.3",size=5)+
  annotate("text", x = 1, y = .75, parse = TRUE,label = "R^2==0.96",size=5)

dev.off()

#################### MALES ##############
tiff("males.tif", width = 8, height = 5, units = 'in', res = 300)
classify_data = classify_mature(Dados_tanaidacea, 
varNames = c("L", "Lquel"), varSex = "Sex", selectSex = "male", method = "ld")
plot(classify_data, xlab = "Body length (mm)", ylab = "Chela length (mm)", 
col = c("black", "black"), pch = c(16,3), lwd = c(2,2), cex.lab = 1.3, cex.axis=1.2,cex = c(2,2),legendPlot = F)

dev.off()
