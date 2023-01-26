####### L50 ########
##### FEMALES #####
library(FSA)
tiff("Plot4.tif", width = 8, height = 4, units = 'in', res = 300)
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

ggplot(data_plot, aes(x=x, y=fitted)) +
  geom_line(size=0.8, color="#FF0000") +
  geom_ribbon(aes(ymin=CIlower, ymax=CIupper, fill="CI"), alpha=0.2,
              linetype = "dashed")+
  geom_line(aes(y=CIupper), color = "black", linetype = "dashed")+
  geom_line(aes(y=CIlower), color = "black", linetype = "dashed")+
  xlab("Body length (mm)")+ylab("Proportion of adult females")+
  scale_fill_manual(name="CI 95%",values=c("#0000FF"))+
  guides(fill="none")+
  theme_light(base_size = 12)+
  geom_point(data=pontos, aes(x = Cl, y=Freq), size=2)+
  geom_segment(aes(x=ident_l50[2], xend=ident_l50[2], y=0, yend=0.5), colour="blue")+
  geom_segment(aes(x=min(data_plot$x), xend=ident_l50[2], y=0.5, yend=0.5), colour="blue")+
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01))+
  geom_point(aes(x=ident_l50[2], y=0.5), colour="blue", size=3.5)
# annotate("text", x=ident_l50[2]+2, y=0.5, label="L  =", size=5)+
dev.off()

#################### MALES ##############
tiff("Plot5.tif", width = 8, height = 4, units = 'in', res = 300)
classify_data = classify_mature(Dados_tanaidacea, 
varNames = c("L", "Lquel"), varSex = "Sex", selectSex = "male", method = "ld")
plot(classify_data, xlab = "Body length (mm)", ylab = "Chela length (mm)", 
col = c("black", "black"), pch = c(19, 3), lwd = c(0.9,0.9), cex = c(1.2,1.2),legendPlot = TRUE)

dev.off()
