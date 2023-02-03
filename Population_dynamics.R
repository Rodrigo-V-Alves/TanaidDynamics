####################### GROWTH #####
ggplot(Dados_tanaidacea, aes(x = L, y = Month, fill=Sex)) +
geom_density_ridges(scale=1.1, size = 0.3, alpha=0.3)+ 
scale_fill_manual(values = c("green", "blue","red"), name = "Sex")+ 
geom_density_ridges(scale=1.1, size = 0.3, alpha=0.4, stat = "binline")+ ylab("Months")+ xlab(label = "Body length  (mm)")+ scale_y_discrete(limit = c("jul'20","jun'20","may'20","apr'20","feb'20","jan'20","dec'19","nov'19","oct'19","sep'19","aug'19","jul'19")) 

data_LFQ_all<-dplyr::select(Dados_tanaidacea, Month, Date, L)
data_LFQ_all$Date <- as.Date(data_LFQ_all$Date, format = "%Y.%m.%d")
LFq_vector <- lfqCreate(data = data_LFQ_all, #Lmin = 0.5,
Lname = "L", Dname = "Date", bin_size = 0.2,
aggregate_dates = F, length_unit = "cm")
plot(LFq_vector, Fname = "catch", hist.sc = .8, 
     image.col = NA, xlab = "Months",
     ylab = "Carapace length - mm",
     hist.col = c("white", "grey", "orange", "darkgreen"),
     ylim=c(0,max(LFq_vector$midLengths+0.5)),
     xlim=range(LFq_vector$dates)+c(-30, 0),
     date.axis = "modern")
lfq_str <- lfqRestructure(LFq_vector, MA = 11, addl.sqrt = F)
plot(lfq_str, Fname = "catch", date.axis = "modern", hist.sc = .5,
     xlim=range(LFq_vector$dates)+c(-30, 0))
plot(lfq_str, Fname = "rcounts", date.axis = "modern", hist.sc = .9,
     hist.col = c("white", "black"), ylim=c(0,max(lfq_str$midLengths)),
     xlim=range(LFq_vector$dates)+c(-30, 0),
     image.col = c(rep(rgb(1,0.8,0.8),1000), "white", rep(rgb(0.8,0.8,1),1000)))
dev.off()

## ESTIMATE K VALUES WITH FIXED LINF ##
res_KScan <- ELEFAN(lfq_str, Linf_fix = 4.35,
                    MA=11, addl.sqrt = F, hide.progressbar = TRUE)
unlist(res_KScan$par) 
KScan<-unlist(res_KScan$par)
res_KScan$Rn_max
res_RSA <- ELEFAN(lfq_str, Linf_range = seq(5,
                                            6,0.1), MA = 11,
                  K_range = seq(2.5,5,0.1), addl.sqrt = F,
                  hide.progressbar = TRUE, contour=5)
points(res_RSA$par["Linf"], res_RSA$par["K"], pch="*", cex=3, col=2)
# show results
unlist(res_RSA$par)
RSA_r<-unlist(res_RSA$par)
RSA_r

####BOOTSTRAPPED ELEFAN_GA ###
MA <- 11
low_par <- list(Linf = 5, K = 2.5, t_anchor = 0, C = 0, ts = 0)
up_par <- list(Linf =  6, K = 5, t_anchor = 0.000001, C = 1, ts = 1)
seasonalised <- F
popSize <- 60
maxiter <- 50
run <- 10
pmutation <- 0.2
nresamp <- 10
###Full bootstrap 
t1 <- Sys.time()
res_EGAF <- ELEFAN_GA_boot(lfq=lfq_str, MA = MA, seasonalised = seasonalised,
                           up_par = up_par, low_par = low_par,
                           popSize = popSize, maxiter = maxiter, run = run, pmutation = pmutation,
                           nresamp = nresamp, parallel = TRUE, no_cores = detectCores(),
                           seed = 1, resample = TRUE
)
t2 <- Sys.time()
t2 - t1


# plot results
#univariate density plot of bootstrapped pars
jpeg(paste("C:\\...", 
           ifelse(seasonalised, "soVBGF", "VBGF"), "univariate_density.jpg", sep="_"), 
     width = 7, height = 5, units = "in", res = 600)
univariate_density(res_EGAF)
dev.off()

# VBGF by time growth curve plot
jpeg(paste("C:\\...", 
           ifelse(seasonalised, "soVBGF", "VBGF"), "growth_time.jpg", sep="_"), 
     width = 7, height = 5, units = "in", res = 600)
tiff("Plot6.tif", width = 8, height = 4, units = 'in', res = 300)
op <- par(mar=c(3,3,1,1), mgp = c(2, 0.5, 0))
CIinfo <- vbgfCI_time(
  res = res_EGAF,
  CI = 95,
  perm.col = adjustcolor("grey50",0.1), perm.lwd = 1,
  ci.col = c("red", "red"), ci.lty = 2, ci.lwd = 1, 
  maxd.col = 1, maxd.lty = 1, maxd.lwd = 2)
par(op)
dev.off()
max_den_GA<-CIinfo$max_den

###Estimation of Natural Mortality - M####
par_growth<-list(Linf= 5.26,
                 K_l= 3.36,
                 t_anchor= 0.0)
lfq_str_stck <-list()
lfq_str_stck  <- c(lfq_str, par_growth)
class(lfq_str_stck) <- "lfq"
Ms <- M_empirical(Linf = lfq_str_stck$Linf, K_l = lfq_str_stck$K, method = "Then_growth")
lfq_str_stck$M <- as.numeric(Ms)
Nat_mort<-lfq_str_stck$M
