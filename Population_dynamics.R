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

res_PW <- powell_wetherall(param = lfq_str,
                           catch_columns = 1:ncol(lfq_str$catch))
# show results
paste("Linf =",(res_PW$Linf_est), "?", (res_PW$se_Linf))

mP_W.ffun_edit_by_ALEX <- function(Len, Cat) { 
  
  
  
  N_c <- length(Len) # N of length classes
  int_Len  <- (max(Len)-min(Len))/ (N_c-1) # interval (class width)
  L_c <- Len - (int_Len/2)             # cutoff lengths (L - 0.5)
  # L_c <- Len - 0.5
  
  # plot(Len, Cat)
  
  
  ### Calculate Mean lengths
  #Catch*L
  Catch_L <- Cat * Len
  
  #Sum of "Catch*L" above each Lc
  #write loop
  
  Sum_of_Catch_times_L <- rep(0, N_c)
  
  for(i in 1:N_c){
    
    Sum_of_Catch_times_L[i] <- sum(Catch_L[i:N_c]);
  }
  
  #Sum of Catch above Lc
  #write loop
  
  Sum_of_Catch <- rep(0, N_c)
  
  for(i in 1:N_c){
    
    Sum_of_Catch[i] <- sum(Cat[i:N_c]);
  }
  
  # Calculate mean length for each Lc
  
  
  mean_length <- Sum_of_Catch_times_L / Sum_of_Catch
  
  #Pauly's modified P-W_plot method
  # mP-W plot
  
  # GAMMA selection: select data from Mode+20% to 90%Lmax
  
  mean_length  # mean length
  L_c   # cutoff length
  
  df_1 <- data.frame(mean_length, L_c, Len, Cat)
  
  d_mode <- df_1$Len[df_1$Cat == max(df_1$Cat)]  # mode of original LFD data
  
  df_gamma <- subset(df_1, Len  > (1.1 * d_mode))  
  df_gamma <- subset(df_gamma, Len  < (0.93 * max(Len)))  
  
  
  mean_length_G  <-  df_gamma$mean_length # Gamma selected mean lengths
  L_c_G   <- df_gamma$L_c # Gamma selected cutoff lengths  
  
  y_p <- (mean_length - L_c)
  y_p_G <- (mean_length_G - L_c_G)
  
  plot((mean_length - L_c) ~ L_c)
  
  # plot((mean_length - L_c) ~ L_c, xlim = c(min(L_c), max(L_c)), ylim = c(min(y_p), max(y_p)))
  
  points((mean_length_G - L_c_G) ~ L_c_G, pch = 16, col = "blue")
  
  
  mP_W_mod1 <- lm((mean_length_G - L_c_G) ~ L_c_G)
  abline(mP_W_mod1, col = "blue")
  
  a.mP_W_mod1 <- summary(mP_W_mod1)$coefficients[1:1]
  b.mP_W_mod1 <- summary(mP_W_mod1)$coefficients[2,1]
  
  #  Linf = a / -b  and Z/K = (1 + b)/(-  b) 
  L_inf_mP_W <-  a.mP_W_mod1 / (-1 * b.mP_W_mod1)
  
  Z_K_mP_W = (1 + b.mP_W_mod1)/(-1 *  b.mP_W_mod1)
  
  sum_lm1<-summary(mP_W_mod1) 
  r_lm1 <- sum_lm1$r.squared
  intercept_lm1 <- sum_lm1$coefficients[1]
  slope_lm1 <- sum_lm1$coefficients[2]
  se_slope_lm1 <- sum_lm1$coefficients[4]
  se_intercept_lm1 <- sum_lm1$coefficients[3]
  lm1.fit <- sum_lm1$r.squared
  SE_slope <- abs(se_slope_lm1)
  confi_slope <- abs(se_slope_lm1) * qt(0.975, sum_lm1$df[2])
  conf_slope <- b.mP_W_mod1 + c(-confi_slope, confi_slope)
  SE_intercept <- abs(se_intercept_lm1)
  confi_intercept <- abs(se_intercept_lm1) * qt(0.975, 
                                                sum_lm1$df[1])
  conf_intercept <-   a.mP_W_mod1 + c(-confi_intercept, 
                                      confi_intercept)
  se_Linf.BH <- (abs(SE_intercept)/abs(a.mP_W_mod1) + 
                   abs(SE_slope)/abs(b.mP_W_mod1)) * (abs(a.mP_W_mod1)/abs(b.mP_W_mod1))
  confi_Linf <- (abs(SE_intercept)/abs(a.mP_W_mod1) + 
                   abs(SE_slope)/abs(b.mP_W_mod1)) * (abs(a.mP_W_mod1)/abs(b.mP_W_mod1)) * 
    qt(0.975, sum_lm1$df[2])
  conf_Linf.BH <- L_inf_mP_W + c(-confi_Linf, confi_Linf)
  ZK.BH <- (-(1 + b.mP_W_mod1)/b.mP_W_mod1)
  se_ZK.BH <- abs(SE_slope)
  confi_ZK <- se_ZK.BH * qt(0.975, sum_lm1$df[2])
  conf_ZK.BH <- Z_K_mP_W + c(-confi_ZK, confi_ZK)
  
  ;list(LinF=L_inf_mP_W, se_Linf=se_Linf.BH, confidenceInt_Linf=conf_Linf.BH,
        ZK=Z_K_mP_W, se_ZK=se_ZK.BH, confidenceInt_ZK=conf_ZK.BH)  # output
  
}  
L <- lfq_str$midLengths
Catch <- rowSums (lfq_str$catch, na.rm = FALSE, dims = 1)
plot(L,Catch)
results<-mP_W.ffun_edit_by_ALEX(L, Catch)
unlist(results)

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
#ELEFAN_SA#####
MA <- 11
low_par <- list(Linf = 5, K = 2.5, t_anchor = 0, C = 0, ts = 0)
up_par <- list(Linf =  6, K = 5, t_anchor = 0.000001, C = 1, ts = 1)
init_par <- NULL #Podemos colocar parametros interm?diarios tamb?m.
seasonalised <- F
SA_temp = 5e+05
SA_time = 60*5
maxit <- 500
nresamp <- 250

ELEF_SA <- ELEFAN_SA(lfq_str, seasonalised = F, init_par = init_par,
                     low_par = low_par, up_par = up_par, 
                     SA_time = SA_time, SA_temp = SA_temp,
                     maxit = maxit, MA = MA)
max_den_SA<-CIinfo$max_den
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

#####Plot conjuto de compara??o de curvas####
jpeg(paste("C:\\...", 
           "fit.curves.jpg", sep="_"), 
     width = 7, height = 5, units = "in", res = 600)
op <- par(mar=c(3,3,1,1), mgp = c(2, 0.5, 0))
plot(lfq_str, Fname = "rcounts",date.axis = "modern", hist.sc = .8,
     hist.col = c("white", "black"), 
     image.col = c(rep(rgb(1,0.8,0.8),1000), "white", rep(rgb(0.8,0.8,1),1000)))
# lt <- lfqFitCurves(LFq_vector, par = res_KScan$par,
#                    draw = TRUE, col = "grey", lty = 2, lwd=1.5)
# lt <- lfqFitCurves(LFq_vector, par = res_RSA$par,
#                    draw = TRUE, col = "darkgreen", lty = 1, lwd=1.5)
lt <- lfqFitCurves(LFq_vector, par = max_den_SA,
                   draw = TRUE, col = "red", lty = 5, lwd=1.5)
lt <- lfqFitCurves(LFq_vector, par = max_den_GA,
                   draw = TRUE, col = "blue", lty = 4, lwd=1.5)
par(op)
dev.off()

par_growth<-list(Linf= 5.26,
                 K_l= 3.36,
                 t_anchor= 0.0)
lfq_str_stck <-list()
lfq_str_stck  <- c(lfq_str, par_growth)
class(lfq_str_stck) <- "lfq"
###Estimation of Natural Mortality - M####
Ms <- M_empirical(Linf = lfq_str_stck$Linf, K_l = lfq_str_stck$K, method = "Then_growth")
lfq_str_stck$M <- as.numeric(Ms)
Nat_mort<-lfq_str_stck$M
