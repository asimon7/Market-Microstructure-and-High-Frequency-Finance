library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(fitdistrplus)
library(scales)

# load the data
WIG20 = fread("fw20_d.csv")[, .(Data, Zamkniecie, Wolumen)]
colnames(WIG20) = c("Date", "Close", "Volume")

# we assume 0% risk-free rate

# multiply by the multiplyer
WIG20$Close = 20 * WIG20$Close

#calculate differences
WIG20[, Return := c(NA, diff(WIG20$Close))]

avg_price = mean(WIG20$Close)
avg_vol = mean(WIG20$Volume)
sd_return = sd(WIG20$Return, na.rm = T)

#compute means and standard deviations
Returns_SD = data.table(
  "MA_5D" = frollmean(WIG20$Return, 5),
  "SD_5D" = frollapply(WIG20$Return, n = 5, FUN =  sd),
  "MA_1Y" = frollmean(WIG20$Return, 250),
  "SD_1Y" = frollapply(WIG20$Return, 250, sd),
  "MA_5Y" = frollmean(WIG20$Return, 5 * 250),
  "SD_5Y" = frollapply(WIG20$Return, n = 5 * 250, sd)
)
# calculate the rolling sharpe ratios
Reg = data.table(
  "REG_5D" = Returns_SD$MA_5D / Returns_SD$SD_5D,
  "REG_1Y" = Returns_SD$MA_1Y / Returns_SD$SD_1Y,
  "REG_5Y" = Returns_SD$MA_5Y / Returns_SD$SD_5Y
)

#combine
WIG20 = cbind(WIG20, Returns_SD, Reg)

# fit parameters of normal distribution to rolling sharpe ratios (shocks to the rolling sharpe ratios)
fit_5D = fitdist(Reg$REG_5D[!is.na(Reg$REG_5D)], distr = "norm")$estimate
fit_1Y = fitdist(Reg$REG_1Y[!is.na(Reg$REG_1Y)], distr = "norm")$estimate
fit_5Y = fitdist(Reg$REG_5Y[!is.na(Reg$REG_5Y)], distr = "norm")$estimate

#set.seed(1)

# table of shocks
j = 14 ##### = 14
#while ((sharpe_mark > 1.4 *sharpe_opt) | (sharpe_stat > sharpe_opt)) {
  set.seed(j)
  shocks = data.frame(
    "5D" = rnorm(4257, fit_5D[1] * 1, fit_5D[2]),
    "1Y" = rnorm(4257, fit_1Y[1] * 1, fit_1Y[2]),
    "5Y" = rnorm(4257, fit_5Y[1] * 1, fit_5Y[2])
  )
  
  WIG20 = na.omit(WIG20)
  
  #table of predictors
  predictors = data.frame(
    "5D" = WIG20$REG_5D[1],
    "1Y" = WIG20$REG_1Y[1],
    "5Y" = WIG20$REG_5Y[1]
  )
  # decays as in the paper
  decay = c(0.2519, 0.0034, 0.0010)
  
  # formula according to the paper
  for (i in 2:4258) {
    predictors[i, ] = predictors[(i-1), ] * (1 - decay) + shocks[(i-1), ]
  }
  
  
  
  model_df = cbind(WIG20$Date, WIG20$Return, predictors)
  
  # create linear regresssion
  model = lm(WIG20$Return[2:4258] ~ X5D[1:4257] + X1Y[1:4257] + X5Y[1:4257], model_df)
  summary(model)
  
  #risk aversion coefficient
  gamma = 10 ^ (-6) #-7, -8, -4, -9
  
  # discount rate
  rho = 1 - exp(-0.02 / 260)
  
  # transaction costs
  lambda = 2 * ((0.001 * avg_price) / (0.0159 * avg_vol * sd_return ^ 2))
  
  
  # alpha (according to the paper)
  alpha = (-(gamma * (1 - rho) + lambda * rho) + sqrt((gamma * (1 - rho) + lambda * rho) ^ 2 + 4 * gamma *
                                                        lambda * (1 - rho) ^ 2) ) / (2 * (1 - rho))
  
  predictors = cbind(1, predictors)
  
  # Optimal portfolio
  X_vec = 0
  
  decay = c(0, decay)
  for (i in 2:4258) {
    X_vec[i] = (1 - alpha / lambda) * X_vec[(i - 1)] + alpha / lambda * (1 / (gamma * sd_return ^ 2)) * 
      sum(predictors[(i - 1), ] * model$coefficients / (1 + decay * alpha / gamma))
  }
  
  
  # Markovitz portfolio
  Markovitz = (gamma * sd_return ^ 2) ^ (-1) * apply(predictors * model$coefficients, 1, sum)
  
  
  results_df = data.frame(
    Date = as.Date(WIG20$Date, tryFormats = c("%d.%m.%Y")),
    X = 1:length(X_vec),
    Optimal = X_vec,
    Markovitz = Markovitz
  )
  
  # ggplot(data = results_df) +
  #   geom_point(aes(x = Date, y = Markovitz / 10^5), col = "navyblue", size = 0.75) +
  #   scale_x_date(date_breaks = "1 year") +
  #   geom_line(aes(x = Date, y = Optimal / 10^5), color = "red", size = 1.2) +
  #   theme(axis.text.x = element_text(angle = 45)) +
  #   ggtitle("Position in WIG20 Futures") +
  #   theme(plot.title = element_text(hjust = 0.5))+
  #   labs(y = "1 / 10^5")
  
  #par(mfrow = c(2, 2))
  
  
  #### Static portfolio ####
  
  Static = 0
  for (i in 2:4258) {
    Static[i] = Static[(i - 1)] + 
      gamma / (gamma + lambda) * ( (gamma * sd_return ^ 2) ^ (-1) * sum(predictors[(i-1), ] * model$coefficients) 
                                   - Static[(i-1)])
  }
  
  results_df = cbind(
    results_df, data.frame(Static = Static)
  )
  
  ### plot results ####
  ggplot(data = results_df) +
    geom_point(aes(x = Date, y = Markovitz, colour = "Markovitz"), size = 0.75) +
    scale_x_date(date_breaks = "1 year") +
    geom_line(aes(x = Date, y = Optimal, colour = "Optimal"), size = 0.75) +
    geom_line(aes(x = Date, y = Static, colour = "Static"), size = 0.75) +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("Position in WIG20 Futures") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(name = "Model", values = c("Optimal" = "red",
                                                  "Static" = "green", "Markovitz" = "navyblue")) +
    labs(y = "Size of the position")
  
  #### sharpe ratios ####
  
  sharp_df = cbind(results_df, WIG20$Return)
  
  res_opt = diff(sharp_df$Optimal) * sharp_df$`WIG20$Return`[2:length(sharp_df[, 1])]
  
  sharpe_opt = mean(res_opt) / sd(res_opt)
  
  res_stat = diff(sharp_df$Static) * sharp_df$`WIG20$Return`[2:length(sharp_df[, 1])]
  
  sharpe_stat = mean(res_stat) / sd(res_stat) 
  
  res_mark = diff(sharp_df$Markovitz) * sharp_df$`WIG20$Return`[2:length(sharp_df[, 1])]
  
  sharpe_mark = mean(res_mark) / sd(res_mark)
#}

sharpe_opt * sqrt(250)
sharpe_mark * sqrt(250)
sharpe_stat * sqrt(250)



#### charts #####
ggplot(data = results_df) +
  geom_point(aes(x = Date, y = Markovitz, colour = "Markovitz"), size = 0.75) +
  scale_x_date(date_breaks = "1 year") +
  geom_point(aes(x = Date, y = Optimal, colour = "Optimal"), size = 0.75) +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Position in WIG20 Futures") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = "Model", values = c("Optimal" = "red",
                                                "Markovitz" = "navyblue")) +
  labs(y = "Size of the position")

ggplot(data = results_df) +
  scale_x_date(date_breaks = "1 year") +
  geom_point(aes(x = Date, y = Optimal, colour = "Optimal"), size = 0.75) +
  geom_point(aes(x = Date, y = Static, colour = "Static"), size = 0.75) +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Position in WIG20 Futures") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = "Model", values = c("Optimal" = "red",
                                                "Static" = "green")) +
  labs(y = "Size of the position")

sharpe_table = data.frame(
  "Daily" = c(sharpe_opt, sharpe_stat, sharpe_mark),
  "Annualized" = c(sharpe_opt, sharpe_stat, sharpe_mark) * sqrt(250)
)

dif_gamma = lapply(c(-4, -7, -8, -9), gamma_fun)

annotate_figure(
  ggarrange(
    plotlist = list(
      dif_gamma[[1]][[1]],
      dif_gamma[[2]][[1]],
      dif_gamma[[3]][[1]],
      dif_gamma[[4]][[1]]
    ),
    common.legend = T
  ),
  top = text_grob("Position in WIG20 Futures over time", face = "bold", size = 12),
  left = text_grob("Size of the position", rot = 90)
)

annotate_figure(
  ggarrange(
    plotlist = list(
      dif_gamma[[1]][[2]],
      dif_gamma[[2]][[2]],
      dif_gamma[[3]][[2]],
      dif_gamma[[4]][[2]]
    ),
    common.legend = T
  ),
  top = text_grob("Position in WIG20 Futures over time", face = "bold", size = 12),
  left = text_grob("Size of the position", rot = 90)
)

sharpe_4_df = rbind(
  dif_gamma[[1]][[3]],
  dif_gamma[[2]][[3]],
  dif_gamma[[3]][[3]],
  dif_gamma[[4]][[3]]
)
sharpe_4_df
