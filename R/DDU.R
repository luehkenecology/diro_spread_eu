library(RcppRoll)

DDU <- function(x, t_seq = tseq,
                subs = SUBS,
                cum_temp = 130,
                min_temp_thres = 14, mosquito_survival_period = 30){
  
  DDU <- ifelse(as.numeric(x) >= min_temp_thres, 
                as.numeric(x) - min_temp_thres, 
                0)
  #
  data_full_666 <- data.frame(year = year(t_seq), DDU_666 = DDU, SUBS)
  
  einsd_666 <- ddply(data_full_666, .(year), summarize, result = sum(DDU_666, na.rm = T))
  
  
  # cumulative sum of DDUs
  DDU_cum <- roll_sum(DDU,
                      n = mosquito_survival_period,
                      fill = 0, align = "right")
  
  DDU_pos <- ifelse(DDU_cum >= cum_temp, 1, 0)
  
  #
  data_full <- data.frame(year = year(t_seq), DDU_pos = DDU_pos, SUBS)
  
  data_full_2 <- subset(data_full, data_full[,3] > 0)
  
  einsd <- ddply(data_full_2, .(year), summarize, result = sum(DDU_pos, na.rm = T))
  
  c(einsd$result, einsd_666$result)
}
