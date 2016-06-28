library(RcppRoll)

ddu_calculations <- function(x, 
                             t_seq = tseq,
                             suit = SUBS,
                             cum_temp = 130,
                             min_temp_thres = 14, # threshold for the development
                             mosquito_survival_period = 30){
  
  # calculate daily DDUs
  ddus <- ifelse(as.numeric(x) >= min_temp_thres, 
                 as.numeric(x) - min_temp_thres, 
                 0)
  
  ##########
  # calculate the yearly sum of daily DDUs
  ##########
  
  # add information of the year and suitability to the daily DDU information
  data_frame_daily_ddu <- data.frame(year = year(t_seq),
                                     ddu = ddus,
                                     suit)
  
  # subset days in the suitability
  data_frame_daily_ddu_sub <- subset(data_frame_daily_ddu, 
                                   data_frame_daily_ddu[,3] > 0)
  
  # calculate yearly sum of daily DDUs
  yearly_sum_ddus <- ddply(data_frame_daily_ddu_sub, .(year), 
                           summarize, 
                           result = sum(ddu, na.rm = T))
  
  ##########
  # calculate potential days of transmission
  ##########
  # cumulative sum of DDUs
  daily_cum_ddu <- roll_sum(ddus,
                            n = mosquito_survival_period,
                            fill = 0,
                            align = "right")
  
  # identify days, which reach the threshold of cumulative DDUs
  ddu_pos <- ifelse(daily_cum_ddu >= cum_temp, 1, 0)
  
  # add information of the year and suitability to the days, 
  # which reach the threshold of cumulative DDUs
  data_frame_cum_ddu <- data.frame(year = year(t_seq),
                                   ddu_pos = ddu_pos,
                                   suit)
  
  # subset days in the suitability
  data_frame_cum_ddu_sub <- subset(data_frame_cum_ddu, 
                                   data_frame_cum_ddu[,3] > 0)
  
  # calculate yearly sum of days allow transmission
  yearly_sum_of_cum_ddu <- ddply(data_frame_cum_ddu_sub, .(year),
                                 summarize,
                                 result = sum(ddu_pos, na.rm = T))
  
  # output
  c(yearly_sum_of_cum_ddu$result, yearly_sum_ddus$result)
}