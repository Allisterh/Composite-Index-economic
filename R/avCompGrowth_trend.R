#' compounded average growth rate for trend
#' 
#' return compounded average growth rate from first cycle to last cycle of a 
#' time series , i.e. trend's growth rate of a time series with cycle character.
#' 
#' @export avCompGrowth_trend 
#' @importFrom devtools install_github
#' @importFrom bbturns bbturns
#' 
#' @param x numeric vector 
#' @param neighbour,mincycle... param of \link{bbturns}
#' @param percentage logical , if \code{TRUE } will return percentage growth rate like 
#' 30, 40, otherwise 0.3, 0.4. 
#' @return Compounded average growth rate (percentage or decimal value).
#' @seealso \link{bbturns}
#' @examples 
#' avCompGrowth_trend(importexport$ImEx)




# Note:
#   先用包[bbturns]计算出转折点信息,然后确定第一个周期和最后一个周期(即一个循环,"峰-谷-峰",或"谷-峰-谷")...
#   即以最先出现的3个转折点作为第一个周期,最后的3个转折点作为最后一个周期.

avCompGrowth_trend = function(x, neighbour = 5, mincycle = 15, phase = 5, extr_limit = 3.5,
                              end_num = 6, mcd_gap = 8, percentage = TRUE)    
  {
  
  
  # exception control
  if (any(is.na(x))) stop("'x' should no 'NA'.")

  x_len = length(x)
  # 计算转折点
  err <- try(library(bbturns),silent=T)
  #正确执行 -- class(err)返回“character” ，出错 -返回 “try-error”
  if ("try-error" %in% class(err))   {
    message("installing package form 'https://github.com/Nisus-Liu/bbturns' ...")
    install_github("Nisu-Liu/bbturns")   # 从GitHub上安装包[bbturns]
  } 
  
  
  turns_inf = suppressMessages(bbturns(x=x, neighbour = neighbour, mincycle = mincycle, phase = phase, extr_limit = extr_limit,
                      end_num=end_num, mcd_gap = mcd_gap) )
  
  peaktrough_idx = turns_inf$peaktrough_idx
  pktr_idx_len = length(peaktrough_idx)
  
  # 
  t1 = peaktrough_idx[1]
  t3 = peaktrough_idx[3]
  
  tn1 = peaktrough_idx[pktr_idx_len - 2]
  tn3 = peaktrough_idx[pktr_idx_len]
  
  # head_cycle_num = t3 - t1 + 1   # 第一个周期的月份数
  # tail_cycle_num = tn3 - tn1 + 1  # 最后周期的月份数
  
  head_center = t1 + (t3-t1)/2  # 第一个周期中心月(周期月份为偶数时,中心月为x.5)
  tail_center = tn1 + (tn3-tn1)/2
  
  tail_head_center_len = round(tail_center - head_center)  # 第一个循环中心到最后个循环中心的间隔
      # 用来计算平均增长率的分母,分母个数不包含第一期的.
  
  
  head_cycle_mean = mean(x[t1:t3])  # 首循环的均值
  tail_cycle_mean = mean(x[tn1:tn3])  # 末循环的均值
  
  comp_gr = (head_cycle_mean / tail_cycle_mean)^(1/tail_head_center_len) - 1
  
  if (percentage)  comp_gr = comp_gr * 100  # 变成百分数
  
  comp_gr
  
}



