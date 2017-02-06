#' Composite Index
#' 
#' composite consistent index from consistent indicators, composite leading index from leading indicators, composite lagging index from lagging indicators.
#' The consistent / leading / lagging indicators were determined by corresponding expert 
#' or algorithms like \link{KL}, \link{crosscor} ... Generally, these are economic indicators,
#' whose value is itself, or cycle part from seasonal-adjusted (\eqn{C}), or its year-on-year growth rate 
#' of trend and cycle part (\eqn{TC}) from seasonal-adjusted.
#'  
#' @export CompositeIndex
#' @param consistent,leading,lagging data frame, including consistent indicators, leading indicators, 
#' lagging indicators respectively .
#' @param co_wgt,le_wgt,la_wgt data frame, including weights respectively for \code{consistent}, 
#' \code{leading}, \code{lagging}.
#' @param rate_method calculating method for symmetry rate. \code{auto} means progress selects \code{division}
#' or \code{difference} automatically .
#' @param start,end start and end of time series, it's usage like \code{start=c(2007,2}, \code{end=c(2017,12}.
#' @param frequency 12 for monthly data , 4 for seasonal data . In addition , \code{start}, \code{end}, and 
#' \code{frequency} will determine a number of observation,which the actually number of data will be equal to
#' . Thus data you input will not be permitted to lack some moth or quarter, otherwise , result will be not 
#' correct .
#' @param baseyear final index will base on a certain year(whose indices' mean will be regarded as 100) to 
#' adjust relative size of index . 
#' @param trend_adjust \code{logical}, whether to adjust trend of index 
#' base on compounded average growth, \code{TRUE} : yes ; \code{FALSE }: no. 
#' @param neighbour...mcd_gap refer to \link{bbturns}, see \url{https://github.com/Nisus-Liu/bbturns}. 
#' @details This package depend on package named \code{bbturns} from \emph{GitHub}, you can get it like :
#' \code{install.packages("devtools")} >> \code{library(devtools)} >> \code{install_github("Nisus-Liu/bbturns")}.
#' @return A data frame including : 
#' \describe{
#'   \{consistent}{consistent index}
#'   \{leading}{leading index }
#'   \{lagging}{lagging index }
#' }
#' @examples 
#' CI <- CompositeIndex(consistent , leading , co_wgt = consistent_wgt, start = c(2006,1), end = c(2016,7), baseyear = 2006)


CompositeIndex = function(consistent, leading = NULL , lagging = NULL, 
                          co_wgt=NULL,le_wgt=NULL,la_wgt=NULL, 
                          rate_method = c("division","difference","auto"),
                          start = numeric(), end = numeric(), frequency = 12, baseyear = start[1],
                          trend_adjust = TRUE, 
                          neighbour = 5, mincycle = 15, phase = 5, extr_limit = 3.5,
                          end_num = 6, mcd_gap = 8
                          )
{
  

  
  # --子函数：平均变化率-------------------------------------------------------
  AverageRate<-function(df,weight=NULL) {
    # 求先行、一致、滞后指标组的平均变化率(各指标的对称变化率的均值)
    # df  --输入数据框,在流程上,刚好是之前用[growth()]计算好的各指标的对称变化率组成的数据框.
    # weight   --存放权重的数据框，对应各个组，赋给相应顺序的指标。指定相对大小即可，不必要求和为1，程序内部会处理
    
    df = data.frame(df)
  
    
    if (is.null(weight)) {  #等权
      
      avrt<-apply(df,1,mean)   ##对每一行求均值，放入avrt中
      # avrt<-avrt/df_col
    }
    # 不等权
    else {
      weight = data.frame(weight)
      # df 和 weight 数据框的列名要一致
      df_name = names(df)
      weight_name = names(weight)
      if (!all(df_name == weight_name))  stop("names of 'df' are not as same as names of 'weight'.")
      
      df_col<-ncol(df)
      
      weight_col<-ncol(weight)
      
      if (df_col!=weight_col) stop("'weight''s length is not equal to 'df''s.")
      
      
      
      
      # 把weight数据框变成与df维数一致
      wgt<-df
      for (col in 1:weight_col) {
        wgt[,col]<-weight[1,col]
      }
      # 给df加权
      
      
      
      w_df<-wgt*df
      # 得到平均变化率分子
      mem<-apply(w_df,1,sum)
      den<-apply(wgt,1,sum)  ##应对用户接入的权重不规范，即和不等于1，如果用户输入的权重恰好规范（之和为1），则den为1
      
      avrt<-mem/den
      
    }
    
    
    avrt
    
    # 注意：未标准化 avrt，因为标准化要以一致组的平均变化率为基准
    
    
    
  }
  
  
  # --子函数：初始合成）-------
  Initial<-function(x) {
    #: 初始合成最后一步
    
    # x    --输入处理好的指数
  
    ini<-c(100,rep(NA,(length(x)-1)))
    x200<-(200+x)/(200-x)
    
    for (i in 1:(length(x)-1)) {
      ini[i+1]<-ini[i]*x200[i+1]
    }
    
    ini
  }
  

  
  # --子函数：生成年份和月度（季度）索引，用于通过年份数字定位具体的数据行---------------
  DateIndex <- function (start = numeric(), end = numeric(), frequency = 1, 
                         ts.eps = getOption("ts.eps") )
  {
    
    if (length(start) !=2 | length(end) != 2) stop("'start' and 'end' should like : 'start = c(2016,10)'")
    if (frequency > 1 && abs(frequency - round(frequency)) < 
        ts.eps) 
      frequency <- round(frequency)
    if (length(start) > 1L) {
      start <- start[1L] + (start[2L] - 1)/frequency
    }
    if (length(end) > 1L) {
      end <- end[1L] + (end[2L] - 1)/frequency
    }
    
    
    
    if (start > end) 
      stop("'start' cannot be after 'end'")
    nobs <- floor((end - start) * frequency + 1.01)
    
    dateseries = seq(start , end , length.out = nobs)
    
    Year = floor(dateseries)  # 存放年度
    
    MonQua = floor( (dateseries-Year) * frequency + 1.01)  # 存放月度或季度
    
    # date = data.frame(Year, MonQua)
    
    lst = list(Year = Year, MonthQuarter = MonQua , MonthQuarter_num = nobs)
    # lst = list(date = date, start = start , end = end , frequency= frequency, dateseries = dateseries)
    lst
    
  }
  
  
  
  # ---------------------------------------
  
  
  
  # 异常控制-------------
  
  consistent = as.data.frame(consistent)
  nCo = nrow(consistent)
  
  
  date_index = DateIndex(start = start , end = end, frequency = frequency)  # 根据指定开始和结束时间生成时间索引
  nobs = date_index$MonthQuarter_num  # 时间索引向量的长度
  if (! (nCo == nobs)) stop("date setted  is not consistent with the numbers of observation in 'consistent'.")
  if (!is.null(leading)) {
    leading = as.data.frame(leading)
    nLe = nrow(leading)
    if (! nLe == nCo) stop("'leading' and 'consistent' must have same number of observations ")
  }
  if (!is.null(lagging)) {
    lagging = as.data.frame(lagging)
    nLa = nrow(lagging)
    if (! nLa == nCo)  stop("'lagging' and 'consistent' must have same number of observations ")
  }
  
  if (!trend_adjust) message("params from 'neighbour' to 'mcd_gap' are omitted.")
  
  # 一致指标组合成---------
  
  ## 求一致指标组对称变化率-----
  
  Co_sr = consistent  # 初始一个数据框 Co_sr 用来存放对称变化率
  for (i in (1:length(consistent))) {
    Co_sr[,i] = growth(consistent[,i], diff_gap = 1, method = rate_method, symrate = TRUE, standard = TRUE ) #SymRate(consistent[,i], diff = diff)   
        # 调用[growth]函数计算对称变化率,并标准化.
    Co_sr[,i][1] = mean(Co_sr[,i][2:(1+frequency/2)])  
          # 对称变化率计算会使第一个值缺失,月度数据采用后面6期均值填补;季度数据采用后面2期数据填补(周期的一半)
  }

  
  
  ## 一致指标组标准化平均变化率---------
  # 转为数据框
  # Co_sr<-as.data.frame(Co_sr)
  
  # 对上面三组指标，求出各自的平均变化率
  
  Co_avrt<-AverageRate(df=Co_sr,weight=co_wgt)
  
  # 以一致组的平均变化率为基准标准化平均变化率,其目的是为了把三个指数当作一个协调一致的体系来应用
  
  Co_avrt_Mean<-mean( abs(Co_avrt) )   # 计算标准化因子时需要(绝对值的均值！！)
  sf_Co<-1  #一致组平均变化率的标准化因子，或者：mean(Co_avrt) / Co_avrt_Mean，自身除以自身所以为1
  
  Co_avrt_std<-Co_avrt/sf_Co   #标准后的平均变化率（水平调整）
  
  ## 求一致指标组的初始合成指数------------
  Co_ini<-Initial(Co_avrt_std)
  # 合成基期值应该是以第一年的指数均值为100
  
  
  ## 一致指标的最终合成指数---------
  
  ### 趋势调整后最终合成指数------
  if (trend_adjust)  {
    # 趋势调整时,需要计算目标趋势
    co_avGr_sum = 0
    for (i in 1:ncol(consistent)) {
      co_avGr_sum = co_avGr_sum + 
        avCompGrowth_trend(x=consistent[,i], neighbour = neighbour, mincycle = mincycle, phase = phase, extr_limit = extr_limit,
       end_num=end_num, mcd_gap = mcd_gap, percentage = TRUE)
          # 这里可以采用百分比形式,因为x的值由于对称变化率是百分比形式的变化率.
      
    }
    co_avGr_mean = co_avGr_sum / ncol(consistent)  # 一致指标组各指标的平均复利增长率的均值,i.e.目标趋势.
    
    # 初始合成指数的平均增长率
    co_ini_avGr = avCompGrowth_trend(x=Co_ini, neighbour = neighbour, mincycle = mincycle, phase = phase, extr_limit = extr_limit,
                                     end_num=end_num, mcd_gap = mcd_gap, percentage = TRUE)
    Co_avrt_std_adj = Co_avrt_std + (co_avGr_mean - co_ini_avGr)
    consistent <- Initial(Co_avrt_std_adj)  #最终合成
    
  }
  
  ### 不进行趋势调整的最终合成指数-----------
  if (!trend_adjust)  consistent = Co_ini  # 不进行趋势调整时,即以初始合成指数当做最终合成指数.
  
  # --生成以基准年份为100的合成指数，注意基准年是可以用户自定的，利用子函数：DateIndex() 得到日期信息，然后得到指定年对应行
  
  Year = date_index$Year  # date_index 函数体开头已经得到的日期索引list。
  
  
  
  CoHeadMean = mean( consistent[Year == baseyear] )  # 得到基准年对应的行的 consistent 值，然后求均值
  consistent = consistent / CoHeadMean  * 100
  
  
  CompIndex<-data.frame(consistent)  #结果存入数据框
  
  
  
  # 求先行、滞后组的合成指数----------------
  
  ## 先行指标组合成----------
    # 对称变化率-平均变化率-标准化-初始合成（和上述一致组一样)
  if (!is.null(leading)) {
    
    Le_sr = leading
    for (i in (1:length(leading))) {
      Le_sr[,i] = growth(leading[,i], diff_gap = 1, method = rate_method, symrate = TRUE, standard = TRUE )  # SymRate(leading[,i], diff = diff)
      Le_sr[,i][1] = mean(Le_sr[,i][2:(1+frequency/2)])  
      # 对称变化率计算会使第一个值缺失,月度数据采用后面6期均值填补;季度数据采用后面2期数据填补(周期的一半)
    }
   
    Le_avrt <- AverageRate(df=Le_sr,weight=le_wgt)
    sf_Le <- mean( abs(Le_avrt) ) / Co_avrt_Mean  ##求先行指标组的标准化因子
    Le_avrt_std <- Le_avrt/sf_Le   ## 标准化
    
    Le_ini<-Initial(Le_avrt_std)
    
    ### 趋势调整后最终合成指数------
    if (trend_adjust)  {
      # 初始合成指数的平均增长率
      le_ini_avGr = avCompGrowth_trend(x=Le_ini, neighbour = neighbour, mincycle = mincycle, phase = phase, extr_limit = extr_limit,
                                       end_num=end_num, mcd_gap = mcd_gap, percentage = TRUE)
      Le_avrt_std_adj = Le_avrt_std + (co_avGr_mean - le_ini_avGr)
      leading <- Initial(Le_avrt_std_adj)  #最终合成
      
    }
    
    ### 不进行趋势调整的最终合成指数-----------
    if (!trend_adjust)  leading = Le_ini  # 不进行趋势调整时,即以初始合成指数当做最终合成指数.    

    
    # --生成以基准年份为100的合成指数
    LeHeadMean = mean( leading[Year == baseyear] ) 
    leading = leading / LeHeadMean  * 100
    
    CompIndex<-data.frame(leading,CompIndex)  #结果并入df
  }
  
  
  ## 对滞后指标组---------------------------
  if (!is.null(lagging))  {
    
    La_sr = lagging
    for (i in (1:length(lagging))) {
      lagging[,i] = growth(lagging[,i], diff_gap = 1, method = rate_method, symrate = TRUE, standard = TRUE ) # SymRate(lagging[,i], diff = diff)
      La_sr[,i][1] = mean(La_sr[,i][2:(1+frequency/2)])  
      # 对称变化率计算会使第一个值缺失,月度数据采用后面6期均值填补;季度数据采用后面2期数据填补(周期的一半)
    }

    La_avrt <- AverageRate(df=La_sr,weight=la_wgt)
    sf_La <- mean( abs(La_avrt) ) / Co_avrt_Mean  # 绝对值的均值！
    La_avrt_std <- La_avrt / sf_La  ##标准化
    
    La_ini<-Initial(La_avrt_std)  # 初始合成

    ### 趋势调整后最终合成指数------
    if (trend_adjust)  {
      # 初始合成指数的平均增长率
      la_ini_avGr = avCompGrowth_trend(x=La_ini, neighbour = neighbour, mincycle = mincycle, phase = phase, extr_limit = extr_limit,
                                       end_num=end_num, mcd_gap = mcd_gap, percentage = TRUE)
      La_avrt_std_adj = La_avrt_std + (co_avGr_mean - la_ini_avGr)
      lagging <- Initial(La_avrt_std_adj)  #最终合成
      
    }
    
    ### 不进行趋势调整的最终合成指数-----------
    if (!trend_adjust)  lagging = La_ini  # 不进行趋势调整时,即以初始合成指数当做最终合成指数.    
    
    
    # --生成以基准年份为100的合成指数
    LaHeadMean = mean( lagging[Year == baseyear] )  
    lagging = lagging / LaHeadMean  * 100
    
    CompIndex=data.frame(CompIndex,lagging)
    
  }
  
  
  
  ## 将三个指数放入数据框返回
  
  # CompIndex<-data.frame(leading,consistent,lagging)
  
  CompIndex
  
  
}










 









