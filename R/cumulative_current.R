
#' convert cumulative value to current value 
#' 
#' some economic time series is cumulative value, and need to be converted to current value. 
#' 
#' @export cumulative2current
#' @importFrom plyr ddply
#' @param x a numeric vector or a data frame 
#' @param cycle 12 if monthly ; 4 if seasonal ...
#' @param start start month or quarter ...
#' @return A data frame. 
#' @seealso \link{current2cumulative}
#' @details No matter what class of \code{x} (vector or data.frame), output value will be a data frame.
#' If \code{x} is data frame, progress will convert it's all columns to current value.
#' @examples 
#' a = c(1:10)
#' b = c(1,3,6,1,8,4,2,9,10,3)
#' c = c(rep(c(1,2,3),c(2,3,5)))
#' df = data.frame(a,b,c)
#' cumulative2current(df,cycle = 5,start = 1 )
#' cumulative2current(df,cycle = 5,start = 2 )
#' cumulative2current(df,cycle = 5,start = 7 ) # fail
#' cumulative2current(df,cycle = 25 )  # when cycle > nobs.
#' cumulative2current(df,cycle = 50,50 )




cumulative2current = function(x,cycle=12,start=1)  {

  
  # --异常/错误控制----
  
  if (any(is.na(x)))  stop("'NA' woundn't be allowed in 'x'.")
  if (start>cycle) stop("'start > cycle' woundn't be allowed.")
  
  df = x
  # --子函数:累计-->当期-------------
  Cum2Currt_sub <- function(df)  {
    # df    --数据框or向量
    # ----
    df <- data.frame(df)
    nr <- nrow(df)
    
    if (nr>1)  {
      df_diff <- df[2:nr,]-df[1:(nr-1),]   # 差分将累计->当期，缺第一行
      
      df_ans <- rbind(df[1,] , df_diff) 
      
    }
    # --如果只有一期，则不需要做差分
    if (nr==1) {
      df_ans <- df
    }
    
    df_ans   #! 返回值一定要放到最后
  }
  
  
  
  df<-data.frame(df)  #转为数据框
  
  N = nrow(df)
  head_num = if (N>=(cycle-start+1))  cycle-start+1  else N  
  tail_num = (N-head_num)%%cycle
  mid_gr_num = (N-head_num-tail_num)%/%cycle
  group_num = mid_gr_num+(head_num>0)+(tail_num>0)
  
  # 分组
  group <- if (tail_num>0) rep(c(1:group_num),c(head_num,rep(cycle,mid_gr_num),tail_num)) else rep(c(1:group_num),c(head_num,rep(cycle,mid_gr_num)))
  
  # # 检查plyr包是否安装
  # err <- try(library("plyr"),silent = T)   # silent   --TRUE,表示报错信息不展示到屏幕
  # # 已安装，class(err)=="character" ; 未安装，class(err) == "try-error"
  # if (class(err)=="try-error")  {
  #   install.packages("plyr")
  #   library("plyr")
  # }
  
  df = data.frame(df,group)  # 横向并入group
  

  df_ans = ddply(df,.(group),.fun = Cum2Currt_sub)  #! .(group) 形式不能变
 
  
  # --踢掉group
  ncol = ncol(df_ans)
  df_ans = df_ans[,-ncol]
  
  df_ans  
  
}







#' convert current value to cumulative value 
#' 
#' sometimes should convert current value to cumulative value if necessity, although using 
#' current value is common.
#' 
#' @export current2cumulative
#' @importFrom plyr ddply
#' @param x a numeric vector or a data frame 
#' @param cycle 12 if monthly ; 4 if seasonal ...
#' @param start start month or quarter ...
#' @return A data frame. 
#' @seealso \link{cumulative2current}
#' @details No matter what class of \code{x} (vector or data.frame), output value will be a data frame.
#' If \code{x} is data frame, progress will convert it's all columns to cumulative value. 
#' @examples 
#' a = c(1:10)
#' b = c(1,3,6,1,8,4,2,9,10,3)
#' c = c(rep(c(1,2,3),c(2,3,5)))
#' df = data.frame(a,b,c)
#' current2cumulative(df,cycle = 5,start = 1 )
#' current2cumulative(df,cycle = 5,start = 2 )
#' current2cumulative(df,cycle = 5,start = 7 ) # fail
#' current2cumulative(df,cycle = 25 )  # when cycle > nobs.
#' current2cumulative(df,cycle = 50,50 )
#' 
current2cumulative = function(x,cycle=12,start=1)  {

  # --异常/错误控制----
  
  if (any(is.na(x)))  stop("'NA' woundn't be allowed in 'x'.")
  if (start>cycle) stop("'start > cycle' woundn't be allowed.")
  
  df = x
  # --子函数:累计-->当期-------------

  
  
  df<-data.frame(df)  #转为数据框
  
  N = nrow(df)
  head_num = if (N>=(cycle-start+1))  cycle-start+1  else N  
  tail_num = (N-head_num)%%cycle
  mid_gr_num = (N-head_num-tail_num)%/%cycle
  group_num = mid_gr_num+(head_num>0)+(tail_num>0)
  
  # 分组
  group <- if (tail_num>0) rep(c(1:group_num),c(head_num,rep(cycle,mid_gr_num),tail_num)) else rep(c(1:group_num),c(head_num,rep(cycle,mid_gr_num)))
  
  # # 检查plyr包是否安装
  # err <- try(library("plyr"),silent = T)   #' silent   --TRUE,表示报错信息不展示到屏幕
  # # 已安装，class(err)=="character" ; 未安装，class(err) == "try-error"
  # if (class(err)=="try-error")  {
  #   install.packages("plyr")
  #   library("plyr")
  # }
  
  df = data.frame(df,group)  # 横向并入group
  
 
 
  df_ans = ddply(df , .(group) , .fun = cumsum)     # cumsum function from pkg base
  

  
  # --踢掉group
  ncol = ncol(df_ans)
  df_ans = df_ans[,-ncol]
  
  df_ans  
  
}

