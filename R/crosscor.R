#' Time Difference Relevance (Cross Correlation Coefficient)
#' 
#' calculate correlation coefficient when crossing date of two time series, 
#' and then separate leading, consistent, lagging indicators from economic indicators.
#' 
#' @details this function and it's role is equal to \link{KL}, 
#' just base on different method for dividing groups. Compare to \link{KL}, \code{crosscor}
#' don't request the time series must include only positive values.

#' @param base a numeric vector, containing a basic indicator for comparing to other indicators in order to recognize who is leading indicator ...
#' @param divergent a data frame containing indicators to be compared with \code{base },
#' and then to divide to 3 data frame, storing lagging, convenient, lagging indicators respectively.
#' @param lag max lag (forward or afterward) for calculating \code{KL information}, at range of \eqn{[-lag, lag]}.
#' @param group_lag threshold value of lag for divide different indicator, default is 3, means that 
#' lagging or leading lag at range of \eqn{[-3, 3]} will group the indicator to consistent indicator.
#' @param method method to calculate correlation coefficient, refer to \link{cor}.
#' @export crosscor
#' @return A list includes some value as follow :
#' \describe{
#'   \item{Coincident_Indicator}{data frame, including coincident indicator}
#'   \item{Leading_Indicator}{data frame, including leading indicator}
#'   \item{Lagging_Indicator}{data frame, lagging indicator}
#'   \item{Cor_Matrix}{matrix including correlation coefficient and lag information}
#'   \item{Cor_Max}{data frame, including vector of \code{Cor_lag} : lag corresponding max 
#'   correlation coefficient and vector of \code{Cor_max} : max correlation coefficient}
#' }
#' @examples 
#' x1 <- c( 5, 9, 5, 12, 13, 8, 14, 14, 8, 10)
#' x2 <- c( 9, 15, 5, 14, 8, 6, 12, 19,8, 7)
#' base<-c( 26, 13, 25, 27 ,25 ,13 ,32 ,20 ,23 ,29)
#' dt<-data.frame(x1, x2, base)
#' crosscor(base,dt,lag = 3,group_lag = 1)


crosscor<-function(base, divergent, lag=12, group_lag=3, method = c("pearson", "kendall", "spearman"))  {

  
  # --异常控制--------------------------------
  # if (any(is.na(base)))  stop("'base' has value of 'NA'")  # 检查输入的数据是否有NA值
  # if (any(base<=0)) stop("'base' isn't a positive sequence")
  # if (any(is.na(divergent)))  stop("'divergent' has value of 'NA'")
  
  
  # --定义存放三类指标的数据框----------------
  Leading<-data.frame(rep(NA,length(base)))
  Coincident<-data.frame((rep(NA,length(base))))
  Lagging<-data.frame((rep(NA,length(base))))
  # --初始化数据框列顺序
  le_i<-0
  co_i<-0
  la_i<-0
  
  
  
  X_num=length(divergent)   #得到数据框divergent中含有的向量个数
  lag<-abs(lag)
  
  
  # --定义kl信息数据框
  
  # 列数==2*lag+1，行数==X_num
  
  # 先生成矩阵
  Cor_Matrix<-matrix(rep(NA,(2*lag+1)*(X_num)),X_num,2*lag+1)
  # 转成数据框
  Cor_Matrix <- as.data.frame(Cor_Matrix)
  colnames(Cor_Matrix) <- c(-lag:lag)
  row.names(Cor_Matrix) <- names(divergent)
  
  # 定义存放最大相关系数值及期数的数据框
  Cor_Max <- matrix(rep(NA,2*X_num),ncol = 2)
  Cor_Max <- as.data.frame(Cor_Max)
  names(Cor_Max) <- c("Cor_lag","Cor_max")
  row.names(Cor_Max) <- names(divergent)
  
  
  
  for (i in 1:X_num) {
    # print(class(divergent[i]))
    # break
    # kl_min <- 0  #最小kl值初始为0，不同向量比较的基准kl值不同
    
    # pxi<-divergent[,i]/sum(divergent[,i])   #注意divergent[,i] 和 divergent[i]的区别，前者是向量；后者是数据框，赋值后，是数据框
    xi <- divergent[,i]
    
    # --错误控制---
    # if (any(xi<=0)) stop(paste("column",i,"of 'divergent' isn't a positive sequence"))
    
    # --lag为负时
    for (l in -lag:0) {
      # 按滞后或超前期截取向量
      y_sub<-base[(abs(l)+1):length(base)]
      xi_sub<-xi[1:(length(xi)-abs(l))]
      
      # 先截取，后标准化  ---------后期可以加上（通用的标准化，对负值依然有效）！！
      # py_sub<-y_sub/sum(y_sub,na.rm = T)  ##考虑缺失值
      # pxi_sub<-xi_sub/sum(xi_sub,na.rm = T)
      py_sub = y_sub  # 没有标准化代码，权且这么着
      pxi_sub = xi_sub
      
      
      # 计算相关系数
      cor_l = cor(pxi_sub,py_sub)
      
      Cor_Matrix[i,l+lag+1] <- cor_l  #相应的kl值填到kl矩阵中
      
      # 第一次计算时得到的相关系数，即认为最大值
      if (l == - lag) {  # lag 在前面已做绝对值处理
        cor_max = cor_l  # 用来存放目前为止最大相关系数值
        cor_lag = l   # 最优滞后或超前期，后续用来决定所计算的向量是先行，滞后，还是一致
      }
      else {
        if (cor_l > cor_max) {
          cor_max = cor_l
          
          cor_lag = l   #将期数换成最大相关系数对应的期数
        }
      }
      
    }
    
    # --lag为正时
    for (l in 1:lag) {
      y_sub<-base[1:(length(base)-abs(l))]
      xi_sub<-xi[(abs(l)+1):length(xi)]
      
      py_sub = y_sub  # 没有标准化代码，权且这么着
      pxi_sub = xi_sub
      
      
      # 计算相关系数
      cor_l = cor(pxi_sub,py_sub)
      
      Cor_Matrix[i,l+lag+1]<-cor_l  #相应的kl值填到kl矩阵中
      
      # 第一次计算时得到的相关系数，即认为最大值
      if (l == - lag) {  # lag 在前面已做绝对值处理
        cor_max = cor_l  # 用来存放目前为止最大相关系数值
        cor_lag = l   # 最优滞后或超前期，后续用来决定所计算的向量是先行，滞后，还是一致
      }
      else {
        if (cor_l > cor_max) {
          cor_max = cor_l
          
          cor_lag = l   #将期数换成最大相关系数对应的期数
        }
      }
    }
    
    
    # 将最大相关系数和期数填入数据框Cor_Max
    Cor_Max[i,1]<-cor_lag
    Cor_Max[i,2]<-cor_max
    
    # 将divergent[i]分到相应的组中去
    group_lag=abs(group_lag)
    if (cor_lag<(-group_lag)) {
      # 先行组
      le_i<-le_i+1
      Leading[le_i]<-divergent[i]
      colnames(Leading)[le_i]<-names(divergent[i])   
    }
    else if (cor_lag>group_lag) {
      # 滞后组
      
      la_i<-la_i+1
      Lagging[la_i]<-divergent[i]
      colnames(Lagging)[la_i]<-names(divergent[i])
    }
    else {            ##[-group_lag,group_lag]为一致组
      # 一致
      co_i<-co_i+1
      Coincident[co_i]<-divergent[i]
      colnames(Coincident)[co_i]<-names(divergent[i])
    }
    
  }
  
  # 展示结果
  # print(Coincident_Indicator)
  # print(Leading_Indicator)
  # print(Lagging_Indicator)
  
  
  Coincident_Indicator <- if (!(length(Coincident)==1 & all(is.na(Coincident)))) Coincident
  Leading_Indicator <- if (!(length(Leading)==1 & all(is.na(Leading)))) Leading
  Lagging_Indicator <- if (!(length(Lagging)==1 & all(is.na(Lagging)))) Lagging
  Cor_Matrix <- Cor_Matrix
  Cor_Max <- Cor_Max
  
  lst = list(Coincident_Indicator = Coincident_Indicator, 
             Leading_Indicator = Leading_Indicator,
             Lagging_Indicator = Lagging_Indicator,
             Cor_Matrix = Cor_Matrix,
             Cor_Max
  )
  
  lst
  
}


