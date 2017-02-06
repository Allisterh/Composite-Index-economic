#' Kullback-Leibler (KL) information
#' 
#' A approach to separate leading, consistent, lagging indicators from economic indicators.
#' 
#' @param base a numeric vector, containing a basic indicator for comparing to other indicators in order to recognize who is leading indicator ...
#' @param divergent a data frame containing indicators to be compared with \code{base },
#' and then to divide to 3 data frame, storing lagging, convenient, lagging indicators respectively.
#' @param lag max lag (forward or afterward) for calculating \code{KL information}, at range of \eqn{[-lag, lag]}.
#' @param group_lag threshold value of lag for divide different indicator, default is 3, means that 
#' lagging or leading lag at range of \eqn{[-3, 3]} will group the indicator to consistent indicator.
#' @export KL
#' @return A list includes some value as follow :
#' \describe{
#'   \item{Coincident_Indicator}{data frame, including coincident indicator}
#'   \item{Leading_Indicator}{data frame, including leading indicator}
#'   \item{Lagging_Indicator}{data frame, lagging indicator}
#'   \item{KL_Matrix}{matrix including KL value and lag information}
#'   \item{KL_Min}{data frame, including vector of \code{KL_lag} : lag corresponding min KL value and 
#'   vector of \code{KL_min} : min KL value}
#' }
#' @examples 
#' x1 <- c( 5, 9, 5, 12, 13, 8, 14, 14, 8, 10)
#' x2 <- c( 9, 15, 5, 14, 8, 6, 12, 19,8, 7)
#' base<-c( 26, 13, 25, 27 ,25 ,13 ,32 ,20 ,23 ,29)
#' dt<-data.frame(x1, x2, base)
#' KL(base,dt,lag = 3,group_lag = 1)
#' 

KL<-function(base, divergent,lag=12,group_lag=3)  {


  
  # --异常控制--------------------------------
  if (any(is.na(base)))  stop("'base' has value of 'NA'")  # 检查输入的数据是否有NA值
  if (any(base<=0)) stop("'base' isn't a positive sequence")
  if (any(is.na(divergent)))  stop("'divergent' has value of 'NA'")
  
  
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
  KL_Matrix<-matrix(rep(NA,(2*lag+1)*(X_num)),X_num,2*lag+1)
  # 转成数据框
  KL_Matrix<-as.data.frame(KL_Matrix)
  colnames(KL_Matrix)<-c(-lag:lag)
  row.names(KL_Matrix)<-names(divergent)
  
  # 定义存放最小kl值及期数的数据框
  KL_Min<-matrix(rep(NA,2*X_num),ncol = 2)
  KL_Min<-as.data.frame(KL_Min)
  names(KL_Min)<-c("KL_lag","KL_min")
  row.names(KL_Min)<-names(divergent)
  
  
  
  for (i in 1:X_num) {
    # print(class(divergent[i]))
    # break
    kl_min<-0  #最小kl值初始为0，不同向量比较的基准kl值不同
    
    # pxi<-divergent[,i]/sum(divergent[,i])   #注意divergent[,i] 和 divergent[i]的区别，前者是向量；后者是数据框，赋值后，是数据框
    xi<-divergent[,i]
    
    # --错误控制---
    if (any(xi<=0)) stop(paste("column",i,"of 'divergent' isn't a positive sequence"))
    
    # --lag为负时
    for (l in -lag:0) {
      # 按滞后或超前期截取向量
      y_sub<-base[(abs(l)+1):length(base)]
      xi_sub<-xi[1:(length(xi)-abs(l))]
      
      # 先截取，后标准化
      py_sub<-y_sub/sum(y_sub,na.rm = T)  ##考虑缺失值
      pxi_sub<-xi_sub/sum(xi_sub,na.rm = T)
      
      # 计算kl值  (乘以10000)
      kl_l<-10000*sum(py_sub*log(py_sub/pxi_sub),na.rm = T)
      
      KL_Matrix[i,l+lag+1]<-kl_l  #相应的kl值填到kl矩阵中
      
      # 第一次计算时得到的kl值，即认为最小kl值
      if (l==-lag) {
        kl_min<-kl_l  # kl -用来存放最小kl值
        kl_lag<-l   #最优滞后或超前期，后续用来决定所计算的向量是先行，滞后，还是一致
      }
      else {
        if (kl_l<kl_min) {
          kl_min<-kl_l
          kl_lag<-l   #将期数换成kl最小对应的期数
        }
      }
      
    }
    
    # --lag为正时
    for (l in 1:lag) {
      y_sub<-base[1:(length(base)-abs(l))]
      xi_sub<-xi[(abs(l)+1):length(xi)]
      
      py_sub<-y_sub/sum(y_sub,na.rm = T)
      pxi_sub<-xi_sub/sum(xi_sub,na.rm = T)
      
      kl_l<-10000*sum(py_sub*log(py_sub/pxi_sub),na.rm = T)
      
      KL_Matrix[i,l+lag+1]<-kl_l 
      
      if (kl_l<kl_min) {
        kl_min<-kl_l
        kl_lag<-l   #将期数换成kl最小对应的期数
      }
    }
    
    
    # 将xi最小的信息量和期数填入数据框KL_Min
    KL_Min[i,1]<-kl_lag
    KL_Min[i,2]<-kl_min
    
    # 将divergent[i]分到相应的组中去
    group_lag=abs(group_lag)
    if (kl_lag<(-group_lag)) {
      # 先行组
      le_i<-le_i+1
      Leading[le_i]<-divergent[i]
      colnames(Leading)[le_i]<-names(divergent[i])   
    }
    else if (kl_lag>group_lag) {
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
  

  
  Coincident_Indicator <- if (!(length(Coincident)==1 & all(is.na(Coincident)))) Coincident
  Leading_Indicator <- if (!(length(Leading)==1 & all(is.na(Leading)))) Leading
  Lagging_Indicator <- if (!(length(Lagging)==1 & all(is.na(Lagging)))) Lagging
  KL_Matrix <- KL_Matrix
  KL_Min <- KL_Min
  
  lst = list(Coincident_Indicator = Coincident_Indicator, 
             Leading_Indicator = Leading_Indicator,
             Lagging_Indicator = Lagging_Indicator,
             KL_Matrix = KL_Matrix,
             KL_Min
             )
  
  lst 
  
}

