#' calculate growth of a series 
#' 
#' calculate some type of growth such as rate of increase, symmetrical rate, in addition get chain relative ratio or 
#' other type ratios by specifying gap.
#' 
#' @param x a numeric vector
#' @param diff_gap the gap of calculating rate, you can get year on year rate of a annual series by specifing \code{diff_gap} for 12.
#' @param method \code{division}: \eqn{(x_t-x_{t-1})/x_{t-1}}; \code{difference}: \eqn{x_t-x_{t-1}}; 
#' \code{auto}: select method automatically.
#' @param symrate symmetry rate if \code{TRUE}.
#' @param standard whether to standard the ratio. \code{FALSE}:no, \code{TRUE}:yes.
#' @export growth
#' @examples 
#' x = 1:30
#' growth(x, method = "division" )
#' growth(x, method = 2)
#' growth(x, method = 3, symrate = T)
#' x[2] = -2
#' growth(x, method = 3, symrate = T)

growth = function(x, diff_gap = 1, method = c("division","difference","auto"),  symrate = FALSE, standard = FALSE )  {
  
  
  #--exception control------
  if (!is.vector(x)) stop("'x' must be a vector")
  if (!is.numeric(x)) stop("'x' must be a numeric vector")
  #--numerist options--
  if (class(method) == "character") {
    method_n = switch(match.arg(method, c("division","difference","auto")),
                      division = 1L,
                      difference = 2L, 
                      auto = 3L)
  }
  else {
    method_n = round(method)
    if (method_n<=0L | method_n > 3L) stop("'method' must at range of 1~3, when 'method' is a numeric vector.")
  }
  
  #--symmetry change rate
  if (symrate)  {  
    x_t<-x[-(1 : diff_gap)]
    lx = length(x)
    x_t1<-x[-((lx - diff_gap + 1) : lx)]
    
    #--difference
    if (method_n == 2L) {
      sym_r <- x_t-x_t1
    }
    #--auto i.e.there are some none-positive value
    if (method_n == 3L) {
      if (any(x<=0)) {
        sym_r <- x_t-x_t1
        message("'x' has some none-positive value, automatically used 'difference' method to calculate rate.")
      }
      else {
        sym_r <- 200*(x_t-x_t1)/(x_t+x_t1)
      }
    }
    #--division
    if (method_n == 1L) {
      sym_r <- 200*(x_t-x_t1)/(x_t+x_t1)
      
    }
    
    
    #--standard, mean of abs is equal to 1
    
    if (standard) {
      
      std_fct <- sum(abs(sym_r)) / (length(sym_r))   ##标准化因子
      sym_r <- sym_r/std_fct
      
    }
    
    
  }
  
  #--asymmery change rate, i.e. normal growth rate.
  if (! symrate) {
    
    
    x_t<-x[-(1 : diff_gap)]
    lx = length(x)
    x_t1<-x[-((lx - diff_gap + 1) : lx)]  # 上期的值
    
    #--difference
    if (method_n == 2L) {
      sym_r <- x_t-x_t1
    }
    #--auto i.e.there are some none-positive value
    if (method_n == 3L) {
      if (any(x<=0)) {
        sym_r <- x_t-x_t1
        message("'x' has some none-positive value, automatically used 'difference' method to calculate rate.")
      }
      else {
        sym_r <- (x_t-x_t1) / x_t1
      }
    }
    #--division
    if (method_n == 1L) {
      sym_r <- (x_t-x_t1) / x_t1
      
    }
    
    
    
    #--standard
    
    if (standard) {
      
      std_fct<-sum(abs(sym_r))/(length(sym_r))   ##标准化因子
      sym_r<-sym_r/std_fct
      
      
    }
    
  }
  
  sym_r = c(NA, sym_r)
  sym_r 
  
}





