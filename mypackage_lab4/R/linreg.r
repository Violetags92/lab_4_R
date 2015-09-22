#' Calculate a function for a multiple regression model
#' @param formula formula.
#' @param data data.frame.
#' @return A linreg object.
#' \usage{print(formula, data)
#'        plot(formula, data)
#'        resid(formula, data)
#'        pred(formula, data)
#'        coef(formula, data)
#'        summary(formula, data)
#'        }
#'@examples {print(Petal.Length~Species, data = iris)
#'           plot(Petal.Length~Species, data = iris)
#'           head(resid(Petal.Length~Species, data = iris))
#'           summary(Petal.Length~Species, data = iris)
#'           pred(Petal.Length~Species, data = iris)
#'           coef(Petal.Length~Species, data = iris)
#'          }
linreg <- setRefClass("linreg",
                      fields = list(formula = "formula", data = "data.frame"),
                      methods = list(
                        initialize = function(formula, data){
                          formula <<- y~x
                          data <<- iris
                          X <<- model.matrix(formula, data)
                          y <<- as.matrix(data[all.vars(formula)[1]])
                          n <<- length(X)
                          p <<- length(y)
                        },
                        bhat = function(){ 
                          return(solve(t(X)%*%X) %*% t(X) %*% y)
                        },
                        yhat = function() {
                          return(X%*%bhat())
                        },
                        resid = function(){
                          return(y - yhat())
                        },
                        df = function() {
                          return(n-p)
                        },
                        rv1 = function() {
                          return((t(resid())%*%resid())/df())
                        },
                        varcoeff = function() {
                          return(as.numeric(rv1())*solve(t(X)%*%X))
                        },
                        tstat = function() {
                          return(bhat() / sqrt(abs(varcoeff()[1,])))
                        },
                        pv = function() {
                          return(pt(bhat(),df()))
                        }
                      )
)

print <- function(formula, data){
  lin <- linreg$new(formula, data)
  return(t(lin$bhat()))
}

plot <- function(formula, data){
  lin <- linreg$new(formula, data)
  a <- lin$resid()
  b <- lin$yhat()
  
  c <- as.data.frame(cbind(a,b))
  p <-  ggplot(c, aes(x = c[,2], y = c[,1]))  + geom_point(pch = 1, size = 4) +
    geom_smooth(linetype = 3, colour = "black", method = "lm",se = FALSE)+
    geom_smooth(linetype = 1, colour = "black", method = "loess",se = FALSE)+theme_bw()
  p <- p +  xlab("Fitted values") +
    ylab("Residuals") +
    ggtitle("Residuals vs Fitted")
  d <- sqrt(abs(lin$resid()))
  e <- as.data.frame(cbind(d,b))
  g <-  ggplot(e, aes(x = e[,2], y = e[,1]))  + geom_point(pch = 1, size = 4) +
    geom_smooth(method='loess', se = FALSE) +
    theme_bw()
  g <- g +  xlab("Fitted values") +
    ylab(label = expression(paste(sqrt("|Standardized residuals|")))) +
    ggtitle(label = "Scale-Location")
}


resid <- function(formula, data){
  lin <- linreg$new(formula, data)
  return(lin$resid())
}

pred <- function(formula, data){
  lin <- linreg$new(formula, data)
  return(lin$yhat())
}
head(pred(Petal.Length~Species, data = iris))
pred(Petal.Length~Species, data = iris)

coef <- function(formula, data){
  lin <- linreg$new(formula, data)
  return(names(lin$bhat()))
}
coef(Petal.Length~Species, data = iris)

summary <- function(formula, data){
  lin <- linreg$new(formula, data = data)
  list( bhat=lin$bhat(),
        resid=lin$resid(),
        varcoef=lin$varcoeff(),
        tstat=lin$tstat(),
        pv=lin$pv(),
        rv1=lin$rv1(),
        df=lin$df()
  )
}



