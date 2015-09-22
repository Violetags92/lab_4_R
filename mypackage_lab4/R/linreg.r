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
                      fields=list(formula="formula", 
                                  data = "data.frame", 
                                  #X = "matrix",
                                  #y = "matrix",
                                  bhat = "matrix"
                                  resid = "matrix",
                                  yhat = "matrix",
                                  df = "integer",
                                  rv1 = "integer",
                                  varcoeff = "matrix",
                                  tstat = "matrix",
                                  pv = "matrix"),
                      methods = list(
                        initialize = function(formula, data){
                          .self$formula <- formula
                          .self$data <- data
                          X <- model.matrix(formula, data)
                          #formulanames <- all.vars(formula)
                          y <- as.matrix(data[all.vars(formula)[1]])#data[,which(names(data) == formulanames[1])]
                          .self$bhat <-  solve(t(X) %*% X) %*% t(X) %*% y
                          .self$yhat <- X %*% .self$bhat
                          .self$resid <- y - .self$yhat
                          .self$df <- length(data)-length(X) #dim(data)[1]-dim(X)[2]
                          .self$rv1 <- (t(.self$resid)%*%.self$resid)/.self$df
                          .self$varcoeff <-(.self$rv1)*solve(t(X)%*%X)#as.numeric(.self$rv1)*solve(t(X)%*%X)
                          .self$tstat <- .self$bhat / sqrt(.self$varcoeff)#.self$bhat / sqrt(diag(.self$varcoeff))
                          .self$pv <- pt(.self$bhat, .self$df)
                        },
                        

                          
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



