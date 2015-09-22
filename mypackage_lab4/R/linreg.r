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
                                  bhat = "matrix",
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
                          y <- as.matrix(data[all.vars(formula)[1]])#data[,which(names(data) == formulanames[1])]
                          .self$bhat <-  solve(t(X) %*% X) %*% t(X) %*% y
                          .self$yhat <- X %*% .self$bhat
                          .self$resid <- y - .self$yhat
                          .self$df <- nrow(data)-ncol(X) #dim(data)[1]-dim(X)[2]
                          .self$rv1 <- as.integer((t(.self$resid)%*%.self$resid)/.self$df)
                          .self$varcoeff <-(.self$rv1)*solve(t(X)%*%X)#as.numeric(.self$rv1)*solve(t(X)%*%X)
                          .self$tstat <- .self$bhat / sqrt(diag(.self$varcoeff))#.self$bhat / sqrt(diag(.self$varcoeff))
                          .self$pv <- pt(.self$bhat, .self$df)
                        },
                        
                        print = function(){
                          return(.self$bhat)
                        },
                        
                        plot = function(){
                          sq_resid <- sqrt(abs(.self$resid))
                          z <- data.frame( .self$resid,  .self$yhat, sq_resid)
                          colnames(z) <- c("a1", "a2", "a3")
                          p <- ggplot(z, aes(a2, a1))  + geom_point(pch = 1, size = 4) +
                            geom_smooth(linetype = 3, colour = "black", method = "lm",se = FALSE)+
                            geom_smooth(linetype = 1, colour = "black", method = "loess",se = FALSE)+theme_bw()
                          p <- p +  xlab("Fitted values") +
                            ylab("Residuals") +
                            ggtitle("Residuals vs Fitted")
                          g <- ggplot(z, aes(x = a2, y = a3))  + geom_point(pch = 1, size = 4) +
                            geom_smooth(linetype = 1, colour = "black", method='loess', se = FALSE) +
                            theme_bw()
                          g <- g +  xlab("Fitted values") +
                            ylab(label = expression(paste(sqrt("|Standardized residuals|")))) +
                            ggtitle(label = "Scale-Location")
                           return(grid.arrange(p, g, nrow = 2))
                        },
                        
                        resid = function(){
                          return(.self$resid)
                        },
                        
                        pred = function(){
                          return(.self$yhat)
                        },
                        
                        summary = function(){
                          summ <- list( bhat=.self$bhat,
                                        resid=.self$resid,
                                        varcoef=.self$varcoeff,
                                        tstat=.self$tstat,
                                        pv=.self$pv,
                                        rv1=.self$rv1,
                                        df=.self$df)
                          return(summ)
                        },
                        a = function(){
                          z <- data.frame(.self$resid, .self$yhat)
                          return(z[,1])
                        }
                      )
)
