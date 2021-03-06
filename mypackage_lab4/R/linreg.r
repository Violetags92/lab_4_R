#' Calculate a function for a multiple regression model
#' @param formula formula.
#' @param data data.frame.
#' @return A linreg object.
#' @field formula A formula
#' @field data A data.frame
#' @field bhat A matrix
#' @field resid A matrix
#' @field yhat A matrix
#' @field df A integer
#' @field rv1 A integer
#' @field varcoeff A matrix
#' @field tstat A matrix
#' @field pv A matrix
#' @examples 
#' lin1 <- linreg(formula = Petal.Length ~ Species, data = iris)
#' lin1$print()
#' lin1$summary()  
linreg <- setRefClass("linreg",
                      fields=list(formula="formula", 
                                  data = "data.frame", 
                                  bhat = "matrix",
                                  resid = "vector",
                                  yhat = "vector",
                                  df = "integer",
                                  rv1 = "numeric",
                                  varcoeff = "matrix",
                                  tstat = "matrix",
                                  pv = "matrix"),
                      
                      methods = list(
                        initialize = function(formula, data){
                        .self$formula <- formula
                          .self$data <- data
                          X <- model.matrix(formula, data)
                          y <- as.matrix(data[all.vars(formula)[1]])
                          .self$bhat <-  solve(t(X) %*% X) %*% t(X) %*% y
                          .self$yhat <- X %*% .self$bhat
                          .self$resid <- y - .self$yhat
                          .self$df <- dim(data)[1]-dim(X)[2]
                          .self$rv1 <- sum((t(.self$resid) %*% (.self$resid)))/.self$df
                          .self$varcoeff <-.self$rv1*(solve(t(X)%*%X))
                          .self$tstat <- .self$bhat / sqrt(diag(.self$varcoeff))
                          .self$pv <-  2*(1-pt(.self$tstat, .self$df))
                        },
                        
                        print = function(){
                          writeLines("Call:")
                          f1 <- as.character(.self$formula)
                          writeLines(c("lm(formula =", f1[2],f1[1],f1[3], ",data=iris)"), sep=" ")
                          writeLines("\n")
                          writeLines("Coefficients:")
                          t(.self$bhat)
                        },
                        
                        plot = function(){
                          sq_resid <- sqrt(abs(.self$resid))
                          z <- data.frame( .self$resid,  .self$yhat, sq_resid)
                          colnames(z) <- c("a1", "a2", "a3")
                          p <- ggplot(z, aes(a2, a1))  + geom_point(pch = 1, size = 4) +
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
                        
                        coeff = function(){
                          writeLines("Coefficientes:")
                          return(c(.self$bhat[1],.self$bhat[2], .self$bhat[3]))
                        },
                        
                        residual = function(){
                          writeLines("Residuals:")
                          return(.self$resid)
                        },
                        
                        pred = function(){
                          writeLines("Predicted values:")
                          return(.self$yhat)
                        },
                        
                        summary = function(){
                          writeLines("Call:")
                          writeLines(c("linreg(formula=", as.character(.self$formula)[2], as.character(.self$formula)[1], as.character(.self$formula)[3], ",data=iris)", .self$data$name), sep=" ")
                          writeLines("\n")
                          sum <- data.frame(.self$bhat, sqrt(diag(.self$varcoeff)), .self$tstat, .self$pv)                         
                          names(sum) <- c("Estimate", "SE", "t-value", "p-value") 
                          summ<-list(Coefficients=sum,
                                     ResidualVariance=.self$rv1, 
                                     DegreesOfFreedom=.self$df)
                          return(summ)
                        }
                      )
)
