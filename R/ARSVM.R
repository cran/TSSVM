ARSVM<-function(data,h){
  size=length(data)
  # creating auto regression setup
  clm_names_x <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10",
                   "x11","x12","x13","x14","x15","x16","x17","x18","x19","x20",
                   "x21","x22","x23","x24","x25","x26","x27","x28","x29","x30")
  clm_names_y <- c("y")
  ft <- auto.arima(data, max.p=size/10, max.q=0, max.d=0, ic="aicc")
  lg <- length(ft$coef)
  data_mat <- matrix(NA, nrow= (size-lg), ncol= lg)
  y <- data[(lg+1):size]
  for (i in 1:(size-lg)) {
    for (j in 1:lg) {
      data_mat[i,j] <- c(data)[j+i-1]
    }
  }
  data_mat_x <- data_mat[, c(lg:1)]
  data_mat <- data.frame(data_mat_x,y)
  colnames(data_mat) <- c(clm_names_x[1:lg],clm_names_y)
  #  converting variable into data frame
  modelsvm = svm(data_mat$y~.,data= data_mat)
  sum=summary(modelsvm)
  W = t(modelsvm$coefs)
  b = modelsvm$rho
  pp1=modelsvm$fitted # fitted values
  # Model accuracy
  Mape=mean(abs((data_mat$y-pp1)/data_mat$y))*100
  RMSE=sqrt(mean((data_mat$y-pp1)^2))
  bb1 <- c(data[(size-lg+1):size])
  bb2 <- t(data.frame(bb1))
  colnames(bb2) <- clm_names_x[1:lg]
  #forecast
  prd <- matrix(NA, nrow= 1, ncol=h)
  x_ind1 <- matrix(NA, nrow= lg, ncol=h)
  x_iind <- double()
  x_iind[(1:lg)] <- bb1
  x_ind <- c(bb1)
  for (i in 1:h){
    x_ind1[,i] <- x_ind[1:lg]
    x_ind2 <- t(data.frame(x_ind1[,i]))
    colnames(x_ind2) <- clm_names_x[1:lg]
    prd[,i] <- predict(modelsvm, newdata = (x_ind2))
    x_iind[i] <- prd[,i]
    x_ind <- c(prd[,i],x_ind1[,i])
  }
  return(list("Optimum lag"=lg, "Model Summary"=sum, "Weights"=W,"constant"=b,"MAPE"=Mape,"RMSE"=RMSE, "fitted"=pp1, "forecasted.values"=round(prd,0)))
}
