plotar <- function(plot){
  par(cex=0.8, mai=c(0.7,0.7,0.1,0.1))
  par(fig=c(0,1,0.5,1))
  plot(plot, ylab = 'residuals', xlab='week') +
    points(plot, pch=16)
  par(fig=c(0,0.5,0,0.5), new=TRUE)
  acf(plot, main='', xlab='lag[1d]', ylab='acf', lag.max = 25)
  par(fig=c(0.5,1,0,0.5), new=TRUE)
  hist(plot,freq = T, main='', ylab = 'count', xlab = 'residuals' )
  par(fig=c(0,1,0,0.9))
}


plot_pred <- function(dados,modelo, x_test){
  predict <- predict(modelo,n.ahead = 14, level = 0.95, newxreg = x_test)$pred
  plot(dados, col='gray', ylab='viagens', main='Reais vs Previstos', xlab='Senanas') +
    points(modelo$ts, pch=16, col='gray')
  lines(x=c(corte,corte), y=c(0,max(dados)), type = 'l', lty=2, col='red')
  lines(modelo$fitted.values, col='blue' )
  lines(predict, col='purple')
  # Add a legend to the plot
  legend('topleft', legend=c("Reais", "Treino", "Teste"),
         col=c("gray","blue", "purple"), lty=1, cex=0.5,
         text.font=1)
}


