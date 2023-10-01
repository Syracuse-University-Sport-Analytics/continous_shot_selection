library(tidyverse)

saveImage <- function(ggplotToSave, widthPixels=1024, heightPixels=1024, filename = "", dpi=300, format="png"){
  sysinf <- Sys.info()
  os <- sysinf['sysname']
  if(os == "Windows"){
    systemDPI = 96
  }else{
    systemDPI = 72
  }
  if (filename == ""){
    filename <-paste0(deparse(substitute(plot)),".",format)
  }
  width = widthPixels/systemDPI
  height=heightPixels/systemDPI
  print(paste0("Saving ", filename, " with a width of ", width, " and height of ", height, " (", dpi, "dpi) assuming system DPI of ", systemDPI ))
  print(ggplotToSave)
  ggsave(filename = filename, width = width, height=height, dpi=dpi)
}
#saveImage(winShareDensity, 940, 717, filename = "visualizations/winShareDensity.png", dpi=300, format="png")


#the following finds the local maxima between intervals min_x and max_x. This is vectorized.
findLocalMaximaSquare <- function(constant, linear, quadratic, min_x, max_x){
  y_at_min_x <- constant + linear * min_x + quadratic * min_x * min_x
  y_at_max_x <- constant + linear * max_x + quadratic * max_x * max_x
  second_derivative <- quadratic * 2
  x_slope_0 <- linear / (-2*quadratic)
  y_x_slope_0 <- constant + linear * x_slope_0 + quadratic * x_slope_0 * x_slope_0
  return(list(x=ifelse(second_derivative < 0, x_slope_0, ifelse(y_at_min_x > y_at_max_x, min_x,max_x)),y=ifelse(second_derivative < 0, y_x_slope_0, ifelse(y_at_min_x > y_at_max_x, y_at_min_x,y_at_max_x))))
}

findLocalMinimaSquare <- function(constant, linear, quadratic, min_x, max_x){
  y_at_min_x <- constant + linear * min_x + quadratic * min_x * min_x
  y_at_max_x <- constant + linear * max_x + quadratic * max_x * max_x
  second_derivative <- quadratic * 2
  x_slope_0 <- linear / (-2*quadratic)
  y_x_slope_0 <- constant + linear * x_slope_0 + quadratic * x_slope_0 * x_slope_0
  return(list(x=ifelse(second_derivative > 0, x_slope_0, ifelse(y_at_min_x > y_at_max_x, min_x,max_x)),y=ifelse(second_derivative > 0, y_x_slope_0, ifelse(y_at_min_x > y_at_max_x, y_at_min_x,y_at_max_x))))
}

plotLocalMaximaSquare <- function(constant, linear, quadratic, min_x, max_x, min_y=50,x_label = "x", y_label="y", title=""){
  local_maxima <- findLocalMaximaSquare(constant, linear, quadratic, min_x, max_x)
  x_seq <- seq(min_x, max_x, (max_x-min_x)/100)
  y_values <- constant + linear * x_seq + quadratic * x_seq * x_seq
  plotLocalMaximaSquareViz <- ggplot(data=tibble(x=x_seq,y=y_values),aes(x=x,y=y)) + geom_line() + geom_hline(yintercept =local_maxima$y) + geom_vline(xintercept = local_maxima$x)+ geom_text(aes(x=0, label=round(local_maxima$y,3), y=local_maxima$y), vjust=1.5)+ geom_text(aes(x=local_maxima$x, label=round(local_maxima$x,3), y=min_y), angle=90, vjust=-.5, hjust=-0.2)+xlab(x_label)+ylab(y_label)+ggtitle(title)+theme_bw()
  #print(plotLocalMaximaSquareViz)
  return(plotLocalMaximaSquareViz)
}

plotLocalMinimaSquare <- function(constant, linear, quadratic, min_x, max_x, min_y=50,x_label = "x", y_label="y", title=""){
  local_maxima <- findLocalMinimaSquare(constant, linear, quadratic, min_x, max_x)
  x_seq <- seq(min_x, max_x, (max_x-min_x)/100)
  y_values <- constant + linear * x_seq + quadratic * x_seq * x_seq
  plotLocalMinimaSquareViz <- ggplot(data=tibble(x=x_seq,y=y_values),aes(x=x,y=y)) + geom_line() + geom_hline(yintercept =local_maxima$y) + geom_vline(xintercept = local_maxima$x)+ geom_text(aes(x=0, label=round(local_maxima$y,3), y=local_maxima$y), vjust=1.2)+ geom_text(aes(x=local_maxima$x, label=round(local_maxima$x,3), y=min_y), angle=-90, vjust=1.5, hjust=-0.2)+xlab(x_label)+ylab(y_label)+ggtitle(title)+theme_bw()
  #print(plotLocalMinimaSquareViz)
  return(plotLocalMinimaSquareViz)
}
#plotLocalMaximaSquare(3, 14, -5, -10, 10)  
#findLocalMaximaSquare(3, 14, -5, 0, 10)
#findLocalMaximaSquare(c(3,3), c(14,14), c(-5,-5), c(0,0), c(10,10))
