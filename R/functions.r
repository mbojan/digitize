#' Read and calibrate a JPG graphic
#' 
#' This function loads the graphical file passed in argument, and allows the
#' user to calibrate it using four points (x1,x2,y1,y2) by clicking the graph.
#' 
#' 
#' @param fname Filename of the graphic to read
#' @return \item{calpoints}{List of the x and y coordinates of the calibration
#' points}
#' @export ReadAndCal
ReadAndCal = function(fname)
{
	ReadImg(fname)
	calpoints <- locator(n=4,type='p',pch=4,col='blue',lwd=2)
	return(calpoints)
}



#' Read a JPG graphic
#' 
#' This function loads the graphical file passed in argument to allow further
#' point designation, but without calibration
#' 
#' 
#' @param fname Filename of the graphic to read
#' @export ReadImg
ReadImg = function(fname)
{
	img <- jpeg::readJPEG(fname)
  op <- par(mar=c(0,0,0,0))
  on.exit(par(op))
	plot.new()
	rasterImage(img,0,0,1,1)
}



#' Point data on an image
#' 
#' This function waits for the user to click the points of which he wants the
#' coordinates
#' 
#' 
#' @param col Color of the points
#' @param type Type of the points
#' @param ... Other arguments for locator
#' @return \item{data}{A list with the coordinates of the points}
#' @export DigitData
DigitData = function(col='red',type='p',...)
{
	type <- ifelse(type=='b','o',type)
	type <- ifelse(type%in%c('l','o','p'),type,'p')
	locator(type=type,col=col,...)
}



#' Calibrate the data acquired from an image
#' 
#' This function corrects the data according to the calibration information.
#' 
#' 
#' @param data A list of digitized values
#' @param calpoints A list of coordinates of the calibration points
#' @param x1 X-coordinate of the leftmost x point (corrected)
#' @param x2 X-coordinate of the rightmost x point (corrected)
#' @param y1 Y-coordinate of the lower y point (corrected)
#' @param y2 Y-coordinate of the upper y point (corrected)
#' @return \item{data}{A data frame with the corrected coordinates of the
#' points}
#' @export Calibrate
Calibrate = function(data,calpoints,x1,x2,y1,y2)
{
	x 		<- calpoints$x[c(1,2)]
	y 		<- calpoints$y[c(3,4)]

	cx <- lm(formula = c(x1,x2) ~ c(x))$coeff
	cy <- lm(formula = c(y1,y2) ~ c(y))$coeff

	data$x <- data$x*cx[2]+cx[1]
	data$y <- data$y*cy[2]+cy[1]

	return(as.data.frame(data))
}
