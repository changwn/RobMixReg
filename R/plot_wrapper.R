
#' The plot wrapper function.
#'
#' @param type The character to choose which type of plot to generate.
#' @param x The independent variables
#' @param y The external variable
#' @param nc The number of components
#' @param inds_in A vector indicate the outlier samples.
#' @param res The result object returned by MLM function.
compPlot <- function(type='rlr', x,y,nc, inds_in, res){

  if(type == 'rlr')
    plot_CTLE(y~x,data=data.frame(x,y),nc,inds_in)

  if(type == 'mr')
    plot_mixtureReg(res, which = 1)

  if(type == 'post')
    plot_mixtureReg(res, which = 2)

  if(type == 'block')
    blockMap(res)

}
