#' Plots a heatmap of the correlations between all columns in a matrix.
#'
#' @param mat A numeric matrix.
#' @param method Method used by \link{\code{cor()}}.
#' @param colfn Function to generate color scale. By default uses \link{\code{col.abs()}}.
#' @param ... Additional arguments to \link{\code{Heatmap()}}.
#' @return A Heatmap object.
#' @importFrom circlize colorRamp2
#' @export
corHeatmap <- function(
	mat, 
	filename='correlation', 
	method='pearson', 
	cell.dim=0.20,
	...
	){
	mat <- as.data.frame(mat)
	mat <- sapply(mat, 
		function(x) sapply(
			mat,
			function(y) cor(x,y,method=method)
		)
	)

	quantHeatmap(
		mat, 
		filename=filename,
		cell.h=cell.dim,
		cell.w=cell.dim,
		...
	)
	#         col <- colfn(mat, quant=quant)
	#         mat[!is.finite(mat)] <- 0
	#         writeHeatmap(mat,
	#                 filename=filename,
	#                 cell.h=cell.dim,
	#                 cell.w=cell.dim,
	#                 char.h=char.dim,
	#                 char.w=char.dim,
	#                 col=col,
	#                 ...)
	#         hm <- hm.char(mat, col=col, ..., char.h=char.dim, char.w=char.dim, legend.w=legend.w)
	#         return(hm)
}

#' Wrapper function for \code{\link{writeHeatmap}}.
#'
#' @param mat A numeric matrix.
#' @param filename The output file name.
#' @param colfn Function to generate color scale. By default uses \link{\code{col.abs()}}.
#' @param quant Quantile used to set color scale limits.
#' @param cell.w Width of heatmap cells.
#' @param cell.h Height of heatmap cells.
#' @param char.dim Character width for row and column names.
#' @param ... Additional arguments to \link{\code{Heatmap()}}.
#' @return A Heatmap object.
#' @importFrom circlize colorRamp2
#' @export
quantHeatmap <- function(
	mat,
	filename, 
	colfn=col.z, 
	quant=0.01, 
	cell.w=0.2,
	cell.h=0.05,
	char.dim=0.1,
	buffer.h=5,
	buffer.w=5,
	...
	){
	mat <- as.matrix(mat)

	col <- colfn(mat, quant=quant)
	mat[!is.finite(mat)] <- 0

	writeHeatmap(mat,
		filename=filename,
		cell.h=cell.h,
		cell.w=cell.w,
		char.h=char.dim,
		char.w=char.dim,
		col=col,
		buffer.h=buffer.h,
		buffer.w=buffer.w,
		...)
}
