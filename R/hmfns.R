#' Wrapper for \code{Heatmap()} which allows specifying cell dimensions and resizing the heatmap accordingly.
#'
#' @param x A numeric matrix to be plotted.
#' @param ... Additional arguments to \code{Heatmap()}.
#' @param cell.h The cell height.
#' @param cell.w The cell width.
#' @param height The heatmap height. Ignored if \code{cell.h} is specified.
#' @param width The heatmap width. Ignored if \code{cell.w} is specified.
#' @param units The unit scale to be used for \code{cell.h} and \code{cell.w}.
#' @return A ComplexHeatmap.
#' @export
hm.cell <- function(
		x,...,
		cell.h=NULL,cell.w=NULL,
		height=NULL,width=NULL,
		heatmap_height=NULL, heatmap_width=NULL,
		units='in'
){
	if(!is.null(cell.h)) height <- unit(nrow(x)*cell.h,units)
	if(!is.null(cell.w)) width <- unit(ncol(x)*cell.w,units)
	return(Heatmap(x,...,height=height,width=width))
}

#' Color scale for a specified quantile. This scale is intended for heatmaps containing negative and positive values, so the range is set to \code{c(quant,1-quant)}.
#'
#' @param x A numeric matrix or vector.
#' @param quant The quantile to be used as the scale limits.
#' @return A \code{colorRamp2} scale which can be passed to \code{Heatmap()}.
#' @export
col.z <- function(x,quant=.01) {
	colorRamp2(
	      c(quantile(x,quant),0,quantile(x,1-quant)),
	      c('blue','white','red')
	)
}

#' Color scale for a specified quantile. This scale is intended for heatmaps containing only positive values, so the range is set to \code{c(0,1-quant)}.
#'
#' @param x A numeric matrix or vector.
#' @param quant The quantile to be used as the upper limit.
#' @return A \code{colorRamp2} scale which can be passed to \code{Heatmap()}.
#' @export
col.abs <- function(x,quant=.05){
	colorRamp2(c(0,quantile(x[x!=0],1-quant)),c('white','black'))
}

#' Color scale for categorical data.
#'
#' @param cond A vector that can be coerced to a factor.
#' @param colfn A function that returns a color map for each level in \code{cond}.
#' @param ... Additional arguments to \code{colfn}.
#' @return A named vector of colors corresponding to the levels of \code{cond}.
#' @export
cond.col <- function(cond,colfn=rainbow,...){
	cond <- as.factor(cond)
	cols <- colfn(length(levels(cond)),...)
	names(cols) <- levels(cond)
	return(cols)
}

#' Creates a color scale for the levels in a vector, then returns a vector assigning a color to each element of the input vector.
#'
#' @param cond A vector that can be coerced to a factor.
#' @param colfn A function that returns a color map for each level in \code{cond}.
#' @param ... Additional arguments to \code{colfn}.
#' @return A vector of colors corresponding to the elements of \code{cond}.
#' @export
cond.col.vec <- function(cond,colfn=rainbow,...) {
	cond.col(cond,colfn=colfn,...)[as.numeric(as.factor(cond))]
}

#' Wrapper for \code{hm.cell()} which applies cell dimensions to simple \code{rowAnnotation}s
#'
#' @param x A numeric matrix to be plotted.
#' @param ... Additional arguments to \code{hm.cell()}.
#' @param conds A matrix of factors to be used as \code{rowAnnotation}s.
#' @param cell.w The cell width.
#' @param heatmap_width The heatmap width Ignored if \code{cell.w} is specified.
#' @return A ComplexHeatmap.
#' @export
hm.ann <- function(x,...,conds=NULL, heatmap_width=NULL, cell.w=NULL){
	if(!is.null(conds)){
		conds <- as.data.frame(conds)
		cols <- lapply(conds,cond.col)
		ann <- do.call(
			'rowAnnotation',
			append(conds,list(col=cols))
		)
		if(!is.null(cell.w)){
			ann.width <- length(conds)*cell.w
			heatmap_width <- heatmap_width + ann.width
		}
	} else ann <- NULL
	hm.cell(x,...,left_annotation=ann,heatmap_width=heatmap_width,cell.w=cell.w)
}

