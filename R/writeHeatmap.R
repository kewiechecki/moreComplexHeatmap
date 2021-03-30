#' Wrapper for \code{Heatmap()} which allows automatic scaling to specified cell dimensions and character width, and writes the output to an appropriately sized imge file.
#'
#' @param mat A numeric matrix to be plotted.
#' @param filename The output file name.
#' @param ... Additional arguments to plotting function.
#' @param fn The plotting function. It should extend \code{\link{ComplexHeatmap::Heatmap()}}
#' @param conds A matrix of factors to be used as \code{rowAnnotation}s. Replaces \code{left_annotation}.
#' @param cell.h The cell height.
#' @param cell.w The cell width.
#' @param char.h The characther width for column names.
#' @param char.w The character width for row names.
#' @param height The heatmap height. Ignored if \code{cell.h} is specified.
#' @param width The heatmap width. Ignored if \code{cell.w} is specified.
#' @param heatmap_height The heatmap height. Ignored if \code{cell.h} is specified.
#' @param heatmap_width The heatmap width. Ignored if \code{cell.w} is specified.
#' @param units The unit scale to be used for \code{cell.h} and \code{cell.w}.
#' @param path The output directory.
#' @param append.date Whether to write the output to a subdirectory with today's date.
#' @param imgfn The function to write the image. It should extend \link{\code{dirfns::dir.out()}}
#' @param buffer.w Buffer width.
#' @param buffer.h Buffer height.
#' @return A ComplexHeatmap.
#' @import ComplexHeatmap
#' @export
writeHeatmap <- function(
	mat, filename,...,
	fn=Heatmap,
	conds=NULL,
	left_annotation=NULL,
	cell.h=NULL,cell.w=NULL,
	char.h=NULL,char.w=NULL,
	#         legend.w=0,
	height=NULL,width=NULL,
	heatmap_height=NULL, heatmap_width=NULL,
	units='in',
	path='.', append.date=F,
	imgfn=dir.pdf,
	buffer.h=0, buffer.w=0
	){
	if(!is.null(cell.h)){
		height <- nrow(mat)*cell.h
	}
	if(!is.null(cell.w)){
		width <- nrow(mat)*cell.w
	}
	
	nchar.h <- max(nchar(colnames(as.matrix(mat))))
	nchar.w <- max(nchar(row.names(as.matrix(mat))))

	if(!is.null(char.h)){
		heatmap_height <- height+nchar.h*char.h
	}
	if(!is.null(char.w)){
		heatmap_width <- width+nchar.h*char.w
	}

	if(!is.null(conds)){
		conds <- as.data.frame(conds)
		cols <- lapply(conds,cond.col)
		left_annotation <- do.call(
			'rowAnnotation',
			append(conds,list(col=cols))
		)
		if(!is.null(cell.w)){
			ann.width <- length(conds)*cell.w
			heatmap_width <- heatmap_width + ann.width
		}
	} 

	heatmap_height <- heatmap_height + buffer.h
	heatmap_width <- heatmap_width + buffer.w

	hm <- fn(mat,
	   ..., 
	   height=unit(height/heatmap_height, 'npc'), 
	   width=unit(width/heatmap_width, 'npc'), 
	   heatmap_height=unit(1, 'npc'),
	   heatmap_width=unit(1, 'npc'),
	   left_annotation=left_annotation)

	imgfn(filename, 
	      path, 
	      append.date=append.date,
	      height=heatmap_height, 
	      width=heatmap_width)
	draw(hm)
	#         fn(mat, 
	#            ..., 
	#            height=unit(height, units), 
	#            width=unit(width, units), 
	#            heatmap_height=unit(heatmap_height, units),
	#            heatmap_width=unit(heatmap_width, units),
	#            left_annotation=left_annotation)
	dev.off()
}
