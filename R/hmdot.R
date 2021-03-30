#' accepts the results of an enrichment test applied to each cell in a matrix
#' and writes a dotplot of the results

#' @param odds A matrix of log2 odds ratios from an enrichment test.
#' @param fdr A matrix of FDR values from an enrichment test.
#' @param q A matrix of test counts for each test.
#' @param ... Additional arguments to \code{hmdot()}.
#' @export
hyperDot <- function(odds, fdr, q, ...){
	logFDR <- -log10(fdr)
	odds[odds==Inf] <- max(odds[is.finite(odds)])
	odds[odds==-Inf] <- min(odds[is.finite(odds)])
	logFDR[!is.finite(logFDR)] <- 0

	col.odds <- col.z(odds)
	col.fdr <- colorRamp2(c(0,2),c('white','black'))
	size.breaks <- round(seq(0,max(q),length.out=6))
	hmdot(odds, logFDR, q, col.mat=col.odds, col.outl=col.fdr, scale=c(0, max(q)), size.breaks=size.breaks, ...)
}

#' Version of hyperDot using p-value instead of number of replicates for dot size.
#' accepts the results of an enrichment test applied to each cell in a matrix
#' and writes a dotplot of the results

#' @param odds A matrix of log2 odds ratios from an enrichment test.
#' @param fdr A matrix of FDR values from an enrichment test.
#' @param q A matrix of test counts for each test.
#' @param plim Maximum p-value on the size scale. Values above \code{plim} are set to \code{plim}.
#' @param ... Additional arguments to \code{hmdot()}.
#' @export
dotPscale <- function(odds, fdr, q, plim=0.01, outl.name='size', size.name='-log10(FDR)', ...){
	fdr[which(fdr<plim)] <- plim
	logFDR <- -log10(fdr)
	odds[is.na(odds)] <- 0
	odds[odds==Inf] <- max(odds[is.finite(odds)])
	odds[odds==-Inf] <- min(odds[is.finite(odds)])
	logFDR[!is.finite(logFDR)] <- 0

	col.odds <- col.z(odds)

	qscale <- c(0,quantile(as.matrix(q)[as.matrix(q)!=0], 0.95))
	col.outl <- colorRamp2(qscale ,c('white','black'))

	size.breaks <- seq(0, -log10(plim), length.out=6)

	hmdot(
	      odds, 
	      q, 
	      logFDR, 
	      col.mat=col.odds, 
	      col.outl=col.outl, 
	      scale=qscale, 
	      size.breaks=size.breaks,
	      outl.name=outl.name,
	      size.name=size.name,
	      ...
	)
}

#' accepts the results of an enrichment test applied to each cell in a matrix
#' and writes a dotplot of the results

#' @param mat A matrix of values shown by the dot color.
#' @param outl A matrix of values shown by the dot outlline.
#' @param size A matrix of values shown by the dot size.
#' @param col.mat A color scale for \code{mat}.
#' @param col.outl A color scale for \code{outlline}.
#' @param scale A vector of length 2 giving the min and max values to scale the size of the dots.
#' @param file The outlput file name.
#' @param path The outlput file path.
#' @param cell.dim The width & height of each heatmap cell in inches.
#' @param width The width of the outlput device in inches.
#' @param height The height of the outlput device in inches.
#' @param ... Additional arguments to \code{hm.cell()}.
#' @export
#' @importFrom grid gpar unit grid.points
hmdot <- function(
	mat, outl, size, 
	col.mat, col.outl, scale, size.breaks,
	mat.name="log2(OR)", outl.name="-log10(FDR)", size.name="size", 
	file, path='.', cell.dim=.15, width=12, height=12, append.date=F,
	...
){
	#         outl <- -log10(outl)
	#         mat[mat<0] <- 0
	mat[mat==Inf] <- max(mat[is.finite(mat)])
	mat[mat==-Inf] <- min(mat[is.finite(mat)])
	#         outl[!is.finite(outl)] <- 0

	#         cexfn <- function(x) unit(min(x/-log10(0.001),1)*cell.dim,'in')
	cexfn <- function(x) unit((1.2*x/max(size))*cell.dim,'in')
	#         col.mat <- col.z(mat)
	#         col.outl <- col.abs(outl)
	#         col.outl <- colorRamp2(c(0,2),c('white','black'))
	cellfn <- function(j, i, x, y, width, height, fill) {
            grid.points(
		x = x, y = y, 
		size=cexfn(size[i, j]),
		pch=16,
                gp = gpar(
			col = col.mat(mat[i, j]) 
			#                         col = col.outl(outl[i, j])
		)
	    )
            grid.points(
		x = x, y = y, 
		size=cexfn(size[i, j]),
		pch=1,
                gp = gpar(
			col = col.outl(outl[i, j])
		)
	    )
        }

	#         mat.breaks <- round(seq(range(attr(col.mat,'breaks'), length.out=6)))
	#         outl.breaks <- round(seq(range(attr(col.outl,'breaks'), length.out=6)))

	lgd <- list(
		Legend(col_fun = col.mat, title = mat.name),# at=mat.breaks),
		Legend(col_fun = col.outl, title = outl.name),# at=outl.breaks),
		Legend(
			title=size.name,
			at=size.breaks,
			type='points',
			background=0,
			pch=16,
			size=unit(sapply(size.breaks,cexfn),'in'),
			legend_gp=gpar(col=1,fill=0)
		)
	)

	hm <- hm.cell(
		mat,
		cell_fun=cellfn,
		#                 name='log2OR',
		#                 col=col.mat,
		rect_gp = gpar(type = "none"),
		cell.w=cell.dim,
		cell.h=cell.dim,
		#                 show_column_dend=F,
		#                 show_row_dend=F,
		show_heatmap_legend=F,
		...
	)

	dir.pdf(
	  file,path,
	  append.date=append.date,
	  width=width,
	  height=height
	)
	draw(hm, annotation_legend_list=lgd)
	dev.off()
}



