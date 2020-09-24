#' accepts the results of an enrichment test applied to each cell in a matrix
#' and writes a dotplot of the results

#' @param odds A matrix of log2 odds ratios from an enrichment test.
#' @param fdr A matrix of FDR values from an enrichment test.
#' @param q A matrix of test counts for each test.
#' @param file The output file name.
#' @param path The output file path.
#' @param cell.dim The width & height of each heatmap cell in inches.
#' @param width The width of the output device in inches.
#' @param height The height of the output device in inches.
#' @param ... Additional arguments to \code{hm.cell()}.
#' @export
hmdot <- function(odds,fdr,q,file,path='.',cell.dim=.15,width=12,height=12, ...){
	logFDR <- -log10(fdr)
	#         odds[odds<0] <- 0
	odds[odds==Inf] <- max(odds[is.finite(odds)])
	odds[odds==-Inf] <- min(odds[is.finite(odds)])
	logFDR[!is.finite(logFDR)] <- 0

	#         cexfn <- function(x) unit(min(x/-log10(0.001),1)*cell.dim,'in')
	cexfn <- function(x) unit((1.5*x/max(q))*cell.dim,'in')
	col.odds <- col.z(odds)
	#         col.fdr <- col.abs(logFDR)
	col.fdr <- colorRamp2(c(0,2),c('white','black'))
	cellfn <- function(j, i, x, y, width, height, fill) {
            grid.points(
		x = x, y = y, 
		size=cexfn(q[i, j]),
		pch=16,
                gp = gpar(
			col = col.odds(odds[i, j]) 
			#                         col = col.fdr(logFDR[i, j])
		)
	    )
            grid.points(
		x = x, y = y, 
		size=cexfn(q[i, j]),
		pch=1,
                gp = gpar(
			col = col.fdr(logFDR[i, j])
		)
	    )
        }

	p.breaks <- c(.001,.005,.01,.05,.1,1)
	q.breaks <- round(seq(0,max(q),length.out=6))

	lgd <- list(
		Legend(col_fun = col.odds, title = "log2(OR)"),
		Legend(col_fun = col.fdr, title = "-log10(FDR)"),
		Legend(
			title="q",
			at=q.breaks,
			type='points',
			background=0,
			pch=16,
			#                         size=unit(sapply(-log10(p.breaks),cexfn),'in'),
			size=unit(sapply(q.breaks,cexfn),'in'),
			legend_gp=gpar(col=1,fill=0)
		)
	)

	hm <- hm.cell(
		odds,
		cell_fun=cellfn,
		#                 col=colfn,
		name='log2OR',
		rect_gp = gpar(type = "none"),
		cell.w=cell.dim,
		cell.h=cell.dim,
		show_column_dend=F,
		show_row_dend=F,
		show_heatmap_legend=F,
		...
	)

	dir.eps(
	  file,path,
	  width=width,
	  height=height
	)
	draw(hm, annotation_legend_list=lgd)
	dev.off()
}


