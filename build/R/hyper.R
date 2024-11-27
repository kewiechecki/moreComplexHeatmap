#' Hypergeometric test for enrichment of conditions in a cluster.
#' @param id A vector of sample IDs.
#' @param conds A vector of the same length as ID giving the condition of each sample.
#' @param clusts A vector of the same length as ID giving the cluster ID of each sample.
#' @param padj.method Method passed to \code{\link{p.adjust}} for multiple hypothesis correction.
#' @seealso \code{\link{phyper2}}, \code{\link{p.adjust}}
#' @export
condHyper <- function(id,conds,clusts,padj.method='fdr'){
	#         id <- id[!is.na(conds)]
	#         cols <- unique(clusts)
	#         clusts <- clusts[!is.na(conds)]
	test <- split(id,conds)
	clusts <- split(id,clusts)
	#         fn <- function(x) sum(!is.na(x))
	lHyper(id, test, clusts, padj.method)
	#         m <- sapply(test,length)
	#         n <- length(id)-m
	#         k <- sapply(clusts,length)
	#         q <- as.data.frame(sapply(clusts,function(m) sapply(test, function(k){
	#                 sum(m%in%k)
	#         })))
	#         log2OR <- mapply(
	#           function(q.i,k.i) mapply(
	#             function(q.ij,m.j) log2(
	#               (q.ij/(k.i-q.ij))/(m.j/(length(id)-m.j))
	#             ),
	#             q.i,m
	#           ),
	#           q,k
	#         )
	#         row.names(log2OR) <- names(test)
	# 
	#         testHyper <- mapply(function(q,k) mapply(
	#           phyper2,q=q-1,k=k,m=m,n=n
	#         ),q=q,k=k)
	#         testFdr <- apply(testHyper,2,p.adjust,method=padj.method)
	#         row.names(testHyper) <- names(test)
	#         return(list(log2OR=log2OR,FDR=testFdr,q=q))
}

#' Hypergeometric test for enrichment of conditions in a cluster.
#' @param bg A vector of sample IDs.
#' @param test A list of vectors of IDs to be tested
#' @param clusts A list of vectors of IDs
#' @param padj.method Method passed to \code{\link{p.adjust}} for multiple hypothesis correction.
#' @seealso \code{\link{phyper2}}, \code{\link{p.adjust}}
#' @export
lHyper <- function(bg,test,clusts,padj.method='fdr'){
	#check all ids are in bg
	test <- sapply(test, intersect, bg)
	clusts <- sapply(clusts, intersect, bg)

	m <- sapply(test,length)
	n <- length(bg)-m
	k <- sapply(clusts,length)
	q <- as.data.frame(sapply(clusts,function(m) sapply(test, function(k){
		sum(m%in%k)
	})))
	log2OR <- mapply(
	  function(q.i,k.i) mapply(
	    function(q.ij,m.j) log2(
	      (q.ij/(k.i-q.ij))/(m.j/(length(bg)-m.j))
	    ),
	    q.i,m
	  ),
	  q,k
	)
	row.names(log2OR) <- names(test)

	testHyper <- mapply(function(q,k) mapply(
	  phyper2,q=q-1,k=k,m=m,n=n
	),q=q,k=k)
	testFdr <- apply(testHyper,2,p.adjust,method=padj.method)
	row.names(testHyper) <- names(test)
	row.names(testFdr) <- names(test)
	return(list(log2OR=log2OR,FDR=testFdr,q=q))
}

#' Two-tailed version of \code{\link{phyper}}. It simply takes the minimum of the upper and lower tails and multiplies the result by 2.
#' @param ... Arguments to \code{phyper()}.
#' @return A p-value.
#' @export
#' @seealso \code{\link{phyper}}
phyper2 <- function(...) min(
	phyper(...,lower.tail=T),
	phyper(...,lower.tail=F)
)*2

#' Write dotplot for hypergeometric test.
#' @param dat A \code{data.frame} with columns as metrics to be tested for enrichment.
#' @param clusts A vector giving the cluster ID of each row in \code{dat}.
#' @param out The output file name passed to \code{\link{dotPscale}}.
#' @param ... Additional arguments to \code{\link{dotPscale}}.
#' @return A list of results from \code{\link{corHeatmap}}.
#' @export
clustHyper <- function(dat, clusts, out){
	# run hypergeometric tests for enrichment of conditions and phenotypes
	hyper <- lapply(dat, function(x) condHyper(row.names(dat),x,clusts))

	# extract fields from test & reformat as matrices
	odds <- do.call(rbind,lapply(hyper,'[[','log2OR'))
	fdr <- do.call(rbind,lapply(hyper,'[[','FDR'))
	qval <- do.call(rbind,lapply(hyper,'[[','q'))

	# split phenotype & condition into separate panels
	rowsplit <- unlist(
		mapply(
		       function(x,y) rep(y,nrow(x$log2OR)),
		       hyper,
		       names(hyper)
		)
	)

	if(length(unique(clusts))>1){
		dotPscale(
			odds, 
			fdr, 
			qval, 
			file='condition', 
			path=out, 
			row_split=rowsplit, 
			row_title_rot=0,
			...
		)
	}
	return(hyper)
}

