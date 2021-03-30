#' Converts intersect names to logical vector.
#'
#' @param id The intersect name.
#' @return A logical vector
combSel <- function(id) as.logical(
  as.numeric(unlist(strsplit(id,'')))
)

#' Runs binomial of a set intersection vs. independence
#'
#' @param x The intersect size.
#' @param id The intersect names.
#' @param mat The logical matrix of intersections.
#' @param n The universe size.
#' @return A numeric vector containing the log2 odds ratio and p-value of binomial test.
#' @import ComplexHeatmap
getOR <- function(x,id,mat,n=NA) {
  sel <- combSel(id)
  if(is.na(n)) n <- sum(comb_size(mat))
  s <- set_size(mat)
  if(all(sel)) {
    expected <- Reduce('*',s[sel]/n)
  } else if(all(!sel)){
    expected <- Reduce('*',1-s[!sel]/n)
  } else {
    expected <- (Reduce('*',s[sel]/n)*Reduce('*',1-s[!sel]/n))
  }
  or <- (x/n)/expected
  p <- binom.test(x,n,expected)$p.value
  return(c(log2OR=log2(or),p=p))
}

#' Plot bars of UpSet.
#'
#' @param ... Arguments to \link{\code{ComplexHeatmap::anno_barplot()}}.
#' @param gp A \link{\code{grid::gpar()}} object.
#' @return A \link{\code{HeatmapAnnotation}} object.
#' @import ComplexHeatmap
upsetAnnoBar <- function(...,gp = gpar(fill='black')) anno_barplot(
  ...,
  border = F,
  gp = gp,
  height = unit(1, "in"),
  width = unit(1,'in')
)

#' Plots color key of UpSet.
#'
#' @param name Color key title.
#' @param colfn Function for mapping colors.
#' @return NULL
#' @import ComplexHeatmap
colKeyFn <- function(name, colfn) {
  pushViewport(viewport())
  color_mapping_legend(ColorMapping(name,col_fun = colfn))
  popViewport()
}

#' Defines function for plotting color key.
#'
#' @param name Color key title.
#' @param colfn Function for mapping colors.
#' @return An \link{\code{AnnotationFunction}}.
#' @import ComplexHeatmap
addColKey <- function(name,colfn) AnnotationFunction(
  fun = colKeyFn,
  var_import = list(name=name,colfn=colfn),
  width = unit(1, "in"),
  which='row'
)

#' Writes UpSet plot to file
#'
#' @param ls A list of sets to be plotted
#' @param file The output file
#' @param n The unverse size. Defaults to the number of unique elements.
#' @param combColFn A function defining the color mapping of intersections.
#' @param ... Additional arguments to \link{\code{ComplexHeatmap::make_comb_mat()}}.
#' @return A Heatmap object.
#' @importFrom circlize colorRamp2
#' @export
getUpset <- function(ls,file,n=NA,combColFn=function(x) "black",...,setClust=F){
  mat <- make_comb_mat(ls,...)
  sel <- sapply(comb_name(mat),combSel)
  combSets <- apply(sel,2,function(x) set_name(mat)[x])
  comb_col <- sapply(combSets,combColFn)
  or <- mapply(
    getOR,
    comb_size(mat),
    comb_name(mat),
    MoreArgs = list(mat=mat,n=n)
  )
  p <- -log10(p.adjust(or['p',],'fdr'))
  comb_order <- order(or[1,],decreasing = T)
  
  if(setClust) {
    setClust <- sapply(
      set_name(mat),
      function(x) sapply(
        combSets,
        function(y) x%in%y
      )
    )
    setClust <- setClust*or[1,]*p
    setClust <- hclust(dist(t(setClust)))
    set_order <- setClust$order
  } else {
    set_order <- set_name(mat)
  }
  
  colfn <- colorRamp2(c(0,3),c('white','black'))
  rowNlab <- as.character(comb_size(mat))
  rowNwidth <- max(nchar(rowNlab))*.1
  colNlab <- as.character(set_size(mat))
  colNheight <- max(nchar(colNlab))*.1
  rowann <- rowAnnotation(
    npeaks = upsetAnnoBar(comb_size(mat)),
    n=anno_text(rowNlab,width=unit(rowNwidth,'in')),
    log2OddsRatio=upsetAnnoBar(
      or[1,],
      gp=gpar(
        fill=colfn(p),
        col=colfn(p)
      )
    ),
    key=addColKey('-log10(FDR)',colfn)
  )
  colann <- columnAnnotation(
    n=anno_text(colNlab,height = unit(colNheight,'in')),
    npeaks=upsetAnnoBar(set_size(mat))
  )
  hmHeight <- ncol(mat)*.25+max(nchar(row.names(mat)))*.1+colNheight
  hmWidth <- nrow(mat)*.25+rowNwidth
  hm <- UpSet(
    t(mat),
    heatmap_width=unit(hmWidth+3,'in'),
    heatmap_height=unit(hmHeight+1,'in'),
    right_annotation = rowann,
    top_annotation = colann,
    col=colfn,
    name='-log10(FDR)',
    set_order = set_order,
    comb_order = comb_order,
    comb_col = comb_col
  )
  dir.eps(file,height=hmHeight+2,width=hmWidth+4)
  draw(hm)
  decorate_annotation('log2OddsRatio',{
    grid.lines(
      c(0,0),c(1,length(comb_col)),
      default.units = "native",
      gp=gpar(lty = 1)
    )
  })
  dev.off()
}

