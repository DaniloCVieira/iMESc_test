
get_correct_predsom<-function(m,pred_som, newdata){
  nasY<-NULL
  nasX<-NULL
  empty_neus<- which(!(1:nrow(m$grid$pts))%in% m$unit.classif)
  nasX<-which(is.na(rowSums(pred_som$predictions[[1]])))
  m$codes[[1]]<-m$codes[[1]][-empty_neus,]

  new_pred<-predict(m,newdata = newdata, whatmap=1)
  newx<-new_pred$predictions[[1]][names(nasX),]
  newy<-NULL
  if(attr(m,"Method")!="Unsupervised"){
    newy<- as.data.frame(new_pred$predictions[[-1]])
    rownames(newy)<-rownames(new_pred$predictions[[1]])
    newy<-newy[names(nasX),]  }


  res<-list(newx=newx,newy=newy)
  newpredX<-predX<-pred_som$predictions[[1]]
  newpredY<-NULL
  predY<-NULL
  if(attr(m,"Method")!="Unsupervised"){
    if(length(pred_som$predictions)>1){
      newpredY<- as.data.frame(pred_som$predictions[[-1]])
      rownames(newpredY)<-rownames(predX)}
    predY<-newpredY
  }

  if(nrow(res$newx)>0){
    predX[rownames(res$newx),]<-res$newx
  }
  if(!is.null(predY)){
  if(nrow(res$newx)>0){
    predY[rownames(res$newy),]<-res$newy
  }}
  if(length(attr(m,"som_type2"))>0){
  if(attr(m,"som_type2")=="Factors"){
    faclist<-attr(m,"faclist")

    predY<-predclass_som(faclist, predY)
  }
}
  res<-list(predX=predX,predY=predY)
  attr(res,"newx")<-newx
  res
}

predclass_som<-function(faclist, predclass){

  relist<-data.frame(matrix(NA,nrow=nrow(predclass),ncol=length(faclist)))
  rownames(relist)<-rownames(predclass)
  colnames(relist)<-names(faclist)
  for(i in 1:length(faclist)){
    cols<-1:ncol(faclist[[i]])
    facres<-  predclass[,cols]
    relist[names(faclist)[i]]<-classmat2classvec(facres)
    predclass<-predclass[,-cols]
  }
  relist
}



getsom_results<-function(som_pred,which)
{
  res<-switch(which,
              'BMUs'=data.frame(bmu_test=som_pred$unit.classif),
              'Data predictions'=som_pred$predictions[[1]],
              'Neuron predictions'=som_pred$unit.predictions[[1]],
              'Class predictions'={
                res=data.frame(som_pred$predictions[[2]])
                colnames(res)<-attr(som_pred,"supv")
                rownames(res)<-rownames(som_pred$predictions[[1]])
                res
              },
  )
  return(res)
}


pcorr<-function(m,npic=10, indicate=c("var","cor"), col.arrow="gray80",cex.var=1, cex=1, pch=16, labels.ind=NULL,bg_palette="matlab.like2", factor.pal="gray", points=T,ncol=1,insetx=0,insety=0,alpha.legend=0.85, predict=FALSE, pred_col="firebrick", alpha_bg=1, border="white",col.text="black", legend=T,newcolhabs){
  indicadores<-NULL
  result<-NULL

  bmuvalues<-as.matrix(kohonen::unit.distances(m$grid,m$grid$toroidal))[,1]
  bmuvalues<-bmuvalues+seq(0.001,0.002,length.out=length(bmuvalues))
  maxdepth<-max(bmuvalues)
  colbmu <- adjustcolor(getcolhabs(newcolhabs,bg_palette,length(bmuvalues) ), alpha_bg)
  colcodes<-colbmu[cut(bmuvalues,breaks = bmuvalues )]
  colcodes[1]<-colbmu[1]
  colreq<-getcolhabs(newcolhabs,factor.pal,2)
if(colreq[1]!=colreq[2]){
  colfactors<-getcolhabs(newcolhabs,factor.pal,nlevels(labels.ind))
  col<-colfactors[labels.ind]} else {
    colfactors<-getcolhabs(newcolhabs,factor.pal,1)
    col<-colfactors
  }

  if(is.null(labels.ind)){
    colfactors<-getcolhabs(newcolhabs,factor.pal,1)
    col<-colfactors

  }

  opar<-par(no.readonly=TRUE)
  shape="straight"
  indicate=match.arg(indicate,c("var","cor"))


  if(npic==0){
    par(mar=c(7,6,2,0))
    set.seed(1)
    if(isFALSE(points)){
      plot(m,"mapping",shape=shape, border=border, main="",keepMargins =T, bgcol=colcodes,codeRendering=F, labels=labels.ind, cex=cex, pch=pch, col=col)}else{

      plot(m,"mapping",shape=shape, border=border, main="",keepMargins =T, bgcol=colcodes,codeRendering=F, labels=NULL, cex=cex, pch=pch, col=col)
      }
    if(!isFALSE(predict)){
      cores<-c(getcolhabs(newcolhabs,factor.pal,length(m$unit.classif)),
                     getcolhabs(newcolhabs,pred_col,length(predict$unit.classif))

      )


      par(mar=c(7,6,2,0))
      set.seed(1)
      if(isFALSE(points)){

        plot(m,"mapping",shape=shape, border=border, main="",keepMargins =T, bgcol=colcodes,codeRendering=F, labels=labels.ind, cex=cex, pch=pch, col=cores,classif=c(m$unit.classif,predict$unit.classif))}else{

          plot(m,"mapping",shape=shape, border=border, main="",keepMargins =T, bgcol=colcodes,codeRendering=F, labels=NULL, cex=cex, pch=pch, col=cores,classif=c(m$unit.classif,predict$unit.classif))
        }

    }
    plotresult<-recordPlot()
  }


  if(npic>0){
    grid.size<-nrow(m$grid$pts)
    nb <- table(factor(m$unit.classif, levels=1:grid.size))
    CORMAP <- apply(do.call(cbind,m$codes),2,weighted.correlation,w=nb,grille=m)
    sigma2  <- sqrt(apply(do.call(cbind,m$codes),2,function(x,effectif){m2<-sum(effectif*(x- weighted.mean(x,effectif))^2)/(sum(effectif)-1)},effectif=nb))

    if(indicate=="cor"){
      indicadores<-names(sort(sigma2,decreasing=T))[1:npic]
      scores<-t(CORMAP)
      Xsp<-  scores[,1]
      Ysp<- scores[,2]
      A<- rowSums(abs(data.frame(S1=scores[names(which(Xsp<0&Ysp>0)),1],S2=scores[names(which(Xsp<0&Ysp>0)),2])))
      B<-rowSums(abs(data.frame(S1=scores[names(which(Xsp>0&Ysp>0)),1],S2=scores[names(which(Xsp>0&Ysp>0)),2])))
      C<-rowSums(abs(data.frame(S1=scores[names(which(Xsp<0&Ysp<0)),1],S2=scores[names(which(Xsp<0&Ysp<0)),2])))
      D<-rowSums(abs(data.frame(S1=scores[names(which(Xsp>0&Ysp<0)),1],S2=scores[names(which(Xsp>0&Ysp<0)),2])))
      Xsp1<-names(A)[order(A, decreasing = T)][1:npic]
      Xsp2<-names(B)[order(B, decreasing = T)][1:npic]
      Ysp1<-names(C)[order(C, decreasing = T)][1:npic]
      Ysp2<-names(D)[order(D, decreasing = T)][1:npic]
      indicadores<- unlist(lapply(data.frame(t(matrix(c(Xsp1,Xsp2,Ysp1,Ysp2),ncol=4))),cbind))
      indicadores= na.omit(indicadores[1:npic])

      result<-scores[indicadores,]
      colnames(result)<-c("cor.x", "cor.y")

    }
    if(indicate=="var") { indicadores<-na.omit(names(sort(sigma2,decreasing=T))[1:npic])
    result<-data.frame(sigma2[indicadores])
    colnames(result)<-"Variance"

    }
    bp=data.frame(na.omit(as.matrix(t(CORMAP))))
    par(mar=c(7,6,2,0))
    {
      set.seed(1)
      if(isFALSE(points)){ plot(m,"mapping",shape=shape, border=border, main="",keepMargins =T, bgcol=colcodes,codeRendering=F, labels=labels.ind, cex=cex, pch=pch, col=col)}else{
        plot(m,"mapping",shape=shape, border=border, main="",keepMargins =T, bgcol=colcodes,codeRendering=F, labels=NULL, cex=cex, pch=pch, col=col)
      }


      set.seed(NULL)
    }
    library("scales")
    bp[,1]<-  rescale(bp[,1], c(min(m$grid$pts[,1]), max(m$grid$pts[,1])))
    bp[,2]<-  rescale(bp[,2], c(min(m$grid$pts[,2]), max(m$grid$pts[,2])))
    boxtext(x =(bp[indicadores,1]), y = (bp[indicadores,2]), labels = colnames(CORMAP[,indicadores]), col.bg = adjustcolor("white", 0.5),  cex=cex.var, border.bg = col.arrow, col.text=col.text)
    colreq<-getcolhabs(newcolhabs,factor.pal,2)

    if(colreq[1]!=colreq[2]) {
      legend("topr",pch=pch,legend=levels(labels.ind), col=getcolhabs(newcolhabs,factor.pal, nlevels(labels.ind)),border=NA, ncol=ncol, inset=c(insetx,insety),cex=cex, bg=adjustcolor('white',alpha.legend))

    }



  }
  plotresult<-recordPlot()
  attr(plotresult,"indicadores")<-indicadores
  attr(plotresult,"result")<-result
  #plotind(m,indicadores)
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(plotresult)
}


pclus<-function(somC,  cex=1, pch=16, labels.ind=NULL,bg_palette="matlab.like2", factor.pal="gray", points=F,ncol=1,insetx=0,insety=0,alpha.legend=0.85,newcolhabs){
  shape="straight"
  opar<-par(no.readonly=TRUE)
  m=somC$som.model
  som_cluster_adj=somC$som.hc
  col_vector=getcolhabs(newcolhabs,bg_palette,somC$groups)
  colreq<-getcolhabs(newcolhabs,factor.pal,2)


  if(colreq[1]!=colreq[2]){
    colfactors<-getcolhabs(newcolhabs,factor.pal,nlevels(labels.ind))
    col<-colfactors[labels.ind]} else {
      colfactors<-getcolhabs(newcolhabs,factor.pal,1)
      col<-colfactors
    }

  set.seed(1)
  if(isFALSE(points)){
    plot(m, type="mapping", main = "", bgcol = col_vector[som_cluster_adj], pchs = pch,border="white",keepMargins =T,codeRendering=F,shape=shape,labels=labels.ind, cex=cex, col=col)
    add.cluster.boundaries(m, som_cluster_adj)}else{
      plot(m, type="mapping", main = "", bgcol = col_vector[som_cluster_adj], pchs = pch,border="white",keepMargins =T,codeRendering=F,shape=shape,labels=NULL, cex=cex, col=col)
      add.cluster.boundaries(m, som_cluster_adj)}
  colreq<-getcolhabs(newcolhabs,factor.pal,2)
  if(colreq[1]!=colreq[2]) {
    if(!is.null(labels.ind)){
    legend("topr",pch=pch,legend=levels(labels.ind), col=getcolhabs(newcolhabs,factor.pal, nlevels(labels.ind)),border=NA, ncol=ncol, inset=c(insetx,insety),cex=cex, bg=adjustcolor('white',alpha.legend))}

  }
  codes.plot<- recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(codes.plot)

}




train.summary_function<-function(m)
{
  traindata <- data.frame(m$data[[1]])
  mean = round(mean(unlist(traindata)), 2)
  n.obs = nrow(traindata)
  n.variables = ncol(traindata)
  summ <- m$grid[-1]
  summ$neighbourhood.fct <- as.character(summ$neighbourhood.fct)
  summ <- do.call(rbind, summ)

  alpha = paste0(m$alpha, collapse = "; ")
  radius = paste0(round(m$radius, 3), collapse = "; ")
  user.weights = m$user.weights
  maxNA.fraction = m$maxNA.fraction
  dist.fcts = m$dist.fcts


  Parameters <-
    rbind(
      errors_som(m),
      n.obs,
      n.variables,
      summ,
      alpha,
      radius,
      user.weights,
      maxNA.fraction,
      dist.fcts

    )



  data.frame(Parameters)

}


errors_som_pred<-function(m,som_pred,data_pred)
{

  m_mirror<-m

  m_mirror$unit.classif<-som_pred$unit.classif
  m_mirror$codes[[1]]<-  som_pred$unit.predictions[[1]]

  aweerros<-aweSOM::somQuality(m_mirror,as.matrix(data_pred))[1:4]

  weights<-table(factor(m_mirror$unit.classif, levels=1:nrow(m$grid$pts)))
  aweerros$neu.uti<-sum(weights==0)/nrow(m_mirror$grid$pts)
  return(round(do.call("rbind",aweerros),2))
}


errors_som<-function(m)
{
  aweerros<-aweSOM::somQuality(m,as.matrix(m$data[[1]]))[1:4]
  weights<-table(factor(m$unit.classif, levels=1:nrow(m$grid$pts)))
  aweerros$neu.uti<-  sum(weights==0)/nrow(m$grid$pts)
  return(round(do.call("rbind",aweerros),2))
}
pchanges<-function(m)
{
  opar<-par(no.readonly=TRUE)
  m$errorsom<-aweSOM::somQuality(m,as.matrix(m$data[[1]]))[1:4]
  weights<-table(factor(m$unit.classif, levels=1:nrow(m$grid$pts)))
  m$errorsom$neu.uti<-  sum(weights==0)/nrow(m$grid$pts)

  errossom<-round(unlist(m$errorsom),3)
m<-readRDS("m.rds")

colnames(m$changes)<-c("Data","Factors")
plot(m,"changes", keepMargins = TRUE, tyle="l")
  #legend("bottoml", legend=c(paste("Quantization:",errossom[1]),paste("Topographic:",errossom[3]),paste("Explain_var:",errossom[2])), bty='n',cex=.7)
  pchanges <- recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(pchanges)

}
pcodes<-function(somC,...)
{
  opar<-par(no.readonly=TRUE)
  sample.names<-gsub("Sd.*","",gsub(".*#","",rownames(somC$som.model$data[[1]])))

  m<-somC$som.model
  shape="straight"
  colhabs=somC$colhabs
  plot(somC$som.model, type="mapping", bgcol = somC$colhabs[somC$som.hc], pchs = 16,border="white",shape=shape,keepMargins =T, labels=sample.names,...)
  add.cluster.boundaries(m, somC$som.hc)
  pcodes<-recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(pcodes)

}
phist<-function(data)
{
  opar<-par(no.readonly=TRUE)
  par(mar=c(5,5,5,1))
  hist(colSums(data, na.rm=T), main="")
  phist<-recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(phist)
}
getGroups<-function(m,dist=NULL, type="som",group_method="kmeans",n.max=10){
  suppressWarnings({

    if(type=="som"){
      x<-kohonen::getCodes(m)
      x.dist=kohonen::object.distances(m,"codes")
    } else {
      x<-m
      x.dist=vegdist(m,dist)
    }

    indexes = c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "sdindex", "dindex", "sdbw")
    results_nb = list()
    safe_nb = safely(NbClust)
    times = list()
    withProgress(message = 'Calculation in progress',detail = 'This may take a while.... ', value = 0,
                 {
                   for(i in 1:length(indexes) ){
                     t = lubridate::now()
                     nb = safe_nb(as.matrix(x.dist),
                                  distance = "euclidean", method = group_method , min.nc = 2
                                  , max.nc = n.max
                                  , index = indexes[i])
                     results_nb[[i]] = nb$result$Best.nc[1]
                     incProgress(1/n.max)
                   }
                 }

    )

    votes<-table(unlist(results_nb))
    groups=as.numeric(names(votes)[which.max(votes)])
    res=list(votes,groups)
    return(res)
  })

}
getelbow<-function(m, k, type="som", method, dist="bray"){

  if(type=="som"){
    x<-kohonen::getCodes(m)
    x.dist=kohonen::object.distances(m,"codes")
  } else {
    #validate(need(anyNA(m)==F, "The selected data contains missing values. Please remove them in Upload>Transform."))
    x<-m
    x.dist=vegdist(m,dist)
  }
  ks=k
  x.clus <- hclust(x.dist, method=method)
  x.grps <- cutree(x.clus, 2:ks)
  TSS <- function(x, g) {
    sum(aggregate(x, by=list(g), function(x) sum(scale(x,
                                                       scale=FALSE)^2))[, -1])
  }

  TSS.all <- apply(x.grps, 2, function(g) TSS(x, g))
  res=data.frame(n_clusters=as.numeric(names(TSS.all)), TSS=TSS.all)

  return(res)



}
elbow_plot<-function(res, sugg=NULL)
{
  {par(mar=c(5,5,5,5))

    plot(res, type="n", ylab="Total within-cluster sum of squares", lwd=2, col="darkgreen", xlab="Number of Clusters", main="Elbow plot", xlim=c(2,nrow(res)),axes=F)
    axis(1)
    axis(2,col.axis="darkgreen",col="darkgreen")
    if(!is.null(sugg))
    {
      ws<-attr(sugg,"ws")
      points<-attr(sugg,"smw")
      abline(v=sugg, col='red', lty=2)
      legend("topr",legend=c(paste("Suggested breakpoint:",sugg),
                             paste("window size:", ws)), bty="n", cex=.8)



      par(new = TRUE)
      plot(points[,1],points[,2], type="l", col="darkcyan", ann=F, axes=F, xlim=c(2,nrow(res)), lty=2)
      axis(4,col.axis="darkcyan",col="darkcyan", lty=2)

      mtext("diss smw",4, line=3, col="darkcyan", crt=90)


    }

    par(new = TRUE)
    plot(res,type="l",lwd=3, col="darkgreen", ann=F, axes=F, xlim=c(2,nrow(res)))

  }

}
upbox<-function(x){
  updateBox(
    x,
    action = "update",
    options = list(
      collapsed = T
    )
  )
}
smw.root<-function(yo, w)
{

  if (w%%2 == 1) {
    stop("window size should be even")
  }
  diss <- NULL
  yo <- data.frame(yo)
  for (i in 1:(nrow(yo) - w + 1)) {
    wy.ord <- yo[i:(i + (w - 1)), ]
    half.a <- sum(wy.ord[1:(length(wy.ord)/2) ])
    half.b <- sum(wy.ord[-c(1:(length(wy.ord)/2))])
    d <- dist(rbind(half.a, half.b))
    diss[i] <- d
    k <- (w/2)
    for (i in 1:((nrow(yo) - w))) {
      k[i + 1] <- (w/2) + i
    }
  }
  result <- data.frame(positions = k, sampleID = rownames(yo)[k],
                       diss = diss)
  return(invisible(result))
}
smw_bp<-function(res,ws)
{

  yo<-res[2]
  smw<-smw.root(yo, ws)[,c(2,3)]
  sqs<-as.numeric(smw$diss>mean(smw$diss))
  re<-split(cbind(smw,sqs), cumsum(c(1, diff(sqs) !=0)))
  sq<-re[unlist(lapply(re, function(x) sum(x$sqs)!=0))][[1]]
  bp=sq[which.max(sq[,2]),1]

  groups<-re[unlist(lapply(re,function(x) sum(x$sig)!=0))]

  #bp=smw[which(smw$diss>mean(smw$diss))[1],1]

  attr(bp,"smw")<-smw
  attr(bp,"ws")<-ws
  return(bp)

}
piecewise<-function(res){
  yo<-res[2]
  rownames(yo)<-res[,1]

  half<-ceiling(nrow(yo)/2)


  if(half %%2==1){max=half+1} else{ max=half}

  ws=seq(2,max, by=2)


  {smw <- list()
    dptemp<-smw.root(yo, ws[1])[,c(2,3)]
    rownames(dptemp)<-dptemp[,1]
    dptemp[,1]<-NULL

    for (j in 2:length(ws)) {
      w1 <- ws[j]
      message("\n SMW analysis (", j, "/", length(ws), "); w =",
              w1, "\n")
      DPtable <- smw.root(yo, w1)
      rownames(DPtable)<-DPtable[,1]
      DPtable[,c(1,2)]<-NULL



      Dmean <- DPtable
      dptemp[rownames(Dmean),j]<-Dmean


    }
  }


  means<-apply(dptemp,1,mean,na.rm=T)
  plot(means)
  sigs<-mean(means)+sd(means)
  bp<-  as.numeric(names(which.max(means[means>sigs])))




  return(bp)

}
addbreaks<-function(sugg)
{
  abline(v=sugg, col='red', lty=2)
  legend("topr",legend=paste("Suggested breakpoint:",sugg), bty="n")

}
pUmatrix<-function(m)
{  opar<-par(no.readonly=TRUE)
shape="straight"
plot(m, type="dist.neighbours", main = "U-Matrix",border="white",shape=shape,keepMargins =T, palette.name=viridis)
pUmatrix<-recordPlot()
on.exit(par(opar),add=TRUE,after=FALSE)
return(pUmatrix)

}
pcounts<-function(m)
{  opar<-par(no.readonly=TRUE)
shape="straight"
plot(m, type="counts", main = "Counts",border="white",shape=shape,keepMargins =T)


pcounts<-recordPlot()
on.exit(par(opar),add=TRUE,after=FALSE)
return(pcounts)
}
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
pproperty<-function(m, main, pic)
{
  opar<-par(no.readonly=TRUE)
  shape="straight"
  plot(m,type="property",property=do.call(cbind,m$codes)[,pic],main=main,cex=0.5,shape=shape,keepMargins = TRUE, palette.name=coolBlueHotRed)
  pproperty<-recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(pproperty)
}

pbox<-function(res, palette="viridis", coefic=1.5, lab_out=NULL ,cex.lab=1, lwd=1, main=paste0(colnames(res)[2],"~",colnames(res)[1]), ylim=range(na.omit(res[,2])), insetx=0, insety=0, show_outs=T,mar_b=5,mar_t=5,mar_l=5,mar_r=NULL, srt=0,srty="horizontal",xlab.adj=0,newcolhabs, showleg=F, xsize=1, ysize=1,tck=-0.05, linexlab=1, lineylab=1, cex_xlab=1, cex_ylab=1, font_xlab="plain", font_ylab='plain', xlab_text=colnames(res)[1], ylab_text=colnames(res)[2], horizontal=F,varwidth=F)
{
  if(is.null(mar_r)){
    mar_r<-2
  }
  if(!is.null(lab_out)){show_outs=F}
  res<-data.frame(res)

  fac_o<-res[,1]
  somprev <- res[,1]


  nlevels <- nlevels(somprev)
  colhabs =  getcolhabs(newcolhabs,palette,nlevels(fac_o))
  names(colhabs)<-levels(fac_o)
  colhabs<-colhabs[levels(somprev)]
  boxout<- boxplot(
    res[, 2] ~ somprev,
    col = colhabs,
    las = 1,
    cex = cex.lab,
    bty = "n", plot=F, range=coefic
  )
  font_xlab<-switch(font_xlab,
                    "plain"=1,
                    "bold"=2,
                    "italic"=3,
                    "bold_italic"=4)
  font_ylab<-switch(font_ylab,
                    "plain"=1,
                    "bold"=2,
                    "italic"=3,
                    "bold_italic"=4)

  outliers<-boxout$out

  legend = levels(somprev)

  par(mar=c(mar_b,mar_l,mar_t,mar_r),fg="gray30")
  boxplot(
    res[, 2] ~ somprev,
    col = colhabs,
    ylab = '',
    xlab = '',
    las = 1,
    cex = 1.1,
    xaxt = "n", yaxt = "n",
    outline=show_outs, border=darken(colhabs, amount=0.5),pch=16,
    cex.lab = cex.lab,
    main=main, ylim=ylim,
    horizontal=horizontal,
    lwd=lwd,
    varwidth=varwidth

  )

  mtext(xlab_text,if(isFALSE(horizontal)){1} else{2}, line=linexlab, cex=cex_xlab, font=font_xlab)
  mtext(ylab_text,if(isFALSE(horizontal)){2} else{1},lineylab, cex=cex_ylab, font=font_ylab)

if(isFALSE(horizontal)){
  parusr<-par("usr")[3]
  pos=1
} else{  parusr<-par("usr")[1]
pos=2}

  axis(side =if(isFALSE(horizontal)){1} else{2}, labels = FALSE,col.axis="gray30", cex.axis=cex.lab, tck=tck)
  textx<-if(isFALSE(horizontal)){1:nlevels(res[,1])} else{parusr - xlab.adj}
  texty<-if(isFALSE(horizontal)){parusr - xlab.adj} else{1:nlevels(res[,1])}

  text(x=textx,y = texty, labels=levels(res[,1]), col="gray30", cex=xsize, xpd=NA,srt=srt, pos=pos)
  if(!is.null(srty)){
  if(srty=='vertical'){
    srty<-0
  } else{
    srty<-1
  }
  } else{
    srty<-1
    }
  axis(if(isFALSE(horizontal)){2} else{1}, col="gray23",
       col.axis="gray30", cex.axis=ysize, tck=tck, las=srty)

  if(length(boxout$out)>0){
    outliers<-which(res[,2] %in% c(boxout$out))

    if(length(outliers)>0){
      outs<-rownames(res)[outliers]
      if(is.data.frame(lab_out)){
        lab_outs<-as.character(lab_out[outs,])[order(as.numeric(lab_out[outs,]))]

        text(boxout$group,boxout$out, labels=as.character(lab_outs), col=  rep(darken(colhabs,0.5), table(factor(boxout$group, levels=levels(as.factor(as.numeric(somprev)))))), cex=cex.lab)
      }

    }
  }
  if(isTRUE(showleg)){
  ncol_legend<-ceiling(length(legend)/10)
  colreq<-getcolhabs(newcolhabs,palette,2)
  if(colreq[1]!=colreq[2])
  {legend("topr",
          legend = legend,
          col = colhabs,
          pch = 15,
          bty = "n",
          cex = cex.lab, ncol=ncol_legend, xpd=T, inset=c(insetx,insety), x.intersp=0.5
  )}
  }
  plotbox<-recordPlot()
  attr(plotbox,"outliers")<-rownames(res)[outliers]
  return(plotbox)

}

