getcolhabs<-function(newcolhabs,palette,n){
  newcolhabs[[palette]](n)
}

plotshape<-function(shape){
  ggplot(st_as_sf(shape)) + geom_sf()+
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))
}

map_discrete_variable<-function(data,get,coords,base_shape=NULL,layer_shape=NULL,main="",size=.14,cex.main=15,cex.axes=13,cex.lab=15,cex.sub=14,cex.leg=11,cex.pt=7,subtitle="",leg="", factors=NULL,showcoords=F, cex.coords=NULL, col.coords="firebrick",col.palette='matlab.like2',col.fac="firebrick",symbol=15, scalesize_size=T,scalesize_color=T, points=T, cex.fac=4, as_factor=F,bmu=F, colored_by_factor=NULL,showguides=F, limits=NULL,layer_col="gray",lighten=0.5,base_col="white",base_lighten=1,newcolhabs, extralayers=NULL,  data_depth=if(!is.null(extralayers)){3+(length(extralayers$layers)*2)} else{NULL},breaks_len=5,mybreaks=NULL,cexmin.pt=0){

  {
    {
    base_shape0<-base_shape
    layer_shape0<-layer_shape
    shapes<-get_shapes(base_shape,layer_shape,coords)
    base_shape<-shapes$base_shape
    layer_shape<-shapes$layer_shape
    BS_ggplot<-layer_shape




    if(isTRUE(as_factor)){
      scalesize_size=F
      scalesize_color=T
      if(isTRUE(bmu)){
        somprev<-somC$som.model$unit.classif
        names(somprev)<-names(somC[[1]])
        colhabs<-somC$colunits
        nlevels<-nrow(somC$som.model$grid$pts)
        name.var="bmu"
      } else{
        somprev<-as.numeric(as.factor(data[,get]))
        names(somprev)<-rownames(data[get])
        nlevels<-nlevels(as.factor(somprev))
        colhabs=getcolhabs(newcolhabs,col.palette,as.vector(nlevels))
        if(is.null(leg)){name.var=get} else {name.var=leg}
      }

      prev=newy<-factor(somprev, levels=1:nlevels)
      geopoint<-cbind(coords[names(prev),],prev)
      colnames(geopoint)<-c("x","y","pop")
      colfac= getcolhabs(newcolhabs,col.fac,as.vector(nlevels))[as.factor(prev)]
    } else{
      if(!is.null(factors)){

        colfac= getcolhabs(newcolhabs,col.fac,as.vector(nlevels(as.factor(factors))))[as.factor(factors)]
      }
    }


    if(!is.null(points)){
      geopoint<-cbind(coords[rownames(data),],data[,get])
      colnames(geopoint)<-c("x","y","pop")
      if(is.null(leg)){name.var=get} else {name.var=leg}
    }



    if(is.null(points)& as_factor==F)
    {
      geopoint<-cbind(coords[rownames(data),])
      colnames(geopoint)<-c("x","y")
      name.var=""

    }
    if(!is.factor(geopoint$pop)){
    if(cexmin.pt==0){
    geopoint[geopoint$pop==min(geopoint$pop),]<-NA
    }}
    geopoint<-na.omit(geopoint)
    if(isTRUE(as_factor)){  mybreaks<-1:nlevels
    } else{
      if(is.factor(geopoint$pop)){ mybreaks<-1:nlevels(geopoint$pop)}else {
        #my<-pretty(geopoint$pop,n=breaks_len)
        #my<-my[which(my>0)]
        #my<-my[which(my<max(geopoint$pop))]
        #mybreaks<-my

      }
    }


    if(is.null(limits)){
      limits<-st_bbox(base_shape)}
    xlimits<-limits[c(1,3)]
    ylimits<-limits[c(2,4)]
    bbox<-t(cbind(xlimits,ylimits))
    #if(any(mybreaks==0)){mybreaks<-mybreaks[-which(mybreaks==0)]}

    suppressWarnings(st_crs(BS_ggplot)<-"+proj=longlat +datum=WGS84 +no_defs")

    p<-ggplot(st_as_sf(to_spatial(geopoint)))
    if(!is.null(layer_shape0)){
      p<-p+geom_sf(data=st_as_sf(layer_shape), fill=lighten(layer_col,lighten), lty=1)
    }




    if(!is.null(base_shape0)){
      p<-p+geom_sf(data=base_shape, fill=lighten(base_col,base_lighten), lty=1)
    }


    if(!is.null(extralayers)){
      for(i in 1:length(  extralayers$layers)){
        col_extra<-getcolhabs(newcolhabs,extralayers$colors[i],nrow(as.data.frame(st_as_sf(extralayers$layers[[i]]))))
        p<-p+geom_sf(data=st_as_sf(extralayers$layers[[i]]), col=lighten(   col_extra,extralayers$alphas[i]), lty=1)
        names( p$layers)[length( p$layers)]<-paste0("extra",i)
        if(extralayers$labels[i]!='None'){
          p<-p+geom_sf_text(data=st_as_sf(extralayers$layers[[i]]),aes(label=get(extralayers$labels[i])), size=extralayers$sizes[i],check_overlap=T,col=col_extra)
          names( p$layers)[length( p$layers)]<-paste0("extra_lab",i)
        }

      }

    }



    #res<-split(extralayers$layers[[i]],extralayers$layers[[i]]$Contour)
    #res<-do.call(rbind,lapply(lapply(res,st_bbox),function(x) c(x$xmin,x$ymin)))
    #res<-data.frame(res)
    #res$id<-rownames(res)
    #colnames(res)[c(1:2)]<-c("x","y")
    #p+geom_sf(data=st_as_sf(extralayers$layers[[i]]), col=lighten(   extralayers$colors[i],extralayers$alphas[i]))+geom_text(data=res,aes(x,y,label=id), angle =45)






    if(!is.null(colored_by_factor)){

      colfactor<-colored_by_factor[,1]
      names(colfactor)<-rownames(data[get])
      nlevels_fac<-nlevels(as.factor(colfactor))
      prev_fac<-colfactor
      col_pts=getcolhabs(newcolhabs,col.palette,as.vector(nlevels_fac))


      colorFAC<-  data.frame(prev_fac=levels(colfactor),col_pts, levels=1:nlevels(colfactor))



      geopoint_fac<-cbind(geopoint,fac=prev_fac[rownames(geopoint)])
      col_pts<-col_pts[rownames(geopoint_fac)]
    } else {col_pts=getcolhabs(newcolhabs,col.palette,100)[2]}
    }

    if(!is.null(points)){
      if(isTRUE(scalesize_size)){
        if(isTRUE(scalesize_color))
        {  p <-  p+ geom_point( data=geopoint, aes(x=x, y=y, size=pop, col=pop), pch=symbol)
        } else if(isFALSE(scalesize_color)&is.null(colored_by_factor))
        {   p <- p+geom_point( data=geopoint, aes(x=x, y=y, size=pop), pch=symbol,color=col_pts)} else if(isFALSE(scalesize_color)&!is.null(colored_by_factor)){
          p <- p+geom_point( data=geopoint_fac, aes(x=x, y=y, size=pop, col=fac), pch=symbol)+ scale_color_manual(name=leg,labels =  colorFAC$prev_fac,values =  colorFAC$col_pts,drop=F )
        }

      } else  if(isFALSE(scalesize_size))
      {
        if(isTRUE(scalesize_color)){
          p<-  p+ geom_point( data=geopoint, aes(x=x, y=y,  col=pop), pch=symbol,size=cex.pt)} else if(isFALSE((scalesize_color))){
            if(!is.null(colored_by_factor)) {
              p <- p+geom_point( data=geopoint_fac, aes(x=x, y=y, col=fac), pch=symbol,size=cex.pt)+ scale_color_manual(name=leg,labels =  colorFAC$prev_fac,values =  colorFAC$col_pts,drop=F)
            } else {
              p<-   p+geom_point( data=geopoint, aes(x=x, y=y), pch=symbol,size=cex.pt, color=col_pts[rownames(geopoint)])
            }
          }
      }
    }
    names( p$layers)[length( p$layers)]<-paste0('points')

    #scale_color_viridis(name=name.var, limits= range(mybreaks)) +
    if(is.null(colored_by_factor)){
      if(isTRUE(as_factor)){p<- p +   geom_point( data=geopoint, aes(x=x, y=y), pch=symbol, color=colhabs[as.factor(prev)]) }else{p<-p+
        scale_size(name=name.var, range=c(cexmin.pt,cex.pt), breaks=mybreaks,guide="none") +

        scale_colour_gradientn (colours=getcolhabs(newcolhabs,col.palette,100), guide="none")+

        guides(size = guide_legend(override.aes = list(colour = as.list( scales::col_numeric(getcolhabs(newcolhabs,col.palette,100), domain = NULL)(mybreaks)),size=scales::rescale(mybreaks,c(1,cex.pt)))))

      }
    }
  }

  #p<-p  +  scale_size(name=name.var, range=c(0,cex.pt), breaks=mybreaks)

if(isTRUE(showguides)){
  xcoords<-pretty(coords[,1])
  ycoords<-pretty(coords[,2])
  p<-p+geom_hline(yintercept=ycoords,color = gray(.5), linetype = "dashed", size = .15)+
    geom_vline(xintercept =xcoords,color = gray(.5), linetype = "dashed", size = .15)
}
p<-p+
  #guides( colour = guide_legend())+

  annotation_scale(location = "br", width_hint = 0.15) +
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(.1, "in"), pad_y = unit(0.1, "in"), style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(bbox[1,]), ylim = c(bbox[2,]), expand = FALSE) +


  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(main, subtitle = subtitle) +
  theme(panel.grid.major = element_blank(),

        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        axis.line=element_line(),
        axis.text=element_text(size=cex.axes),
        axis.title=element_text(size=cex.lab,face="bold"),
        plot.title=element_text(size=cex.main),
        plot.subtitle=element_text(size=cex.sub),
        legend.text=element_text(size=cex.leg),
        legend.title=element_text(size=cex.leg))
if(!is.null(factors)){

  geopoint0<-cbind(coords[rownames(data),],factors)
  colnames(geopoint0)<-c("x","y","factors")
  p<-p+  geom_text( data=geopoint0, aes(x=x, y=y, label=factors,),size=cex.fac,colour=colfac)
}
if(isTRUE(showcoords)){
  geopoint0<-cbind(coords[rownames(data),],data[,get])
  colnames(geopoint0)<-c("x","y","pop")
  p<-p+geom_point( data=geopoint0, aes(x=x, y=y),  size=cex.coords, pch=3, colour=col.coords)

}




if(!is.null(extralayers)){
  if(!is.null(data_depth)){
    point_layer<-p$layers[ grep("points",  names(p$layers))]
    old_layer<-p$layers[-grep("points",  names(p$layers))]
    new_p <- append(old_layer, point_layer, after=data_depth-1)
    p$layers<-new_p}

  }

p}


lighten <- function(color, factor = 0.5) {
  if ((factor > 1) | (factor < 0)) stop("factor needs to be within [0,1]")
  col <- col2rgb(color)
  col <- col + (255 - col)*factor
  col <- rgb(t(col), maxColorValue=255)
  col
}
to_spatial<-function(coords,  crs.info="+proj=longlat +datum=WGS84 +no_defs"){
  suppressWarnings({
    colnames(coords)[1:2]<-c("Long","Lat")
    coordinates(coords)<-~Long+Lat
    proj4string(coords) <-CRS(crs.info)
    return(coords)
  })
}
inline<-function (x) {
  tags$div(style="display:inline-block; margin: 0px", x)
}
inline2<-function (x) {
  tags$div(style="display:inline-block; margin-top: -100px", x)
}



scale_color_matlab<-function (..., alpha = 1, begin = 0, end = 1, direction = 1,discrete = FALSE, option = "D",palette=c(
  "matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1",'Grays',"heat",'Purples',"Blues",'Greens',"black","gray","royalblue", "firebrick","forestGreen",'goldenrod3',"white"),
  newcolhabs)
{

  if (discrete) {
    discrete_scale("colour", palette,  newcolhabs[[palette]], ...)
  } else {
    scale_color_gradientn(colours = do.call( newcolhabs[[palette]],args=list(n=256)),...)

  }
}

colsbyprof<-function(depths,cols=c('lightblue','darkblue'))
{
  if(is.null(names(depths))){ids<-1:length(depths)} else {ids<-names(depths)}
  rbPal <- colorRampPalette(cols)
  maxdepth<-max(depths)
  profcols <- rbPal(maxdepth)[as.numeric(cut(depths,breaks = depths))]
  lev<-  pretty(depths)
  proflev <- rbPal(max(lev))[as.numeric(cut(lev,breaks = max(lev)))]
  attr(profcols, 'prettydepth')<-lev
  attr(profcols, 'prettycols')<-proflev
  return(profcols)}


scale_color_2<-function (palette="viridis",newcolhabs)
{
  newcolhabs[[palette]](256)
}

