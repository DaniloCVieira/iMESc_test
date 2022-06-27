

server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^3)



  palette=c(
    "matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1",'Grays',"heat",'Purples',"Blues",'Greens',"black","gray","royalblue", "firebrick","forestGreen",'goldenrod3',"white"
  )
  symbols<-c("pch1","pch2","pch3","pch4")

  dfcolors <- data.frame(
    val = palette
  )


  for(i in 1:length(palette))  {
    palette1<-base64enc::dataURI(file = paste0('meta/palette',i,".png"),
                                 mime = "image/png")
    dfcolors$img[i]<- sprintf(paste0(
      img(src = palette1, height = '20',width = '70',
          style="margin-top:-40px;margin-bottom:-40px;margin-left:-20px;margin-right:-20px;") ))}



  #colors_solid<-reactiveValues(df=dfcolors[c(11:17),])
  #colors_gradient<-reactiveValues(df=dfcolors[c(1:10),])


  myBookmarks<-vals<-reactiveValues(
    temp_path=paste0(if(!length(grep("connect/apps",getwd()))>0){(getwd())} else{"/srv/shiny-server/MyShinyApp"},'/mybooks.rds'),
    map_data_raster=NULL,
    DT_results=0,
    saved_data=NULL,
    saved_labels=NULL,
    saved_base_shape=0,
    saved_layer_shape=0,
    saved_model=NULL,
    saved_kdataWSS=NULL,
    saved_kdataaccRF=NULL,
    saved_kmodelaccRF=NULL,
    saved_kmodelWSS=NULL,
    saved_kcustom=NULL,
    saved_kmodelvotes=NULL,
    saved_kdatavotes=NULL,
    saved_maps=NULL,
    saved_divset_value=0,
    som_unsaved=0,
    cur_cmap="discrete",
    # data
    data_map=NULL,
    cur_var_map=NULL,
    rf_sigs=0,
    var_map=NULL,
    #cur inputs
    cur_choices_map=NULL,
    cur_tab=NULL,
    cur_data=NULL,
    cur_datasomY=NULL,
    curview_databank="data",
    cur_partsom='None',
    cur_nb="new cur_nb (unsaved)",
    cur_rf="new rf (unsaved)",
    cur_partrf=NULL,
    cur_train="new som (unsaved)",
    cursomtab=0,
    currftab=0,
    cur_somtype="Unsupervised",
    cur_som_type2="Numeric",
    cur_testsom=NULL,
    cur_rftype="Classification",
    cur_rfsup=NULL,
    cur_rfY=NULL,
    cur_dist_mds="Choose one",
    cur_testrf=NULL,
    cur_saved_maps=NULL,

    bmu_p_symbol_size=1,
    bmu_symbol_size=1,
    bmu_p_bg_transp=0.3,
    bmu_bg_transp=0.3,
    bmu_p_factors=1,
    cur_bmu_factors=1,
    bmu_insertx=0,
    bmu_inserty=0,
    bmu_ncol=1,
    bmu_leg_transp=0,
    bmu_cexvar=1,
    curview_desc_options='Summaries',
    curview_summ_options='Data',
    shptool="upload",
    bmu_points=T,
    bmuleg=F,
    # bags
    bagdata=c(T,T),
    bagmodel=FALSE,
    baghc=F,
    bagpart0=0,
    baghc0=0,
    bagbp0=0,
    bag_rf=F,
    bagdataraw=0,
    bagshp=F,
    bag_submenu=0,
    bag_rf_data=0,
    bag_user_bp=NULL,
    bagHC=0,

    # hands
    hand_plot=0,
    hand_save=0,
    hand_save2=NULL,
    hand_save3=NULL,
    hand_down=0,

    #results
    bmu_p_results=0,
    cm_rf=0,
    conf_rf=0,
    som_results=NULL,
    pcorr_results=0,

    map_data_disc=NULL,
    map_data_interp=NULL,
    map_fac_disc=NULL,
    map_fac_interp=NULL,
    smw_dp=NULL,
    rfd_res=0,
    rfm_res=0,
    RF_results=NULL,
    rf_unsaved=0,
    map_res=0,

    ##plot
    conf_som=0,
    bmus_pred_plot=0,
    rda_plot=0,
    seg_rda_plot=0,
    pbox_plot=0,
    pclus_plot=0,
    bmus_plot=0,
    pprop_plot=0,
    varplot=0,
    pmds_plot=0,
    ppca_plot=0,
    pdend_plot=0,
    ptree_plot=0,

    #internal
    new_facts=NULL,
    selall=NULL,
    loadlabel=0,
    segrda_windows=NULL,
    plot_we=0,
    plot_dp=NULL,
    splitBP=F,
    insertbmu=F,
    sidebar=T,
    urlDF=0,
    colors_img=dfcolors,
    newcolhabs=list(
      "viridis" = get('viridis'),
      "plasma" = get('plasma'),
      "matlab.like2" = colorRampPalette(matlab.like2(1000)[-1]),
      "black" = colorRampPalette("black"),
      "gray" = colorRampPalette("gray"),
      "white" = colorRampPalette("white"),
      "royalblue" = colorRampPalette("royalblue"),
      "firebrick" = colorRampPalette("firebrick"),
      "forestGreen" = colorRampPalette("forestGreen"),
      "goldenrod3" =  colorRampPalette("goldenrod3"),

      "Rushmore1"=colorRampPalette(wes_palette("Rushmore1",100,type ="continuous")),
      "FantasticFox1"=colorRampPalette(wes_palette("FantasticFox1",100,type ="continuous")),
      "Blues"=colorRampPalette(RColorBrewer::brewer.pal(9,"Blues")),
      "heat"=get('heat.colors'),
      "Purples"=colorRampPalette(RColorBrewer::brewer.pal(9,"Purples")),
      "Greens"=colorRampPalette(RColorBrewer::brewer.pal(9,"Greens")),
      "Grays"=colorRampPalette(c("gray90","gray10"))

    )
  )


  getsolid_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(vals$colors_img$val%in%solid)
    pic
  })
  getgrad_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    grad<-names(res1[res1==F])
    pic<-which(vals$colors_img$val%in%grad)
    pic
  })


  symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
  df_symbol <- data.frame(
    val = c(16,15,17,18,8,1,5,3)
  )
  for(i in 1:length(symbols))
  {
    symbol1<-base64enc::dataURI(file = paste0('meta/pch',i,".png"), mime = "image/png")

    df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}


  bmu_p_bgpalette<-reactiveValues(df=dfcolors$val[16])
  bmu_bgpalette<-reactiveValues(df=dfcolors$val[16])
  bmu_p_training<-reactiveValues(df=dfcolors$val[1])
  bmu_facpalette<-reactiveValues(df=dfcolors$val[11])
  bmu_p_test<-reactiveValues(df=dfcolors$val[14])
  bmu_p_border_grid<-reactiveValues(df=dfcolors$val[4])
  bmu_border_grid<-reactiveValues(df=dfcolors$val[4])
  bmu_p_dotlabel<-reactiveValues(df='symbols')
  bmu_dotlabel<-reactiveValues(df='symbols')
  bmu_p_symbol<-reactiveValues(df= df_symbol$val[1])
  bmu_symbol<-reactiveValues(df= df_symbol$val[1])
  observeEvent(input$distance,{
    vals$cur_dist_mds<-input$distance
  })
  observeEvent(input$som_type2,{
    vals$cur_som_type2<-input$som_type2
  })
  observeEvent(input$som_test_pick,{
    vals$cur_partsom<-input$som_test_pick
  })
  observeEvent(input$som_test_ref,{
    vals$cur_testsom<-input$som_test_ref
  })
  observeEvent(input$data_somY,{
    vals$cur_datasomY<-input$data_somY
  })
  observeEvent(input$choices_map,{
    vals$cur_choices_map=input$choices_map
  })




  observeEvent(input$upload_insert,{
    if(input$up_or_ex=="upload"){
      validate(need(length(input$labels$datapath)>0,"error"))
      validate(need(length(input$filedata$datapath)>0,"error"))
    }
    datalist<-getdatalist()


    if(input$up_or_ex == 'use example data'){
      envi <-data.frame(fread("meta/envi_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
      rownames(envi) <- envi[, 1]
      envi[, 1] <- NULL
      envi<-data_migrate(datalist[[1]],envi,"envi_araca")
       vals$saved_data[["envi_araca"]]<-envi
    }

    else {
      vals$saved_data <- c(vals$saved_data, datalist)
    }
    upload_bag$df<-0
    removeModal()
    updateSelectInput(session,"data_bank",selected=input$data_name)
    vals$cur_data<-input$data_name
  })
  observeEvent(input$data_bank,{
    vals$cur_data<-input$data_bank
  })
  observeEvent(input$data_upload0,{
    req(length(vals$saved_data)>0)
    vals$cur_data<-input$data_upload0
  })
  observeEvent(input$data_som,{
    vals$cur_data<-input$data_som
  })
  observeEvent(input$data_div,{
    vals$cur_data<-input$data_div})
  observeEvent(input$data_som,{
    vals$cur_data<-input$data_som
  })
  observeEvent(input$data_map,{
    vals$cur_data=input$data_map
  })
  observeEvent(input$data_hc,{
    vals$cur_data=input$data_hc
  })



  output$menu_nb_out<-renderUI({module_ui_nb("module_nb")})
  mod1 <- callModule(module_server_nb, "module_nb",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
  output$menu_svm_out<-renderUI({module_ui_svm("module_svm") })
  mod_svm <- callModule(module_server_svm, "module_svm",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
  output$menu_rf_out<-renderUI({module_ui_rf("module_rf") })
  mod_rf <- callModule(module_server_rf, "module_rf",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
  stopmap<-reactiveValues(df=F)
  output$map_panel<-renderUI({
    fluidRow(
      fluidRow(style="background-color: white",
               column(12,uiOutput("map_part1")),
               column(12,id="side_map",class="map_control_style",
                      uiOutput("automap")
               )
      ),

      uiOutput("map_part2")

    )

  })
  output$automap<-renderUI({
    req(input$saved_maps=='new map'|isTRUE(input$stack_map))
    column(12,

           span("+ Auto-refresh",
                inline(
                  switchInput(
                    inputId = "automap",
                    label = NULL,
                    value = TRUE,
                    onStatus = "success",
                    size ="mini",inline=T
                  )
                ), inline(uiOutput("gomap")),
                inline(uiOutput("automap_war"))
           )
    )
  })
  output$gomap<-renderUI({
    req(isFALSE(input$automap))
    inline(
      bsButton('gomap',span(icon("fas fa-arrow-alt-circle-right"),"Render Map"), style="button_active", width="100px")
    )
  })
  gomap<-reactiveValues(df=F)
  observeEvent(input$automap,{
    if(isFALSE(input$automap)){    stopmap$df<-T}
  })
  observeEvent(input$gomap,{
    stopmap$df<-F
    stopbigmap$df<-F

  })
  observeEvent(input$order_stack,{
    showModal(
      modalDialog(
        easyClose = T,
        footer = column(12,inline(actionButton("run_order_stack","Apply order")),modalButton("close")),
        column(12,
               fluidRow(
                 rank_list(
                   text = "Drag the layers in any desired order",
                   labels = names(get_stacklist()),
                   input_id = "rank_list_3",
                   class="custom-sortable"
                 ),
                 tags$style(
                   HTML("
          .custom-sortable .rank-list-item {
            height: 25px;
          padding:5px;
           background-color: #BDB;
          border: 1px solid white;
          }
        ")
                 )
               ))
      )
    )
  })
  reac_stack_order<-reactiveValues(df=0)
  observeEvent(input$run_order_stack,{

    vals$saved_maps<-vals$saved_maps[input$rank_list_3]
    removeModal()
    reac_stack_order$df<-T


  })
  get_stacklist<-reactive({
    #req(input$saved_maps!='new map')
    req(length(vals$saved_maps)>1)
    res_list<-lapply(vals$saved_maps,function(x) attr(x,"my_rst"))
    nuls<-which(unlist(lapply(res_list, function (x)
      !is.null(x))))
    res_list<-res_list[nuls]
    names(res_list)<-names(vals$saved_maps)[nuls]
    res_list
  })
  observeEvent(input$add_map,{
    updatePickerInput(session,'saved_maps',selected="new map",choices=c("new map",names(vals$saved_maps)))
    if(isTRUE(input$edit_map)){
      updateButton(session,"edit_map",value=F)
    }

  })
  observeEvent(input$cmap,{

    vals$cur_cmap<-input$cmap
  })
  upload_bag<-reactiveValues(df=0, df0=0)
  observeEvent(input$save_map,{
    data<-vals$saved_data[[input$data_map]]
    p<-getmap()
    factors <- attr(getdata_map(),"factors")
    var_name=input$var_map
    if(input$cmap=="discrete"){
      vals$hand_save<-"Save discrete model"}
    if(input$cmap=="interpolation"){
      vals$hand_save<-"Save interpolation model"}
    if(input$cmap=="raster"){
      vals$hand_save<-"Save raster"}
    vals$hand_save3<-NULL
    showModal(
      hand_save_modal()
    )
  })
  output$add_map<-renderUI({
    req(input$saved_maps!='new map')
    inline(bsButton("add_map",tipify(icon("fas fa-plus"),"create a new map"), style="button_active"))

  })
  output$show_saved_maps<-renderUI({
    pickerInput("saved_maps",NULL, choices=c("new map",names(vals$saved_maps)), width="150px")

  })
  output$save_map<-renderUI({
    req(input$saved_maps=='new map'|isTRUE(input$edit_map))

    bsButton('save_map',tipify(icon("fas fa-save"),"Save Map"), style="button_active")
  })
  output$mantel_map<-renderUI({
    req(input$saved_maps=="new map")
    bsButton('mantel_map',tipify(icon("fas fa-chart-line"),"Mantel correlogram"), style="button_active", type="toggle")
  })
  output$stop_map<-renderUI({
    req(input$cmap=="interpolation"|input$cmap=="discrete")
    req(input$saved_maps=="new map")
    req(isTRUE(warbigmap$df))
    div(
      p(strong("Warning: Auto-refresh disabled", style="color:red"),"Use the button 'Render Map' to proceed with Discretization/Interpolation. The selected datalist has", nrow(getdata_map()), "observations. Discretizating/Interpolating big data can be time consuming. The tool Raster can be faster. ")
    )
  })
  observeEvent(input$stop_map,{
    stopmap$df<-F
  })
  stopbigmap<-reactiveValues(df=F)
  warbigmap<-reactiveValues(df=F)
  observeEvent(input$data_map,{
    if(nrow(getdata_map())>1000){
      stopmap$df<-T
      stopbigmap$df<-T
      warbigmap$df<-T
    } else{
      stopbigmap$df<-F
      stopmap$df<-F
      warbigmap$df<-F
    }

  })
  observeEvent(stopbigmap$df,{
    if(isTRUE(stopbigmap$df)){
      updateSwitchInput(session,"automap",value=F)

    }
  })
  output$scatter_3d<-renderUI({
    req(input$cmap=='discrete')
    req(input$choices_map=="Data-Attribute")
    inline(tipify(bsButton("scatter_3d",icon("fas fa-cube"), style="button_active", type="toggle"),"3D mode"))})
  output$stack_scatter_3d<-renderUI({

    req(input$cmap=='discrete')
    # req(input$choices_map=="Data-Attribute")
    inline(tipify(bsButton("stack_scatter_3d",icon("fas fa-layer-group"), style="button_active", type="toggle"),"3D stack"))
  })
  observeEvent(input$scatter_3d,{
    if(input$scatter_3d %% 2){
      updateButton(session,'stack_scatter_3d',value=F)
    }
  })
  observeEvent(input$stack_scatter_3d,{
    if(input$stack_scatter_3d %% 2){
      updateButton(session,'scatter_3d',value=F)
    }
  })
  output$trash_map<-renderUI({
    req(input$saved_maps!='new map')
    inline(tipify(bsButton("trash_map",icon("far fa-trash-alt"), style="button_active"),"3D mode"))})
  observeEvent(input$trash_map,{
    if(input$trash_map%%2){
      vals$saved_maps[[input$saved_maps]]<-NULL
      if(length(vals$saved_maps)>0){
        updatePickerInput(session,"saved_maps",selected=names(vals$saved_maps)[1])}}
  })
  output$edit_map<-renderUI({
    req(input$saved_maps!='new map')
    inline(bsButton("edit_map",icon("fas fa-edit"), style="button_active", type="toggle"))})
  cur_surface<-reactiveValues(df=F)
  output$surface_map<-renderUI({
    req(input$saved_maps!='new map')
    req(!isTRUE(input$edit_map))
    req(!is.null(attr(vals$saved_maps[[input$saved_maps]],"my_rst")))
    bsButton('surface_map',tipify(icon("fas fa-mountain"),"create a surface Map"), style="button_active", type="toggle", value=cur_surface$df)
  })
  observeEvent(input$surface_map,{
    cur_surface$df<-input$surface_map
  })
  output$stack_map<-renderUI({
    req(input$cmap=='interpolation'|input$cmap=='raster')
    #req(!isTRUE(input$edit_map))

    if(!length(vals$saved_maps)>1){
      tags$div(datatitle="Create stacked raster map. Requires more than one saved interpolated map.",style="z-index:100;  delay:0",bsButton('stack_map',icon("fas fa-layer-group"), style="button_active", type="toggle", disabled = T))
    } else {
      res_list<-get_stacklist()
      if(length(res_list)>1){
        tags$div(datatitle="Create stacked raster map",style="z-index:100;  delay:0",bsButton('stack_map',icon("fas fa-layer-group"), style="button_active", type="toggle", disabled = F))
      } else{
        tags$div(datatitle="Create stacked raster map. Requires more than one saved interpolated map.",style="z-index:100;  delay:0",bsButton('stack_map',icon("fas fa-layer-group"), style="button_active", type="toggle", disabled = T))
      }
    }






  })
  output$rgl_map<-renderUI({
    req(input$saved_maps!='new map')
    req(!is.null(attr(vals$saved_maps[[input$saved_maps]],"my_rst")))
    req(isTRUE(input$surface_map))
    bsButton('rgl_map',tipify(icon("fas fa-video"),"Show RGL"), style="button_active", type="toggle")
  })
  output$ss_rgl<-renderUI({
    req(input$cmap=='discrete')
    req(length(ss_map$df)>1)
    bsButton('ss_rgl',tipify(icon("fas fa-video"),"Show RGL"), style="button_active", type="toggle")
  })
  output$sr_rgl<-renderUI({
    req(isTRUE(input$stack_map))
    bsButton('sr_rgl',tipify(icon("fas fa-video"),"Show RGL"), style="button_active", type="toggle")
  })
  observeEvent(input$sr_rgl,{
    delay(100,

          if(isTRUE(input$sr_rgl)){
            updateButton(session,'sr_rgl',value=F)
          }

    )
  })
  observeEvent(input$ss_rgl,{
    delay(100,

          if(isTRUE(input$ss_rgl)){
            updateButton(session,'ss_rgl',value=F)
          }

    )
  })
  output$map_ssrgl<-renderUI({
    req(isTRUE(input$ss_rgl))
    div(
      renderPrint(rgl.dev.list()),
      rglwidgetOutput("ssrgl_out",  width = 600, height = 600)
    )
  })
  output$map_srrgl<-renderUI({
    req(isTRUE(input$sr_rgl))
    div(
      renderPrint(rgl.dev.list()),
      rglwidgetOutput("srrgl_out",  width = 600, height = 600)
    )
  })
  output$srrgl_out<-renderRglwidget({

    req(isTRUE(input$sr_rgl))

    res_list<-get_stacklist()[input$stack_layers]
    validate(need(length(res_list)>1,"Requires at least two layers"))
    data<-getdata_map()
    coords<-attr(getdata_map(),"coords")

    zs<-c()
    co<-c()
    labs<-list()
    showlab<-c()
    for(i in 1:length(names(get_stacklist())))
    {
      zs[i]<-input[[paste0("stack_z",i)]]
      co[i]<-input[[paste0("stack_co",i)]]
      showlab[i]<-input[[paste0("stack_lab",i)]]
      labs[[i]]<-attr(getdata_map(),"factors")[rownames(coords), input[[paste0("stack_labels",i)]]]


    }
    layer_shape=if(isTRUE(input$srmap_layer)){
      attr(data,"layer_shape") } else{ NULL}
    srcol_layer=adjustcolor(getcolhabs(vals$newcolhabs,input$srlayer_col,1), input$srlayer_lighten)


    #options(rgl.useNULL = TRUE)
    rgl.bg(color=c('white','white'))

    anim_stack1(res_list,
                zvalue=zs,
                xlim=c(input$srlong_xmin,input$srlong_xmax),
                ylim=c(input$srlat_xmin,input$srlat_xmax),
                layer_shape=layer_shape,
                col_layer=srcol_layer,
                zmin=input$stack_zmin,
                zmax=input$stack_zmax,
                newcolhabs=vals$newcolhabs,
                coords=coords,
                col.labels=input$col_factor_stack,
                cex.labels=input$pt_factor_stack,
                labels=labs,
                col.coords=input$col_coords_stack,
                cex.coords=input$pt_coords_stack,
                show_coords=co,
                show_labels=showlab,
                texmipmap=F,texmagfilter="nearest",texminfilter="nearest.mipmap.nearest",texenvmap=F,size=1,point_antialias=F,line_antialias=F,depth_mask=F,lit =F,top=F
    )




  })
  output$ssrgl_out<-renderRglwidget({

    req(isTRUE(input$ss_rgl))
    data=ss_map$df
    coords=  attr(ss_map$df,"coords")
    base_shape=if(isTRUE(input$ssmap_base)){
      attr(data,"base_shape") } else{ NULL}
    layer_shape=if(isTRUE(input$ssmap_layer)){
      attr(data,"layer_shape") } else{ NULL}
    sscol_layer=adjustcolor(getcolhabs(vals$newcolhabs,input$sslayer_col,1), input$sslayer_lighten)
    sscol_base=adjustcolor(getcolhabs(vals$newcolhabs,input$ssbase_col,1), input$ssbase_lighten)

    pal<-c()
    cex<-c()
    zs<-c()
    co<-c()
    labs<-list()
    showlab<-c()

    for(i in 1:length(ss_map$df))
    {
      pal[i]<-input[[paste0("pt_palette",i)]]
      cex[i]<-input[[paste0("ss_cex",i)]]
      zs[i]<-input[[paste0("ss_z",i)]]
      labs[[i]]<-attr(getdata_map(),"factors")[rownames(coords), input[[paste0("ss_labels",i)]]]
      co[i]<-input[[paste0("ss_co",i)]]
      showlab[i]<-input[[paste0("ss_lab",i)]]

    }


    #options(rgl.useNULL = TRUE)
    rgl.bg(color=c('white','white'))

    stack_scatter_rgl(data,coords,base_shape,layer_shape,
                      col.palette=pal,
                      newcolhabs=vals$newcolhabs,
                      pt_cex=cex,
                      spacing=1,
                      expand=input$ss3d_exp,
                      theta=input$ss3d_theta,
                      phi=input$ss3d_phi,
                      r=input$ss3d_eye,
                      d=input$ss3d_d,
                      xlim=c(input$sslong_xmin,input$sslong_xmax),
                      ylim=c(input$sslat_xmin,input$sslat_xmax),
                      col_base=sscol_base,
                      col_layer=sscol_layer,
                      z=zs,
                      zmin=input$ss_zmin,
                      zmax=input$ss_zmax,
                      ticktype=input$ss_ticktype,
                      xlab=input$ss_xlab,
                      ylab=input$ss_ylab,
                      zlab=input$ss_zlab,
                      leglab.adj=input$ss_leglab.pos,
                      legtit.posy=input$ss_legtitle.posy,
                      breaks=input$ss_breaks+1,
                      legwidth=50,
                      legtit.posx=input$ss_legtitle.posx,
                      title.srt=0,
                      lab.srt=0,
                      col.labels=input$col_factor_ss,
                      cex.labels=input$pt_factor_ss,
                      labels=labs,
                      col.coords=input$col_coords_ss,
                      cex.coords=input$pt_coords_ss,
                      show_coords=co,
                      show_labels=showlab,
                      texmipmap=F,texmagfilter="nearest",texminfilter="nearest.mipmap.nearest",texenvmap=F,front="filled", back="points",size=1,point_antialias=F,line_antialias=F,depth_mask=F,lit =F,top=F)




  })
  observeEvent(input$save_map,{
    if(input$saved_maps!='new map'|isTRUE(input$edit_map)){
      updateRadioButtons(session,"hand_save", selected ='over')
    }
    if(input$saved_maps=='new map'){
      updateRadioButtons(session,"hand_save", selected ='create')
    }
  })
  observeEvent(input$surface_map,{
    if(isTRUE(input$surface_map)){
      updateButton(session,"edit_map",value=F)
    }
  })
  observeEvent(input$edit_map,{
    if(isTRUE(input$edit_map)){
      updateButton(session,"surface_map",value=F)
    }
  })
  observeEvent(input$load_savepoint_yes,{
    removeModal()
  })
  observeEvent(input$colored_map,{
    if(isTRUE(input$colored_map)){
      updateCheckboxInput(session,"scalesize_color","Color", F)}
  })
  observeEvent(input$scalesize_color,{
    if(isTRUE(input$scalesize_color)){
      updateCheckboxInput(session,"colored_map",value=F)}
  })
  output$map_part1<-renderUI({
    req(input$saved_maps=="new map"|isTRUE(input$edit_map))

    column(12,
           span(
             inline(pickerInput("data_map",
                                "Y Datalist",
                                choices =    names(vals$saved_data),

                                selected=vals$cur_data, width="200px")),
             inline(
               pickerInput(
                 "choices_map",
                 "Target:",
                 choices = c("Data-Attribute","Factor-Attribute"),
                 selected=vals$cur_choices_map, width="130px")
             ),
             inline( uiOutput("map_get")),
             inline( uiOutput("map_filter1")),
             inline( uiOutput("map_filter2")),
             inline(uiOutput("ss_add"))
           )
    )
  })
  output$ss_add<-renderUI({
    req(isTRUE(input$stack_scatter_3d))
    tags$div(datatitle="Click to include the variable in the stacked scatter map",
             actionButton("add_scatter_stack",icon("fas fa-plus"),style = "animation: glowing 1000ms infinite;")
    )
  })
  cur_map_filter1<-reactiveValues(df="None")
  output$map_filter1<-renderUI({
    req(input$data_map)
    data<-vals$saved_data[[input$data_map]]
    factors<-attr(data,"factors")
    inline(pickerInput("var_map_filter1",label = "Filter",choices = c("None",colnames(factors)), width="100px", selected=cur_map_filter1$df))
  })
  observeEvent(input$var_map_filter1,{
    cur_map_filter1$df<-input$var_map_filter1
  })
  cur_map_filter2<-reactiveValues(df=1)
  output$map_filter2<-renderUI({
    req(input$var_map_filter1!="None")
    data<-vals$saved_data[[input$data_map]]
    factors<-attr(data,"factors")
    fac<-factors[,input$var_map_filter1]
    inline(pickerInput("var_map_filter2",label = "Level",choices = levels(fac), width="100px", selected = cur_map_filter2$df))
  })
  observeEvent(input$var_map_filter2,{
    cur_map_filter2$df<-input$var_map_filter2
  })
  output$map_get<-renderUI({
    choices=  switch(input$choices_map,
                     'Factor-Attribute'=rev(colnames(attr(getdata_map(),"factors"))),
                     'Data-Attribute'=colnames(getdata_map()))
    inline(pickerInput("var_map",label = "select a variable",choices = choices,selected=vals$cur_var_map, width="200px"))
  })
  vars_fac<-reactiveValues(df=0)
  vars_data<-reactiveValues(df=0)
  observe({
    vars_fac$df<-rev(colnames(attr(getdata_map(),"factors")))
    vars_data$df<-colnames(getdata_map())
  })
  observeEvent(input$saved_maps,{
    if(input$saved_maps=="new map"){
      if(isTRUE(reac_stack_order$df)){
        reac_stack_order$df<-F
        updateButton(session,"stack_map",value=T)} else{
          updateButton(session,"stack_map",value=F)
        }

      updateButton(session,"surface_map", value=F)

    }
  })
  observeEvent(input$stack_map,{
    if(input$stack_map %% 2){
      updatePickerInput(session,'saved_maps',choices=c(names(vals$saved_maps)))
    }

  })
  map_new<-reactiveValues(df=F)
  ##map part
  output$map_part2<-renderUI({
    div(
      uiOutput("side_map00"),
      uiOutput("side_map0_stack"),

      column(8,
             column(12,uiOutput("map_options")),
             column(12,  withSpinner(type=8,color="SeaGreen",uiOutput("map_out")))),



    )
  })
  get_mapdisc<-reactive({

    res_list<-lapply(vals$saved_maps,function(x) attr(x,"my_rst"))
    nuls<-which(unlist(lapply(res_list, function (x)
      is.null(x))))
    res_list<-res_list[nuls]
    names(res_list)<-names(vals$saved_maps)[nuls]
    res_list


  })
  output$side_map0_stack<-renderUI({
    req(isTRUE(input$stack_map))
    column(4,id="sidebar_map",class="well2",
           fluidRow(class="map_control_style",
                    column(12,class="well2",
                           fluidRow(
                             p(tipify(icon("fas fa-question-circle",style="color: gray"),"Select the layers to stack", placement = "bottom", options=list(container="body")),'+ Layers:'),
                             div(uiOutput("stack_raster")))),
                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("stack_zlim")
                           )),

                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("srmap_layer")
                           )),
                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("sr_limits")
                           )),
                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("show_map_coords_stack")
                           )),
                    uiOutput("show_map_labels_stack"),
                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("sr_label_options")
                           )),
                    div(tipify(icon("fas fa-question-circle",style="color: gray"),"Click to order the stack layers", placement = "bottom", options=list(container="body")), actionLink("order_stack","+ Order stack")),
                    column(12,
                           fluidRow(class="well2",
                                    div("+ Legend adjustment",
                                        column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Legend width", placement = "bottom", options=list(container="body")),
                                               '+ Width:',inline(numericInput("stack_width.legend",NULL, 40, width="75px",step=1))),
                                        column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Legend y position", placement = "bottom", options=list(container="body")),
                                               '+ Y pos:',inline(numericInput("stack_legadj",NULL, 0, width="75px",step=.1))),
                                        column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Legend bar height", placement = "bottom", options=list(container="body")),
                                               '+ Bar height:',inline(numericInput("stack_legbar.h",NULL, 0.2, width="75px",step=.1))),
                                        column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Adjustment of the titles along the y-axis:", placement = "bottom", options=list(container="body")),
                                               '+ Title pos:',inline(numericInput("stack_legtitle.pos",NULL, 0.3, width="75px",step=.1))),
                                        column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Adjustment of the labels along the y-axis:", placement = "bottom", options=list(container="body")),
                                               '+ Label pos:',inline(numericInput("stack_leglab.pos",NULL, 0.3, width="75px",step=.1)))

                                    ))
                    ),
                    uiOutput("sr_3dcontrol")

           )
    )
  })
  output$sr_3dcontrol<-renderUI({
    column(12,
           fluidRow(class="well2",
                    "+ 3D control",
                    column(12,
                           div(tipify(icon("fas fa-question-circle",style="color: gray"),"a expansion factor applied to the z coordinates. Often used with 0 < expand < 1 to shrink the plotting box in the z direction", placement = "bottom", options=list(container="body")),
                               '+ exp:',
                               inline(numericInput("stack_exp",NULL, 1, width="75px", step=.1))
                           ),
                           div(tipify(icon("fas fa-question-circle",style="color: gray"),"azimuthal direction", placement = "bottom", options=list(container="body")),
                               '+ theta:',
                               inline(numericInput("stack_theta",NULL, 0, width="75px",step=10))
                           ),
                           div(tipify(icon("fas fa-question-circle",style="color: gray"),"colatitude direction", placement = "bottom", options=list(container="body")),
                               '+ phi:',
                               inline(numericInput("stack_phi",NULL, 40, width="75px", step=5))
                           ),
                           div(tipify(icon("fas fa-question-circle",style="color: gray"),"rhe distance of the eyepoint from the centre of the plotting box", placement = "bottom", options=list(container="body")),
                               '+ Eye point:',
                               inline(numericInput("stack_eye",NULL, 1.73, width="75px"))
                           ),
                           div(
                             tipify(icon("fas fa-question-circle",style="color: gray"),"a value which can be used to vary the strength of the perspective transformation. Values of d greater than 1 will lessen the perspective effect and values less and 1 will exaggerate it", placement = "bottom", options=list(container="body")),
                             '+ persp strength:',
                             inline(numericInput("stack_d",NULL, 1, width="50px"))
                           ),
                           div(tipify(icon("fas fa-question-circle",style="color: gray"),"Transparency of the layers", placement = "bottom", options=list(container="body")),
                               '+ Transparency:',
                               inline(numericInput("stack_alpha",NULL, 0, width="75px",step=1, max=1, min=0)))


                    )))
  })
  output$sr_label_options<-renderUI({
    div("+ Label options",
        column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"'simple' draws just an arrow parallel to the axis to indicate direction of increase; 'detailed' draws normal ticks as per 2D plots", placement = "bottom", options=list(container="body")),
               span("+ Ticktype:",
                    pickerInput("sr_ticktype",NULL,choices=c("detailed", "simple"),inline=T, width="75px"))
        ),
        column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"X axis label", placement = "bottom", options=list(container="body")),
               span("+ xlab:",
                    textInput("sr_xlab",NULL,"Longitude", width="100px"))
        ),
        column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Y axis label", placement = "bottom", options=list(container="body")),
               span("+ ylab:",
                    textInput("sr_ylab",NULL,"Latitude", width="100px"))
        ),
        column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Z axis label", placement = "bottom", options=list(container="body")),
               span("+ zlab:",
                    textInput("sr_zlab",NULL,"Layer", width="100px"))
        )
    )
  })
  output$sr_limits<-renderUI({
    base_shape=attr(getdata_map(),"base_shape")
    layer_shape=attr(getdata_map(),"layer_shape")
    limits<-get_limits(limits=NULL,base_shape, layer_shape, coords = attr(getdata_map(),"coords"))

    div(div(span("+ Limits:",style="color: #05668D"),
            div(style="margin-top: -20px;border-bottom: 1px solid SeaGreen",
                div(span("Long",style="margin-left: 70px"),span("Lat",style="margin-left: 60px")),
                div(
                  splitLayout("+ min:",cellWidths = c("50px",'90px',"90px"),
                              numericInput("srlong_xmin",NULL, value=limits[1,1], width="87px", step=0.01),
                              numericInput("srlat_xmin",NULL, limits[1,2], width="87px", step=0.01))),
                div(
                  splitLayout("+ max:",cellWidths = c("50px",'90px',"90px"),
                              numericInput("srlong_xmax",NULL, value=limits[2,1], width="87px", step=0.01),
                              numericInput("srlat_xmax",NULL, limits[2,2], width="87px", step=0.01)))


            )))
  })
  output$srmap_layer<-renderUI({
    req(length(attr(getdata_map(),"layer_shape"))>0)

    column(12,
           fluidRow(
             class="well2",
             span(
               "+",
               checkboxInput("srmap_layer","Layer Shape", T, width="120px"),
               div(
                 span("+ Color:",
                      inline(tags$div(class = "ident-picker", pickerInput(inputId = "srlayer_col",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="40px", selected="gray")))
                 ),
                 span("+ Transp:",
                      numericInput("srlayer_lighten",NULL,value=0.6, width="50px", min=0, max=1, step=0.1)
                 )
               )


             )
           ))
  })
  output$stack_zlim<-renderUI({
    div(tipify(icon("fas fa-question-circle",style="color: gray"),"Z limits", placement = "bottom", options=list(container="body")),
        span("+ zmin:",
             inline(numericInput("stack_zmin",NULL, 1, width="40px",step=1))
        ),
        span("zmax:",
             inline(numericInput("stack_zmax",NULL, length(names(get_stacklist()))+1, width="40px",step=1))
        ))
  })
  output$stack_raster<-renderUI({
    res<-list()
    for( i in 1:length(names(get_stacklist()))){
      res[[i]]<-div(
        names(get_stacklist())[i],
        div(
          span("+ z-value",
               numericInput(paste0("stack_z",i),NULL, i, width="55px",step=0.1)
          )
        ),
        div(
          span("+",
               inline(checkboxInput(paste0("stack_co",i),"coords", T, width="55px"))
          )
        ),
        div(
          span("+",
               inline(checkboxInput(paste0("stack_lab",i),"labels", F, width="55px")),
               inline(
                 conditionalPanel(paste0("input.",paste0("stack_lab",i)," % 2"),{
                   pickerInput(paste0("stack_labels",i),NULL, choices=colnames( attr(getdata_map(),"factors")),options=list(container="body"), width="75px", inline=T)
                 })
               )
          )
        )
      )
    }

    checkboxGroupInput("stack_layers",NULL, choiceValues =names(get_stacklist()),choiceNames=res,selected=names(get_stacklist()), width="75px")
  })
  output$show_map_coords_stack<-renderUI({
    co<-c()
    for(i in 1:length(names(get_stacklist())))
    {co[i]<-input[[paste0("stack_co",i)]]}
    req(any(co))
    div("+ Coords style",
        column(12,' + Color:',
               pickerInput(inputId = "col_coords_stack",
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="75px")),
        column(12,
               '+ Size:',
               inline(numericInput("pt_coords_stack",NULL, 1, width="75px", step=0.1))
        )
    )
  })
  output$show_map_labels_stack<-renderUI({
    showlab<-c()
    for(i in 1:length(names(get_stacklist())))
    {showlab[i]<-input[[paste0("stack_lab",i)]]}

    req(any(showlab))
    column(12,
           fluidRow(
             class="well2",
             div(' + Labels style:',
                 column(12,' + Color:',
                        pickerInput(inputId = "col_factor_stack",
                                    label = NULL,
                                    choices =   vals$colors_img$val[getsolid_col()],
                                    choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="75px")),
                 column(12,
                        '+ Size:',
                        inline(numericInput("pt_factor_stack",NULL, 1, width="75px", step=0.1))
                 )
             )
           ))


  })
  sidebarPanel()
  output$side_map00<-renderUI({
    req(input$saved_maps=="new map"|isTRUE(input$surface_map)|isTRUE(input$mantel_map)|isTRUE(input$edit_map))
    column(4,id="sidebar_map",class="well2", style="margin-bottom: 150px",


           uiOutput("cmap"),
           uiOutput("side_map"),
           uiOutput("side_disc_stack")
    )
  })
  output$side_map<-renderUI({
    if(length(input$stack_scatter_3d)>0){
      req(isFALSE(input$stack_scatter_3d))}
    fluidRow(id="side_map",class="map_control_style",
             fluidRow(
               uiOutput("map_00_palette")),
             uiOutput("map_side01"),
             uiOutput("map_side02"),
             uiOutput('map_axes'),
             uiOutput("map_side04"),
             uiOutput("map_side05")
    )
  })
  output$side_disc_stack<-renderUI({
    req(isTRUE(input$stack_scatter_3d))

    fluidRow(class="map_control_style",

             column(12,
                    fluidRow(class="well2",
                             uiOutput('added_scatter_stack')
                    ))
             ,

             uiOutput('ss_war'),
             uiOutput("ssmap_control")

    )
  })
  output$ssmap_control<-renderUI({
    req(
      length(ss_map$df)>1
    )
    base_shape=attr(getdata_map(),"base_shape")
    layer_shape=attr(getdata_map(),"layer_shape")
    limits<-get_limits(limits=NULL,base_shape, layer_shape, coords = attr(getdata_map(),"coords"))

    div(
      uiOutput('ss_3dcontrol'),
      column(12,
             fluidRow(
               class="well2",
               uiOutput("show_map_coords_ss")
             )),
      column(12,
             fluidRow(
               class="well2",
               uiOutput("show_map_labels_ss")
             )),
      column(12,
             fluidRow(class="well2",
                      uiOutput("ssmap_zlim")
             )),
      column(12,
             fluidRow(class="well2",
                      uiOutput("ssmap_leg")
             )),

      column(12,
             fluidRow(class="well2",
                      uiOutput("ssmap_base")
             )),



      column(12,
             fluidRow(class="well2",
                      uiOutput("ssmap_layer")
             )),


      column(12,
             fluidRow(class="well2",
                      div(span("+ Limits:",style="color: #05668D"),
                          div(style="margin-top: -20px;",
                              div(span("Long",style="margin-left: 70px"),span("Lat",style="margin-left: 60px")),
                              div(
                                splitLayout("+ min:",cellWidths = c("50px",'90px',"90px"),
                                            numericInput("sslong_xmin",NULL, value=limits[1,1], width="87px", step=0.01),
                                            numericInput("sslat_xmin",NULL, limits[1,2], width="87px", step=0.01))),
                              div(
                                splitLayout("+ max:",cellWidths = c("50px",'90px',"90px"),
                                            numericInput("sslong_xmax",NULL, value=limits[2,1], width="87px", step=0.01),
                                            numericInput("sslat_xmax",NULL, limits[2,2], width="87px", step=0.01)))


                          ))
             ))

    )})
  output$ss_3dcontrol<-renderUI({
    column(12,
           fluidRow(class="well2",
                    "+ 3D control",
                    column(12,
                           div(tipify(icon("fas fa-question-circle",style="color: gray"),"a expansion factor applied to the z coordinates. Often used with 0 < expand < 1 to shrink the plotting box in the z direction", placement = "bottom", options=list(container="body")),
                               '+ exp:',
                               inline(numericInput("ss3d_exp",NULL, 1, width="75px", step=.1))
                           ),
                           div(tipify(icon("fas fa-question-circle",style="color: gray"),"azimuthal direction", placement = "bottom", options=list(container="body")),
                               '+ theta:',
                               inline(numericInput("ss3d_theta",NULL, 0, width="75px",step=10))
                           ),
                           div(tipify(icon("fas fa-question-circle",style="color: gray"),"colatitude direction", placement = "bottom", options=list(container="body")),
                               '+ phi:',
                               inline(numericInput("ss3d_phi",NULL, 40, width="75px", step=5))
                           ),
                           div(tipify(icon("fas fa-question-circle",style="color: gray"),"rhe distance of the eyepoint from the centre of the plotting box", placement = "bottom", options=list(container="body")),
                               '+ Eye point:',
                               inline(numericInput("ss3d_eye",NULL, 1.73, width="75px"))
                           ),
                           div(
                             tipify(icon("fas fa-question-circle",style="color: gray"),"a value which can be used to vary the strength of the perspective transformation. Values of d greater than 1 will lessen the perspective effect and values less and 1 will exaggerate it", placement = "bottom", options=list(container="body")),
                             '+ persp strength:',
                             inline(numericInput("ss3d_d",NULL, 1, width="50px"))
                           )
                    )))
  })
  output$ssmap_leg<-renderUI({
    fluidRow(
      column(12,"+ Legend adustment",
             column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Number of breaks for continuous values", placement = "bottom", options=list(container="body")),
                    '+ Breaks:',inline(numericInput("ss_breaks",NULL, 4, width="75px",step=1))),

             column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Adjustment of the titles along the y-axis:", placement = "bottom", options=list(container="body")),
                    '+ Title y-pos:',inline(numericInput("ss_legtitle.posy",NULL, 1.15, width="75px",step=.1))),
             column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Adjustment of the titles along the x-axis:", placement = "bottom", options=list(container="body")),
                    '+ Title x-pos:',inline(numericInput("ss_legtitle.posx",NULL, 1, width="75px",step=.1))),


             column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Adjustment of the labels along the y-axis:", placement = "bottom", options=list(container="body")),
                    '+ Label pos:',inline(numericInput("ss_leglab.pos",NULL, 0.85, width="75px",step=.1)))

      ))
  })
  output$ssmap_zlim<-renderUI({
    zs<-c()
    for(i in 1:length(ss_map$df))
    {
      zs[i]<-input[[paste0("ss_z",i)]]

    }

    fluidRow(
      column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Z limits", placement = "bottom", options=list(container="body")),
             span("+ zmin:",
                  inline(numericInput("ss_zmin",NULL, min(zs), width="40px",step=1))
             ),
             span("zmax:",
                  inline(numericInput("ss_zmax",NULL, value=max(zs)*1.5, width="35px", step=0.01))
             )),
      column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"'simple' draws just an arrow parallel to the axis to indicate direction of increase; 'detailed' draws normal ticks as per 2D plots", placement = "bottom", options=list(container="body")),
             span("+ Ticktype:",
                  pickerInput("ss_ticktype",NULL,choices=c("detailed", "simple"),inline=T, width="75px"))
      ),
      column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"X axis label", placement = "bottom", options=list(container="body")),
             span("+ xlab:",
                  textInput("ss_xlab",NULL,"Longitude", width="100px"))
      ),
      column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Y axis label", placement = "bottom", options=list(container="body")),
             span("+ ylab:",
                  textInput("ss_ylab",NULL,"Latitude", width="100px"))
      ),
      column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Z axis label", placement = "bottom", options=list(container="body")),
             span("+ zlab:",
                  textInput("ss_zlab",NULL,"Layer", width="100px"))
      )
    )
  })
  output$show_map_coords_ss<-renderUI({
    co<-c()
    for(i in 1:length(ss_map$df))
    {co[i]<-input[[paste0("ss_co",i)]]}
    req(any(co))
    div("+ Coords style",
        column(12,' + Color:',
               pickerInput(inputId = "col_coords_ss",
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="75px")),
        column(12,
               '+ Size:',
               inline(numericInput("pt_coords_ss",NULL, 1, width="75px", step=0.1))
        )
    )
  })
  output$show_map_labels_ss<-renderUI({
    showlab<-c()
    for(i in 1:length(ss_map$df))
    {showlab[i]<-input[[paste0("ss_lab",i)]]}

    req(any(showlab))
    column(12,
           fluidRow(
             class="well2",
             div(' + Labels style:',
                 column(12,' + Color:',
                        pickerInput(inputId = "col_factor_ss",
                                    label = NULL,
                                    choices =   vals$colors_img$val[getsolid_col()],
                                    choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="75px")),
                 column(12,
                        '+ Size:',
                        inline(numericInput("pt_factor_ss",NULL, 1, width="75px", step=0.1))
                 )
             )
           ))


  })
  added_ss3d<-reactiveValues(df=NULL)
  ss1<-reactiveValues(df=NULL)
  output$added_scatter_stack<-renderUI({
    req(!is.null( ss_map$df))
    lapply(ss_map$df, function(x){
      name<-attr(x,"name")
      observeEvent(input[[paste0("ss_del",name)]],{
        ss_map$df[name]<-NULL
      })
    })
    div(
      added_ss3d$df)
  })
  observeEvent(ss_map$df, {
    delay(10,
          added_ss3d$df<- added_ss3d$df[names(ss_map$df)]

    )
  })
  ss_map<-reactiveValues(df=NULL)
  bag_name_ss<-reactive({
    bag<-1
    name0<-input$var_map
    name1<-paste(name0,bag)
    if(name1%in%names(ss_map$df))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(ss_map$df)) break
      }

    }
    paste(name0,bag)
  })
  ss3d_add<-reactive({
    if(input$choices_map=="Data-Attribute"){
      ss<-getdata_map()[filtermap(),input$var_map, drop=F]
    } else{
      ss<-attr(getdata_map(),"factors")[filtermap(),input$var_map, drop=F]
    }



    coords<-attr(getdata_map(),"coords")
    attr(ss,"name")<-bag_name_ss()
    ss_map$df[[bag_name_ss()]] <- ss
    attr(ss_map$df,"coords")<-coords
    attr(ss_map$df,"base_shape")<-attr(getdata_map(),"base_shape")
    attr(ss_map$df,"layer_shape")<-attr(getdata_map(),"layer_shape")
  })
  observeEvent(input$add_scatter_stack,{
    ss3d_add()

  } )
  observeEvent(input$add_scatter_stack,{
    lapply(ss_map$df,function(x){
      i=which(names(ss_map$df)%in%attr(x,"name"))
      added_ss3d$df[[attr(x,"name")]]<-list(
        div(
          span("+",attr(x,"name")),
          column(12,span("+ Palette:",
                         pickerInput(inputId = paste0("pt_palette",i),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),inline=T, width="55px")
          )),
          column(12,span("+ Size:",
                         numericInput(paste0("ss_cex",i),NULL, 1, width="35px",step=0.1)
          )),
          column(12,span("+ Z-value:",
                         numericInput(paste0("ss_z",i),NULL, i, width="35px",step=0.1)
          )),
          column(12,
                 span("+",
                      inline(checkboxInput(paste0("ss_co",i),"coords", T, width="55px"))
                 )
          ),
          column(12,
                 span("+",
                      inline(checkboxInput(paste0("ss_lab",i),"labels", F, width="55px")),
                      inline(
                        conditionalPanel(paste0("input.",paste0("ss_lab",i)," % 2"),{
                          pickerInput(paste0("ss_labels",i),NULL, choices=colnames( attr(getdata_map(),"factors")),options=list(container="body"), width="75px", inline=T)
                        })
                      )
                 )),

          actionLink(paste0("ss_del",attr(x,"name")),icon("far fa-trash-alt"))

        )
      )
    }

    )
  })
  output$ssmap_base<-renderUI({

    req(!is.null(attr(getdata_map(),"base_shape")))
    column(12,
           fluidRow(
             class="well2",
             span(
               "+",
               checkboxInput("ssmap_base","Base Shape", T, width="120px"),
               div(
                 span("+ Color:",
                      inline(tags$div(class = "ident-picker", pickerInput(inputId = "ssbase_col",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="40px", selected="white")))
                 ),
                 span("+ Transp:",
                      numericInput("ssbase_lighten",NULL,value=0.6, width="50px", min=0, max=1, step=0.1)
                 )
               )
             )
           ))
  })
  output$map_extra_layer<-renderUI({
    shp<-attr(vals$saved_data[[input$data_map]],"extra_shape")
    shapes<-names(shp)

    req(length(shapes)>0)

    res<-list()


    for(i in 1:length(shapes)){
      atributos_shp<-attributes(shp[[i]])$names

      res[[i]]<-list(
        span(
          span(
            "+",
            checkboxInput(paste0("map_extra_layer",i),paste("Extra-Layer",i), T, width="120px")
          ),
          div(
            span("+ Color:",
                 inline(tags$div(
                   class = "ident-picker",
                   pickerInput(inputId = paste0("ssextra_layer_col",i),label = NULL,choices =   vals$colors_img$val,choicesOpt = list(content =   vals$colors_img$img),options=list(container="body"),inline=F, width="40px", selected="gray")
                 ))
            ),
            span("+ Transp:",
                 numericInput(paste0("ssextra_layer_lighten",i),NULL,value=0.6, width="50px", min=0, max=1, step=0.1)
            )


          ),
          div(
            span("+", paste("Label",i), inline(
              pickerInput(paste0("feat_extra",i),NULL,choices=c("None",atributos_shp),inline=T)
            ) ),
            span("+ size",numericInput(paste0("feat_extra_size",i),NULL,value=2, width="50px", min=0, max=1, step=0.1))
          )
        )
      )
    }

    column(12,
           fluidRow(
             class="well2",
             res,
             inline(uiOutput("data_depth")),
             div(
               span("- Remove layer", inline(
                 pickerInput("remove_extra",NULL,choices=c(shapes),inline=T)),
                 actionLink("del_extra",icon("far fa-trash-alt")))
             )
           ))
  })
  output$data_depth<-renderUI({
    req(input$cmap=="discrete")
    shp<-attr(vals$saved_data[[input$data_map]],"extra_shape")
    span("+ Data depth ",
         numericInput("data_depth",NULL,value=3+(length(shp)*2), width="50px", min=1, step=1)
    )
  })
  observeEvent(input$del_extra,{
    attr(vals$saved_data[[input$data_map]],"extra_shape")[[input$remove_extra]]<-NULL
  })
  output$ssmap_layer<-renderUI({
    req(length(attr(getdata_map(),"layer_shape"))>0)

    column(12,
           fluidRow(
             class="well2",
             span(
               "+",
               checkboxInput("ssmap_layer","Layer Shape", T, width="120px"),
               div(
                 span("+ Color:",
                      inline(tags$div(class = "ident-picker", pickerInput(inputId = "sslayer_col",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="40px", selected="gray")))
                 ),
                 span("+ Transp:",
                      numericInput("sslayer_lighten",NULL,value=0.6, width="50px", min=0, max=1, step=0.1)
                 )
               )


             )
           ))
  })
  ss3d_out<-reactiveValues(df=0)
  output$ss3d<-renderUI({

    req(isTRUE(input$stack_scatter_3d))
    validate(need(length(ss_map$df)>1,"Add at least two variables to generate the plot"))



    data=ss_map$df
    coords=  attr(ss_map$df,"coords")
    base_shape=if(isTRUE(input$ssmap_base)){
      attr(data,"base_shape") } else{ NULL}
    layer_shape=if(isTRUE(input$ssmap_layer)){
      attr(data,"layer_shape") } else{ NULL}
    sscol_layer=adjustcolor(getcolhabs(vals$newcolhabs,input$sslayer_col,1), input$sslayer_lighten)
    sscol_base=adjustcolor(getcolhabs(vals$newcolhabs,input$ssbase_col,1), input$ssbase_lighten)

    pal<-c()
    cex<-c()
    zs<-c()
    co<-c()
    labs<-list()
    showlab<-c()
    for(i in 1:length(ss_map$df))
    {
      pal[i]<-input[[paste0("pt_palette",i)]]
      cex[i]<-input[[paste0("ss_cex",i)]]
      zs[i]<-input[[paste0("ss_z",i)]]
      labs[[i]]<-attr(getdata_map(),"factors")[rownames(coords), input[[paste0("ss_labels",i)]]]
      co[i]<-input[[paste0("ss_co",i)]]
      showlab[i]<-input[[paste0("ss_lab",i)]]
    }

    #data<-do.call(cbind,data)
    div(


      renderPlot({
        stack_scatter3D(data,coords,base_shape,layer_shape,
                        col.palette=pal,
                        newcolhabs=vals$newcolhabs,
                        pt_cex=cex,
                        spacing=1,
                        expand=input$ss3d_exp,
                        theta=input$ss3d_theta,
                        phi=input$ss3d_phi,
                        r=input$ss3d_eye,
                        d=input$ss3d_d,
                        xlim=c(input$sslong_xmin,input$sslong_xmax),
                        ylim=c(input$sslat_xmin,input$sslat_xmax),
                        col_base=sscol_base,
                        col_layer=sscol_layer,
                        z=zs,
                        zmin=input$ss_zmin,
                        zmax=input$ss_zmax,
                        ticktype=input$ss_ticktype,
                        xlab=input$ss_xlab,
                        ylab=input$ss_ylab,
                        zlab=input$ss_zlab,
                        leglab.adj=input$ss_leglab.pos,
                        legtit.posy=input$ss_legtitle.posy,
                        breaks=input$ss_breaks+1,
                        legwidth=50,
                        legtit.posx=input$ss_legtitle.posx,
                        title.srt=0,
                        lab.srt=0,
                        col.labels=input$col_factor_ss,
                        cex.labels=input$pt_factor_ss,
                        labels=labs,
                        col.coords=input$col_coords_ss,
                        cex.coords=input$pt_coords_ss,
                        show_coords=co,
                        show_labels=showlab)
        ss3d_out$df<-recordPlot()
      })
    )

  })
  output$cmap<-renderUI({
    req(!isTRUE(input$mantel_map))
    req(!isTRUE(input$surface_map))
    fluidRow(
      radioGroupButtons(
        "cmap", choiceNames = list(
          tipify(span(icon("fas fa-map-marker-alt"),"Discrete"),"Discrete"),
          tipify(span(icon("fas fa-route"),"Interpolation"),"Interpolation"),
          tipify(span(icon("fas fa-th"),"Raster"), "Create a Raster using the target data and its coordinates without model")
        ),choiceValues =list("discrete","interpolation","raster"), status="button_active",justified =T, selected=vals$cur_cmap)
    )
  })
  output$map_00_palette<-renderUI({
    req(!isTRUE(input$mantel_map))
    column(12,
           div(
             class="well2",style="padding-top:1px;
           padding-bottom:1px;
          ",
          "+ Data palette:",pickerInput(inputId = "pt_palette",label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),inline=T, width="75px"))
    )
  })
  output$map_side01 <- renderUI({
    req(!isTRUE(input$surface_map))
    req(!isTRUE(input$mantel_map))
    fluidRow(id="map_side01",


             uiOutput("map_1b_discrete"),
             column(12,uiOutput("scatter_colorby")),
             conditionalPanel("input.cmap=='interpolation'",{
               column(12,
                      div(class="well2",
                          fluidRow(
                            column(12,"+ Model"),
                            column(12,

                                   uiOutput("map_1c_grid"),
                                   uiOutput("map_1d_idw"),
                                   uiOutput("map_1d_knn")
                            )
                          )
                      ))
             }),
             column(12, uiOutput('map_coords')),
             column(12,uiOutput('map_labels')),
             uiOutput("map_1a_base"),
             column(12,uiOutput("map_extra_layer")),
             uiOutput("map_1f_layer"),
             uiOutput("map_1e_limits")



    )
  })
  observeEvent(input$cmap,{
    if(input$cmap=="interpolation"|input$cmap=="raster"){
      updateButton(session,'scatter_3d', value=F)
      updateButton(session,'stack_scatter_3d', value=F)
    }
  })
  output$map_side02<-renderUI({
    req(isTRUE(input$surface_map)|isTRUE(input$scatter_3d))
    req(!isTRUE(input$mantel_map))
    if(input$cmap=="interpolation"|input$cmap=="raster"){
      req(input$saved_maps!="new map")} else{
        req(isTRUE(input$scatter_3d))
      }
    fluidRow(
      column(12,
             span("+ z-colors:",style="vertical-align: top;",
                  uiOutput("show_z_colors", inline = T)),

             column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"a expansion factor applied to the z coordinates. Often used with 0 < expand < 1 to shrink the plotting box in the z direction", placement = "bottom", options=list(container="body")),
                    '+ exp:',
                    inline(numericInput("surf_exp",NULL, 0.3, width="75px", step=.1))
             ),
             column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"azimuthal direction", placement = "bottom", options=list(container="body")),
                    '+ theta:',
                    inline(numericInput("surf_theta",NULL, 0, width="75px",step=10))
             ),
             column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"colatitude direction", placement = "bottom", options=list(container="body")),
                    '+ phi:',
                    inline(numericInput("surf_phi",NULL, 40, width="75px", step=5))
             ),
             column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"rhe distance of the eyepoint from the centre of the plotting box", placement = "bottom", options=list(container="body")),
                    '+ Eye point:',
                    inline(numericInput("surf_r",NULL, 1.73, width="75px"))
             ),
             column(12,
                    tipify(icon("fas fa-question-circle",style="color: gray"),"a value which can be used to vary the strength of the perspective transformation. Values of d greater than 1 will lessen the perspective effect and values less and 1 will exaggerate it", placement = "bottom", options=list(container="body")),
                    '+ persp strength:',
                    inline(numericInput("surf_d",NULL, 1, width="50px"))
             ),
             uiOutput("leg_surface")


      )
    )


  })
  output$leg_surface<-renderUI({
    req(isTRUE(input$surface_map))
    div(
      column(12,
             '+ Leg width:',
             inline(numericInput("surface_width.legend",NULL, 40, width="75px",step=1))),
      column(12,
             '+ Leg height:',
             inline(numericInput("surface_height.legend",NULL, 20, width="75px",step=1)))
    )
  })
  output$map_labels<-renderUI({
    req(!isTRUE(input$mantel_map))
    div(class="well2",
        fluidRow(
          column(12,

                 span("+",checkboxInput("showfactors","Labels", F, width="75px")),uiOutput("show_map_labels")
          )
        )
    )

  })
  output$map_coords<-renderUI({
    req(!isTRUE(input$mantel_map))
    div(class="well2",
        fluidRow(
          column(12,

                 span("+",checkboxInput("showcoords","Coords", F, width="75px")),uiOutput("show_map_coords")
          )
        )
    )
  })
  output$map_axes<-  renderUI({
    div(class="well2",
        fluidRow(
          column(12,span("+",checkboxInput("showguides","Guides", F, width="75px"))),

          uiOutput("show_guides_options"),

          uiOutput("show_map_axes")
        )
    )
  })
  lines_vals<-c("solid","dashed","dotted")
  dflines<-data.frame(vals=1:3)
  for(i in  1:length(lines_vals))  {
    dflines$img[i]<- sprintf(paste(
      div(style=paste('border-top: 1px',lines_vals[i],'SeaGreen;',"padding: 2px; color: black"))))}
  output$show_guides_options<-renderUI({
    req(isTRUE(input$surface_map))
    column(12,
           column(12,' + Color:',
                  pickerInput(inputId = "col_guides",
                              label = NULL,
                              choices =   vals$colors_img$val[getsolid_col()],
                              choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="75px")),
           column(12,
                  '+ line:',
                  inline(
                    pickerInput("lty_guides",NULL, choices = dflines$val,
                                choicesOpt=list(content = dflines$img), width="75px",
                    )
                  )
           ),
           column(12,
                  '+ Width:',
                  inline(numericInput("lwd_guides",NULL, 1, width="75px",step=0.5))
           )

    )
  })
  output$show_map_axes<-renderUI({
    #req(!isTRUE(input$surface_map))
    div(



      column(12,' +	Axes:',
             column(12,
                    '+ Size:',
                    inline(numericInput("pt_legend",NULL, 12, width="75px"))
             )),
      column(12,' +	Title:',
             inline(textInput("map_title",NULL,input$var_map, placeholder = "Title", width="150px"))),
      column(12,' +	Legend:',
             column(12,
                    '+ label',
                    inline(textInput("map_legend",NULL,input$var_map, placeholder = "legend", width="150px"))
             ),
             column(12,uiOutput("breaks_map")))
    )
  })
  output$breaks_map<-renderUI({
    req(input$cmap=="discrete")
    req(input$choices_map=="Data-Attribute")
    req(input$var_map)
    vector<-getdata_map()[filtermap(),input$var_map]
    my<-pretty(pretty(vector))
    my<-my[which(my>min(vector))]
    my<-my[which(my<max(vector))]
    div(

      tipify(icon("fas fa-question-circle",style="color: gray"),"Enter a vector of breaks (comma delimited, within the data range)", options=list(container="body")),
      "+ breaks",
      textInput('breaks_map', NULL, paste(my, collapse = ", "), width="200px")
    )
  })
  output$map_side05<-renderUI({
    req(isTRUE(input$mantel_map))
    column(12,class="map_control_style",
           p(strong("Mantel Correlogram"),tipify(icon("fas fa-question-circle",style="color: gray"),"Estimates the spatial dependence at discrete distance classes. The analysis uses progressive correction of multiple-testing, as described in Legendre and Legendre (1998, p. 721). The p-values are corrected by the method holm.", placement = "bottom", options=list(container="body"))),
           style="margin-left: 0px;",
           column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Number of classes. If n.class=0, the Sturges equation will be used.", placement = "bottom", options=list(container="body")),
                  ' + nclass:',
                  numericInput("mantel_nclass",label=NULL,value=0,width="75px")
           ),
           column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"For the second half of the distance classes, cutoff = TRUE limits the correlogram to the distance classes that include all points. If cutoff = FALSE, the correlogram includes all distance classes.", placement = "bottom", options=list(container="body")),
                  ' + cutoff:',
                  pickerInput("mantel_cutoff",label=NULL,choices=c("true", "false"),width="75px")
           ),
           column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Type of correlation in calculation of the Mantel statistic", options=list(container="body")),
                  ' + r.type:',
                  pickerInput("mantel_r.type",label=NULL,choices=c("pearson", "spearman","kendall"),width="75px")
           ),
           column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Number of permutations for the tests of significance. Default: nperm=999. For large data files, permutation tests are rather slow.", placement = "bottom", options=list(container="body")),
                  ' + nperm:',
                  numericInput("mantel_nperm",label=NULL,value=99,width="75px")
           ),
           column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"Significance level for the points drawn with black symbols in the correlogram", placement = "bottom", options=list(container="body")),
                  ' + alpha:',
                  numericInput("mantel_alpha",label=NULL,value=0.05,width="75px", min=0, max=1)
           ),
           inline(radioButtons("spatial_stats","Show", choiceValues =list("plot",'mantel.res','break.pts'),choiceNames =list(
             span("Plot"),
             tipify(span('Results'), "
A table with the distance classes as rows and the class indices, number of distances per class, Mantel statistics, and p-values as columns. A positive Mantel statistic indicates positive spatial correlation. ", placement = "right"),
tipify(span('Breakpoints'), "The break points computed", placement = "right")



           )))
    )


  })
  output$map_1a_base<-renderUI({

    req(!is.null(attr(getdata_map(),"base_shape")))
    column(12,
           div(
             class="well2",
             span(
               "+",
               checkboxInput("map_1a_base","Base Shape", T, width="120px"),
               inline(
                 conditionalPanel("input.cmap=='discrete'",{
                   span(
                     span("+ Color:",
                          inline(tags$div(class = "ident-picker",pickerInput(inputId = "base_col",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="40px", selected="white")))
                     ),
                     span("+ Transp:",
                          numericInput("base_lighten",NULL,value=0.6, width="50px", min=0, max=1, step=0.1)
                     )
                   )
                 })
               )

             )
           ))
  })
  output$map_1b_discrete<-renderUI({

    req(input$cmap=="discrete")
    #req(isFALSE(input$surface_map)|is.null(input$surface_map))
    column(12,

           div(class="well2",
               " + Points:",
               column(12,' + Shape:',
                      pickerInput(inputId = "pt_symbol",
                                  label = NULL,
                                  choices = df_symbol$val,
                                  choicesOpt = list(content = df_symbol$img), inline=T, width="75px")),

               column(12,
                      '+ Size::',inline(uiOutput("map_pt_range"))),
               if(input$choices_map=='Data-Attribute' & isFALSE(input$scatter_3d)){
                 fluidRow(
                   column(12,
                          span("+",checkboxInput("scale_map","Scale", T, width="75px")),
                          column(12,
                                 uiOutput("scalesize_size"),
                                 uiOutput("scalesize_color")
                          )

                   ),

                   column(12,"+",checkboxInput("colored_map","Color by:", F, width="75px"),
                          uiOutput("mapfac",inline=T))
                 )
               }

           ))


  })
  output$map_pt_range<-renderUI({
    if(input$choices_map=="Data-Attribute"){
      div(
        "min:",
        inline(numericInput("pt_points_min",NULL, 0, width="45px", min=0, step=0.1)),
        "max:",
        inline(numericInput("pt_points",NULL, 1, width="45px"))

      )} else{
        inline(numericInput("pt_points",NULL, 1, width="45px"))
      }
  })
  output$map_1c_grid<-renderUI({

    req(input$cmap=="interpolation")
    #req(isFALSE(input$surface_map)|is.null(input$surface_map))

    column(12," + Grid",
           column(12,tipify(icon("fas fa-question-circle",style="color: gray")," sample size for sampling point locations within the map area", placement = "bottom", options=list(container="body")),
                  ' + resolution:',
                  inline(uiOutput("res_map"))
           )


    )


  })
  res_map_cur<-reactiveValues(df=20000)
  output$res_map<-renderUI({
    numericInput(
      "res_map", NULL, res_map_cur$df, min = 1, width="75px"
    )
  })
  observeEvent(input$res_map,{
    res_map_cur$df<-input$res_map
  })
  output$map_1d_idw<-renderUI({
    req(input$cmap=="interpolation")
    req(input$choices_map=="Data-Attribute")
    #req(isFALSE(input$surface_map)|is.null(input$surface_map))
    column(12," + IDW ",
           column(12, uiOutput("interp_inputs")))
  })
  output$map_1f_layer<-renderUI({
    req(!isTRUE(input$surface_map))
    req(!isTRUE(input$mantel_map))
    req(input$saved_maps=="new map")
    req(length(attr(getdata_map(),"layer_shape"))>0)

    column(12,
           div(
             class="well2",
             span(
               "+",
               checkboxInput("map_1f_layer","Layer Shape", T, width="120px"),
               div(
                 span("+ Color:",
                      inline(tags$div(class = "ident-picker", pickerInput(inputId = "layer_col",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="40px", selected="gray")))
                 ),
                 span("+ Transp:",
                      numericInput("layer_lighten",NULL,value=0.6, width="50px", min=0, max=1, step=0.1)
                 )
               )


             )
           ))
  })
  output$map_1e_limits<-renderUI({
    base_shape=attr(getdata_map(),"base_shape")
    layer_shape=attr(getdata_map(),"layer_shape")
    limits<-get_limits(limits=NULL,base_shape, layer_shape, coords = attr(getdata_map(),"coords"))
    column(12,
           div(class="well2",

               span("+ Limits:",style="color: #05668D"),
               div(style="margin-top: -20px;",
                   div(span("Long",style="margin-left: 70px"),span("Lat",style="margin-left: 60px")),
                   div(
                     splitLayout("+ min:",cellWidths = c("50px",'90px',"90px"),
                                 numericInput("long_xmin",NULL, value=limits[1,1], width="87px", step=0.01),
                                 numericInput("lat_xmin",NULL, limits[1,2], width="87px", step=0.01))),
                   div(
                     splitLayout("+ max:",cellWidths = c("50px",'90px',"90px"),
                                 numericInput("long_xmax",NULL, value=limits[2,1], width="87px", step=0.01),
                                 numericInput("lat_xmax",NULL, limits[2,2], width="87px", step=0.01)))


               )
           ))
  })
  output$show_z_coords<-renderUI({
    req(isTRUE(input$surface_map))
    my_rst<-attr(vals$saved_maps[[input$saved_maps]],'my_rst')

    z<-max(my_rst@data@values, na.rm=T)
    column(12,
           span("+ z: ",numericInput("z_coords",NULL, z, width="75px")))

  })
  output$show_z_colors<-renderUI({
    req(length(names(vals$saved_maps))>0)
    inline(pickerInput("saved_maps2",NULL, choices=names(vals$saved_maps),options=list(container="body"), width="130px"))  })
  output$show_map_coords<-renderUI({
    req(isTRUE(input$showcoords))
    column(12,
           column(12,' + Color:',
                  pickerInput(inputId = "col_coords",
                              label = NULL,
                              choices =   vals$colors_img$val[getsolid_col()],
                              choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="75px")),
           column(12,
                  '+ Size:',
                  inline(numericInput("pt_coords",NULL, 1, width="75px"))
           ),
           uiOutput("show_z_coords")
    )
  })
  output$show_map_labels<-renderUI({
    req(isTRUE(input$showfactors))
    column(12,
           column(12,' + Factor:',
                  pickerInput("labels_coords",NULL, choices=colnames( attr(getdata_map(),"factors")),options=list(container="body"), width="75px", inline=T)),
           #

           column(12,' + Color:',
                  pickerInput(inputId = "col_factor",
                              label = NULL,
                              choices =   vals$colors_img$val[getsolid_col()],
                              choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="75px")),
           column(12,
                  '+ Size:',
                  inline(numericInput("pt_factor",NULL, 1, width="75px"))
           ),
           if(isTRUE(input$surface_map)){my_rst<-attr(vals$saved_maps[[input$saved_maps]],'my_rst')
           z<-max(my_rst@data@values, na.rm=T)
           column(12, span("+",numericInput("z_labels",NULL, z, width="75px")))
           }
    )
  })
  output$scalesize_size<-renderUI({
    req(isTRUE(input$scale_map))
    column(12,"+",checkboxInput("scalesize_size","Size", T, width="75px"))
  })
  output$scalesize_color<-renderUI({
    req(isTRUE(input$scale_map))
    column(12,"+",checkboxInput("scalesize_color","Color", T, width="75px")
    )
  })
  output$interp_inputs<-renderUI({
    req(input$choices_map=="Data-Attribute")
    fluidRow(
      column(12, tipify(icon("fas fa-question-circle",style="color: gray"),"for local kriging, the number of nearest observations that should be used for a kriging prediction or simulation, where nearest is defined in terms of the space of the spatial locations. By default (empty), all observations are used", placement = "bottom", options=list(container="body")),"+ nmax:",
             numericInput(
               "nmax_idp", NULL, NA, min = 0, width="75px"
             )
      ),
      column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"for local kriging, if the number of nearest observations within distance maxdist is less than nmin, a missing value will be generated- see maxdist", placement = "bottom", options=list(container="body")),"+ nmin:",
             numericInput(
               "nmin_idp", NULL, 3, min = 0, width="75px"
             )
      ),
      column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"maximum number of observations to select per octant (3D) or quadrant (2D); only relevant if maxdist has been defined as well", placement = "bottom", options=list(container="body")),"+ omax:",
             numericInput(
               "omax", NULL, 0, min = 0, width="75px"
             )
      ),
      column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"for local kriging, only observations within a distance of maxdist from the prediction location are used for prediction; if combined with nmax, both criteria apply", placement = "bottom", options=list(container="body")),"+ maxdist:",
             numericInput(
               "maxdist", NULL, NA, min = 0, width="75px"
             )
      ),
      column(12,tipify(icon("fas fa-question-circle",style="color: gray"),"numeric; specify the inverse distance weighting power", placement = "bottom", options=list(container="body")),
             "+ idp:",
             numericInput(
               "idp", NULL, 4, min = 0, width="75px"
             )
      )
    )
  })
  output$map_1d_knn<-renderUI({
    req(input$choices_map=="Factor-Attribute")
    req(input$cmap=="interpolation")
    column(12," + KNN ",
           column(12, tipify(icon("fas fa-question-circle", style="color: gray"), "number of neighbours considered.", options=list(containder="body")),
                  "+ k:",
                  numericInput(
                    "nmax", NULL, 4, min = 0, width="75px"
                  )
           ))


  })
  output$mapfac<-renderUI({
    req(input$colored_map %% 2)
    pickerInput("map_lab",NULL, choices=colnames(attr(getdata_map(),"factors")),options=list(container="body"), width="75px", inline=T)})
  observeEvent(input$mantel_map,{
    if(isTRUE(input$mantel_map)){
      updateButton(session,"edit_map", value=F)
      updateButton(session,"surface_map", value=F)
      updateButton(session,"stack_map", value=F)
    }
  })
  observeEvent(input$edit_map,{
    if(isTRUE(input$edit_map)){
      updateButton(session,"mantel_map", value=F)
      updateButton(session,"surface_map", value=F)
      updateButton(session,"stack_map", value=F)
    }
  })
  observeEvent(input$surface_map,{
    if(isTRUE(input$surface_map)){
      updateButton(session,"mantel_map", value=F)
      updateButton(session,"edit_map", value=F)
      updateButton(session,"stack_map", value=F)
    }
  })
  observeEvent(input$stack_map,{
    if(isTRUE(input$stack_map)){
      updateButton(session,"mantel_map", value=F)
      updateButton(session,"edit_map", value=F)
      updateButton(session,"surface_map", value=F)
    }
  })
  output$map_options<-renderUI({
    fluidRow(
      inline(div(style="margin-top:10px",uiOutput("add_map", inline=T))),

      inline(div(style="margin-top:10px",uiOutput("show_saved_maps", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("mantel_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("edit_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("scatter_3d", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("stack_scatter_3d", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("save_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("surface_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("rgl_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("ss_rgl", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("sr_rgl", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("stack_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("trash_map", inline=T))),
      bsButton('downp_map',tipify(icon("fas fa-download"),"Download plot"), style="button_active"),
      inline(div(style="margin-top:10px",uiOutput("stop_map", inline=T))),

    )
  })
  automap_war<-reactive({
    renderUI({
      div(em(icon("fas fa-hand-point-left"),"Click to apply input changes"))
    })
  })
  disc_data_map<-reactive({
    if(isTRUE(input$automap)){
      output$automap_war<-renderUI({NULL})
      vals$map_res<-map_data_disc()
    } else {
      if(isFALSE(stopmap$df)){
        stopmap$df<-T
        output$automap_war<-renderUI({NULL})
        vals$map_res<-map_data_disc()
      }else{
        if(isFALSE(stopbigmap$df)){
          output$automap_war<-automap_war()
          vals$map_res<-isolate(map_data_disc())}
      }
    }
  })
  interp_data_map<-reactive({
    if(isTRUE(input$automap)){
      output$automap_war<-renderUI({NULL})
      vals$map_res<-map_data_interp()
    } else {
      if(isFALSE(stopmap$df)){
        stopmap$df<-T
        output$automap_war<-renderUI({NULL})
        vals$map_res<-map_data_interp()
      }else{
        if(isFALSE(stopbigmap$df)){
          output$automap_war<-automap_war()
          vals$map_res<-isolate(map_data_interp())}
      }
    }
  })
  raster_map<-reactive({
    if(isTRUE(input$automap)){
      output$automap_war<-renderUI({NULL})
      vals$map_res<-map_raster()
    } else {
      if(isFALSE(stopmap$df)){
        stopmap$df<-T
        output$automap_war<-renderUI({NULL})
        vals$map_res<-map_raster()
      }else{
        if(isFALSE(stopbigmap$df)){
          output$automap_war<-automap_war()
          vals$map_res<-isolate(map_raster())}
      }
    }
  })
  disc_fac_map<-reactive({
    if(isTRUE(input$automap)){
      output$automap_war<-renderUI({NULL})
      vals$map_res<-map_fac_disc()
    } else {
      if(isFALSE(stopmap$df)){
        stopmap$df<-T
        output$automap_war<-renderUI({NULL})
        vals$map_res<-map_fac_disc()
      }else{
        if(isFALSE(stopbigmap$df)){
          output$automap_war<-automap_war()
          vals$map_res<-isolate(map_fac_disc())}
      }
    }
  })
  interp_fac_map<-reactive({
    if(isTRUE(input$automap)){
      output$automap_war<-renderUI({NULL})
      vals$map_res<-map_fac_interp()
    } else {
      if(isFALSE(stopmap$df)){
        stopmap$df<-T
        output$automap_war<-renderUI({NULL})
        vals$map_res<-map_fac_interp()
      }else{
        if(isFALSE(stopbigmap$df)){
          output$automap_war<-automap_war()
          vals$map_res<-isolate(map_fac_interp())}
      }
    }
  })
  output$map_out<-renderUI({
    req(input$choices_map)
    req(input$cmap)
    req(isFALSE(stopbigmap$df))


    if(input$choices_map=="Data-Attribute"){
      if(input$cmap=='discrete'){
        disc_data_map()
      }
      if(input$cmap=='interpolation'){interp_data_map()}
      if(input$cmap=='raster'){
        validate(need(input$choices_map=="Data-Attribute","This functionality is currently only available for Data-Attribute"))
        vals$map_res<-raster_map()}
    }
    if(input$choices_map=='Factor-Attribute'){
      if(input$cmap=='discrete'){disc_fac_map()}
      if(input$cmap=='interpolation'){interp_fac_map()}
    }


    if(input$cmap=='raster'){
      validate(need(input$choices_map=="Data-Attribute","Raster tool currently only available for the Data-Attribute"))}
    fluidRow(
      uiOutput('map_out1'),
      uiOutput("scatter_out"),
      uiOutput("ss3d"),
      uiOutput("map_ssrgl"),
      uiOutput("map_srrgl"),
      uiOutput("map_out2"),
      uiOutput("map05"))
  })
  output$map_out1<-renderUI({
    if(length(input$stack_scatter_3d)>0){
      req(isFALSE(input$stack_scatter_3d))}
    req(!isTRUE(input$scatter_3d))
    req(!isTRUE(input$mantel_map))
    req(input$saved_maps=="new map"|isTRUE(input$edit_map))
    fluidRow(
      conditionalPanel(
        "input.cmap=='interpolation'& input.choices_map=='Data-Attribute'",{
          div(renderPlot(vals$map_data_interp))
        }),

      conditionalPanel(
        "input.cmap=='interpolation'& input.choices_map=='Factor-Attribute'",{
          div(renderPlot(vals$map_fac_interp))
        }),

      conditionalPanel(
        "input.cmap=='discrete'& input.choices_map=='Data-Attribute'",{
          div(renderPlot(suppressWarnings(print(vals$map_data_disc))))
        }),

      conditionalPanel(
        "input.cmap=='discrete'& input.choices_map=='Factor-Attribute'",{
          div(renderPlot(vals$map_fac_disc))
        }),
      conditionalPanel(
        "input.cmap=='raster'& input.choices_map=='Data-Attribute'",{
          div(

            renderPlot(vals$map_data_raster))
        })
    )
  })
  output$scatter_colorby<-renderUI({
    req(isTRUE(input$scatter_3d))
    span("+",
         checkboxInput("scatter_4D"," 4D::", F, width="50px"),inline(uiOutput("colorzby")),
         uiOutput("scatter_4D"))


  })
  output$colorzby<-renderUI({
    req(isTRUE(input$scatter_4D))
    em(a("color z by:"))
  })
  output$scatter_4D<-renderUI({
    selected<-if(input$choices_map=="Data-Attribute"){
      "Variable"
    } else{'Factor' }
    req(isTRUE(input$scatter_4D))
    column(12,"Datalist::",
           inline(pickerInput("scatter_datalistz",NULL, choices=names(vals$saved_data),options=list(container="body"), width="125px", inline=T,
                              selected=input$data_map)),
           span(pickerInput("scatter_colorby",NULL, choices=c("Variable","Factor"),options=list(container="body"), width="75px", inline=T, selected=selected), "::",inline(uiOutput("scatter_colorz"))),
    )
  })
  output$scatter_colorz<-renderUI({
    data<-vals$saved_data[[input$scatter_datalistz]]
    choices<-if(input$scatter_colorby=="Variable"){colnames(data)} else{
      colnames(attr(data,"factors"))
    }
    pickerInput("scatter_colorz",NULL, choices=choices,options=list(container="body"), width="90px", inline=T, selected=input$var_map)
  })
  scatter_out<-reactiveValues(df=0)
  output$scatter_out<-renderUI({
    req(isTRUE(input$scatter_3d))
    data<-getdata_map()
    factors<-attr(data,"factors")
    coords<-attr(data,"coords")
    base_shape=if(isTRUE(input$map_1a_base)){
      attr(data,"base_shape") } else{ NULL}
    layer_shape=if(isTRUE(input$map_1f_layer)){
      attr(data,"layer_shape") } else{ NULL}
    z<-data[,input$var_map]

    if(isTRUE(input$scatter_4D)){
      data<-vals$saved_data[[input$scatter_datalistz]]
      leg<-input$scatter_colorz
      colvar<-colvar0<-if(input$scatter_colorby=="Variable"){
        data[,input$scatter_colorz]
      } else{
        factors<-attr(data,"factors")
        factors[,input$scatter_colorz]
      }
    } else{ leg=input$var_map}

    z00<-z0<-if(isTRUE(input$scatter_4D)){
      colvar} else{z}
    col= getcolhabs(vals$newcolhabs,input$pt_palette,n=100)
    z0<-scales::rescale(as.numeric(z0),c(1,100))
    pt_col<-col[cut(z0, breaks=max(z0))]

    if(is.factor(z00)){
      lab=levels(z00)
      at=1:length(lab)

    } else{
      lab=pretty(z00)
      at=pretty(z00)
    }
    at=scales::rescale(as.numeric(at),c(.3,.8))

    col_layer=adjustcolor(getcolhabs(vals$newcolhabs,input$layer_col,1), input$layer_lighten)
    col_base=adjustcolor(getcolhabs(vals$newcolhabs,input$base_col,1), input$base_lighten)
    div(
      renderPlot({
        layout(matrix(1:2,ncol=2), width = c(6,1),height = c(1,1))
        get_scatter3D(z,
                      base_shape,
                      layer_shape,
                      coords,
                      col.grid  = "gray",
                      colkey=F,
                      symbol=as.numeric(input$pt_symbol),
                      scale_points = F,
                      pt_cex=input$pt_points,
                      fac_cex = input$pt_factor,
                      col_base=col_base,
                      col_layer=col_layer,
                      showlabels=input$showfactors,
                      showpoints=T,
                      xlim=c(input$long_xmin,input$long_xmax),
                      ylim=c(input$lat_xmin,input$lat_xmax),
                      labels= factors[,input$labels_coords],
                      pt_col=pt_col,
                      fac_col=if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL},
                      zlab=input$var_map,
                      theta=input$surf_theta,
                      phi=input$surf_phi,
                      r=input$surf_r,
                      d=input$surf_d,
                      exp=input$surf_exp
        )
        legend_image <- as.raster(matrix(col, ncol=1))
        par(mar=c(5,0,5,2), xpd=T)
        plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
        mtext(leg,3, line=-3)
        rasterImage(legend_image, 0, .3, .8,.8)
        text(x=1.6, y = as.numeric(rev(at)), labels = lab)
        scatter_out$df<-recordPlot()
      })
    )
  })
  output$map01<-renderUI({
    req(input$saved_maps!="new map")
    req(!isTRUE(input$surface_map))
    req(!isTRUE(input$stack_map))
    req(!isTRUE(input$mantel_map))
    renderPlot(vals$saved_maps[[input$saved_maps]])

  })
  output$map02<-renderUI({
    req(!isTRUE(input$mantel_map))
    req(isTRUE(input$surface_map))
    req(!isTRUE(input$rgl_map))

    column(12,
           uiOutput("coarser"),
           uiOutput("persp_out"))
  })
  output$coarser<-renderUI({
    coarser<-attr( persp_res$df,"coarser")
    req(!is.null(coarser))
    coa<- if(coarser==1){
      input$saved_maps
    } else{
      input$saved_maps2

    }
    p(strong("Warning:", style="color: red"),"The selected rasters have different resolutions and were united to the coarser resolution (",coa,")")






  })
  output$map03<-renderUI({
    req(!isTRUE(input$mantel_map))
    req(isTRUE(input$surface_map))
    req(isTRUE(input$rgl_map))
    rglwidgetOutput("m3d_map",  width = 600, height = 600)
  })
  output$map04<-renderUI({
    req(!isTRUE(input$mantel_map))
    req(isTRUE(input$stack_map))

    rlist<-get_stacklist()[input$stack_layers]

    div(

      plotOutput("stack_out")
    )
  })
  output$map_out2<-renderUI({
    req(input$saved_maps!="new map")
    fluidRow(
      uiOutput("map01"),
      uiOutput("map02"),
      uiOutput("map03"),
      uiOutput("map04")
    )})
  mantel_plot<-reactiveValues(df=0)
  output$map05<-renderUI({
    req(isTRUE(input$mantel_map))
    req(length(input$var_map)>0)
    zvalues<-getdata_map()[,input$var_map]
    coords<-attr(getdata_map(),"coords")

    colnames(coords) <- c ("x", "y")
    geo.dist<-geodist(x=coords, paired=T, measure="geodesic")

    dist.mat<-dist(zvalues)
    correlog<-mantel.correlog(dist.mat, geo.dist, n.class=input$mantel_nclass, break.pts=NULL,cutoff=input$mantel_cutoff, r.type=input$mantel_r.type, nperm=input$mantel_nperm, mult="holm", progressive=TRUE )


    # Comparing
    fluidRow(
      switch(input$spatial_stats,
             "plot"=renderPlot({
               plot(correlog,input$mantel_alpha, xlab="Distance class (m)")
               mantel_plot$df<-recordPlot()
             }),
             'mantel.res'=renderPrint(correlog$mantel.res),
             'break.pts'=renderPrint(data.frame(breaks=correlog$break.pts))


      )
    )
  })
  output$package_refs<-renderPrint({

    packlist<-list()
    for(i in 1:length(list.of.packages))
    {
      pack<-list.of.packages[i]
      a<-citation(pack)
      b<-gsub("\n\nA..*","",format(a)[2])
      c<-gsub(".*\n\n","",b)
      d<-gsub("\n","",c)
      packlist[i]<-d
    }
    packlist

  })
  introjs_exit <- function(){
    runjs("introJs().exit()")
  }
  observeEvent(input$tour_databank,{guide1$highlight('menu_data', session = NULL, is_id = TRUE)$init()$start()})
  #jsCode <- "shinyjs.pageCol = function(obj){obj._currentStep}"
  up_open<-reactiveValues(df=F)
  observeEvent(input$guide1_cicerone_next$highlighted,{
    req(length(input$guide1_cicerone_next$highlighted)>0)
    req(input$guide1_cicerone_next$highlighted=='menu_data')
    updateTabItems(session, "tabs", "menu_upload")
    delay(500,{   guide1$highlight("menu_data")$init()$start(step = 3)})

  })
  observeEvent(input$guide1_cicerone_next,{
    #renderPrint(reactiveValuesToList(input))
    #req(up_open$df==F)
    #req(input$bank_button)
    req(length(input$guide1_cicerone_next$highlighted)>0)
    if(input$guide1_cicerone_next$highlighted=='bank_button'){
      showModal(upload_modal())
      up_open$df<-T

      #introjs(session, options=list(steps=steps))
      delay(800,{
        guide1$highlight(".modal",is_id=F)$init()$start(step = 4)
      })
      #runjs("Shiny.setInputValue('guide1_cicerone_next', 'upload_mod2');")
    }


  })
  observeEvent(input$guide1_cicerone_next,{
    req(length(input$guide1_cicerone_next$highlighted)>0)
    req(length(input$guide1_cicerone_next$before_previous)>0)
    req(input$guide1_cicerone_next$highlighted=="datalist_str" &
          input$guide1_cicerone_next$before_previous=="example_data"
    )
    {
      updateRadioButtons(session,"up_or_ex", selected="use example data")
    }
  })
  rea<-reactiveValues(df=0)
  observeEvent(input$guide1_cicerone_next,{
    req(length(input$guide1_cicerone_next$highlighted)>0)
    req(input$guide1_cicerone_next$highlighted=="example_data")
    req(input$up_or_ex=="use example data")
    runjs("Shiny.setInputValue('next_upload', true);")
    rea$df<-T
  })
  output$tour_cur<-renderUI({

    div(

      splitLayout(
        renderPrint(input$guide2_cicerone_previous),
        renderPrint(input$guide2_cicerone_next)
      )
    )
  })
  observeEvent(input$guide1_cicerone_previous,{
    req(length(input$guide1_cicerone_previous$highlighted)>0)
    req(input$guide1_cicerone_previous$highlighted=="upload_mod2")
    {
      removeModal()}
  })
  observeEvent(input$guide1_cicerone_previous,{
    req(length(input$guide1_cicerone_previous$highlighted)>0)
    req(input$guide1_cicerone_previous$highlighted=="example_data")
    # runjs("Shiny.setInputValue('guide1_cicerone_previous', NULL);")
    showModal(upload_modal())

    #delay(500,  runjs("Shiny.setInputValue('next_upload', false);"))
    updateRadioButtons(session,"up_or_ex", selected="use example data")
    guide1$move_forward()
    guide1$move_forward()
    #guide1$move_forward()
    #guide1$move_forward()
    delay(800,  guide1$clone()$init()$start(step = 7))
  })
  observeEvent(input$guide1_cicerone_next,{
    req(length(input$guide1_cicerone_next$highlighted)>0)
    req(length(input$guide1_cicerone_previous)>0)
    req(input$guide1_cicerone_previous$highlighted=="example_data"& input$guide1_cicerone_next$highlighted=="datalist_str")
    {
      #showModal(upload_modal())
      delay(800,  guide1$clone()$init()$start(step = 7))

    }
  })
  observeEvent(input$guide2_cicerone_previous$highlighted,{
    req(input$guide2_cicerone_previous$highlighted=="upload_insert")
    runjs("Shiny.setInputValue('back_upload', true);")
    delay(800,{
      guide1$highlight(".modal",is_id=F)$init()$start(step = 4)
    })

  })
  observe({
    req(isTRUE(input$next_upload))
    req(isTRUE(  rea$df))
    req(length(input$upload_insert)>0)
    delay(800, guide2$init()$start())
  })
  observeEvent(input$guide2_cicerone_next$highlighted,{
    req(input$guide2_cicerone_next$highlighted=="upload_insert")
    runjs("Shiny.setInputValue('upload_insert', true);")

  })
  observeEvent(input$help_databank,{
    guide3$init()$start()
  })
  output$menutitle<-renderUI({gettitle()})
  observe(vals$saved_divset_value<-isolate(length(vals$saved_data)+1))
  getdata_hc <- reactive({
    req(input$data_hc)
    data=vals$saved_data[[input$data_hc]]
    validate(need(length(data)>0,"no data found"))

    data
  })
  getdata_map <- reactive({
    req(input$data_map)
    req(length(vals$saved_data[[input$data_map]])>0)
    data=vals$saved_data[[input$data_map]]
    #req(nrow(data)>0)
    #req(input$var_map_filter1)
    factors<-attr(data, "factors")
    coords<-attr(data,"coords")
    data
  })
  filtermap<-reactive({
    data<-getdata_map()
    pic<-rownames(data)
    if(length(input$var_map_filter1)>0){
      if(input$var_map_filter1!="None"){
        factors<-attr(data,"factors")
        filtro <- as.character(input$var_map_filter1)
        filtro2 <- as.character(input$var_map_filter2)
        pic0 <- which(as.character(factors[, filtro]) == filtro2)
        pic<-pic[pic0]
      }
    }
    pic
  })
  getdata_div<-reactive({
    req(input$data_div)
    data=vals$saved_data[[input$data_div]]

    data
  })
  observeEvent(input$bmu_p_bgpalette,
               {bmu_p_bgpalette$df<-input$bmu_p_bgpalette})
  observeEvent(input$bmu_p_dotlabel,{
    bmu_p_dotlabel$df<-input$bmu_p_dotlabel
  })
  observeEvent(input$bmu_p_training,{
    bmu_p_training$df<-input$bmu_p_training
  })
  observeEvent(input$bmu_p_test,{
    bmu_p_test$df<-input$bmu_p_test
  })
  observeEvent(input$bmu_p_symbol,{
    bmu_p_symbol$df<-input$bmu_p_symbol
  })
  observeEvent(input$bmu_p_symbol_size,{
    vals$bmu_p_symbol_size<-input$bmu_p_symbol_size
  })
  observeEvent(input$bmu_p_bg_transp,{
    vals$bmu_p_bg_transp<-input$bmu_p_bg_transp
  })
  observeEvent(input$bmu_p_border_grid,{
    bmu_p_border_grid$df<-input$bmu_p_border_grid
  })
  observeEvent(input$bmu_p_factors,{
    vals$bmu_p_factors<-input$bmu_p_factors
  })
  observeEvent(input$bmu_bgpalette,{
    bmu_bgpalette$df<-input$bmu_bgpalette
  })
  observeEvent(input$bmu_dotlabel,{
    bmu_dotlabel$df<-input$bmu_dotlabel
  })
  observeEvent(input$bmu_facpalette,{
    bmu_facpalette$df<-input$bmu_facpalette
  })
  observeEvent(input$bmu_symbol,{bmu_symbol$df<-input$bmu_symbol})
  observeEvent(input$bmu_symbol_size,{
    vals$bmu_symbol_size<-input$bmu_symbol_size
  })
  observeEvent(input$bmu_cexvar,{vals$bmu_cexvar<-input$bmu_cexvar})
  observeEvent(input$bmu_bg_transp,{
    vals$bmu_bg_transp<-input$bmu_bg_transp
  })
  observeEvent(input$bmu_border_grid,{
    bmu_border_grid$df<-input$bmu_border_grid
  })
  observeEvent(input$bmu_factors,{
    vals$cur_bmu_factors<-input$bmu_factors
  })
  observeEvent(input$bmu_insertx,{vals$bmu_insertx<-input$bmu_insertx})
  observeEvent(input$bmu_inserty,{vals$bmu_inserty<-input$bmu_inserty})
  observeEvent(input$bmu_ncol,{vals$bmu_ncol<-input$bmu_ncol})
  observeEvent(input$bmu_leg_transp,{
    vals$bmu_leg_transp<-input$bmu_leg_transp
  })
  gettitle<-reactive({
    div(style="color: #05668D", id="menu-title",


        switch(input$tabs,
               "menu_intro"={
                 h4(strong("Introduction"))
               },
               "menu_upload"={
                 h4(strong("Data Bank",
                           popify(actionLink('help_databank',icon("fas fa-question-circle")), NULL,"Create, manage and download datalists and their associated results. Click for start the tutorial on this panel", options = list(container="body"))
                 ))
               },
               'menu_explore'={
                 h4(strong("Descriptive tools", pophelp(NULL,"Explore and modify data along boxplots, histograms and classic ordination techniques")))
               },
               'menu_nb'={
                 h4(strong("Naive Bayes", pophelp(NULL,"classification technique based on Bayes Theorem with an assumption of independence among predictors")))

               },
               'menu_svm'={
                 h4(strong("Support Vector Machine", pophelp(NULL," Support vector machine methods can handle both linear and non-linear class boundaries")))

               },
               'menu_som'={
                 h4(strong("Self-Organizing Maps"),
                    actionLink('introhelp',icon("fas fa-info-circle")))
               },
               'menu_hc'={
                 h4(strong("Hierarchical Clustering"),
                    actionLink('hclusthelp', icon("fas fa-info-circle")))
               },
               'menu_rf'={
                 h4(strong("Random Forest"),
                    actionLink('rfhelp', icon("fas fa-info-circle")))
               },
               'menu_maps'={
                 h4(strong("Spatial tools"))
               },
               'menu_box'={
                 h4(strong("Box plots"))
               },
               'menu_div'={
                 h4( strong("Biological diversity indices"))
               },
               'menu_down'={
                 h4(strong("Download Center"))
               }

        )

    )
  })
  output$upload_example<-renderUI({
    radioButtons("up_or_ex",NULL,choiceValues  = list("upload", "use example data"),choiceNames   = list(tipify(div("upload"),"upload your own data",placement = "top", options=list(container="body")), tipify(div("use example data"),"Use Nematode datasets from Araca Bay as example",placement = "top", options=list(container="body"))),selected = "upload", inline = T)
  })
  upload_modal <- reactive({

    res<-modalDialog(

      div(id="upload_mod2",
          h3("Upload Panel",
             popify(actionLink("uphelp0",tipify(icon("fas fa-question-circle"),"click for help")),"Data bank structure",as.character(paste0(
               "Each Datalist in the Databank requires at least two files: the ",code("Data-Attribute")," (observations) and the ",code('Factor-Attribute'),". Optionally, the user can upload files containing spatial information (coordinates, base and layer shape). More information in the help icons on the label of each input."
             )),trigger="hover", options=list(container="body"))),
          div(style = "background: white; height:500px",
              br(),
              uiOutput("upload_input")


          )

      ),

      footer =  uiOutput("upload_control"),
      size = "l",
      easyClose = TRUE
    )

    res
  })
  output$upload_control<-renderUI({
    req(length(getdatalist())>0)
    bsButton("next_upload","Continue", width="125px")
  })
  output$back_upload<-renderUI({
    actionButton("back_upload","Back", width="125px")
  })
  observeEvent(input$next_upload, {
    showModal(
      tags$div(id="upload_mod3",
               modalDialog(
                 easyClose = T,

                 div(
                   h3("Insert Datalist"),
                   column(12,uiOutput("data_list")),
                   column(12,align="right",
                          fluidRow(
                            inline(uiOutput("back_upload")),
                            inline(
                              div(class='upload_insert',actionButton("upload_insert","Insert datalist", width="125px"))
                            )

                          )
                   )


                 ),
                 footer = br(),
               )
      )


    )
    delay(1000,  guide1$init()$start(step = 8))

  })
  observeEvent(input$back_upload, {
    showModal(
      upload_modal()
    )
  })
  output$textbreak<-renderText("This action creates a single binary column per factor level, with 1 indicating the class of that particular observation")
  output$menu_upload_out<-renderUI({
    div(uiOutput("upload_selection"),
        uiOutput("panel_main"))

  })
  observeEvent(input$desc_options,{
    vals$cur_desc_options<-input$desc_options
  })
  output$upload_selection<-renderUI({
    if(sum(vals$bagdata==c(T,T))==2){col="gray"} else{ col="#0093fcff"}
    change_alert<-paste0("#data_upload {border-color:",col,";border-width: 2px;}")
    column(12,
      tags$style(change_alert),
      pickerInput("data_upload0",NULL,choices=names(vals$saved_data),  selected=vals$cur_data, options=list(container="body","style-base" = "form-control", style = "")))

  })
  observeEvent(input$tools_drop7,{

    module_ui_cc("cc")
    mod_downcenter <- callModule(module_server_cc, "cc",  vals=vals,df_colors=vals$colors_img,newcolhabs=newcolhabs)
  })
  cur_tag<-reactiveValues(df=1)
  observeEvent(input$tag_edit,{
    cur_tag$df<-input$tag_edit
  })
  cur_tag_fun<-reactiveValues(df='Order of the levels')
  observeEvent(input$tag_fun,{
    cur_tag_fun$df<-input$tag_fun
  })
  output$pop_tag<-renderUI({
    factors<-attr(getdata_upload(),"factors")
    fluidRow(column(12,h4("Edit factors",style="border-bottom: 1px solid white;"),style=" background: #05668D; color: white"),
    column(12,
           div(
             splitLayout(
               selectInput("tag_fun","Edit:",c("Order of the levels",'Levels label'), selected=cur_tag_fun$df),
               selectInput("tag_edit","Factor",rev(colnames(factors)), selected=cur_tag$df))),
           conditionalPanel("input.tag_fun=='Levels label'",{
             div(
               splitLayout(cellWidths = c("45%","45%","10%"),

                           uiOutput("tag_edit_level"),
                           uiOutput("tag_edit_newlevel"),
                           div(style="margin-top: 24px",
                               tipify(
                                 actionButton("tag_edit_apply",icon("fas fa-magic")),"Use this button to apply changes"
                               )
                           )
               )
             )
           }),
           conditionalPanel("input.tag_fun=='Order of the levels'",{
             splitLayout(  cellWidths = c("80%","20%"),
                           div(uiOutput("toorder_tag"),style="font-size: 11px;"),
                           div(style="margin-top: 24px",
                               tipify(
                                 actionButton("tag_order_apply",icon("fas fa-magic")),"Use this button to apply changes"
                               )
                           )
             )
           })

    ))
  })
  output$tag_edit_level<-renderUI({
    factors<-attr(getdata_upload(),"factors")
    selectInput('tag_edit_level',"Level:",levels(factors[,input$tag_edit]))
  })
  output$tag_edit_newlevel<-renderUI({
    textInput('tag_edit_newlevel',"New label:",placeholder="Text a new label")
  })
  output$pop_na<-renderUI({
    fluidRow(
      column(12,h4("Data imputation",tipify(icon("fas fa-question-circle"), "Impute missing values"),style="border-bottom: 1px solid white;"),style=" background: #05668D; color: white"),
      column(12,

             selectInput("na_targ","Target", c("Data-Attribute","Factor-Attribute"), width="150px"),
             uiOutput("pop_na2")
      )
    )
  })
  output$na_warning<-renderUI({
    req(input$na_method=='bagImpute')
    div(
      strong("warning",style="color: red"),"This method has higher computational cost and can be very time-consuming."
    )
  })
  output$na_methods<-renderUI({
    selectInput("na_method", span("Method",actionLink("na_help",icon("fas fa-question-circle"), type="toggle")),c("median/mode","knn","bagImpute","medianImpute"), width="120px")
  })
  observeEvent(input$na_targ,{
    choices=if(input$na_targ=="Data-Attribute"){
      c("median/mode","Knn","bagImpute","medianImpute")
    } else{
      c("median/mode")
    }
    updateSelectInput(session,'na_method', choices=choices)
  })
  output$imputation_help<-renderUI({
    hidden(
      column(12, id="na_help_text", style="background: white; white-space: normal;",
             h4("Imputation methods",style="border-bottom: 1px solid SeaGreen;",actionLink("naclose",icon('fas fa-window-close'))),

             div(
               strong("median/mode:"),
               p('Data-Attribute columns are imputed with the median;'),
               p('Factor-Attribute columns are imputed with the mode')

             ),
             div(
               strong("Knn"),
               p('k-nearest neighbor imputation is only available for the Data-Attribute. It is carried out by finding the k closest samples (Euclidian distance) in the dataset. This method automatically center and scale your data.')),
             div(
               strong("bagImpute"),
               p('Only available for the Data-Attribute. Imputation via bagging fits a bagged tree model for each predictor (as a function of all the others). This method is simple, accurate and accepts missing values, but it has much higher computational cost. ')),
             div(
               strong("medianImpute"),
               p('Only available for the Data-Attribute. Imputation via medians takes the median of each predictor in the training set, and uses them to fill missing values. This method is simple, fast, and accepts missing values, but treats each predictor independently, and may be inaccurate.'))

      )
    )
  })
  observeEvent(input$na_help,{
    shinyjs::toggle("na_help_text")
  })
  observeEvent(input$naclose,{
    shinyjs::toggle("na_help_text")
  })
  observeEvent(input$na_pop,{
    if(!input$na_pop %%2){
      shinyjs::hide("na_help_text")
    }
  })
  output$info_save_changes<-renderUI({
    req(!sum(vals$bagdata==c(T,T))==2)
    div(
      icon("fas fa-hand-point-right"),"Save your changes"
    )
  })
  output$bttn_save_changes<- renderUI({
    req(length(vals$bagdata)==2)
    inline(
      span(id='tools_drop8',
           inline(
             actionButton("tools_save", icon("fas fa-save"),style  = if(sum(vals$bagdata==c(T,T))==2){"button_change"} else{"animation: glowing2 1000ms infinite;"})
           )
      )
    )
  })


output$change_control<-renderUI({
  req(length(vals$saved_data)>0)
  div(
         span(
           tipify(bsButton(
             'cogs',span(span("Pre-processing", style="font-size: 12px"),icon("fas fa-cogs")),style='header_btts',type="toggle",
           value=T),"Pre-Processing options"),
           inline(
             tipify( bsButton("savepoints_button",span(icon("fas fa-thumbtack")),style="header_btts"),"Save/Load project")
           )
         )

  )
})



  observeEvent(input$match,{
    if(!input$match %% 2)
      updateSelectInput(session,"filter_data", selected="none")
  })
  output$selectVar<-renderUI({
fluidRow(
  column(12,h4("Select/Remove variables",style="border-bottom: 1px solid white;"),style=" background: #05668D; color: white"),
    column(12,style="overflow-x: scroll;height:350px;overflow-y: scroll",


           uiOutput("remove_sp"),


           uiOutput("filterby_indvars")))
  })
  output$tools_split<-renderUI({
    validate(need(sum(vals$bagdata==c(T,T))==2,"You have unsaved changes. Save the changes to perform the split"))

  })
  observeEvent(input$addpart,{
    vals$hand_save<-"add partition"
    vals$hand_save2<-"This action adds the partition as a factor in the Factor-Attribute."
    vals$hand_save3<-NULL
    toggleDropdownButton('dropID_split')
    showModal(
      hand_save_modal()
    )})
  output$partition_preview<-renderPrint({get_partition()})
  output$menu_bank_out<-renderUI({
    column(12, class="databank_page",style="margin-left: 5px",
           uiOutput("bank_tools"),
           uiOutput("view_out"))
  })
  output$bank_tools<-renderUI({
    column(12,
           inline(
             div(class="create_datalist",
                 popify(actionButton("bank_button", icon("fas fa-plus")),"Data Input",textinput(),options=list(container="body"))
             )
           ),
         span(
           inline(
             uiOutput("bank_input")
           ),
           inline(
             uiOutput("tools_bank")
           )
         )


           )
  })
  output$bank_input<-renderUI({
    if(length(vals$saved_data)<1) {
      column(12,
             p(style="margin-top: 10px;",
               icon("fas fa-hand-point-left", style="font-size: 30px; color: SeaGreen"),em("Get started by creating a Datalist")
             ))
    } else {
      div(
        pickerInput("data_bank",NULL,choices=names(vals$saved_data), selected=vals$cur_data, width="200px")
      )
    }
  })
  check_model<-function(data,attr="svm"){
    names_attr<-names(attr(data,attr))
    unsaved<-paste0('new ',attr,' (unsaved)')
    if(length(names_attr>0)){
      if(any(names_attr==unsaved)){
        pic<-which(names_attr==unsaved)
        names_attr<-names_attr[-pic]
        if(length(names_attr)>0){return(T)} else{
          return(F)
        }
      } else{ return(T)}
    } else{F}
  }
  output$tools_bank<-renderUI({
    data<-getdata_bank()

    choices<-list(
      "data","factors","coords",
      "extra_shape",
      if(!is.null(attr(data,"som"))){"som"},
      if(check_model(data,"rf")){"rf"},
      if(check_model(data,"nb")){"nb"},
      if(check_model(data,"svm")){"svm"}
    )

    attributes<-list(
      div(id="001",icon("fas fa-archive")),
      div(id="c002",icon("fas fa-boxes")),
      div(id="003",icon("fas fa-map-marker-alt")),
      div(id="004",icon("far fa-map")),
      if(!is.null(attr(data,"som"))){ div(id="006",icon("fas fa-braille"))},
      if(check_model(data,"rf")){ div(id="007",span(icon("fas fa-tree"),icon("fas fa-tree", style = "margin-left: -8px;"),icon("fas fa-tree", style = "margin-left: -8px;")))},
      if(check_model(data,"nb")) { div(id="008", img(src=nb_icon2,height='20',width='20'))},
      if(check_model(data,"svm")) { div(id="009", img(src=svm_icon2,height='20',width='20'))}
    )


    attributes<-attributes[which(unlist(lapply(attributes, function(x) !is.null(x))))]
    choices<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]

    div(

           # renderPrint(names(attr(data,"rf"))),
           inline(
             radioGroupButtons(
               "view_datalist",NULL,
               choiceNames =attributes,
               choiceValues =choices,status="button_active",selected = vals$curview_databank

             )
           ),
           popify(bsButton("trash_datalist",icon("far fa-trash-alt"), style="button_active"),NULL,"Remove Datalist"),

    bsPopover("001",NULL,paste0(span("Data-Attribute", style="color: red"))),
    bsPopover("c002",NULL,"Factor-Attribute"),
    bsPopover("003",NULL,"Coords-Attribute"),
    bsPopover("004",NULL,"Shapes-Attribute"),
    bsPopover("006",NULL,"Som-Attribute"),
    bsPopover("007",NULL,"RF-Attribute"),
    bsPopover("008",NULL,"NB-Attribute"),
    bsPopover("009",NULL,"SVM-Attribute"),
    )

  })
  output$viewsom<-renderUI({
    fluidRow(
      splitLayout(
        cellWidths = c("30%","50%"),
        h5(
          span(strong("Som-Attribute"),
               tipify(actionButton("downcenter_som",icon("fas fa-download")),"Download table")
          ),
          popify(bsButton("trash_som",icon("fas fa-edit"), style="button_active",type="toggle"),NULL,"Select and remove a som")
        ),
        h5(
          conditionalPanel("input.trash_som % 2",{ div(
            splitLayout(cellWidths = c("30%","10%"),
                        selectInput("remove_som", NULL, choices=names(attr(vals$saved_data[[input$data_bank]],"som"))),
                        actionButton("delete_som",icon("fas fa-trash-alt")))
          )})
        )
      ),
      renderTable(combsom(), rownames = T,striped=T, spacing ="xs")

    )
  })
  combsom<-reactive({
    combsom<-do.call("cbind",lapply(attr(getdata_bank(),"som"),train.summary_fun))
    colnames(combsom)<-names(attr(getdata_bank(),"som"))
    combsom
  })
  combsom_down<-reactive({
    combsom_down<-do.call("cbind",lapply(attr(vals$saved_data[[input$data_bank]],"som"),train.summary_fun))
    colnames(combsom_down)<-names(attr(vals$saved_data[[input$data_bank]],"som"))
    combsom_down
  })
  observeEvent(input$view_datalist,
               {vals$curview_databank<-input$view_datalist})

  output$merge_button<-renderUI({
    req(length(input$merge_datalist_in)>1)
    div(
      actionButton("merge_datalist_go",icon('fas fa-compress-alt')),"Click to the merge the selected Datalists"
    )
  })
  observeEvent(input$data_bank,{
    data=vals$saved_data[[input$data_bank]]
    output$viewdata<-renderUI({
      fluidRow(
        style="overflow-x: scroll;",


        div(class="datalist_tools",
            h5(strong("Data-Attribute"),
               inline(
                 div(
                   inline(tipify(actionButton("downcenter_data",icon("fas fa-download")),"Download table")),
                   inline(
                     dropdownButton(icon=icon("fas fa-share-alt"),
                                    status ="button_active" ,  inline = T, circle=F,
                                    inputId ="merge_datalist",
                                    tooltip = tooltipOptions(title = "Merge the multiple Datalist using ID observations", placement="bottom"),
                                    column(12,style="background-color:#E8E8E8; border: 1px solid SeaGreen",
                                           div(style="margin-top: 5px; margin-bottom: 5px; ",
                                               checkboxGroupInput("merge_datalist_in", "Select the Datalists",choices=names(vals$saved_data)),
                                               uiOutput("merge_button")
                                           )
                                    )
                     )
                   )

                 )
               )

            )),
        if(length(attr(data,"data.factors"))>0){
          column(12,h5(
            p(strong("Wargning:", style="color: red"),
              strong("Factors were removed from the Data-Attribute")),
            p("Use the 'Pre-processing & Descriptive tools' sidebar menu to handle this factors in the Data-Attribute")
          ))
        },
        column(12,
               if(ncol(getdata_bank())<1000){
                 uiOutput("data_attr")
               } else{"Preview not available for data with more than 1000 columns"}
        )

      )


    })
  })
  observeEvent(input$merge_datalist_go,{
    vals$hand_save<-"Merge Datalists"
    validate(need(length(input$merge_datalist_in)>1,"Select at least two Datalists to merge"))
    showModal(
      hand_save_modal()
    )})
  output$viewfactors<-renderUI({
    fluidRow(style="overflow-x: scroll;",
             div(class="datalist_tools",h5(
               span(strong("Factor-Attribute")),
               inline(
                 div(
                   inline(
                     tipify(actionButton("downcenter_factors",icon("fas fa-download")),"Download table", placement = "top")
                   ),
                   dropdownButton(icon=icon("fas fa-edit"),
                                  status ="button_active" ,  inline = T, circle=F,tooltip = tooltipOptions(title = "Remove a factor", placement="top"),
                                  div(
                                    checkboxGroupInput("remove_factor", "select a factor", choices=colnames(attr(vals$saved_data[[input$data_bank]],"factors"))),
                                    tipify(actionButton("delete_factor_yes",icon("fas fa-trash-alt")),"Click to remove")
                                  )),
                   inline(
                     dropdown(icon=icon("fas fa-share-alt"),
                              status ="button_active" ,  inline = T, circle=F,
                              tooltip = tooltipOptions(title = "Add a factor from another Datalist", placement="top"),
                              div(
                                splitLayout(
                                  cellWidths = c("30%","30%",'30%',"10%"),

                                  div(
                                    p(strong("Select a Datalist:")),
                                    pickerInput("import_datalist", NULL, choices=names(vals$saved_data)[names(vals$saved_data)!=input$data_bank], width="200px")
                                  ),
                                  div(
                                    p(strong("Attribute:")),
                                    pickerInput("import_attr", NULL, choices=c("Factor-Attribute","Data-Attribute"), width="150px")),
                                  uiOutput("import_factor"),
                                  column(12,style="margin-top: 20px",
                                         tipify(actionButton("add_factor",icon("fas fa-plus")),"Click to the add the factor to the current Datalist")
                                  )
                                )
                              )
                     )
                   )
                 )
               )

             )),
             div(
               tags$style('#x1_factors td {padding: 0}'),
               inline(
                 DT::dataTableOutput("x1_factors")
               )
             )
    )
  })
  observeEvent(input$add_factor,{
    showModal(
      modalDialog(
        uiOutput("import_factor_yes"),
        easyClose = T,
        size = "m",
        title="Are you sure?"
      )
    )
  })
  output$import_factor_yes<-renderUI({
    factor_i<-attr(vals$saved_data[[input$import_datalist]],"factors")
    factor_t<-attr(vals$saved_data[[input$data_bank]],"factors")
    #cond<-sum(rownames(factor_t)%in%rownames(factor_i))==nrow(factor_t)
    cond<-any(rownames(factor_t)%in%rownames(factor_i))

    column(12,
           p(strong("action:"),"Import factor from another Datalist"),
           p(strong("Source:"),em(input$import_datalist,"::",input$import_attr,"::",input$import_factor,style="color: SeaGreen")),
           p(strong("Target:"),em( input$data_bank,":Factor-Attribute",style="color: #05668D")),
           textInput("newfactor","Target factor name:",input$import_factor),

           if(!cond){p(style="color: red",strong("error:"),em("Source and target IDs do not match"))} else{ bsButton("add_factor_yes","Confirm", style="button_active",block=T)}
    )
  })
  observeEvent(input$add_factor_yes,{

    factor_i<- if(input$import_attr=="Factor-Attribute"){
      attr(vals$saved_data[[input$import_datalist]],"factors")
    } else{
      vals$saved_data[[input$import_datalist]]
    }
    fac<-if(input$import_attr=="Data-Attribute"){
      as.factor(factor_i[,input$import_factor])
    } else{factor_i[,input$import_factor] }

    factor_t<-attr(vals$saved_data[[input$data_bank]],"factors")
    factor_t[rownames(factor_i),input$newfactor]<-fac
    attr(vals$saved_data[[input$data_bank]],"factors")<-factor_t

    removeModal()
  })
  output$import_factor<-renderUI({

    choices=if(input$import_attr=="Factor-Attribute"){
      colnames(attr(vals$saved_data[[input$import_datalist]],"factors"))}else{
        colnames(vals$saved_data[[input$import_datalist]])
      }
    div(
      p(strong("Select the variable:")),
      pickerInput("import_factor", NULL, choices=choices, width="150px")
    )

  })
  output$viewcoords<-renderUI({
    if(is.null(attr(getdata_bank(),"coords"))) {
      fluidRow(

        column(12,
               p(strong("No coords found in Datalist:",style="color: red"),em(input$data_bank))),
        uiOutput("add_coords_intru"),
        column(12,style="margin-top: 20px",
               splitLayout(
                 cellWidths = c("30%","20%"),
                 column(12,
                        fileInput(inputId = "add_coords_file",label = NULL)),
                 column(12,
                        uiOutput("add_coords_button"))

               ))

      )
    } else{
      fluidRow(
        h5(id="databank_coords",strong("Coords-Attribute"),tipify(actionButton("downcenter_coords",icon("fas fa-download")),"Download table"),
           actionButton("delete_coords",icon("fas fa-trash-alt"))),
        tags$style('#DTcoords td {padding: 0}'),
        inline(
          DT::dataTableOutput("DTcoords")
        )
      )
    }

  })
  output$DTcoords<-DT::renderDataTable(data.frame(attr(getdata_bank(),"coords")),options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')
  output$viewbase<-renderUI({
    req(input$pick_elayers)
    req(input$pick_elayers=="Base Shape")

    column(12,
           #renderPlot(bankShpfile()),
           if(is.null(attr(getdata_bank(),"base_shape"))) {fluidRow(

             column(12,
                    p(strong("No base_shape found in Datalist:",style="color: red"),em(input$data_bank))),
             uiOutput("add_base_intru"),
             column(12,style="margin-top: 20px",
                    splitLayout(
                      cellWidths = c("30%","20%"),
                      column(12,
                             fileInput(inputId = "add_base_file",label = NULL)),
                      column(12,
                             uiOutput("add_base_button"))

                    ))

           )} else {
             renderPlot({
               base_shape<-attr(getdata_bank(),"base_shape")
               ggplot(st_as_sf(base_shape)) + geom_sf()+
                 theme(panel.background = element_rect(fill = "white"),
                       panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))
             })
           }

    )
  })
  output$viewlayer<-renderUI({

    req(input$pick_elayers)
    req(input$pick_elayers=="Layer Shape")
    div(
      if(is.null(attr(getdata_bank(),"layer_shape"))) {fluidRow(

        column(12,
               p(strong("No layer_shape found in Datalist:",style="color: red"),em(input$data_bank))),
        uiOutput("add_layer_intru"),
        column(12,style="margin-top: 20px",
               splitLayout(
                 cellWidths = c("30%","20%"),
                 column(12,
                        fileInput(inputId = "add_layer_file",label = NULL)),
                 column(12,
                        uiOutput("add_layer_button"))

               ))

      )} else    {
        fluidRow(

          renderPlot({
            layer_shape<-attr(getdata_bank(),"layer_shape")
            ggplot(st_as_sf(layer_shape)) + geom_sf()+
              theme(panel.background = element_rect(fill = "white"),
                    panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))

          })
        )
      }


    )
})
  observeEvent(input$trash_datalist,{
    showModal(
      modalDialog(
        column(12,
               fluidRow(
                 column(12,strong("Remove Datalist", style="color: red"),em(input$data_bank, col="gray20",style="height: 150px")),
                 column(12,style="margin-top: 20px; margin-bottom: 20px", bsButton("delete_datalist",icon=icon("far fa-trash-alt"),"Delete", style="button_active", block=T, value=F))
               )
        ),
        title=strong("Are you sure?"),
        easyClose = T,
        size = "l"

      )
    )

  })
  observeEvent(input$delete_datalist,{
    vals$saved_data[[input$data_bank]]<-NULL
  })
  output$view_out<-renderUI({
    column(12,style="margin-top: -20px",

           conditionalPanel("input.view_datalist=='data'",{
             uiOutput("viewdata")
           }),
           conditionalPanel("input.view_datalist=='factors'",{
             uiOutput("viewfactors")
           }),
           conditionalPanel("input.view_datalist=='coords'",{
             uiOutput("viewcoords")
           }),

           conditionalPanel("input.view_datalist=='extra_shape'",{
             uiOutput("viewshapes")
           }),

           conditionalPanel("input.view_datalist=='som'",{
             uiOutput("viewsom")
           }),
           conditionalPanel("input.view_datalist=='rf'",{
             div(

               uiOutput("viewrf"),
             )
           }),
           conditionalPanel("input.view_datalist=='nb'",{
             div(

               uiOutput("viewnb"),
             )
           }),
           conditionalPanel("input.view_datalist=='svm'",{
             div(

               uiOutput("viewsvm"),

             )
           })
    )
  })
  layers_choices<-reactive({
    base_shape<-attr(vals$saved_data[[input$data_bank]],"base_shape")
    layer_shape<-attr(vals$saved_data[[input$data_bank]],"layer_shape")
    eshape<-attr(vals$saved_data[[input$data_bank]],"extra_shape")

    pic<-which(unlist(lapply(list(base_shape,layer_shape),function(x)length(x)>0)))
    choices=c(c("Base Shape","Layer Shape"),names(eshape))
    choices
  })
  output$viewshapes<-renderUI({
    choices<-layers_choices()
    column(12,style="margin-top: 10px",
           span(
             tipify(bsButton('shp_view', span(icon("fas fa-plus"),icon("fas fa-layer-group")),style  = "button_active"),"Create/Crop Shapes",
             ),
             inline(
               pickerInput("pick_elayers","Shapes:",  choices, width="200px")
             ),
             actionButton("delete_elayer_shape",icon("fas fa-trash-alt"))

           ),


           uiOutput('viewbase'),
           uiOutput('viewlayer'),
           uiOutput("elayers")
           )

  })
  observeEvent(input$delete_elayer_shape,{
    req(input$pick_elayers!='Base Shape')
    req(input$pick_elayers!='Layer Shape')
    attr(vals$saved_data[[input$data_bank]],"extra_shape")[[input$pick_elayers]]<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
  })
  observeEvent(input$delete_elayer_shape,{
    req(input$pick_elayers=='Base Shape')
    attr(vals$saved_data[[input$data_bank]],"base_shape")<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
  })
  observeEvent(input$delete_elayer_shape,{
    req(input$pick_elayers=='Layer Shape')
    attr(vals$saved_data[[input$data_bank]],"layer_shape")<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
  })
  output$elayers<-renderUI({
    req(input$pick_elayers)
   div(

     #renderPrint(attr(vals$saved_data[[input$data_bank]],"extra_shape")),
     renderPlot({
       shape<-
         attr(vals$saved_data[[input$data_bank]],"extra_shape")[[input$pick_elayers]]
       req(!is.null(shape))
       ggplot(st_as_sf(shape)) + geom_sf()+
         theme(panel.background = element_rect(fill = "white"),
               panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))



     })
   )
  })
  output$rf_remove<-renderUI({
    div(
      popify(bsButton("trash_rf",icon("fas fa-edit"), style="button_active",type="toggle"),NULL,"Select and remove a rf"),
      inline(
        conditionalPanel("input.trash_rf % 2",{
          div(
            inline(
              pickerInput("remove_rf", NULL, choices=names(attr(vals$saved_data[[input$data_bank]],"rf")), width = "250px")
            ),
            actionButton("delete_rf",icon("fas fa-trash-alt"))
          )
        })
      )
    )
  })
  observeEvent(input$delete_rf,{
    attr(vals$saved_data[[input$data_bank]],"rf")[input$remove_rf]<-NULL
  })
  output$nb_remove<-renderUI({
    div(
      popify(bsButton("trash_nb",icon("fas fa-edit"), style="button_active",type="toggle"),NULL,"Select and remove a nb"),
      inline(
        conditionalPanel("input.trash_nb % 2",{
          div(
            inline(
              pickerInput("remove_nb", NULL, choices=names(attr(vals$saved_data[[input$data_bank]],"nb")), width = "250px")
            ),
            actionButton("delete_nb",icon("fas fa-trash-alt"))
          )
        })
      )
    )
  })
  observeEvent(input$delete_nb,{
    attr(vals$saved_data[[input$data_bank]],"nb")[input$remove_nb]<-NULL
  })
  output$svm_remove<-renderUI({
    div(
      popify(bsButton("trash_svm",icon("fas fa-edit"), style="button_active",type="toggle"),NULL,"Select and remove a svm"),
      inline(
        conditionalPanel("input.trash_svm % 2",{
          div(
            inline(
              pickerInput("remove_svm", NULL, choices=names(attr(vals$saved_data[[input$data_bank]],"svm")), width = "250px")
            ),
            actionButton("delete_svm",icon("fas fa-trash-alt"))
          )
        })
      )
    )
  })
  observeEvent(input$delete_svm,{
    attr(vals$saved_data[[input$data_bank]],"svm")[input$remove_svm]<-NULL
  })
  getglobal_nb<-reactive({

    models<-lapply(attr(vals$saved_data[[input$data_bank]],"nb"), function (x) x[[1]])
    res<-caret_global_stats(models)
    res
  })
  output$viewnb<-renderUI({
    div(
      h4(strong("NB-Attribute"),inline(uiOutput('nb_remove'))),
      numericInput("nb_round","Round table:",value=3, step=1, min=1,width="100px"),
      column(12,style="background: white",

             div(span(
               strong("Naive Bayes models"),inline(
                 div(actionButton("down_nbtable_class",icon("fas fa-download")))
               )
             )),
             tags$style('#nbtab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
             tags$style('#nbtab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
             inline(DT::dataTableOutput('nbtab_class'))

      )

    )
  })
  output$nbtab_class <- DT::renderDataTable({
    vals$nbtable_class<-table<-getglobal_nb()$class
    ncol_base<-max(attr(table,"ncol_train"))
    container<-container_global_caret(table)
    pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
    table[,pic]<-round(table[,pic],3)
    table
    DT::formatStyle(
      DT::formatStyle(
        DT::datatable(as.matrix(table),
                      container =container,
                      options=list(
                        rownames=T,
                        info=FALSE,autoWidth=T,dom = 't')),
        c(1), `border-left` = "solid 1px"
      ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
    )

  }, rownames = TRUE,class ='compact cell-border')
  observeEvent(input$down_nbtable_class,{
    vals$hand_down<-"nb_stats_class"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  getglobal_rf<-reactive({

    models<-lapply(attr(vals$saved_data[[input$data_bank]],"rf"), function (x) x[[1]])
    res<-caret_global_stats(models)
    res
  })
  output$viewrf<-renderUI({
    rf_regs<-getglobal_rf()$regs
    rf_class<-getglobal_rf()$class

    div(
      h4(strong("RF-Attribute"),inline(uiOutput('rf_remove'))),
      numericInput("rf_round","Round table:",value=3, step=1, min=1,width="100px"),
      if(length(rf_class)>0){
        column(12,style="background: white",

               div(span(
                 strong("Classification models"),inline(
                   div(actionButton("down_rftable_class",icon("fas fa-download")))
                 )
               )),
               tags$style('#rftab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               tags$style('#rftab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               inline(DT::dataTableOutput('rftab_class'))

        )},
      hr(),
      if(length(rf_regs)>0){
        column(12,style="background: white",
               div(span(
                 strong("Regression models"),inline(
                   div(actionButton("down_rftable_reg",icon("fas fa-download")))
                 )
               )),
               tags$style('#rftab_regs td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               tags$style('#rftab_regs th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               inline(DT::dataTableOutput('rftab_regs'))
        )
      }
    )
  })
  output$rftab_class <- DT::renderDataTable({
    vals$rftable_class<-table<-getglobal_rf()$class
    ncol_base<-max(attr(table,"ncol_train"))
    container<-container_global_caret(table)
    pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
    table[,pic]<-round(table[,pic],input$rf_round)
    table
    DT::formatStyle(
      DT::formatStyle(
        DT::datatable(as.matrix(table),
                      container =container,
                      options=list(
                        rownames=T,
                        info=FALSE,autoWidth=T,dom = 't')),
        c(1), `border-left` = "solid 1px"
      ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
    )

  }, rownames = TRUE,class ='compact cell-border')
  output$rftab_regs <- DT::renderDataTable({
    vals$rftable_reg<-table<-getglobal_rf()$regs
    ncol_base<-max(attr(table,"ncol_train"))
    container<-container_global_caret(table)
    pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
    table[,pic]<-round(table[,pic],input$rf_round)
    table
    DT::formatStyle(
      DT::formatStyle(
        DT::datatable(table,
                      container =container,
                      options=list(
                        rownames=T,
                        info=FALSE,autoWidth=T,dom = 't')),
        c(1), `border-left` = "solid 1px"
      ),       c(9,13), `border-right` = "solid 1px"
    )

  }, rownames = TRUE,class ='compact cell-border')
  observeEvent(input$down_rftable_reg,{
    vals$hand_down<-"rf_stats_reg"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$down_rftable_class,{
    vals$hand_down<-"rf_stats_class"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  getglobal_svm<-reactive({

    models<-lapply(attr(vals$saved_data[[input$data_bank]],"svm"), function (x) x[[1]])
    res<-caret_global_stats(models)
    res
  })
  output$viewsvm<-renderUI({
    svm_regs<-getglobal_svm()$regs
    svm_class<-getglobal_svm()$class

    div(
      h4(strong("SVM-Attribute"),inline(uiOutput('svm_remove'))),
      numericInput("svm_round","Round table:",value=3, step=1, min=1,width="100px"),
      if(length(svm_class)>0){
        column(12,style="background: white",

               div(span(
                 strong("Classification models"),inline(
                   div(actionButton("down_svmtable_class",icon("fas fa-download")))
                 )
               )),
               tags$style('#svmtab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               tags$style('#svmtab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               inline(DT::dataTableOutput('svmtab_class'))

        )},
      hr(),
      if(length(svm_regs)>0){
        column(12,style="background: white",
               div(span(
                 strong("Regression models"),inline(
                   div(actionButton("down_svmtable_reg",icon("fas fa-download")))
                 )
               )),
               tags$style('#svmtab_regs td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               tags$style('#svmtab_regs th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               inline(DT::dataTableOutput('svmtab_regs'))
        )
      }
    )
  })
  output$svmtab_class <- DT::renderDataTable({
    vals$svmtable_class<-table<-getglobal_svm()$class
    ncol_base<-max(attr(table,"ncol_train"))
    container<-container_global_caret(table)
    pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
    table[,pic]<-round(table[,pic],input$svm_round)
    table
    DT::formatStyle(
      DT::formatStyle(
        DT::datatable(as.matrix(table),
                      container =container,
                      options=list(
                        rownames=T,
                        info=FALSE,autoWidth=T,dom = 't')),
        c(1), `border-left` = "solid 1px"
      ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
    )

  }, rownames = TRUE,class ='compact cell-border')
  output$svmtab_regs <- DT::renderDataTable({
    vals$svmtable_reg<-table<-getglobal_svm()$regs
    ncol_base<-max(attr(table,"ncol_train"))
    container<-container_global_caret(table)
    pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
    table[,pic]<-round(table[,pic],input$svm_round)
    table
    DT::formatStyle(
      DT::formatStyle(
        DT::datatable(table,
                      container =container,
                      options=list(
                        rownames=T,
                        info=FALSE,autoWidth=T,dom = 't')),
        c(1), `border-left` = "solid 1px"
      ),       c(9,13), `border-right` = "solid 1px"
    )

  }, rownames = TRUE,class ='compact cell-border')
  observeEvent(input$down_svmtable_reg,{
    vals$hand_down<-"svm_stats_reg"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$down_svmtable_class,{
    vals$hand_down<-"svm_stats_class"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$delete_factor_yes,{
    facs<- attr(vals$saved_data[[input$data_bank]],"factors")
    vals$cur_partsom<-"None"
    updatePickerInput(session,"som_test_pick","None")
    facs[input$remove_factor]<-NULL

    attr(vals$saved_data[[input$data_bank]],"factors")<-facs
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")

  })
  output$x1_factors = {DT::renderDataTable(attr(getdata_bank(),"factors"),options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')}
  observeEvent(input$delete_coords,{
    showModal(
      modalDialog(
        title="Are you sure?",
        column(12, style="margin-top: 20px; margin-bottom: 20px",
               h5(strong("Remove coords from",style="color: red"),span("Datalist::"),em(input$data_bank,style="color: gray")),
               bsButton("delete_coords_yes",icon=icon("far fa-trash-alt"),"Confirm", style="button_active",block=T))
        ,
        easyClose = T,
        size = "s"

      )
    )
  })
  observeEvent(input$delete_coords_yes,{
    attr(vals$saved_data[[input$data_bank]],"coords")<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="coords")


  })
  output$add_coords_button<-renderUI({
    req(length(input$add_coords_file$datapath)>0)
    actionButton("add_coords",icon=icon("fas fa-arrow-right"),
                 strong(tipify(icon("fas fa-warehouse"),"Click to save to the Datalist"),
                        "Add", style="button_active"))
  })
  output$add_coords_intru<-renderUI({
    column(12,
           column(12,style="margin-top: 20px",
                  strong("1. Use the button below to upload one.")),
           if(length(input$add_coords_file$datapath)>0){
             fluidRow(
               column(12,style="margin-top: 20px",
                      strong("2. Click ",strong("Add", style="color: SeaGreen")," to insert it to the Datalist:", em(input$data_bank, style="color: SeaGreen"))),
               column(12,
                      uiOutput("error_coords"))
             )
           }
    )
  })
  output$add_base_button<-renderUI({
    req(length(input$add_base_file$datapath)>0)
    actionButton("add_base",icon=icon("fas fa-arrow-right"),
                 strong(tipify(icon("fas fa-warehouse"),"Click to save to the Datalist"),
                        "Add", style="button_active"))
  })
  output$add_base_intru<-renderUI({
    column(12,
           column(12,style="margin-top: 20px",
                  strong("1. Use the button below to upload one.")),
           if(length(input$add_base_file$datapath)>0){
             column(12,style="margin-top: 20px",
                    strong("2. Click ",strong("Add", style="color: SeaGreen")," to insert it to the Datalist:", em(input$data_bank, style="color: SeaGreen")))
           }
    )
  })
  output$add_layer_button<-renderUI({
    req(length(input$add_layer_file$datapath)>0)
    actionButton("add_layer",icon=icon("fas fa-arrow-right"),
                 strong(tipify(icon("fas fa-warehouse"),"Click to save to the Datalist"),
                        "Add", style="button_active"))
  })
  output$add_layer_intru<-renderUI({
    column(12,
           column(12,style="margin-top: 20px",
                  strong("1. Use the button below to upload one.")),
           if(length(input$add_layer_file$datapath)>0){
             column(12,style="margin-top: 20px",
                    strong("2. Click ",strong("Add", style="color: SeaGreen")," to insert it to the Datalist:", em(input$data_bank, style="color: SeaGreen")))
           }

    )
  })
  observeEvent(input$delete_base_shape,{
    showModal(
      modalDialog(
        title="Are you sure?",
        column(12, style="margin-top: 20px; margin-bottom: 20px",
               h5(strong("Remove base_shape from",style="color: red"),span("Datalist::"),em(input$data_bank,style="color: gray")),
               bsButton("delete_base_shape_yes",icon=icon("far fa-trash-alt"),"Confirm", style="button_active",block=T)),
        easyClose = T,
        size = "s"

      )
    )
  })
  observeEvent(input$delete_base_shape_yes,{
    attr(vals$saved_data[[input$data_bank]],"base_shape")<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
  })
  observeEvent(input$delete_layer_shape,{
    showModal(
      modalDialog(
        title="Are you sure?",
        column(12, style="margin-top: 20px; margin-bottom: 20px",
               h5(strong("Remove layer_shape from",style="color: red"),span("Datalist::"),em(input$data_bank,style="color: gray")),
               bsButton("delete_layer_shape_yes",icon=icon("far fa-trash-alt"),"Confirm", style="button_active",block=T)),
        easyClose = T,
        size = "s"

      )
    )
  })
  observeEvent(input$delete_layer_shape_yes,{
    attr(vals$saved_data[[input$data_bank]],"layer_shape")<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
  })
  observeEvent(input$delete_som,{
    showModal(
      modalDialog(
        title="Are you sure?",
        column(12, style="margin-top: 20px; margin-bottom: 20px",
               h5(strong("Remove som",style="color: red"),em(input$remove_som,style="color: gray")),
               bsButton("delete_som_yes",icon=icon("far fa-trash-alt"),"Confirm", style="button_active",block=T)),
        easyClose = T,
        size = "s"

      )
    )
  })
  observeEvent(input$delete_som_yes,{
    attr(vals$saved_data[[input$data_bank]],"som")[input$remove_som]<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="som")
  })
  observeEvent(input$add_base,{

    t<-try({get(gsub(" ", "", capture.output(
      load(input$add_base_file$datapath, verbose = T)
    )[2]))})
    if("try-error" %in% class(t)) {t<-readRDS(input$add_base_file$datapath) }

    attr(vals$saved_data[[input$data_bank]],"base_shape")<-t
    updateRadioGroupButtons(
      session,"view_datalist", selected="extra_shape"
    )

  })
  observeEvent(input$add_layer,{

    t<-try({get(gsub(" ", "", capture.output(
      load(input$add_layer_file$datapath, verbose = T)
    )[2]))})
    if("try-error" %in% class(t)) {t<-readRDS(input$add_layer_file$datapath) }
    attr(vals$saved_data[[input$data_bank]],"layer_shape")<-t

    updateRadioGroupButtons(
      session,"view_datalist", selected="extra_shape"
    )
    t
  })
  observeEvent(input$add_coords_file,{
    output$error_coords<-renderUI("bank compleated")
  })
  observeEvent(input$add_coords,{

    coords<-data.frame(fread(input$add_coords_file$datapath))
    rownames(coords) <- coords[, 1]
    coords[, 1] <- NULL
    if(ncol(coords)!=2){ output$error_coords<-
      renderUI({
        column(12, strong(
          "Invalid Entries. The first column must contain the name of the observations. The second and third columns must contain the logitude and latitude respectively", style="color: red"))
      })}

    if(any(rownames(coords)%in%rownames(vals$saved_data[[input$data_bank]]))==F) {
      output$error_coords<-
        renderUI({
          column(12, strong(
            "None of the IDs of the banked coordinates are compatible with the ids of the selected datalist. Please bank coodinates with valid IDs", style="color: red"))
        })
    }

    req(any(rownames(coords)%in%rownames(vals$saved_data[[input$data_bank]])))

    attr(vals$saved_data[[input$data_bank]],"coords")<-na.omit(
      coords[rownames(vals$saved_data[[input$data_bank]]),]
    )
    updateRadioGroupButtons(
      session,"view_datalist", selected="coords"
    )

  })
  getdata_bank<-reactive({
    req(input$data_bank)
    vals$saved_data[[input$data_bank]]
  })






  output$data_attr<-renderUI({
    res<-if(ncol(getdata_bank())<1000){
      div(
        tags$style('#DT_data td {padding: 0}'),
        inline(
          DT::dataTableOutput("DT_data")
        )
      )
    } else{"Too big"}
    #screenshot(scale=3, timer=2)
    res
  })
  output$DT_data<-DT::renderDataTable(getdata_bank(),options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')
  getordinal<-reactive({
    column(12,style="overflow-y: scroll;height: 250px",
           splitLayout(
             cellWidths = c("30%","5%","40%","15%"),
             column(12,
                    p("Drag and drop the labels to define their levels and click",span("'",icon("fas fa-arrow-right"),"'")," to pre-save the variable",style='white-space: normal;'),
                    splitLayout(  cellWidths = c("20%","80%"),
                                  div(
                                    style="margin-top:25px",
                                    uiOutput("toorder_or")),
                                  div(uiOutput("toorder_in"),style="font-size: 11px;"),
                                  verbatimTextOutput("teste_lev"),
                                  #div(uiOutput("toorder_lev"),style="font-size: 11px")
                    )),
             popify(actionButton("create_ordinal",div(popify(icon("fas fa-arrow-right"),'Create new ordinal factor'),style='white-space: normal;')),NULL,"Create new ordinal factor"),
             column(12,style="overflow: scroll; z-index: 1;",uiOutput("toorder_out")),

             popify(actionButton("insert_ordinal",div(strong('Include new variables'),style='white-space: normal;'), icon=icon("fas fa-arrow-right"), width="100px"),NULL,"Click to insert the new variables in the Data-Attribute")

           )
    )
  })
  output$toorder_or<-renderUI({
    res<-list()
    for(i in 1:length(input$rank_list_1)){
      res[[i]]<-div(actionButton(paste('ord',i),i, style=" height: 25px;padding:5px"))
    }
    res


  })
  output$ordinal_out<-renderUI({

    getordinal()
  })
  output$toorder_out<-renderUI({

    column(12,
           strong(paste("pre-saved:")),
           fluidRow(
             tags$style('#order_out td {padding: 0}'),
             inline(
               DT::dataTableOutput("order_out")
             )
           )


    )
  })
  output$order_out<-DT::renderDataTable(data.frame(vals$new_facts),options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')
  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  hand_save_modal<-reactive({

    modalDialog(
      withSpinner(type=8,color="SeaGreen",uiOutput("databank_storage")),
      title=strong('Databank storage',icon("fas fa-warehouse")),
      footer=column(12,
                    fluidRow(tipify(bsButton("preview_button",icon("fas fa-eye"), block=F),"Preview"), modalButton(strong("cancel")),
                             inline(uiOutput("save_confirm"))
                    ),
                    conditionalPanel("input.preview_button % 2",{
                      column(12,style="margin-top: 20px",align="left",
                             column(12,strong("Preview")),
                             uiOutput("preview"))
                    })),
      size="l",
      easyClose = T
    )
  })
  output$databank_storage<-renderUI({
    column(12,
           fluidRow(
             column(12,p(strong("action:"),em("*",vals$hand_save,style="color: SeaGreen")), p(vals$hand_save2,style="color: gray")),
             column(12,vals$hand_save3),
             column(12,style='margin-top: 10px; margin-left: 10px',
                    splitLayout(cellWidths = c("30%","70%"),
                                radioButtons("hand_save",NULL,
                                             choiceNames= list(div(style="height: 50px","create"),
                                                               div(style="height: 50px","overwrite")),
                                             choiceValues=list('create',"over")),
                                column(12,div(style="height: 50px",
                                              withSpinner(type=8,color="SeaGreen",uiOutput("data_create"))),
                                       div(style="height: 50px",
                                           withSpinner(type=8,color="SeaGreen",uiOutput("data_over"))))
                    ))

           )
    )
  })
  output$save_confirm<-renderUI({
    req(isTRUE(data_store$df)|isTRUE(data_overwritte$df))
    actionButton("data_confirm",strong("confirm"))
  })
  bag_mapname<-reactive({
    bag<-1
    name0<-input$var_map
    name1<-paste0(name0," (",bag,")")
    if(name1%in%names(vals$saved_maps))
    {
      repeat{
        bag<-bag+1
        name1<-paste0(name0," (",bag,")")
        if(!name1%in%names(vals$saved_maps)) break
      }
    }
    paste0(name0," (",bag,")")

  })
  bag_agg<-reactive({
    bag<-1
    name0<-'Datalist_agg'
    name1<-paste0(name0," (",bag,")")
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste0(name0," (",bag,")")
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste0(name0," (",bag,")")

  })
  bag_extralayer<-reactive({
    bag<-1
    name0<-'Extra-Layer'
    name1<-paste0(name0," (",bag,")")
    if(name1%in%names(attr(vals$saved_data[[input$data_bank]],"extra_shape")))
    {
      repeat{
        bag<-bag+1
        name1<-paste0(name0," (",bag,")")
        if(!name1%in%names(attr(vals$saved_data[[input$data_bank]],"extra_shape"))) break
      }
    }
    paste0(name0," (",bag,")")

  })
  observeEvent(input$insert_ordinal,{
    vals$hand_save<-"Include ordinal variables in the Data-Attribute"
    showModal(
      hand_save_modal()
    )})
  observeEvent(input$insert_classMat,{
    vals$hand_save<-"Include binary columns in the Data-Attribute"
    showModal(
      hand_save_modal()
    )})
  observeEvent(input$tools_save,{
    vals$hand_save<-"Save changes"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(
      hand_save_modal()
    )})
  observe({
    req(vals$hand_save=="Create factor using breakpoints from the dissimilarity profile")
    req(input$data_confirm %% 2)
    updateRadioGroupButtons(session,'desc_options',selected='segRDA')
    updateTabsetPanel(session,'segrda_panels',selected='DP')
  })
  observeEvent( input$data_confirm,{
    removeModal()
    switch(vals$hand_save,
           "Save new som in" = {savesom()},
           "Save changes"= {  savechanges_comb()},
           "Include binary columns in the Data-Attribute"=  {savebinary()},
           "Include ordinal variables in the Data-Attribute"=  {saveordinal()},
           "Save Clusters"= {saveclusters()},
           "Create data list from aggregation results"= {saveagg()},
           "Add an Extra-Layer-Attribute to the Datalist"=addlayer(),
           "Save diversity results"= { savediv()},
           "Create a datalist with the variables selected in the Random Forest Explainer"= {datalistrf()},
           "Create a datalist with the model errors"= {datalistrferrors()},
           "Create a datalist with the prediction errors"= {datalistrf_prederrors()},
           "Create a datalist with the RF predictions"= {datalistrf_predicions()},
           "Create a datalist with the variables included in the N most frequent interactions in the RF"= {datalistrfinter()},
           "Create a datalist from the selected observations"= { save_dataobs()},
           "Create factor using breakpoints from the dissimilarity profile"= {save_bpfac()},
           "add partition"= {savepart()},
           "Save som predictions"= {savesompred()},
           "Save errors from som predictions (X)"= {datalist_som_errorsX()},
           "Save errors from som predictions (Y)"= {datalist_som_errorsY()},
           "Save new rf in"= {saverf()},
           "Save interpolation model"=saveinterp(),
           "Save discrete model"=savediscrete(),
           "Save raster"=saveinterp(),
           "Merge Datalists"=mergedata()
    )
  })
  mergedata<-reactive({
    to_merge<-vals$saved_data[input$merge_datalist_in]
    pic<-which.max(unlist(lapply(to_merge, function(x) nrow(x))))
    data1<-to_merge[[pic]]
    newdata<-do.call(cbind,to_merge)
    colnames(newdata)<- unlist(lapply(to_merge, colnames))


    to_merge_fac<-lapply(to_merge, function (x) attr(x,"factors"))
    newfac<-do.call(cbind,to_merge_fac)
    pic_fac<-which(duplicated(unlist(lapply(to_merge_fac, colnames)))==F)
    newfac<- newfac[pic_fac]
    colnames(newfac)<-unlist(lapply(to_merge_fac, colnames))[pic_fac]

    newdata<-data_migrate(data1,newdata,input$merge_newname)
    attr(newdata,"factors")<-newfac[rownames(data1),]
    if(input$hand_save=="create"){
      vals$saved_data[[input$merge_newname]]<-newdata}
    else{
      vals$saved_data[[input$merge_over]]<-newdata
    }

  })
  saveinterp<-reactive({
    req(input$interps_newname)
    p<-vals$map_res
    if(input$hand_save=="create"){
      cur<-input$interps_newname
      vals$saved_maps[[cur]]<-p
      vals$cur_saved_maps<-cur
    } else{
      cur<-input$interps_over
      vals$saved_maps[[cur]]<-p
      vals$cur_saved_maps<-cur
    }
    updatePickerInput(session,"saved_maps",selected=cur, choices=names(vals$saved_maps))

  })
  savediscrete<-reactive({
    req(input$discrete_newname)
    p<-vals$map_res
    if(input$hand_save=="create"){
      cur<-input$discrete_newname
      vals$saved_maps[[cur]]<-p
      vals$cur_saved_maps<-cur
    } else{
      cur<-input$discrete_over
      vals$saved_maps[[cur]]<-p
      vals$cur_saved_maps<-cur
    }
    updatePickerInput(session,"saved_maps",selected=cur, choices=names(vals$saved_maps))

  })
  datalist_som_errorsX<-reactive({
    temp<-data.frame(get_sompred_results())
    if(input$hand_save=="create") {
      temp<-data_migrate(getdata_som(),temp,input$predsom_eX_newname)
      vals$saved_data[[input$predsom_eX_newname]]<-temp
    } else{
      temp<-data_migrate(getdata_som(),temp,input$predsom_eX_over)
      vals$saved_data[[input$predsom_eX_over]]<-temp
    }
  })
  datalist_som_errorsY<-reactive({
    temp<-data.frame(get_sompred_results())
    if(input$hand_save=="create") {
      temp<-data_migrate(getdata_som(),temp,input$predsom_eY_newname)
      vals$saved_data[[input$predsom_eY_newname]]<-temp
    } else{
      temp<-data_migrate(getdata_som(),temp,input$predsom_eY_over)
      vals$saved_data[[input$predsom_eY_over]]<-temp
    }
  })
  savediv<-reactive({
    divInds<-divI()
    data<-getdata_div()
    temp<-data_migrate(data,divInds,input$div_newname)
    if(input$hand_save=="create"){
      vals$saved_data[[input$div_newname]]<-temp
      vals$cur_data<-input$div_newname
    } else{
      vals$saved_data[[input$div_over]]<-temp
      vals$cur_data<-input$div_over
    }
  })
  addlayer<-reactive({
    if(is.null(attr(vals$saved_data[[input$data_map]],"extra_shape"))){
      attr(vals$saved_data[[input$data_map]],"extra_shape")<-list()
    }
    if(input$hand_save=="create"){
      attr(vals$saved_data[[input$data_map]],"extra_shape")[[input$extra_layer_newname]]<-filtershp()} else{
        attr(vals$saved_data[[input$data_map]],"extra_shape")[[input$extra_layer_over]]<-filtershp()
      }
  })
  saveagg<-reactive({
    temp<-aggreg()
    if(input$hand_save=="create"){
      temp<-data_migrate(temp,temp,input$agg_over)
      vals$saved_data[[input$agg_newname]]<-temp
      vals$cur_data<-input$agg_newname
    } else{
      temp<-data_migrate(temp,temp,input$agg_newname)
      vals$saved_data[[input$agg_over]]<-temp
      vals$cur_data<-input$agg_over
    }
  })
  saveclusters<-reactive({

    vals$baghc0<-vals$baghc0+1
    hc <- phc()
    temp <- hc$somC
    if(input$hand_save=="create"){
      attr(vals$saved_data[[input$data_hc]],"factors")[names(temp),input$hc_newname]<-as.factor(temp)
    } else{
      attr(vals$saved_data[[input$data_hc]],"factors")[input$hc_over]<-as.factor(temp)
    }




  })
  modal_dialog<-function(){
    modalDialog(
      title="Sucess",
    )
  }
  hand_save_out<-reactive(
    modalDialog()
  )
  saved_sompred<-reactiveValues()
  predsom_name<-reactive({
    zero<-"Som_Pred"
    if(input$som_models!="new som (unsaved)"){
      zero=paste0(input$som_models,"_Pred")
    }
    name<-if(!length(saved_sompred$df)>0){
      zero
    } else{
      paste(zero,length(saved_sompred$df))
    }
    name
  })
  bag_divname<-reactive({
    bag<-1
    name0<-paste("Div_results")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("Div_results",bag)

  })
  bag_sompred_eX<-reactive({
    bag<-1
    name0<-paste("Som_pred_errorsX")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("Som_pred_errorsX",bag)

  })
  bag_sompred_eY<-reactive({
    bag<-1
    name0<-paste("Som_pred_errorsY")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("Som_pred_errorsY",bag)

  })
  bag_hc<-reactive({
    bag<-1
    name0<-paste("HC")
    name1<-paste(name0,bag)
    if(name1%in%colnames(attr(vals$saved_data[[input$data_hc]],"factors")))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%colnames(attr(vals$saved_data[[input$data_hc]],"factors"))) break
      }
    }
    paste("HC",bag)

  })
  output$nointerpsmodel<-renderPrint(
    cat("no saved map to overwritte")
  )
  classmat_out <- reactive({
    column(12,
           p(icon("fas fa-exclamation-circle"),"factors transformed into class membership matrix"),
           p(icon("fas fa-exclamation-circle"),"New variables created with class membership  indicated by '1'"),
           verbatimTextOutput("classmat_vars"),
           p(icon("fas fa-exclamation-circle"),"We advise using the scale option in the forward analyses"))
  })
  output$classmat_vars <- renderPrint({
    colnames(getclassmat(attr(getdata_upload(), 'data.factors')))
  })
  observeEvent(input$load_savepoint,{
    vals$load_path<-input$load_savepoint$datapath
  })
  observeEvent(input$bank_button, {
    showModal(upload_modal())
  })
  observeEvent(input$layer_back, {
    showModal(upload_modal())
  })
  observeEvent(input$base_back, {
    showModal(upload_modal())
  })
  observeEvent(input$shp_back,{
    upload_modal()
  })
  output$data_list<-renderUI({datalist_render(getdatalist()[[1]],F)})
  output$upload_input<-renderUI({
    column(12,style="background: white",
           div(class="example_data",

               uiOutput("upload_example")),
           uiOutput("upload_input2")

    )
  })
  output$upload_input2<-renderUI({
    fluidRow(
      fluidRow(
        splitLayout(
          absolutePanel(
            h5(strong("Required files:"), style="color: SeaGreen;"),
            absolutePanel(style="border-bottom: 2px dashed SeaGreen;border-left: 2px dashed SeaGreen;height: 25px;  margin-left: 20px; margin-right: -100px", width ="500px"),
            br(),
            br(),
            br(),
            h5(strong("Optional files:",style="color: #05668D")),
            em("*Required for the", style="margin-left: 10px"),
            p(em('spatial tools menu'), style="margin-bottom: -15px; margin-left: 10px"),
            absolutePanel(style="border-bottom: 2px dashed #05668D;height: 20px;  margin-left: 20px; margin-right: -60px", width ="100px")

          ),


          column(12,class="datalist_str",
                 style="color:  SeaGreen;",
                 h5(strong("Name the datalist")),
                 uiOutput("datalistname")
                 ,

                 column(12,div(style=" margin-left: 60px;",absolutePanel(style="border-left: 2px solid Lavender; height:500px;margin-top: -15px"))),

                 column(12,
                        div(style=" margin-left: 60px;",
                            splitLayout(
                              absolutePanel(style="border-left: 2px dashed SeaGreen; height:50px; border-bottom: 2px dashed SeaGreen;", width='50px'),
                              column(12,
                                     h5(strong("Data-Attribute:",style="color:  SeaGreen"),
                                        popify(actionLink('uphelp', icon("fas fa-question-circle")),"Upload the observations",textupload(), trigger = "hover")),
                                     if(length(input$up_or_ex)>0){
                                       if(input$up_or_ex=='upload') {
                                         fluidRow(style="margin-bottom:-35px;",
                                                  fileInput(inputId = "filedata",label = NULL,accept = c(".csv"), placeholder=if(length(input$filedata$datapath)>0){input$filedata$name})
                                         )
                                       }else{em("nematodes from Araca Bay, Brazil")}
                                     }
                              ),
                              cellWidths = c("16%","84%")),



                        ),



                        div(style=" margin-left: 60px;",

                            splitLayout(
                              absolutePanel(style="border-left: 2px dashed SeaGreen; height:50px; border-bottom: 2px dashed SeaGreen;", width='50px'),
                              column(12,

                                     h5(strong("Factor-Attribute:",style="color:  SeaGreen"),
                                        popify(actionLink('labhelp', icon("fas fa-question-circle")),"Upload the factors",textlab(), trigger = "hover",options=list(container="body"))),
                                     if(length(input$up_or_ex)>0){
                                       if(input$up_or_ex=='upload'){
                                         fluidRow(style="margin-bottom:-35px",
                                                  fileInput("labels", NULL, accept = c(".csv"),placeholder=if(length(input$labels$datapath)>0){input$labels$name})
                                         )
                                       }else{em("sampling factors")}

                                     }
                              ),
                              cellWidths = c("16%","84%")),


                        ),
                        div(style=" margin-left: 60px;",
                            splitLayout(
                              absolutePanel(style="border-left: 2px dashed #05668D; height:50px; border-bottom: 2px dashed #05668D;", width='50px'),
                              column(12,style="color:  #05668D;",
                                     h5(strong("Coords-Attribute*:"),
                                        popify(actionLink('cohelp', icon("fas fa-question-circle")),"Upload the coordinates", textcoords(), trigger = "hover",options=list(container="body"))),
                                     if(length(input$up_or_ex)>0){
                                       if(input$up_or_ex=='upload'){
                                         fluidRow(style="margin-bottom:-35px",
                                                  fileInput(inputId = "coords",label =  NULL,accept = c(".csv"),placeholder=if(length(input$coords$datapath)>0){input$coords$name})
                                         )
                                       } else {
                                         em("sampling coordinates")}
                                     }
                              ),
                              cellWidths = c("16%",'84%')
                            )
                        ),


                        div(style=" margin-left: 60px;  ",
                            splitLayout(
                              absolutePanel(style="border-left: 2px dashed #05668D; height:50px; border-bottom: 2px dashed #05668D;", width='50px'),
                              column(12,style="color:  #05668D;",
                                     h5(strong("Base shape*:",actionLink("basehelp", tipify(icon("fas fa-question-circle"),"Click for more details")))),
                                     if(length(input$up_or_ex)>0){
                                       if(input$up_or_ex=='upload'){
                                         fluidRow(style="margin-bottom:-35px",
                                                  fileInput(inputId = "base_shape",label = NULL,placeholder=if(length(input$base_shape$datapath)>0){input$base_shape$name})
                                         )
                                       }else {em("base shape of the Araca Bay")}
                                     }

                              ),
                              cellWidths = c("16%",'84%')
                            )
                        ),
                        div(style=" margin-left: 60px; ",
                            splitLayout(
                              absolutePanel(style="border-left: 2px dashed #05668D; height:50px; border-bottom: 2px dashed #05668D;", width='50px'),
                              column(12,style="color:  #05668D;",
                                     h5(strong("Layer shape*:",actionLink("layerhelp", tipify(icon("fas fa-question-circle"),"Click for more details")))),
                                     if(length(input$up_or_ex)>0){
                                       if(input$up_or_ex=='upload'){
                                         fluidRow(style="margin-bottom:-35px",
                                                  fileInput(inputId = "layer_shape",label = NULL,placeholder=if(length(input$layer_shape$datapath)>0){input$layer_shape$name})
                                         )
                                       }else{em("layer shape of the Araca Bay")}
                                     }


                              ),
                              cellWidths = c("16%",'84%')
                            )
                        )

                 )


          ),
          cellWidths = c("20%","80%")
        )
      )
    )
  })
  output$datalistname<-renderUI({
    textInput("data_name", NULL, value=paste0(if(input$up_or_ex == 'use example data'){'nema_araca'} else {gsub(".csv","",input$filedata$name)}))
  })
  fingerprint_model <-reactive({
      do.call("cbind", lapply(vals$saved_model, function (x)
        attr(x, "fingerprint")))
    })
  fingerprint_data <- reactive({
    res <-
      lapply(vals$saved_data, function (x)
        attr(x, "fingerprint_data"))

    res
  })
  get_sup_som_test<-reactive({
    data = getdata_som()
    data_Y<-vals$saved_data[[input$data_somY]]
    factors<-if(input$som_type2=="Factors"){
      attr(data_Y,"factors")[,input$selecfac, drop=F]} else{data_Y}
    res<-factors[rownames(attr(data,"test_data")),]

    res


  })
  observeEvent(input$som_part,{
    updateTabsetPanel(session,"som_tab","som_tab1")
  })
  observeEvent(input$bookmark1, {
    reactiveValuesToList(input)
    session$doBookmark()
  })
  output$bookmarkBtn <- {
    downloadHandler(
      filename = function() {
        paste("Savepoint_",Sys.Date(), ".rds", sep = "")
      },
      content = function(file) {
        withProgress(message = "Preparing the download ...",
                     min = 1,
                     max = 1,
                     {
                       tosave<-isolate(reactiveValuesToList(vals))
                       tosave<-c(tosave["saved_data"],
                                 tosave["newcolhabs"],
                                 tosave['colors_img'],
                                 tosave[grep("cur",names(tosave))])


                       saveRDS(tosave, file)
                       beep(10)

                     })

      }
    )
  }
  dialogmodal <- function() {
    modalDialog(
      column(12,style="heigh: 500px",
             column(12,style="background: #ecf6f9;border: 1px solid gray;",
                    br(),
                    column(12,strong(icon("fas fa-thumbtack"),'Create a savepoint:',style="color: #05668D")),
                    column(12,
                           splitLayout(
                             cellArgs = list(style='white-space: normal;'),
                             column(12,uiOutput("downbook"))

                           ))
             ),


             column(12,style="background: #ecf6f9;border: 1px solid gray;",
                    column(12, style="margin-top: 10px;",

                           tipify(strong(icon("fas fa-external-link-alt"),"Load a savepoint:",style="color: #05668D"),"Restore the dashboard with a previously saved savepoint (.rds file)")),
                    br(),
                    br(),
                    column(6,align = "right", fileInput("load_savepoint", label=NULL,accept =".rds"))

             ),
             uiOutput("validade_savepoint"),
             br(),
             br()
      ),
      title = p(strong("Save points", style="color: SeaGreen")),
      footer = modalButton("close"),
      size = "m",
      easyClose = TRUE
    )
  }
  observeEvent(input$load_savepoint,{
    output$validade_savepoint<-renderUI({
      validate(need(length(grep(".rds",input$load_savepoint$datapath))>0,"Error: the uploaded file is not an rds file"))
    })

  })
  observeEvent(input$savepoints_button, {
    showModal(dialogmodal())
  })
  output$downbook<-renderUI({

    fluidRow(
      column(12,strong("Download the rds file")),
      column(12,tipify(downloadButton("bookmarkBtn", style = "font-size: 12px", label=paste("Download", input$savepoint_name)),"This action allows you to load the created savepoint later (panel bellow).", options=list(container="body")))
    )})
  observeEvent(input$load_savepoint,{
                 validate(need(length(grep(".rds",input$load_savepoint$datapath))>0,"Requires rds file"))
                 showModal(
                   modalDialog(
                     uiOutput("savepoint_request"),
                     title="",
                     footer = column(12,inline(uiOutput("savepoints_proceed")),modalButton("close"))

                   )
                 )})
  output$savepoints_proceed<-renderUI({
    req(length(grep(".rds",input$load_savepoint$datapath)))
    inline(actionButton("load_savepoint_yes","Proceed"))
  })
  output$savepoint_request<-renderUI({
    validate(need(length(grep(".rds",input$load_savepoint$datapath))>0,"Requires rds file"))
    column(12,
           h5('Are you sure? '),
           column(12,p(strong("Warning:", style="color: #0093fcff"),"The application will restart with the loaded savepoint and unsaved changes will be lost")))

  })
  #observe({

  react<-reactiveValues(df=NULL)
  mybooks<-reactive({
    validate(need(length(grep(".rds",input$load_savepoint$datapath))>0,"Requires rds file"))

    mybooks<-readRDS(input$load_savepoint$datapath)
    #myinputs<- reactiveValuesToList(mybooks[[2]])
    #react$df<-myinputs[order(names(myinputs))]
    #mybooks[[1]]
  })
  observeEvent(input$load_savepoint_yes,{

    mybooks<-mybooks()

    #temp2_path<-paste0(vals$temp_path)
    #shinyjs::runjs(paste0("window.location = '",mybooks$urlDF,"';"))
    #runjs(paste0(mybooks$urlDF))
    # copy reactiveValues() to reactiveValues()
    # runjs(paste0("window.open('", mybooks$urlDF, "', '_blank');"))

    for (var in names(mybooks)) {
      vals[[var]] <- mybooks[[var]]
    }
    updateTextInput(session, "tabs", value = mybooks$cur_tab)
    #file.copy(input$load_savepoint$datapath,mybooks$urlDF)
    #file.copy(input$load_savepoint$datapath,temp2_path)
    #vals$temp_path<-temp2_path

    #isolate(updateTabItems(session, "tabs", "menu_intro"))
    #isolate(updateTabItems(session, "tabs", "menu_upload"))

    #isolate(updateTabItems(session, "tabs", "menu_explore"))
    #isolate(updateTabItems(session, "tabs", "menu_maps"))

    beep(10)
  })
  teste<-reactive({
    req(length(react$df)>0)
    newinputs<-react$df
    #newinputs<-newinputs[-unlist(which(newinputs=="Done"))]
    inputlist<-reactiveValuesToList(input)
    inputlist<-inputlist[names(newinputs)]
    try(
      for (var in names(newinputs)) {
        if(length(inputlist[[var]])>0){
          if(react$df[[var]]!="Done"){
            updateTextInput(session, paste(var), value = paste(newinputs[[var]]))}
          react$df[[var]]<-"Done"
        }
      }
    )
    done<-which(react$df=="Done")
    react$df<-react$df[-done]

  })
  observeEvent(input$tabs,{
    vals$cur_tab<-input$tabs
  })
  output$divname <- renderUI({textInput("divname",NULL,value = paste0("Div_res", vals$saved_divset_value))})
  output$divtabs <- renderUI({
    req(length(vals$saved_data)>0)
    column(12,
           splitLayout(cellWidths = c("30%","30%","10%"),cellArgs = list(style='white-space: normal;'),
                       uiOutput("divcontrol"),
                       column(12,style="margin-top: 35px",
                              dropdownButton(label = "Diversity indexes", status ="button_active" ,  inline = T, circle=F,tooltip = tooltipOptions(title = "Select the indexes", placement="bottom"),width="350px",
                                             checkboxGroupInput("divInds",
                                                                NULL,
                                                                choiceValues =list("N","S","margalef","D","H","J'","Dom_rel","Skewness"),
                                                                selected=c("N","S","margalef","D","H","J'","Dom_rel","Skewness"),
                                                                inline = F,

                                                                choiceNames =list(
                                                                  span("N", actionLink('Nhelp',icon("fas fa-question-circle")), conditionalPanel("input.Nhelp % 2",{uiOutput("Nhelp")})),
                                                                  span("S",actionLink('Shelp',icon("fas fa-question-circle")), conditionalPanel("input.Shelp % 2",{uiOutput("Shelp")})),
                                                                  span("margalef",actionLink('mhelp',icon("fas fa-question-circle")), conditionalPanel("input.mhelp % 2",{uiOutput("mhelp")})),
                                                                  span("D",actionLink('Dhelp',icon("fas fa-question-circle")), conditionalPanel("input.Dhelp % 2",{uiOutput("Dhelp")})),
                                                                  span("H",actionLink('Hhelp',icon("fas fa-question-circle")), conditionalPanel("input.Hhelp % 2",{uiOutput("Hhelp")})),
                                                                  span("J",actionLink('Jhelp',icon("fas fa-question-circle")), conditionalPanel("input.Jhelp % 2",{uiOutput("Jhelp")})),
                                                                  span("Dom_rel",actionLink('Domhelp',icon("fas fa-question-circle")), conditionalPanel("input.Domhelp % 2",{uiOutput("Domhelp")})),
                                                                  span("Skewness",actionLink('Skhelp',icon("fas fa-question-circle")),conditionalPanel("input.Skhelp % 2",{uiOutput("Skhelp")})))
                                             )
                              )),
                       column(12,style="margin-top: 35px",
                              popify(bsButton("tools_savediv", icon("fas fa-save"),style  = "button_active", type="action",value=FALSE),NULL,"Save diversity results",options=list(container="body")
                              ))
           ),
           column(12,style="overflow-x: scroll",
                  column(12,strong("Results", style = "color: SeaGreen;")),
                  verbatimTextOutput("div_results"))
    )
  })
  output$div_results<-  renderPrint({
    req(length(divI())>0)
    divI()
  })
  output$down_data <- {
    downloadHandler(
      filename = function() {
        if(input$down_choices=="data"){
          paste0("data_",input$getdata_down,"_", Sys.Date(), ".csv")} else{
            paste0("factors_",input$getdata_down,"_", Sys.Date(), ".csv")
          }
      }, content = function(file) {
        write.table(x=data.frame(get_down()),file,append=T,quote=F,row.names=T,col.names=NA, sep=input$down_data_sep,
                    dec=input$down_data_dec)})}
  output$divcontrol <- renderUI({
    fluidRow(
      p(
        uiOutput("data_div"),


      )



    )

  })
  output$Nhelp<-renderUI({
    column(12,style="background: white",
           strong("Number of individuals"),
           p("Total number of individuals")
    )
  })
  output$Shelp<-renderUI({
    column(12,style="background: white",
           strong("Species richness"),
           p("the total number of species in each observation")
    )
  })
  output$mhelp<-renderUI({
    column(12,style="background: white",
           strong("Margalef diversity"),
           p("The total number of species weighted by the logarithm of the total number of individuals", withMathJax(helpText("$$ margalef = \\frac{(S-1)}{lnN}$$")))
    )
  })
  output$Dhelp<-renderUI({
    column(12,style="background: white",
           strong("Simpson diversity"),
           p("the probability that two individuals drawn at random from an infinite community would belong to the same species",withMathJax(helpText("$$ D = \\sum p^2$$")))
    )
  })
  output$Hhelp<-renderUI({
    column(12,style="background: white",
           strong("Shannon diversity"),
           p("This index  considers both species richness and evenness. The uncertainty is measured by the Shannon Function 'H'. This term is the measure corresponding to the entropy concept defined by:",withMathJax(helpText("$$ H = \\sum_{n=1}^n (p_i*\\ln p_i)$$")))
    )
  })
  output$Jhelp<-renderUI({
    column(12,style="background: white",
           strong("Shannon evenness J'"),
           p("Evenness is a measure of how different the abundances of the species in a community are from each other. The Shannon evennes is defined by:",
             withMathJax(helpText("$$ J' = \\frac{H}{\\ln(S)}$$")))
    )
  })
  output$Domhelp<-renderUI({
    column(12,style="background: white",
           strong("Relative Dominance'"),
           p("A simple measure of dominance where",em(HTML(paste0("N",tags$sub("i")))),", the abundance of the most abundant species is divided by N:",
             withMathJax(helpText("$$ Dom_{rel} = \\frac{N_1}{N} $$")))
    )
  })
  output$Skhelp<-renderUI({
    column(12,style="background: white",
           strong("LogSkew'"),
           p("Skew is the third moment of a probability distribution, measuring asymmetry. Right skew (positive numbers) indicates more probability on the right (abundant) side. Left skew (negative numbers) indicates more probability on the left side. All species abundance distributions are strongly right skewed on an arithmetic scale, so the more interesting measure is skew on the log scale:",
             withMathJax(
               helpText("$$ LogSkew = \\frac{\\sum {\\frac{(log(n_i)-\\mu)^3}{S}}}{
                       [\\sum {\\frac{(log(n_i)-\\mu)^2}{S}}]^\\frac{3}{2} \\frac{S}{(S-2)} \\sqrt{[\\frac{(S-1)}{S}]}
               } $$")
             )),p("where ",em(HTML(paste0(HTML("&mu;"),tags$sub("i"))),paste0("is the mean of log("),em(HTML(paste0("n",tags$sub("i")))),")"))
    )
  })
  observeEvent(input$tools_savediv,{
    vals$hand_save<-"Save diversity results"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(
      hand_save_modal()
    )
  })
  output$data_div<-renderUI({
    selectInput("data_div", "Datalist:", choices = names(vals$saved_data), selected=vals$cur_data, selectize=T)
  })
  divI<-reactive({
    req(input$divInds)
    abund=getdata_div()
    choices=input$divInds
    res=list()
    if("N"%in%choices){res$N<-rowSums(abund)}
    if("S"%in%choices){res$S<-vegan::specnumber(abund)}
    if("margalef"%in%choices){res$margalef=(specnumber(abund)-1)/log(rowSums(abund))}
    if("D"%in%choices){res$D<-vegan::diversity(abund, index="simpson")}
    if("H"%in%choices){res$H<-vegan::diversity(abund)}
    if("J'"%in%choices){
      H<-vegan::diversity(abund)
      S<-vegan::specnumber(abund)
      res$J <- H/log(S)}
    if("Dom_rel"%in%choices){res$Dom_rel<-apply(decostand(abund, "total"),1,sort,T)[1,]}
    if("Skewness"%in%choices){res$Skewness=apply(abund,1,skewness)}
    res<-data.frame(do.call(cbind,res))

    res
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("iMESc.zip", sep = "")
    },

    content = function(name1) {
      fs <- c()
      fs <-
        c('meta',"code_to_run.R","app.R","iMESc.R"

        )

      zip(zipfile = name1, files = fs)

      if (file.exists(name1)) {
        file.rename(name1, name1)
      }
    },
    contentType = "application/zip"
  )

  observeEvent(input$var_map,{
    vals$cur_var_map<-input$var_map
  })
  observeEvent(input$var_map,{
    vals$cur_var_map<-input$var_map
  })
  output$outliers<-renderUI({
    outliers
  })
  accu <- reactive({
    confu <- getConfusion(som_part)
    sum(diag(round(as.matrix(confu[, -ncol(confu)]), 2)))


  })
  choices_hc <- reactive({
    req(input$data_hc)
    a <- if (length(   names(vals$saved_data) > 0)) {
      "data"
    } else {
      NULL
    }

    b <-    if(length(attr(vals$saved_data[[input$data_hc]],"som"))>0){"som codebook"}else{NULL}
    res <- c(a, b)
    res
  })
  output$som_errors<-renderUI({
    res<-errors_som(vals$som_results)
    res_names<-rownames(res)
    fluidRow(
      strong("Quality measures:"),
      p(pophelp("Quantization error","Average squared distance between the data points and the map prototypes to which they are mapped. Lower is better.",placement="right"),em("Quantization error:"),strong(res[1,1])),
      p(pophelp("Percentage of explained variance","Similar to other clustering methods, the share of total variance that is explained by the clustering (equal to 1 minus the ratio of quantization error to total variance). Higher is better.",placement="right"),em("Percentage of explained variance:"),strong(res[2,1])),
      p(pophelp("Topographic error","Measures how well the topographic structure of the data is preserved on the map. It is computed as the share of observations for which the best-matching node is not a neighbor of the second-best matching node on the map. Lower is better: 0 indicates excellent topographic representation (all best and second-best matching nodes are neighbors), 1 is the maximum error (best and second-best nodes are never neighbors).",placement="right"),em("Topographic error:"),strong(res[3,1])),
      p(pophelp("Kaski-Lagus error","Combines aspects of the quantization and topographic error. It is the sum of the mean distance between points and their best-matching prototypes, and of the mean geodesic distance (pairwise prototype distances following the SOM grid) between the points and their second-best matching prototype.",placement="right"),em("Kaski-Lagus error:"),strong(res[4,1])),
      p(pophelp("Neuron Utilization","The percentage of neurons that are not BMU of any observation",placement="right"),em("Neuron Utilization:"),strong(res[5,1] ))
    )
  })
  output$down_kohonen_results <- {
    downloadHandler(
      filename = function() {
        paste0("Kohonen","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$som_results,file)
      })
  }
  get_supsom_list<-reactive({
    factors<-attr(vals$saved_data[[input$data_somY]],'factors')[rownames(getdata_som()),input$selecfac, drop=F]
    re<-lapply(factors,function(x) classvec2classmat(x))
    res<-do.call(cbind,re)
    names(re)<-colnames(factors)
    attr(res,"faclist")<-re
    res






  })
  labels <- reactive({
    vals$loadlabel<-0
    if (input$up_or_ex == 'upload') {
      validate(need(length(input$labels$datapath)!=0,"Factor file is required"))
      labels <-
        data.frame(fread(input$labels$datapath, stringsAsFactors = T,na.strings=c("","NA")))
      rownames(labels) <- labels[, 1]
      #colnames(labels)[1]<-"id"
      labels[, 1] <- NULL
      labels[which(unlist(lapply(labels, is.numeric)))] <-
        do.call("data.frame", lapply(labels[which(unlist(lapply(labels, is.numeric)))], function(x) as.factor(x)))




    } else {
      labels <-data.frame(fread("meta/factors_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
      #labels <-data.frame(fread("DADOS SANSED_OFICIAL CENPES.csv", stringsAsFactors = T))

      rownames(labels) <- labels[, 1]
      #colnames(labels)[1]<-"id"
      labels[, 1] <- NULL
      labels[which(unlist(lapply(labels, is.numeric)))] <-
        do.call("data.frame", lapply(labels[which(unlist(lapply(labels, is.numeric)))], function(x) as.factor(x)))




    }

    labels

  })
  coords<-reactive({
    if (input$up_or_ex == 'use example data') {
      coords <-data.frame(fread("meta/coords_araca.csv"))
      #coords <-data.frame(fread("coords.csv"))
    } else {
      if(length(input$coords$datapath)>0){coords <- data.frame(fread(input$coords$datapath))} else{coords=NULL}}
    rownames(coords) <- coords[, 1]
    coords[, 1] <- NULL
    #coords<-read.csv("04_coords.csv",sep=";", row.names=1)
    coords
  })
  base_shape <- reactive({
    if (input$up_or_ex == 'use example data') {
      get(gsub(" ","",capture.output(load("meta/base_shape_araca",verbose=T))[2]))
      #get(gsub(" ","",capture.output(load("0005_base_shape",verbose=T))[2]))
    } else {
      if(length(input$base_shape$datapath)>0){
        t<-try({get(gsub(" ", "", capture.output(
          load(input$base_shape$datapath, verbose = T)
        )[2]))})
        if("try-error" %in% class(t)) {t<-readRDS(input$base_shape$datapath) }
        t

      }else{NULL}

    }
  })
  layer_shape <- reactive({

    if (input$up_or_ex == 'use example data'){
      get(gsub(" ","",capture.output(load("meta/layer_shape_araca",verbose=T))[2]))
      #get(gsub(" ","",capture.output(load("0006_layer_shape",verbose=T))[2]))
    } else {
      if(length(input$layer_shape$datapath)>0){
        t<-try({get(gsub(" ", "", capture.output(
          load(input$layer_shape$datapath, verbose = T)
        )[2]))})
        if("try-error" %in% class(t)) {t<-readRDS(input$layer_shape$datapath) }
        t

      }else{NULL}

    }})
  observeEvent(input$desc_options,{
    vals$bag_submenu<-switch(input$desc_options,
                             'Boxplot'="boxplot",
                             'MDS'="mds",
                             'RDA'="rda",
                             'PCA'="pca")
  })
  border_alert<-reactive({
    if(sum(vals$bagdata==c(T,T))==2){col="gray"} else{ col="#0093fcff"}
    style<-paste("border: 3px solid",col)
    style
  })
  observeEvent(input$desc_options,
               {vals$curview_desc_options<-input$desc_options})
  observeEvent(input$summ_options,
               {vals$curview_summ_options<-input$summ_options})
  output$stats_cbox <- renderUI({
    column(12,style="background: white",
           p(strong("Box plot")),
           p(splitLayout(
             uiOutput("box_y_input"),
             uiOutput("box_factor"),
             splitLayout(
               uiOutput("filter_box1"),
               uiOutput("filter_box2")
             )
           ))

    )
  })
  output$stats_cpca<-renderUI({
    column(12,style="background: white",
           p(strong("Principal Component Analysis"))

    )
  })
  output$stats_cmds<-renderUI({
    column(12,style="background: white",
           p(strong("Nonmetric Multidimensional Scaling "))
    )
  })
  updp<-reactiveValues(df=F)
  updp0<-reactiveValues(df=F)
  observeEvent(input$segrda_panels,{
    req(isFALSE(updp0$df))
    if(input$segrda_panels=='pwRDA'){
      if(length( vals$smw_dp)>0){
        updp0$df<-T
        updp$df<-T
        updateTabsetPanel(session,'segrda_panels','DP')
      }
    }

  })
  output$ord_side<-renderUI({
    fluidRow(class="map_control_style",style="color: #05668D",
             div(
               span("+",
                    checkboxInput("segrda_scale",span("Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=T, width = "100px")
               )
             ),

             div(
               span("+",
                    inline(
                      checkboxInput("segrda_ord",strong("Axis ordination",pophelp(NULL,"Both the SMW and pwRDA analyses depend on ordered datasets. Defaults to ordering both response and explanatory matrices using one of the axes of the RDA model. If unchecked,please make sure all your inputs are already ordered.")), value=T)
                    ),
                    inline(uiOutput("ord_check"))
               )
             ),
             uiOutput("ord_sure")


    )
  })
  output$ord_check<-renderUI({
    req(isTRUE(input$segrda_ord))
    inline(numericInput("axis_ord_segrda",NULL, value=1, step=1, width="30px"))
  })
  output$ord_sure<-renderUI({
    req(isFALSE(input$segrda_ord))
    div(style='white-space: normal;',
        strong("Wargning:", style="color: SeaGreen"),"Make sure both X and Y are previously ordered")
  })
  uploadShpfile<-eventReactive(input$shp, {
    user_shp <- Read_Shapefile(input$shp)
    vals$bagshp<-T
    user_shp
  })
  output$save_feature <- {
    downloadHandler(
      filename = function() {
        paste0("feature_shape","_", Sys.Date())
      }, content = function(file) {
        saveRDS(filtershp(),file)
      })

  }
  observeEvent(input$shp_view,{
    showModal(
      shp_tool()
    )
  })
  observeEvent(input$shp_tool,{
    showModal(
      shp_tool()
    )
  })
  observeEvent(input$shp_tool2,{
    showModal(
      shp_tool()
    )
  })
  output$shp_help<-renderUI({

    div(id="shp_help",
        strong("1. Upload "),
        popify(a("shape files*"),"Shape files",options=list(style="width: 600px", container="body"), placement = "right",
                            HTML(
                              paste0(
                                "Shapefiles are a simple, nontopological format for storing the geometric location and attribute information of geographic features. The shapefile format defines the geometry and attributes of geographically referenced features in three or more files with specific file extensions that should be stored in the same project workspace. Requires at least three files:",
                                div(HTML(paste0(hr()))),
                                div(HTML(paste0(strong(".shp --")," The main file that stores the feature geometry; required."))),
                                div(HTML(paste0(strong(".shx --")," The index file that stores the index of the feature geometry; required."))),

                                div(HTML(paste0(strong(".dbf --")," The dBASE table that stores the attribute information of features; required."))),
                                div(HTML(paste0(hr()))),
                                div(HTML(paste0("There is a one-to-one relationship between geometry and attributes, which is based on record number. Attribute records in the dBASE file must be in the same order as records in the main file. "))),
                                div(HTML(paste0(em(
                                  "Each file must have the same prefix, for example, basin.shp, basin.shx, and basin.dbf"
                                ))))





                              )
                            )


        )," at once",




    )

  })
  shp_tool<-reactive({
    div(id="shp_toobox",style="width: 600px",
      modalDialog(
        div(
          tabsetPanel(id="shp_tab",
            tabPanel("Create Shape",value="tab_create",
                     sidebarLayout(
                       sidebarPanel(width=5,style="height:320px",
                         div(
                           uiOutput('shp_help'),
                           fileInput(inputId = "shp", NULL, multiple = TRUE, accept = c('.shp', '.dbf','.sbn', '.sbx', '.shx', '.prj'), width="200px")),
                         uiOutput("shp_feature1")
                       ),
                       mainPanel(width=7,
                         div(
                           uiOutput("SHP_plot")

                         )
                       ))
                     ),
            tabPanel("Crop Shapes",value="tab_crop",
                     uiOutput("shp_crop")),
            column(12,style="align: center",uiOutput("shp_control"))

          )
        ),
        title=strong("Shapefile toolbox"),
        easyClose = T
      )
    )
  })
output$shp_crop<-renderUI({
  choices<-layers_choices()
  div(
    validate(need(length(choices)>1,"Requires a Datalist with at least two shapes")),

    sidebarPanel(
        div(style="margin: 15px",
            div(strong("1. Shape A:")),
            pickerInput("croplayerA",NULL,  choices, width="150px")),
        div(uiOutput("croplayerA_plot"))
    ),
    sidebarPanel(
        uiOutput("croplayer_B"),
        div(uiOutput("croplayerB_plot"))
    ),
    sidebarPanel(
      div(style="height: 217px",
        uiOutput("croplayer_C"))
    )
    ,

  )
})
getcrop<-reactive({
  req(length(shapes_list())>0)
  new<-shapes_list()
  req(input$croplayerA)
  req(input$croplayerB)
  a<-st_as_sf(new[[input$croplayerA]])
  b<-st_as_sf(new[[input$croplayerB]])
  st_combine( st_crop(a,b))
})
output$croplayer_C<-renderUI({
  res<-getcrop()
  div(style="margin-top: 20px",
      div(strong("3. New shape:"),style="margin-bottom: 20px"),
      renderPlot({
        par(mar=c(0,0,0,0))
        plot(res, col="gray")
      },height = 150)
  )
})
output$croplayerA_plot<-renderUI({
  req(input$croplayerA)
  req(length(shapes_list())>0)
  renderPlot({
    plotshape(shapes_list()[[input$croplayerA]])
  },height = 150)
})
output$croplayerB_plot<-renderUI({
  req(input$croplayerB)
  req(length(shapes_list())>0)
  renderPlot({
    plotshape(shapes_list()[[input$croplayerB]])
  },height = 150)
})
shapes_list<-reactive({
  base_shape<-attr(vals$saved_data[[input$data_bank]],"base_shape")
  layer_shape<-attr(vals$saved_data[[input$data_bank]],"layer_shape")
  eshape<-attr(vals$saved_data[[input$data_bank]],"extra_shape")
  pic<-which(unlist(lapply(list(base_shape,layer_shape),function(x)length(x)>0)))
  eshape[['Base Shape']]<-base_shape
  eshape[['Layer Shape']]<-layer_shape
  new=c(eshape)
  new
})
output$croplayer_B<-renderUI({
  choices<-layers_choices()
  pic<-which(choices!=input$croplayerA)
  div(style="margin: 15px",
      div(strong("2. Shape B:")),
    div(pickerInput("croplayerB",NULL,  choices[pic], width="150px"))

  )

})
output$shp_control<-renderUI({
  req(length(input$shp$datapath)!=0|input$shp_tab=="tab_crop")
  if(input$shp_tab=="tab_crop"){
    choices<-layers_choices()
    validate(need(length(choices)>1,"Requires a Datalist with at least two shapes"))
  }
  div(class="well",
    div(
      inline(pickerInput("shp_include","4. Include shape as:", c("Base-Shape","Layer-Shape","Extra-Shape"),width="150px")),
      inline(uiOutput("shp_datalist")),
      inline(
        conditionalPanel("input.shp_include=='Extra-Shape'",
                         textInput("extra_layer_newname", "5. Name of of layer",bag_extralayer(), width="200px"))
      ),
      tipify(bsButton("add_shape",span(icon("fas fa-map"),icon("fas fa-arrow-circle-right")), style='button_active'),"Click to add the Shape into the selected Datalist"),

      inline(
        tipify(downloadButton("save_feature",label =NULL),"use the Download button to save the shapes as a single object. This file can be uploaded in the 'Data Input' menu as base_shape or layer_shape.", options=list(container="body"))
      )
    )
  )
})
  observeEvent(input$add_shape,{
    shape<- if(input$shp_tab=="tab_create"){
      filtershp()
    }else{
      getcrop()
    }
    res<-switch(input$shp_include,
           "Base-Shape"={attr(vals$saved_data[[input$data_bank]],"base_shape")<-shape},
           "Layer-Shape"={attr(vals$saved_data[[input$data_bank]],"layer_shape")<-shape},
           'Extra-Shape'={
             attr(vals$saved_data[[input$data_bank]],"extra_shape")[[input$extra_layer_newname]]<-shape
           }
           )
    removeModal()
    res

  })
  output$SHP_plot<-renderUI({
    req(length(filtershp())>0)
   div(
     strong("3. New shape:"),
     renderPlot({
       ggplot(st_as_sf(filtershp())) + geom_sf()+
         theme(panel.background = element_rect(fill = "white"),
               panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))


     }, height = 300)
   )
  })
  output$shp_datalist<-renderUI({
    req(length(vals$saved_data)>0)
    pickerInput("shp_datalist","5. Select the Datalist:",choices=names(vals$saved_data), width="200px")
  })
  observeEvent(input$close_shp,{

    removeModal()
  })
  output$shp_back<-renderUI({
    req(vals$shptool=="upload")
    actionButton("shp_back","back")
  })
  output$shp_feature1<-renderUI({

    req(isTRUE(vals$bagshp))
    atributos_shp<-attributes(uploadShpfile())$names
    div(
      div(strong("2. Filter the features:")),
      splitLayout(
        div(
          div(a("feature 1:")),
          pickerInput('shp_feature1',NULL,choices=c("None",atributos_shp))
        ),
        uiOutput("shp_feature2")
      )
    )
  })
  filtershp<-reactive({

    bacias<-    uploadShpfile()
    req(isTRUE(vals$bagshp))
    req(input$shp_feature1)
    req(input$shp_feature2)
    if(input$shp_feature1!="None"&input$shp_feature2!="None")
    {
      bacias<-uploadShpfile()
      bacias<-bacias[bacias[[input$shp_feature1]]==input$shp_feature2,]

    }
    bacias
  })
  output$shp_feature2<-renderUI({
    req(isTRUE(vals$bagshp))
    lev_attrs<-unique(uploadShpfile()[[input$shp_feature1]])
    div(
      div(a("feature 2:")),
      pickerInput('shp_feature2',NULL,choices=c("None",lev_attrs))
    )
  })
  downcenter<-reactive({


    modalDialog(
      if(!sum(vals$bagdata==c(T,T))==2){"the selected datalist has unsaved changes.Please save them before continuing."} else{
        column(12,
               h5(strong(vals$hand_down)),
               splitLayout(cellWidths = c("30%","70%"),
                           column(12,
                                  radioButtons("down_type",strong("format",tipify(icon("fas fa-question-circle"),"file extension", options=list(container="body"))),c(".xlsx",".csv"))),

                           conditionalPanel("input.down_type=='.csv'",{
                             splitLayout(
                               column(12,
                                      radioButtons("down_sep",strong("sep",tipify(icon("fas fa-question-circle"),"the field separator string. Values within each row of x are separated by this string.", options=list(container="body"))),
                                                   choiceValues =list(",",";"),
                                                   choiceNames =list(
                                                     "comma",
                                                     "semicolon"
                                                   )
                                      )),
                               column(12,

                                      conditionalPanel("input.down_sep==';'",{
                                        radioButtons("down_dec",strong("dec",tipify(icon("fas fa-question-circle"), "the string to use for decimal points in columns", options=list(container="body"))),
                                                     choiceValues =list(".",","),
                                                     choiceNames=list(
                                                       "dot","comma"))
                                      }),
                                      conditionalPanel("input.down_sep==','",{
                                        column(12,
                                               radioButtons("down_dec",strong("dec",tipify(icon("fas fa-question-circle"), "the string to use for decimal points in columns", options=list(container="body"))),
                                                            choiceValues =list("."),
                                                            choiceNames=list("dot")
                                               ))
                                      })
                               )

                             )
                           })
               ),
               column(12,
                      downloadButton("download_action",NULL,icon=icon("fas fa-download"),style="width: 50%"))

        )
      }
      ,

      title=h4(icon("fas fa-download"),strong("Download")),
      size="m",
      easyClose = T

    )
  })
  observeEvent(input$downcenter_data,{
    vals$hand_down<-"data"
    showModal(downcenter())

  })
  observeEvent(input$downcenter_factors,{

    vals$hand_down<-"factors"
    showModal(downcenter())
  })
  observeEvent(input$downcenter_coords,{

    vals$hand_down<-"coords"
    showModal(downcenter())
  })
  observeEvent(input$downcenter_som,{

    vals$hand_down<-"som"
    showModal(downcenter())
  })
  output$download_action <- {
    downloadHandler(
      filename = function() {
        paste0(vals$hand_down,"_", Sys.Date(), input$down_type)
      }, content = function(file) {
        if(input$down_type==".csv"){
          write.table(x=data.frame(getdown()),file,append=T,quote=F,row.names=T,col.names=NA, input$down_sep,
                      dec=input$down_dec)
        }
        if(input$down_type==".xlsx"){
          library('readxl')
          write_xlsx(cbind(id=rownames(getdown()),getdown()), file)
        }
        removeModal()

      })

  }
  output$pop_transform<-renderUI({
    fluidRow(
      id="tool3",
      column(12,h4("Transformation tools",style="border-bottom: 1px solid white;"),style=" background: #05668D; color: white"),

      column(12,
           column(12,
                  p(p(style="margin-bottom: -2px;",
                      strong("Transformation",actionLink("transfhelp", tipify(icon("fas fa-question-circle"), "Click for more details"), style = "margin-left: 2px"))),
                    selectizeInput(
                      "transf", NULL,
                      choices = NULL,
                      selected="None",
                      options = list(

                        options = list(
                          list(label="None", value="None", tooltip="No transformation"),
                          list(label = "log2", value = "log2", tooltip = " logarithmic base 2 transformation as suggested by Anderson et al. (2006): log_b (x) + 1 for x > 0, where b is the base of the logarithm; zeros are left as zeros. Higher bases give less weight to quantities and more to presences, and logbase = Inf gives the presence/absence scaling. Please note this is not log(x+1). Anderson et al. (2006) suggested this for their (strongly) modified Gower distance, but the standardization can be used independently of distance indices.",style="background: red"),
                          list(label = "log10", value = "log10", tooltip = " logarithmic base 10 transformation as suggested by Anderson et al. (2006): log_b (x) + 1 for x > 0, where b is the base of the logarithm; zeros are left as zeros. Higher bases give less weight to quantities and more to presences, and logbase = Inf gives the presence/absence scaling. Please note this is not log(x+1). Anderson et al. (2006) suggested this for their (strongly) modified Gower distance, but the standardization can be used independently of distance indices."),
                          list(label = "total", value = "total", tooltip = "divide by the line (observation) total"),
                          list(label = "max", value = "max", tooltip = "divide by column (variable) maximum"),
                          list(label = "frequency", value = "frequency", tooltip = "divide by column (variable) total and multiply by the number of non-zero items, so that the average of non-zero entries is one"),
                          list(label = "range", value = "range", tooltip = "standardize column (variable) values into range 0 ... 1. If all values are constant, they will be transformed to 0"),
                          list(label = "pa", value = "pa", tooltip = "scale x to presence/absence scale (0/1)"),
                          list(label = "chi.square", value = "chi.square", tooltip = "divide by row sums and square root of column sums, and adjust for square root of matrix total"),
                          list(label = "hellinger", value = "hellinger", tooltip = "square root of method = 'total'"),
                          list(label = "sqrt2", value = "sqrt2", tooltip = "square root"),
                          list(label = "sqrt4", value = "sqrt4", tooltip = "4th root"),
                          list(label = "log2(x+1)", value = "log2(x+1)", tooltip = " logarithmic base 2 transformation (x+1)"),
                          list(label = "log10(x+1)", value = "log10(x+1)", tooltip = "logarithmic base 10 transformation (x+1)"),
                          list(label = "BoxCox", value = "BoxCox", tooltip = "Designed for non-negative responses. boxcox transforms nonnormally distributed data to a set of data that has approximately normal distribution. The Box-Cox transformation is a family of power transformations."),
                          list(label = "YeoJohnson", value = "YeoJohnson", tooltip = "Similar to the Box-Cox model but can accommodate predictors with zero and/or negative values "),
                          list(label = "expoTrans", value = "expoTrans", tooltip = "Exponential transformation")

                        ),



                        #items = list("Option 1"),
                        valueField = "value",
                        labelField = "label",
                        render = I("{
      item: function(item, escape) {
        return '<div>' + item.label + '</div>';
      },
      option: function(item, escape) {
        return '<div title=\"' + item.tooltip + '\">' + item.label + '</div>';
      }
    }")
                      )
                    ))
           ),
    column(12,
           p(
             p( p(strong('Scale'),actionLink("scalehelp", tipify(icon("fas fa-question-circle"), "Click for more details"), style = "margin-left: 2px"))),
             p(
               splitLayout(
                 checkboxGroupInput("scale", NULL, choices =
                                      c("scale")),
                 conditionalPanel("input.scale=='scale'", {
                   checkboxGroupInput("center", NULL, choices = c("center"))

                 }))
             ),
             h4(strong("Numeric/Factor conversion"),style="border-bottom: 1px solid SeaGreen;"),
             p(
               tipify(bsButton("convert_factor","Numeric/Factor conversion",style  = "button_active"),"Tools to transform data-attribute factors into numeric values and vice versa")
             )

           )
    )

    ))
  })
  output$remove_sp<-renderUI({
    fluidRow(style="background: white; margin-top:10px",
             column(12,span("Value-based remotion",tipify(icon("fas fa-question-circle"), "Check to select a filter"))),
             uiOutput("pct_rareabund"),
             uiOutput("pct_rarefreq"),
             uiOutput("pct_raresing")
    )
  })


  output$pct_rareabund<-renderUI({
    div(
      inline( checkboxInput("rareabund","Abund<",F, width='100px')),
      inline( conditionalPanel("input.rareabund==T",{
        numericInput('pct_rare', "%", value =0.1, width='100px')
      })),
      bsTooltip("rareabund","Remove variables with an value less than x-percent of the total"),
      bsTooltip("pct_rare","Percentage")
    )
  })
  output$pct_rarefreq<-renderUI({
    div(
      inline(checkboxInput("rarefreq","Freq<",F, width='100px')),
      inline(conditionalPanel("input.rarefreq==T",{
        numericInput('pct_prev', "%", value =0.1, width='100px')
      })),
      bsTooltip("rarefreq","Remove variables occurring in less than  x-percent of the number of samples"),
      bsTooltip("pct_prev","Percentage")
    )
  })
  output$pct_raresing<-renderUI({
    div(
      inline(checkboxInput("raresing","Singletons",F, width='100px')),
      inline(
        conditionalPanel("input.raresing==T",{
          uiOutput("war_raresing")
        })
      ),
      bsTooltip("raresing","Requires a counting data. Remove variables occurring only once")

    )
  })
  output$war_raresing<-renderUI({
    req(sum(apply(data_cogs$df,2, is.integer), na.rm=T)==0)
    em("This option requires a counting data", style="color: red")

  })
  dataraw <- reactive({
    req(input$up_or_ex)
    if (input$up_or_ex == 'use example data') {
      data <-data.frame(fread("meta/nema_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
      #data <-data.frame(fread("DADOS SANSED_OFICIAL CENPES.csv", stringsAsFactors = T))
      rownames(data) <- data[, 1]
      data[, 1] <- NULL


    } else {
      validate(need(length(input$filedata$datapath)!=0,"Data file is required"))

      data <-
        data.frame(fread(input$filedata$datapath, stringsAsFactors = T,na.strings=c("","NA")))
      rownames(data) <- data[, 1]
      data[, 1] <- NULL


    }
    data
  })
  getdatalist<-reactive({
    data=dataraw()
    if (any(names(datastr(data)$type.vars) == "factor")){

      for(i in 1:ncol(data)){
        new<-as.numeric(as.character(data[,i]))
        if(!sum(is.na(new))==length(new)){data[,i]<-new}
      }
      num_cols<-which(unlist(lapply(data, function(x)is.numeric(x))))#ok
      data.numerics <-data[, num_cols,drop=F]
      data.factors<-data[, which(unlist(lapply(data, function(x)is.factor(x)))),drop=F]
      rownames(data.numerics)<-rownames(data)
      data<-data.numerics
      attr(data,"data.factors")<-data.factors}
    attr(data,"nobs_ori")<-nrow(data)
    attr(data,"nvar_ori")<-ncol(data)

    if(input$up_or_ex == 'use example data'){
      attr(data,"filename")<-'nema_araca.csv'
      attr(data, "datalist") <- paste("Datalist_nema_araca")
    } else {attr(data,"filename")<-input$data_name
    attr(data, "datalist") <- input$data_name

    }

    attr(data, "transf")<-attr(data, "transf")
    #attr(data, "transf")<-newattribs
    factors_in<-labels()[rownames(data),,drop=F]

    validate(need(sum(rownames(factors_in)%in%rownames(data))>0,"Data-Attribute and Factors-Attribute must have conforming IDs (first column). Check these files and reload again;"))
    attr(data, "factors") <- factors_in
    attr(data,"coords") <- coords()[rownames(data),]
    attr(data,"base_shape") <- base_shape()
    attr(data,"layer_shape") <- layer_shape()

    datalist = list(data)
    names(datalist) <- input$data_name
    datalist
  })
  observeEvent(input$upload_insert,{
    if(input$up_or_ex=="upload"){
      validate(need(length(input$labels$datapath)>0,"error"))
      validate(need(length(input$filedata$datapath)>0,"error"))
    }
    datalist<-getdatalist()

    if(is.null(vals$saved_data)){

      vals$saved_data<-datalist

      if(input$up_or_ex == 'use example data'){
        envi <-data.frame(fread("meta/envi_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
        rownames(envi) <- envi[, 1]
        envi[, 1] <- NULL
        envi<-data_migrate(datalist[[1]],envi,"Datalist_envi_araca")
        attr(envi,"filename")<-'envi_araca.csv'
        attr(envi, "datalist") <- paste("Datalist_envi_araca")
        envi<-list(envi)
        names(envi)<-"Datalist_envi_araca"
        vals$saved_data<-c(vals$saved_data, envi)
      }

    } else {
      vals$saved_data <- c(vals$saved_data, datalist)
    }
    upload_bag$df<-0
    removeModal()
    updateSelectInput(session,"data_bank",selected=input$data_name)
    vals$cur_data<-input$data_name
  })
  output$filterdata <- renderUI({
    filterdata()
  })

  update_selecvar<-reactiveValues(df=F)
  observe({
    if(isTRUE(update_selecvar$df)){
      delay(50,{
        toggleDropdownButton('dropID_selecvar')
        update_selecvar$df<-F
      })


    }
  })
  data_cogs<-reactiveValues(df=0)
  output$rfdata <- renderUI({
    column(
      12,
      br(),
      selectInput(
        "rfdata",
        "Select the data for the Random Forest analysis",
        choices =    names(vals$saved_data)
      )
    )
  })
  cutdata.reactive <- reactive({
    req(input$method.hc0)
    req(input$disthc)
    req(input$hcdata_palette)
    req(input$usesuggK)
    somC <-
      cutdata2(getdata_hc(),
               picK(),
               method.hc = input$method.hc0,
               dist = input$disthc, input$hcdata_palette,
               newcolhabs=vals$newcolhabs)
    somC
  })
  phc <- reactive({
    req(input$model_or_data)
    req(input$usesuggK)
    req(input$method.hc0)


    if (input$model_or_data == "som codebook") {
      if(input$usesuggK=="suggested"){
        req(input$suggsKmodel)
      }
      somC <- cutsom.reactive()
    } else if (input$model_or_data == "data") {
      if(input$usesuggK=="suggested"){
        req(input$suggsKdata)
      }
      somC <- cutdata.reactive()
    }

    somC

  })
  getmodel_hc <- reactive({
    req(input$som_hc)
    attr(getdata_hc(),"som")[[as.character(input$som_hc)]]
  })
  output$phc <- renderUI({


    fluidRow(
      column(12,actionButton('downp_hcut',icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
      column(12,plotOutput("hc_dend")
      )
    )


  })
  output$hc_dend<-renderPlot({
    req(input$model_or_data)
    req(input$method.hc0)
    suppressWarnings( hc_plot(phc()))
  })
  pclus_factors<- reactive({
    if (length(input$pclus_factors)>0) {
      as.factor(attr(getdata_hc(),"factors")[, input$pclus_factors])
    } else{NULL}
  })
  output$pclus_code<-renderPlot({
    req(input$pclus_facpalette)

    pclus(
      somC=cutsom.reactive(),
      cex = as.numeric(input$pclus_symbol_size),
      factor.pal = as.character(input$pclus_facpalette),
      labels.ind = pclus_factors(),
      pch=as.numeric(input$pclus_symbol),
      points=bmu_clus_points(),
      bg_palette=input$hcmodel_palette,
      ncol=input$ncol_pclus,
      insetx=input$insertx_pclus,
      insety=input$inserty_pclus,
      alpha.legend=input$bgleg_pclus,
      newcolhabs=vals$newcolhabs
    )


    vals$pclus_plot<-recordPlot()
  })
  observeEvent(input$bmu_dotlabel,{
    vals$bmu_points<-if(bmu_dotlabel$df == 'symbols'){ T} else {F}

  })
  bmu_clus_points<-reactive({
    req(input$dot_label_clus)
    if(input$dot_label_clus == 'symbols'){ T} else {F}

  })
  output$showgrid <- renderPlot({
    if(isTRUE(input$splitdata_som)){data=training_data$df}else{
      data = getdata_som()}


    validate(need(input$xdim!="", ""))
    validate(need(input$ydim!="", ""))
    validate(need( (input$xdim*input$ydim)<=nrow(data), "The number of map units must be less than or equal to the number of observations. Please decrease the 'xdim' and/or 'ydim' dimensions"))
    par(mar = c(0, 0, 0, 0))
    m <-
      supersom(
        as.matrix(data),
        grid = kohonen::somgrid(input$xdim, input$ydim, topo = input$topo, neighbourhood.fct=tunesom$neighbourhood.fct, toroidal=toroidal()),
        rlen = 5
      )
    grid<-m$grid[[1]]

    plot(
      m,
      shape = "straight",
      type = "mapping",
      pch = NA,
      main = ""
    )
    text(grid,labels=1:nrow(grid))
  })
  output$var_pproperty <- renderUI({
    data = data.frame(getdata_som())
    column(
      12,
      selectInput(
        "variable_pproperty",
        label = "select a variable",
        choices = colnames(data)
      )
    )
  })
  # pred<-attr(vals$saved_data[[input$data_map]],"predictions")
  #d<-if(length(pred)>0){"predictions"} else {NULL}
  observeEvent(input$scale_map,{
    if(isFALSE(input$scale_map)){

      updateCheckboxInput(session,"scalesize_size","Size", F)
      updateCheckboxInput(session,"scalesize_color","Color", F)
    }
  })
  scalesize_size<-reactive({

    res<-if(isFALSE(input$scale_map)){F} else{input$scalesize_size}
  })
  scalesize_color<-reactive({
    if(isFALSE(input$scale_map)| isTRUE(input$colored_map)){F} else{input$scalesize_color}
  })
  l1<-reactive({
    list(
      data_map=input$data_map,
      choices_map=input$choices_map,
      var_map= input$var_map,
      showfactors= input$showfactors,
      labels_coords= input$labels_coords,
      colored_map= input$colored_map,
      map_lab= input$map_lab,
      map_1a_base= input$map_1a_base,
      map_1f_layer= input$map_1f_layer,
      showcoords= input$showcoords,
      pt_points=input$pt_points+6,
      pt_coords=input$pt_coords+1,
      pt_factor= input$pt_factor+2,
      col_factor= if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL},
      pt_legend=input$pt_legend,
      pt_legend= input$pt_legend,
      map_legend=input$map_legend,
      col_coords= if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL},
      pt_palette= input$pt_palette,
      pt_symbol=  input$pt_symbol,
      scale_map=input$scale_map,
      scalesize_size= input$scalesize_size,
      scalesize_color=input$scalesize_color,
      pt_factor=input$pt_factor,
      showguides=input$showguides,
      long_xmin=input$long_xmin,
      lat_xmin=input$lat_xmin,
      long_xmax=input$long_xmax,
      lat_xmax= input$lat_xmax,
      var_map=input$var_map,
      res_map=input$res_map,
      nmax=input$nmax,
      idp=input$idp
    )
  })
  observeEvent(input$edit_map,{

    req(length(vals$saved_maps)>0)
    req(isTRUE(input$edit_map))

    p<-vals$saved_maps[[input$saved_maps]]
    l<-attr(p,"args")

    for(i in names(l)){
      try(updateTextInput(session,i, value=l[[i]]))
    }
  })
  idw_reac<-reactive({
    req(length(input$maxdist)>0)
    maxdist=if(is.na(input$maxdist)){Inf}else{as.numeric(input$maxdist)}
    nmax=if(is.na(input$nmax_idp)){Inf}else{as.numeric(input$nmax_idp)}

    c(maxdist,nmax)
  })
  observeEvent(input$maxdist,{

    if(!is.na(input$maxdist)){
      if(is.na(input$nmax_idp)){
        updateNumericInput(session,"nmax_idp", value=3)
      }
    }
  })
  map_data_disc <- reactive({
    #req(isFALSE(stopmap$df))
    req(input$long_xmin)
    req(input$lat_xmin)
    req(input$long_xmax)
    req(input$lat_xmax)

    get<-vars_data$df[which(vars_data$df==input$var_map)]
    data <- getdata_map()[filtermap(),,drop=F]
    req(get%in%colnames(data))

    colored_by_factor<-
      if(isTRUE(input$colored_map)){
        attr(data,"factors")[as.character(input$map_lab)]

      } else {NULL}
    req(input$breaks_map)
    mybreaks<-as.numeric(unlist(strsplit(input$breaks_map,",")))
    validate(need(min(mybreaks)>=min(data[,get])&max(mybreaks)<=max(data[,get]),paste0("Breaks must be within the data range (","min: ",min(data[,get]),"; max: ",max(data[,get]),")")))

    m<-
      map_discrete_variable(
        data = data,
        coords = attr(data,"coords")[rownames(data),],
        base_shape = if(isTRUE(input$map_1a_base)){
          attr(data,"base_shape")
        },
        layer_shape =   if(isTRUE(input$map_1f_layer)){
          attr(data,"layer_shape")
        } else{ NULL},
        get = get,
        main = input$map_title,
        factors=labcoords(),
        showcoords=input$showcoords,
        cex.pt = input$pt_points+6,
        cexmin.pt=input$pt_points_min,
        cex.coords=input$pt_coords+1,
        cex.fac=input$pt_factor+2,
        col.fac=input$col_factor,
        cex.axes=input$pt_legend,
        cex.lab=input$pt_legend,
        leg=input$map_legend,
        col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL},
        col.palette=input$pt_palette,
        symbol=as.numeric(input$pt_symbol),
        scalesize_size= scalesize_size(),
        scalesize_color=scalesize_color(),
        points=T, input$pt_factor+6,
        as_factor=F,
        bmu=F,
        colored_by_factor=  colored_by_factor,
        showguides=input$showguides,
        limits= cbind(
          c(input$long_xmin,  input$lat_xmin),
          c(input$long_xmax,  input$lat_xmax)
        ),
        layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
        lighten=input$layer_lighten,
        base_col=getcolhabs(vals$newcolhabs,input$base_col,1),
        base_lighten=input$base_lighten,
        newcolhabs=vals$newcolhabs,
        extralayers=extralayers(),
        data_depth=input$data_depth,
        breaks_len=input$breaks_len,
        mybreaks=mybreaks
      )

    attr(m,"args")<-l1()
    vals$map_data_disc<-m

  })
  extralayers<-reactive({
    shapes<-attr(vals$saved_data[[input$data_map]],"extra_shape")
    if(length(names(shapes))>0){
      trulayers<-c()
      for(i in 1:length(shapes)){
        trulayers[i]<-input[[paste0("map_extra_layer",i)]]
      }
      req(is.logical(trulayers))

      layers<-list()
      colors=c()
      alphas=c()
      labels<-c()
      sizes<-c()
      for(i in 1:length(shapes))
      {
        layers[[i]]<-attr(vals$saved_data[[input$data_map]],"extra_shape")[[i]]
        colors[i]<-input[[paste0("ssextra_layer_col",i)]]
        alphas[i]<-input[[paste0("ssextra_layer_lighten",i)]]
        labels[i]<-input[[paste0("feat_extra",i)]]
        sizes[i]<-input[[paste0("feat_extra_size",i)]]

      }
      if(length(colors[which(trulayers)])>0){

        extralayers<-list(colors=colors[which(trulayers)],alphas=alphas[which(trulayers)], layers=layers[which(trulayers)], labels=labels[which(trulayers)], sizes=sizes[which(trulayers)])}else{     extralayers<-NULL}



    } else{
      extralayers<-NULL
    }

    extralayers
  })
  map_fac_disc<-reactive({
    get<-vars_fac$df[which(vars_fac$df==input$var_map)]
    col=input$pt_palette
    data <- attr(getdata_map(),"factors")[filtermap(),,drop=F]
    coords<-attr(getdata_map(),"coords")[rownames(data),]
    req(get%in%colnames(data))

    m<-suppressWarnings(
      map_discrete_variable(
        data = data,
        coords = coords,
        base_shape =if(isTRUE(input$map_1a_base)){attr(getdata_map(),"base_shape") } else { NULL},
        layer_shape =if(isTRUE(input$map_1f_layer)){attr(getdata_map(),"layer_shape") } else { NULL},
        get = get,
        main = input$map_title,
        factors=labcoords(),
        showcoords=input$showcoords,
        cex.pt = input$pt_points+6,
        cex.coords=input$pt_coords+1,
        cex.fac=input$pt_factor+2,
        col.fac=input$col_factor,
        cex.axes=input$pt_legend,
        cex.lab=input$pt_legend,
        leg=input$map_legend,
        col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,5)[1] } else{ NULL},
        col.palette=col,
        symbol=as.numeric(input$pt_symbol),
        scalesize_size= F,
        scalesize_color=F,
        points=T, input$pt_factor+6,
        as_factor=F,
        bmu=F,
        colored_by_factor=attr(getdata_map(),"factors")[as.character(input$var_map)][filtermap(),, drop=F],
        showguides=input$showguides,
        limits=as.matrix( cbind(
          c(input$long_xmin,  input$lat_xmin),
          c(input$long_xmax,  input$lat_xmax)
        )),
        layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
        lighten=input$layer_lighten,
        base_col=getcolhabs(vals$newcolhabs,input$base_col,1),
        base_lighten=input$base_lighten,
        newcolhabs=vals$newcolhabs,
        extralayers=extralayers(),
        data_depth=input$data_depth
      )
    )
    attr(m,"args")<-l1()
    vals$map_fac_disc<-m

  })
  labcoords<-reactive({
    labcoords<-if(!isTRUE(input$showfactors)){NULL} else{
      attr(getdata_map(),"factors")[filtermap(),as.character(input$labels_coords)]
    }
    labcoords
  })
  model_idw_data<-reactive({
    #req(isFALSE(stopmap$df))
    maxdist=idw_reac()[1]
    nmax=idw_reac()[2]
    get<-vars_data$df[which(vars_data$df==input$var_map)]
    data <- getdata_map()[filtermap(),,drop=F]
    coords<-attr(data,"coords")[rownames(data),]
    base_shape =if(isTRUE(input$map_1a_base)){attr(data,"base_shape") } else { NULL}
    req(get%in%colnames(data))
    p<-Map_model(data=data,
                 get=get,
                 coords=coords,
                 base_shape=base_shape,
                 layer_shape=NULL,
                 res=input$res_map,
                 k=input$nmax,
                 idp=input$idp,
                 limits=as.matrix( cbind(
                   c(input$long_xmin,  input$lat_xmin),
                   c(input$long_xmax,  input$lat_xmax)
                 )),
                 nmax=nmax,
                 nmin=as.numeric(input$nmin_idp),
                 omax=as.numeric(input$omax),
                 maxdist=maxdist
    )


    vals$map_data_interp<-p
    p
  })
  map_data_interp<-reactive({

    p0<-p<-model_idw_data()

    get<-vars_data$df[which(vars_data$df==input$var_map)]
    data <- getdata_map()[filtermap(),,drop=F]
    coords<-attr(data,"coords")[rownames(data),]
    layer_shape =if(isTRUE(input$map_1f_layer)){attr(data,"layer_shape") } else { NULL}
    p<-map_style(data=data,
                 get=get,
                 coords=coords,
                 p=p,
                 main = input$map_title,
                 subtitle="",
                 cex.axes=input$pt_legend,
                 cex.lab=input$pt_legend,
                 cex.main=15,
                 cex.sub=14,
                 cex.leg=11,
                 factors=labcoords(),
                 cex.fac=input$pt_factor+2,
                 col.fac=if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL},
                 showcoords=input$showcoords,
                 col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL},
                 cex.coords=input$pt_coords+1,
                 showguides=input$showguides,
                 layer_shape=layer_shape,
                 col.palette=input$pt_palette,
                 layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
                 lighten=input$layer_lighten,
                 leg=input$map_legend,
                 newcolhabs=vals$newcolhabs,
                 extralayers=extralayers())
    attr(p,"args")<-l1()
    attr(p,"limits")<-attr(p0,"limits")
    attr(p,"base_shape")<-attr(p0,"base_shape")
    attr(p,"layer_shape")<-attr(p0,"layer_shape")
    attr(p,"interp_model")<-attr(p0,"interp_model")
    my_rst<-attr(p0,"my_rst")
    attr(my_rst,"col.palette")<-input$pt_palette
    attr(p,"my_rst")<-my_rst
    attr(p,"data_z")<-attr(p0,"data_z")
    vals$map_data_interp<-p

  })
  model_knn_data<-reactive({
    #req(isFALSE(stopmap$df))
    data <-attr(getdata_map(),"factors")[filtermap(),,drop=F]
    get<-vars_fac$df[which(vars_fac$df==input$var_map)]
    coords<-attr(getdata_map(),"coords")[rownames(data),]
    base_shape =if(isTRUE(input$map_1a_base)){attr(getdata_map(),"base_shape") } else { NULL}
    req(get%in%colnames(data))
    p<-Map_model(data=data,
                 get=get,
                 coords=coords,
                 base_shape=base_shape,
                 layer_shape=NULL,
                 res=input$res_map,
                 k=input$nmax,
                 idp=input$idp,
                 limits=as.matrix( cbind(
                   c(input$long_xmin,  input$lat_xmin),
                   c(input$long_xmax,  input$lat_xmax)
                 ))
    )


    vals$map_fac_interp<-p
    p
  })
  map_fac_interp<-reactive({
    #req(isFALSE(stopmap$df))
    p0<-p<-model_knn_data()
    data <-attr(getdata_map(),"factors")[filtermap(),,drop=F]
    get<-vars_fac$df[which(vars_fac$df==input$var_map)]
    coords<-attr(getdata_map(),"coords")[rownames(data),]
    layer_shape =if(isTRUE(input$map_1f_layer)){attr(getdata_map(),"layer_shape") } else { NULL}
    p<-map_style(data=data,
                 get=get,
                 coords=coords,
                 p=p,
                 main = input$map_title,
                 subtitle="",
                 cex.axes=input$pt_legend,
                 cex.lab=input$pt_legend,
                 cex.main=15,
                 cex.sub=14,
                 cex.leg=11,
                 factors=labcoords(),
                 cex.fac=input$pt_factor+2,
                 col.fac=if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL},
                 col.palette=input$pt_palette,
                 showcoords=input$showcoords,
                 col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL},
                 cex.coords=input$pt_coords+1,
                 showguides=input$showguides,
                 layer_shape=layer_shape,
                 layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
                 lighten=input$layer_lighten,
                 leg=input$map_legend,
                 newcolhabs=vals$newcolhabs,
                 extralayers=extralayers())
    attr(p,"args")<-l1()
    attr(p,"limits")<-attr(p0,"limits")
    attr(p,"base_shape")<-attr(p0,"base_shape")
    attr(p,"layer_shape")<-attr(p0,"layer_shape")
    attr(p,"interp_model")<-attr(p0,"interp_model")
    my_rst<-attr(p0,"my_rst")
    attr(my_rst,"col.palette")<-isolate(input$pt_palette)
    attr(p,"my_rst")<-my_rst
    attr(p,"data_z")<-attr(p0,"data_z")
    vals$map_fac_interp<-p

  })
  data_raster<-reactive({
    get<-vars_data$df[which(vars_data$df==input$var_map)]
    data <- getdata_map()[filtermap(),,drop=F]
    coords<-attr(data,"coords")[rownames(data),]
    base_shape =if(isTRUE(input$map_1a_base)){attr(data,"base_shape") } else { NULL}
    limits<-as.matrix( cbind(
      c(input$long_xmin,  input$lat_xmin),
      c(input$long_xmax,  input$lat_xmax)
    ))
    req(get%in%colnames(data))
    p<-mapraster(data,get,coords, base_shape,limits)
    vals$map_data_raster<-p
    p
  })
  map_raster<-reactive({
    validate(need(input$choices_map=="Data-Attribute","This functionality is currently only available for Data-Attribute"))

    p0<-p<-data_raster()


    get<-vars_data$df[which(vars_data$df==input$var_map)]
    data <- getdata_map()[filtermap(),,drop=F]
    coords<-attr(data,"coords")[rownames(data),]
    layer_shape =if(isTRUE(input$map_1f_layer)){attr(data,"layer_shape") } else { NULL}
    p<-map_style(data=data,
                 get=get,
                 coords=coords,
                 p=p,
                 main = input$map_title,
                 subtitle="",
                 cex.axes=input$pt_legend,
                 cex.lab=input$pt_legend,
                 cex.main=15,
                 cex.sub=14,
                 cex.leg=11,
                 factors=labcoords(),
                 cex.fac=input$pt_factor+2,
                 col.fac=if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL},
                 showcoords=input$showcoords,
                 col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL},
                 cex.coords=input$pt_coords+1,
                 showguides=input$showguides,
                 layer_shape=layer_shape,
                 col.palette=input$pt_palette,
                 layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
                 lighten=input$layer_lighten,
                 leg=input$map_legend,
                 newcolhabs=vals$newcolhabs,
                 extralayers=extralayers())
    attr(p,"args")<-l1()
    attr(p,"limits")<-attr(p0,"limits")
    attr(p,"base_shape")<-attr(p0,"base_shape")
    attr(p,"layer_shape")<-attr(p0,"layer_shape")
    my_rst<-attr(p0,"my_rst")
    attr(my_rst,"col.palette")<-input$pt_palette
    attr(p,"my_rst")<-my_rst
    attr(p,"data_z")<-attr(p0,"data_z")

    # path<-'D:/OneDrive/PÓS-DOC/Articles/MS_meiofauna/Figuras_MS_Fabi/Rasters/'
    #saveRDS(my_rst,paste0(path,get,".rds"))
    vals$map_data_raster<-p

  })
  persp_res<-reactiveValues(df=0)
  output$persp_out<-renderUI({
    req(input$saved_maps!="new map")
    req(isTRUE(input$surface_map))

    p1<-vals$saved_maps[[input$saved_maps]]

    zlab<-attr(p1,'args')['var_map']
    p1<- attr(p1,"my_rst")
    p2<-if(length(vals$saved_maps)>1){
      attr(vals$saved_maps[[input$saved_maps2]],"my_rst")
    } else{NULL}

    fluidRow(
      renderPlot({
        my_rst<-p1
        data_z<- attr(p,"data_z")

        if(is.factor(data_z)){
          colors<- getcolhabs(vals$newcolhabs,input$pt_palette,nlevels(data_z))
        } else {
          colors<- getcolhabs(vals$newcolhabs,input$pt_palette,length(my_rst@data@values))
        }

        my_rst2=p2
        pmat<-get_4D(my_rst,my_rst2=my_rst2,colors, theta=input$surf_theta, phi=input$surf_phi,r=input$surf_r,d=input$surf_d,xlab="long", ylab="Latitude", zlab=zlab, exp=input$surf_exp,wlegend=input$surface_width.legend,hlegend=input$surface_height.legend)
        if(isTRUE(input$showcoords)){
          coords<-attr(getdata_map(),"coords")
          labels<-attr(getdata_map(),"factors")[rownames(coords),input$labels_coords]
          xx <- coords[,1]
          yy <- coords[,2]
          zz <- rep(input$z_coords, nrow(coords))
          mypoints <- trans3d(xx,yy,zz,pmat = pmat)
          points(mypoints,pch = 3,col = if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL}, cex=input$pt_coords)
        }
        if(isTRUE(input$showfactors)){
          coords<-attr(getdata_map(),"coords")
          labels<-attr(getdata_map(),"factors")[rownames(coords),input$labels_coords]
          xx <- coords[,1]
          yy <- coords[,2]
          zz <- rep(input$z_labels, nrow(coords))
          mypoints <- trans3d(xx,yy,zz,pmat = pmat)
          text(mypoints,labels=labels,pch = 3,col = if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL}, cex=input$pt_factor)
        }
        if(isTRUE(input$showguides)){
          grid4D(coords,my_rst, col= getcolhabs(vals$newcolhabs,input$col_guides,1), pmat=pmat,lty=as.numeric(input$lty_guides),lwd=input$lwd_guides)
        }
        persp<-recordPlot()
        attr(persp,"coarser")<-attr(pmat,"coarser")
        persp_res$df<-persp

      })
    )



  })
  getmap<-reactive({
    if(input$cmap=='discrete'){
      if(input$choices_map=="Data-Attribute"){  p<-vals$map_data_disc}
      if(input$choices_map=="Factor-Attribute"){  p<-vals$map_fac_disc}

    }

    if(input$cmap=='interpolation') {
      if(input$choices_map=="Data-Attribute"){  p<-vals$map_data_interp}
      if(input$choices_map=="Factor-Attribute"){  p<-vals$map_fac_interp}
    }

    if(input$cmap=='raster'){  p<-vals$map_data_raster}
    suppressWarnings(print(p))
  })
  stack_out<-reactiveValues(df=0)
  output$stack_out<-renderPlot({

    if(isTRUE(input$automap)){
      output$automap_war<-renderUI({NULL})
      vals$map_res<-get_stackmap()
      vals$map_res
    } else {
      if(isFALSE(stopmap$df)){
        stopmap$df<-T
        output$automap_war<-renderUI({NULL})
        vals$map_res<-get_stackmap()
        vals$map_res
      }else{
        if(isFALSE(stopbigmap$df)){
          output$automap_war<-automap_war()
          vals$map_res<-isolate(get_stackmap())
          vals$map_res}
      }
    }

  })
  get_stackmap<-reactive({


    res_list<-get_stacklist()[input$stack_layers]
    validate(need(length(res_list)>1,"Requires at least two layers"))
    coords<-attr(getdata_map(),"coords")
    data<-getdata_map()

    layer_shape=if(isTRUE(input$srmap_layer)){
      attr(data,"layer_shape") } else{ NULL}
    srcol_layer=adjustcolor(getcolhabs(vals$newcolhabs,input$srlayer_col,1), input$srlayer_lighten)


    zs<-c()
    co<-c()
    labs<-list()
    showlab<-c()
    for(i in 1:length(names(get_stacklist())))
    {
      zs[i]<-input[[paste0("stack_z",i)]]
      co[i]<-input[[paste0("stack_co",i)]]
      showlab[i]<-input[[paste0("stack_lab",i)]]
      labs[[i]]<-attr(getdata_map(),"factors")[rownames(coords), input[[paste0("stack_labels",i)]]]
    }


    pmat<-stack4D_2(res_list,
                    legtitle.pos=input$stack_legtitle.pos,
                    leglabpos=input$stack_leglab.pos,
                    legbar.h=input$stack_legbar.h,
                    legy.adj=input$stack_legadj,
                    width.legend=input$stack_width.legend,
                    theta=input$stack_theta,
                    phi=input$stack_phi,
                    stack_eye=input$stack_eye,
                    d=input$stack_d,
                    exp=input$stack_exp,
                    transp=abs(1-input$stack_alpha),
                    layer_shape=layer_shape,

                    colgrid=input$stack_colguides,
                    coords=coords,
                    labels=labs,
                    col.labels=if(length(input$col_factor_stack)>0){   getcolhabs(vals$newcolhabs,input$col_factor_stack,1) } else{ NULL},
                    cex.labels=input$pt_factor_stack,
                    col.coords=if(length(input$col_coords_stack)>0){   getcolhabs(vals$newcolhabs,input$col_coords_stack,1) } else{ NULL},
                    cex.coords=input$pt_coords_stack,
                    show_coords=co,
                    show_labels=showlab,
                    newcolhabs=vals$newcolhabs,
                    z.value=zs,
                    zmin=input$stack_zmin,
                    zmax=input$stack_zmax,
                    xlim=c(input$srlong_xmin,input$srlong_xmax),
                    ylim=c(input$srlat_xmin,input$srlat_xmax),
                    ticktype=input$sr_ticktype,
                    xlab=input$sr_xlab,
                    ylab=input$sr_ylab,
                    zlab=input$sr_zlab,
                    col_layer=srcol_layer
    )


    if(isTRUE(input$showguides_stack)){
      grid4D(coords,res_list[[1]], col=input$col_guides_stack, pmat=pmat,lty=as.numeric(input$lty_guides_stack),lwd=input$lwd_guides_stack)
    }

    stack_out$df<-recordPlot()
    stack_out$df

  })
  output$m3d_map<-renderRglwidget({

    req(input$saved_maps!="new map")
    req(isTRUE(input$surface_map))

    p1<-vals$saved_maps[[input$saved_maps]]
    zlab<-attr(p1,'args')['var_map']
    p1<- attr(p1,"my_rst")
    p2<-if(length(vals$saved_maps)>1){
      attr(vals$saved_maps[[input$saved_maps2]],"my_rst")
    } else{NULL}

    rgl.open()
    rgl.bg(color=c('white','white'))
    anim_4D(r1=p1,r2=p2,r3=NULL,colors=input$pt_palette, exp=input$surf_exp,   newcolhabs=vals$newcolhabs)
    rglwidget()

  })

  output$pclus_tools<-renderUI({
    column(12,
           fluidRow(
             popify(bsButton("downp_pclus", span(icon("fas fa-download"),icon("fas fa-image")),style  = "button_active", type="action"),NULL,"Download plot",
                    options=list(container="body"))
           )
    )
  })
  output$pop_pcorr<-renderUI({
    fluidRow(class="map_control_style",style="color: #05668D",
             div(

               span("+ ",
                    inline(checkboxInput("varfacmap_action", span("Variable factor map",actionLink("varfacmap", tipify(icon("fas fa-question-circle"), "Click for more details"))),value =T, width="100px")))
             ),
             conditionalPanel("input.varfacmap_action % 2", {
               column(12,
                      div(span("+",inline(
                        pickerInput("var_corr_codes",NULL,
                                    choices = c("Highest correlations", "Clockwise-correlations"), width="150px"
                        )))),
                      div(
                        span("+ Number",
                             inline(
                               tipify(
                                 numericInput("npic", NULL, value = 10, min = 2, width="75px"),"Number of variables to display"
                               )))
                      ))
             }),
             br(),
             div(span("+ Display:",inline(
               radioButtons(
                 "bmu_dotlabel",NULL , choices = c("labels", "symbols"),selected=bmu_dotlabel$df,inline=T, width="100px")
             ))),

             div(span("+ Obs color:",inline(
               tipify(
                 pickerInput(inputId = "bmu_facpalette",
                             label =NULL,
                             choices = vals$colors_img$val,
                             choicesOpt = list(content = vals$colors_img$img),
                             selected=bmu_facpalette$df, width="75px"),
                 "Symbol colors. Choose a gradient to color observations by a factor"
               )
             ))),
             uiOutput("bmu_fac_control"),
             conditionalPanel("input.bmu_dotlabel=='symbols'",
                              div(span("+ Shape",inline(
                                tipify(
                                  pickerInput(inputId = "bmu_symbol",
                                              label = NULL,
                                              choices = df_symbol$val,
                                              choicesOpt = list(content = df_symbol$img),

                                              selected=bmu_symbol$df, width="75px")
                                  ,"symbol shape"
                                )
                              ))))
             ,
             div(span("+ Size:",
                      inline(
                        tipify(numericInput("bmu_symbol_size",strong(),value = vals$bmu_symbol_size,min = 0.1,max = 3,step = .1, width="75px"),"symbol size")
                      ))),
             div(span("+ Background",
                      inline(
                        tipify(
                          pickerInput(inputId = "bmu_bgpalette",
                                      label = NULL,
                                      choices = vals$colors_img$val,
                                      choicesOpt = list(content = vals$colors_img$img),

                                      selected=bmu_bgpalette$df, width="75px"),
                          "Color of grid units"
                        )
                      )
             )),
             div(span("+ Transparency:",
                      inline( tipify(
                        numericInput("bmu_bg_transp",NULL,
                                     value=vals$bmu_bg_transp, min=0, max=1,step=0.1, width="75px"),
                        "Background transparency"))
             )),
             div(span("+ Border", inline(
               tipify(
                 pickerInput(inputId = "bmu_border_grid",
                             label =NULL,
                             choices =  vals$colors_img$val[getsolid_col()] ,
                             choicesOpt = list(
                               content =  vals$colors_img$img[getsolid_col()] ),
                             selected= vals$colors_img$val[getsolid_col()] [7], width="75px"),
                 "Grid border color"
               )
             ))),
             div(span("+ Var color:",inline(
               tipify(
                 pickerInput(
                   inputId = "bmu_var_color",
                   label =NULL,
                   choices =  vals$colors_img$val[getsolid_col()] ,
                   choicesOpt = list(
                     content =  vals$colors_img$img[getsolid_col()] ), width="75px",
                   options=list(container="body")
                 ),
                 "Variable color"
               )
             ))),
             div(span("+ Var size",
                      inline(
                        tipify(numericInput("bmu_cexvar",NULL,value = vals$bmu_cexvar,min = 0.1,max = 3,step = .1, width="75px"),"variable text size (only for the variable factor map)")
                      ))),
             uiOutput("bmu_legend_out"),
             div(
               tipify(
                 actionLink(
                   'downp_bmu',span("+ Download",span(icon("fas fa-download"),icon("fas fa-image")))
                 ),
                 "download BMU plot",     options=list(container="body")
               )
             ),

             div(
               tipify(
                 actionLink(
                   'down_pcorr_results',span("+ Download",span(icon("fas fa-download"),icon("fas fa-table")))
                 ),
                 "download variable factor results",     options=list(container="body")
               )
             )





    )

  })
  output$bmu_legend_out<-renderUI({
    req(input$bmu_dotlabel=="symbols")
    col<-getcolhabs(vals$newcolhabs,input$bmu_facpalette,2)
    req(col[1]!=col[2])

    div(
      actionLink('bmu_legend' ,h5("+ Legend adjustment:",style="color: blue")),
      uiOutput('pcorr_legcontrol')
    )
  })
  output$pop_pclus<-renderUI({
    fluidRow(class="map_control_style",style="color: #05668D",
             div(span("+ Display:",inline(
               radioButtons(
                 "dot_label_clus", NULL, choices = c("labels", "symbols"), inline=T, width="100px")
             ))),
             div(span("+ Obs color",inline(
               tipify(pickerInput(inputId = "pclus_facpalette",
                                  label =NULL,
                                  choices = vals$colors_img$val,
                                  choicesOpt = list(content = vals$colors_img$img),
                                  selected=vals$colors_img$val[2],
                                  options=list(container="body"), width="75px"),
                      "Symbol colors. Choose a gradient to color observations by a factor"
               )
             ))),
             uiOutput("pclus_fac_control"),
             div(span(
               "+ size",inline(
                 tipify(numericInput("pclus_symbol_size",NULL,value = 1,min = 0.1,max = 3,step = .1, width="75px"),"symbol size")
               )
             )),
             conditionalPanel("input.dot_label_clus=='symbols'",{
               div(span("+ Shape",
                        tipify(pickerInput(inputId = "pclus_symbol",
                                           label = NULL,
                                           choices = df_symbol$val,
                                           choicesOpt = list(content = df_symbol$img),
                                           options=list(container="body"), width="75px")
                               ,"symbol shape")))
             })
             ,




             uiOutput('pclus_legcontrol')

    )



  })
  observeEvent(input$bmu_legend,{
    vals$bmuleg<-if(isFALSE(vals$bmuleg)){T} else if(isTRUE(vals$bmuleg)){F}

  })
  output$pcorr_legcontrol<-renderUI({
    req(isTRUE(vals$bmuleg))
    column(12,
           div(span("+ leg x",inline(
             tipify(numericInput("bmu_insertx",NULL,value=vals$bmu_insertx,step=0.05, width="75px"),"legend position relative to the x location")
           ))),
           div(span("+ leg y",
                    inline(
                      tipify(numericInput("bmu_inserty",NULL,value=vals$bmu_inserty,step=0.05, width="75px"),"legend position relative to the y location")
                    ))),
           div(span("+ ncol leg",
                    inline(
                      tipify(numericInput("bmu_ncol",NULL,value=vals$bmu_ncol,step=1, width="75px"),"the number of columns in which to set the legend items")
                    ))),
           div(span("+ bg leg",
                    inline(
                      tipify(numericInput("bmu_leg_transp",NULL,value=vals$bmu_leg_transp,step=0.05, max=1, width="75px"),"Legend background transparency")
                    )))
    )


  })
  output$pclus_legcontrol<-renderUI({
    col<-getcolhabs(vals$newcolhabs,input$pclus_facpalette,2)
    req(col[1]!=col[2])

    {
      div(
        div(span("+ leg x:",tipify(numericInput("insertx_pclus",NULL,value=0,step=0.05, width="75px"),"legend position relative to the x location"))),
        div(span("+ leg y:",tipify(numericInput("inserty_pclus",NULL,value=0.4,step=0.05, width="75px"),"legend position relative to the y location"))),
        div(span("+ ncol leg:",tipify(numericInput("ncol_pclus",NULL,value=1,step=1, width="75px"),"the number of columns in which to set the legend items"))),
        div (span("+ bg leg:",tipify(numericInput("bgleg_pclus",NULL,value=0.85,step=0.05, max=1, width="75px"),"Legend background transparency")))
      )
    }
  })
  output$pcorr_control <- renderUI({
    column(12,style="background: white",
           p(strong(h4("Best matching units"))),
           sidebarLayout(
             sidebarPanel(uiOutput("pop_pcorr", width=300)),
             mainPanel(
               div( plotOutput('pCorrCodes'),
                    #renderPrint({
                    #m=vals$som_results
                    #grid<-m$grid$pts
                    #dtopo<-unit.distances(m$grid)
                    #dcodes<-object.distances(m,"codes")
                    #res<-unlist(lapply(codes, function (x)  sum(dist(x)/dist(dtopo))))
                    #sort(res, dec=T)})

               )
             )
           ))
  })
  observeEvent(input$down_pcorr_results,{
    vals$hand_down<-"pcorr"
    showModal(downcenter())
  })
  output$pclus_control <- renderUI({
    column(12,
           p(strong(h4("Codebook clusters"))),
           uiOutput("pclus_tools"),
           sidebarLayout(
             sidebarPanel(uiOutput("pop_pclus")),
             mainPanel( plotOutput("pclus_code"))
           )
    )

  })
  output$bmu_fac_control<-renderUI({
    col<-getcolhabs(vals$newcolhabs,input$bmu_facpalette,2)


    if(input$bmu_dotlabel=='labels'|col[1]!=col[2])
    {span("+ Labels",
          inline(
            tipify(pickerInput("bmu_factors",NULL,
                               choices = c(colnames(attr(getdata_som(),"factors"))), selected=vals$cur_bmu_factors, width="75px"),"color observations by factor")
          )
    )
    }})
  output$pclus_fac_control<-renderUI({
    col<-getcolhabs(vals$newcolhabs,input$pclus_facpalette,2)


    req(input$dot_label_clus == 'labels'|col[1]!=col[2])
    span("+ Labels:", inline(
      pickerInput("pclus_factors",NULL,
                  choices = c(colnames(attr(getdata_hc(),"factors"))), width="75px")
    ))


  })
  BMUs<-reactive({
    req(length(input$varfacmap_action)>0)
    if (isTRUE(input$varfacmap_action)) {
      npic=as.numeric(input$npic)} else{
        npic = 0
      }
    p <-pcorr(
      m=vals$som_results,
      npic = npic,
      indicate = var_corr_codes.reactive(),
      pch = as.numeric(bmu_symbol$df),
      labels.ind = bmu_factors_reac(),
      cex =as.numeric(vals$bmu_symbol_size),
      bg_palette = as.character(bmu_bgpalette$df),
      factor.pal=as.character(bmu_facpalette$df),
      points=vals$bmu_points,
      cex.var=vals$bmu_cexvar,
      ncol=vals$bmu_ncol,
      insetx=vals$bmu_insertx,
      insety=vals$bmu_inserty,
      alpha.legend=vals$bmu_leg_transp,
      alpha_bg=vals$bmu_bg_transp,
      border=getcolhabs(vals$newcolhabs,input$bmu_border_grid,1),

      col.text= getcolhabs(vals$newcolhabs,input$bmu_var_color,1),
      newcolhabs=vals$newcolhabs,

    )
    vals$bmus_plot<-recordPlot()
    p



  })
  output$pCorrCodes <- renderPlot({
    #req(input$varfacmap_action)
    #req(input$npic)
    p<-BMUs()
    vals$pcorr_results<-data.frame(attr(p,"result"))
    p
  })
  output$pchanges <- renderPlot({
    pchanges(vals$som_results)
  })
  output$pcounts <- renderPlot({
    pcounts(vals$som_results)
  })
  output$pUmatrix <- renderPlot({
    pUmatrix(vals$som_results)
  })
  output$pproperty <- renderPlot({
    req(input$variable_pproperty)
    pproperty(vals$som_results,
              input$variable_pproperty,
              input$variable_pproperty)
    vals$pprop_plot<-recordPlot()
  })
  bmu_symbol.reac <- reactive({
    if (input$topo == "hexagonal") {
      16
    } else if (input$topo == 'rectangular') {
      15
    }
  })
  center.reative <- reactive({
    switch(input$scale,
           "center" = T)

  })


  {
    introhelpmodal <- function() {
      modalDialog(
        div(textsom())
        ,
        title = "Self-Organizing Maps",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(input$introhelp, {
      showModal(introhelpmodal())
    })


  }
  #screeplothelpmodal
  {
    screeplothelpmodal <- function() {
      modalDialog(
        div(textscreeplot())
        ,
        title = "Scree plot",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(input$screeplothelp, {
      showModal(screeplothelpmodal())
    })


  }
  #voteshelpmodal
  {
    voteshelpmodal <- function() {
      modalDialog(
        div(textvotes())
        ,
        title = "Votes on the best number of clusters",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(input$voteshelp, {
      showModal(voteshelpmodal())
    })


    output$voteshelp <- renderText({
      if (input$votesh %% 2) {
        paste0(
          br(),
          p(
            icon("fas fa-exclamation-circle"),
            code("K"),
            "is passed to the",
            code("max.nc"),
            "argument"
          ),
          p(
            icon("fas fa-exclamation-circle"),
            code("min.nc"),
            "argument is fixed to 2"
          ),
          p(
            icon("fas fa-exclamation-circle"),
            code('index'),
            "argument fixed to:",
            code(
              "kl",
              "ch",
              "hartigan",
              "ccc",
              "scott",
              "marriot",
              "trcovw",
              "tracew",
              "friedman",
              "rubin",
              "cindex",
              "db",
              "silhouette",
              "duda",
              "pseudot2",
              "beale",
              "ratkowsky",
              "ball",
              "ptbiserial",
              "gap",
              "frey",
              "mcclain",
              "gamma",
              "gplus",
              "tau",
              "dunn",
              "sdindex",
              "dindex",
              "sdbw"
            ),
            ". Only indexes running safely are computed."
          ),
          p(
            icon("fas fa-exclamation-circle"),
            "The remaining arguments are fixed to their default values;"
          ),
          h4("NbClust {NbClust}"),
          getHelp('NbClust')
        )
      }
    })
  }
  #hchelpmodal
  {
    hchelpmodal <- function() {
      modalDialog(
        div(textclustering())
        ,
        title = "Hierarchical Clustering",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(input$hchelp, {
      showModal(hchelpmodal())
    })




  }
  #removemodal
  {
    removemodal <- function() {
      modalDialog(
        textremove(),
        title = "Remove",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(input$transfhelp, {
      showModal(transfmodal())
    })

    transfmodal <- function() {
      div(id="teste",
        modalDialog(
          texttransf(),
          title = "Transformation",
          footer = modalButton("close"),
          size = "m",
          easyClose = TRUE
        )
      )
    }

    observeEvent(input$Remove, {
      showModal(removemodal())
    })
  }
  #somgridmodal
  {
    somgridmodal <-reactive({

      modalDialog(
        textsomgrid(),
        title = h4(strong("somgrid")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )

    })

    observeEvent(input$somgridhelp, {
      showModal(somgridmodal())
    })

    output$somgridhelp <- renderText({
      if (input$somgridh %% 2) {
        paste0(br(),
               h4("somgrid {kohonen}"),
               getHelp('unit.distances'))
      } else if (input$kohonen %% 2) {
        getHelp('kohonen')
      }
    })


  }
  #supersommodal
  {
    supersommodal <- function() {
      modalDialog(
        textsupersom(),
        title = h4(strong("Supersom")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$supersomhelp, {
      showModal(supersommodal())
    })

    output$supersomhelp <- renderText({
      if (input$supersomh %% 2) {
        paste0(
          br(),
          h4("supersom {kohonen}"),
          p(
            icon("fas fa-exclamation-circle"),
            "Argument",
            code("X"),
            "fixed to your uploaded and pre-treated data;"
          ),
          p(
            icon("fas fa-exclamation-circle"),
            code("a1"),
            "and" ,
            code("a2") ,
            "refers to the",
            code("alpha"),
            "argument"
          ),
          p(
            icon("fas fa-exclamation-circle"),
            code("r1"),
            "and" ,
            code("r2") ,
            "refers to the",
            code("radius"),
            "argument"
          ),
          p(
            icon("fas fa-exclamation-circle"),
            "Arguments fixed to their default values:",
            code("whatmap"),
            ", " ,
            code("user.weights"),
            ", " ,
            code("dist.fcts"),
            ", " ,
            code("cores"),
            ", " ,
            code("init"),
            ", " ,
            code("normalizeDataLayers")
          ),
          getHelp('supersom')
        )
      } else if (input$kohonen %% 2) {
        getHelp('kohonen')
      }
    })


  }
  #varfacmapmodal
  {
    varfacmapmodal <- function() {
      modalDialog(
        textvarfacmap(),
        title = h4(strong("Variable factor map")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$varfacmap, {
      showModal(varfacmapmodal())
    })




  }
  #Propertymodal

  #scalehelpmodal
  {
    scalehelpmodal <- function() {
      modalDialog(
        textscale(),
        title =  h4(strong("Scale")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$scalehelp, {
      showModal(scalehelpmodal())
    })

    output$scalehelp_out <- renderText({
      if (input$scalehelph %% 2) {
        paste0(
          br(),
          h4("scale {base}"),
          p(
            icon("fas fa-exclamation-circle"),
            code("scale"),
            "argument",
            " fixed to",
            code("TRUE")
          ),
          getHelp(help(scale, "base"))

        )
      }
    })


  }
  #interpmodal
  {
    interpmodal <- function() {
      modalDialog(
        textinterp(),
        title =  h4(strong("Interpolation")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$interphelp, {
      showModal(interpmodal())
    })

    output$interphelp <- renderText({
      if (input$interph %% 2) {
        paste0(
          br(),
          p(
            icon("fas fa-exclamation-circle"),
            "only the argument ",
            code("idp"),
            " is available to be passed to ",
            code("idw"),
            " function. The the remaining arguments are fixed to their default values;"
          ),
          h4("idw {gstat}"),
          getHelp('krige')
        )
      }
    })


  }
  #hclustmodal
  {
    hclustmodal <- function() {
      modalDialog(
        texthclust(),
        title = h4(strong("Hierarchical Clustering")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$hclusthelp, {
      showModal(hclustmodal())
    })

    output$hclusthelp <- renderText({
      if (input$hclusth %% 2) {
        paste0(
          br(),
          p(
            icon("fas fa-exclamation-circle"),
            "Argument ",
            code("members"),
            " is fixed to =",
            code("NULL")
          ),

          h4("hclust {stats}"),
          getHelp('hclust')
        )
      }
    })


  }
  {
    pcahelpmodal <- function() {
      modalDialog(
        textpcahelp(),
        title = "pcahelp",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$pcahelp, {
      showModal(pcahelpmodal())
    })
    output$pcahelphelp <- renderText({
      if (input$pcahelph %% 2) {
        paste0(
          br(),
          h4("prcomp  {base}"),
          p(
            icon("fas fa-exclamation-circle"),
            "Default for the S3 method. The 'center' and 'scale' arguments are set in the panel",code("3.1 Tranformation"),"from the dashboard."
          ),

          getHelp('prcomp')
        )
      }
    })

  }
  #mdshelpmodal
  {
    mdshelpmodal <- function() {
      modalDialog(
        textmdshelp(),
        title = "mdshelp",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$mdshelp, {
      showModal(mdshelpmodal())
    })

    output$mdshelphelp <- renderText({
      if (input$mdshelph %% 2) {
        paste0(
          br(),
          h4("Multidimensional Scaling  {vegan}"),
          p(
            icon("fas fa-exclamation-circle"),
            "Only",
            code("distance"),
            "argument is passed to",
            code("metaMDS"),
            "function from 'vegan'",
            " . The remaining arguments are fixed to their default values;'"
          ),

          getHelp('metaMDS')
        )
      }
    })


  }
  #sugtopohelpmodal
  {
    sugtopohelpmodal <- function() {
      modalDialog(
        textsugtopohelp(),
        title = "Suggested topology",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$sugtopohelp, {
      showModal(sugtopohelpmodal())
    })



  }
  #rfbphelpmodal
  {
    rfbphelpmodal <- function() {
      modalDialog(
        textrfbphelp(),
        title = "Split moving window",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$rfbphelp, {
      showModal(rfbphelpmodal())
    })




  }

  #bphelpmodal
  {
    bphelpmodal <- function() {
      modalDialog(
        textbphelp(),
        title = "Split moving window",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$bphelp, {
      showModal(bphelpmodal())
    })




  }
  output$decostand <- renderText({
    if (input$decostand %% 2) {
      paste0(
        br(),
        h4("decostand {vegan}"),

        p(
          icon("fas fa-exclamation-circle"),
          "Only",
          code("method"),
          "argument is passed to",
          code("decostand"),
          "The remaining arguments are fixed to their default values;"
        ),
        getHelp('decostand')
      )


    } else if (input$vegan %% 2) {
      paste0(br(),
             h4(h4("vegan {package}")),
             getHelp('vegan'))
    }
  })
  {
    basemodal <- function() {
      modalDialog(
        textbase(),
        title = "base shape",
        footer = column(12,actionButton("base_back","back"),modalButton("close")),
        size = "l",
        easyClose = TRUE
      )
    }

    observeEvent(input$basehelp, {
      showModal(basemodal())
    })



  }
  {
    layermodal <- function() {
      modalDialog(
        textlayer(),
        title = "layer shape",
        footer =column(12,actionButton("layer_back","back"),modalButton("close")),
        size = "l",
        easyClose = TRUE
      )
    }

    observeEvent(input$layerhelp, {
      showModal(layermodal())
    })




  }


  output$codechunk_base <- renderPrint({
    codebase()
  })
  output$codechunk_layer <- renderPrint({
    codelayer()
  })
  smw_labinfo <- reactive({
    strong(
      "split moving window",
      tipify(
        actionLink("bphelp", icon("fas fa-question-circle")),
        "Performs split moving window to detect significant discontinuities in the relationship between the number of clusters and WSS values. Click for more information."
      )
    )
  })
  accrf_labinfo <- reactive({
    strong(
      "split moving window",
      tipify(
        actionLink("bphelp", icon("fas fa-question-circle")),
        "Performs split moving window to detect significant discontinuities in the relationship between the number of clusters and accRF values. Click for more information."
      )
    )
  })
  smw_data_WSS <- reactive({
    conditionalPanel("output.plot_data_WSS",

                     div(
                       column(
                         5,
                         style = "margin-top: 5px;",
                         checkboxInput("smw_data_WSS", value = T, smw_labinfo())
                       ),
                       column(
                         3,
                         tipify(strong("w"), "window size"),
                         numericInput(
                           "w_data_WSS",
                           NULL,
                           value = 6,
                           min = 2,
                           step = 2
                         )
                       )
                     ))
  })
  smw_model_WSS <- reactive({
    conditionalPanel("output.plot_model_WSS", {
      div(
        column(
          5,
          style = "margin-top: 5px;",
          checkboxInput("smw_model_WSS", value = T, smw_labinfo())
        ),
        column(
          3,
          tipify(strong("w"), "window size"),
          numericInput(
            "w_model_WSS",
            NULL,
            value = 6,
            min = 2,
            step = 2
          )
        )
      )
    })

  })
  smw_data_accRF <- reactive({
    conditionalPanel("output.plot_data_accRF", {
      div(
        column(
          5,
          style = "margin-top: 5px;",
          checkboxInput("smw_data_accRF", value = T,
                        accrf_labinfo())
        ),
        column(
          3,
          tipify(strong("w"), "window size"),
          numericInput(
            "w_data_accRF",
            NULL,
            value = 6,
            min = 2,
            step = 2
          )
        )
      )
    })


  })
  smw_model_accRF <- reactive({
    conditionalPanel("output.plot_model_accRF", {
      div(
        column(
          5,
          style = "margin-top: 5px;",
          checkboxInput("smw_model_accRF", value = T, accrf_labinfo())
        ),
        column(
          3,
          tipify(strong("w"), "window size"),
          numericInput(
            "w_model_accRF",
            NULL,
            value = 6,
            min = 2,
            step = 2
          )
        )
      )
    })
  })
  output$data_WSS <- renderUI({
    column(
      12,
      column(2, uiOutput("K_WSS_data")),
      column(
        2,
        actionButton("getWSSdata", "run", style = "margin-top: 20px;")
      ),

      smw_data_WSS(),
      column(12, uiOutput('plot_data_WSS'))


    )
  })
  output$model_WSS <- renderUI({
    column(
      12,
      column(2,
             uiOutput("K_WSS_model")),
      column(
        2,
        actionButton("getWSSmodel", "run", style = "margin-top: 20px;")
      ),

      column(12, smw_model_WSS()),
      column(12, uiOutput('plot_model_WSS'))

    )


  })
  output$data_accRF <- renderUI({
    column(
      12,
      column(3, uiOutput("input_accRF_data")),
      column(2, uiOutput("K_accRF_data")),
      column(
        2,
        actionButton("getaccRFdata", "run", style = "margin-top: 20px;")
      ),
      smw_data_accRF(),
      column(12, uiOutput('plot_data_accRF'))

    )
  })
  output$model_accRF <- renderUI({
    column(
      12,
      column(3, uiOutput("input_accRF_model")),
      column(2, uiOutput("K_accRF_model")),
      column(
        2,
        actionButton("getaccRFmodel", "run", style = "margin-top: 20px;")
      ),
      smw_model_accRF(),
      column(12, uiOutput('plot_model_accRF'))


    )
  })
  screeplot_control <- reactive({
    column(
      12,
      style = "margin-left: 10px; margin-top: 10px",
      div(strong(
        "Scree plot", actionLink('screeplothelp', icon("fas fa-question-circle"))
      ), style = "margin-bottom: 5px;"),
      radioButtons(
        "screeplot_type",
        NULL,
        choices = c("WSS", "accRF"),
        inline = T
      ),
      uiOutput("screeplot_control2")
    )

  })
  output$screeplot_control2 <- renderUI({
    fluidRow(
      conditionalPanel(
        "input.model_or_data=='data' & input.screeplot_type=='WSS'",
        {
          uiOutput("data_WSS")
        }
      ),
      conditionalPanel(
        "input.model_or_data=='som codebook' & input.screeplot_type=='WSS'",
        {
          uiOutput("model_WSS")
        }
      ),
      conditionalPanel(
        "input.model_or_data=='data' & input.screeplot_type=='accRF'",
        {
          uiOutput("data_accRF")
        }
      ),
      conditionalPanel(
        "input.model_or_data=='som codebook' & input.screeplot_type=='accRF'",
        {
          uiOutput("model_accRF")
        }
      )

    )
  })
  WSSdata <- eventReactive(input$getWSSdata, {
    withProgress(message = "Running elbow analysis... this may take a while....",
                 min = 1,
                 max = 1,
                 {
                   getelbow(
                     getdata_hc(),
                     k = input$K_WSS_data,
                     type = "data",
                     method = input$method.hc0,
                     dist = input$disthc
                   )
                 })

  })
  WSSmodel <- eventReactive(input$getWSSmodel, {
    withProgress(message = "Running elbow analysis... this may take a while....",
                 min = 1,
                 max = 1,
                 {
                   getelbow(
                     getmodel_hc(),
                     k = input$K_WSS_model,
                     type = "som",
                     method = input$method.hc0,
                     dist = input$disthc
                   )
                 })

  })
  suggsKmodel <- reactive({
    selectInput(
      "suggsKmodel",
      strong("suggested K"),
      selectize=T,
      choices = c(
        paste(
          vals$saved_kmodelWSS,
          attr(vals$saved_kmodelWSS, "suggmethod")
        ),
        paste(
          vals$saved_kmodelaccRF,
          attr(vals$saved_kmodelaccRF, "suggmethod")
        ),
        paste(
          vals$saved_kmodelvotes,
          attr(vals$saved_kmodelvotes, "suggmethod")
        )
      )
    )
  })
  output$suggsKmodel <- renderUI({
    suggsKmodel()
  })
  picK <- reactive({
    req(input$usesuggK)
    req(input$model_or_data)


    if (input$usesuggK == "suggested")
    {


      if (length(input$suggsKmodel) > 0 | length(input$suggsKdata) > 0) {
        if (input$model_or_data == "data") {
          req(input$suggsKdata)
          as.numeric(gsub("([0-9]+).*$", "\\1", input$suggsKdata))
        } else if (input$model_or_data == "som codebook") {
          req(input$suggsKmodel)
          as.numeric(gsub("([0-9]+).*$", "\\1", input$suggsKmodel))
        }

      }
    } else  if (input$usesuggK == "user-defined") {
      if (input$model_or_data == "data") {
        req(input$customKdata)
        as.numeric(input$customKdata)
      } else if (input$model_or_data == "som codebook") {
        req(input$customKmodel)
        as.numeric(input$customKmodel)
      }



    }

  })
  suggsKdata <- reactive({
    selectInput(
      "suggsKdata",
      strong("suggested K"),
      selectize=T,
      choices = c(
        paste(vals$saved_kdataWSS, attr(vals$saved_kdataWSS, "suggmethod")),
        paste(
          vals$saved_kdataaccRF,
          attr(vals$saved_kdataaccRF, "suggmethod")
        ),
        paste(
          vals$saved_kdatavotes,
          attr(vals$saved_kdatavotes, "suggmethod")
        )
      )
    )
  })
  observeEvent(input$customK,
               vals$saved_kcustom <- isolate(input$customK))

  output$suggsKdata <- renderUI({
    suggsKdata()
  })
  output$customKdata <- renderUI({
    numericInput("customKdata",
                 "Number of clusters",
                 value = 2,
                 step = 1)


  })
  output$customKmodel <- renderUI({
    numericInput("customKmodel",
                 "Number of clusters",
                 value = 2,
                 step = 1)
  })
  usesuggK <- reactive({
    if (length(vals$saved_kdataWSS) > 0 |
        length(vals$saved_kmodelWSS) > 0 | length(vals$saved_kmodelvotes) > 0) {
      radioButtons("usesuggK",
                   "Cluster type",
                   c("user-defined", "suggested"),
                   selected = "suggested")
    } else {
      radioButtons("usesuggK",
                   "Cluster type",
                   c("user-defined"),
                   selected = "user-defined")
    }



  })
  output$usesuggK <- renderUI({
    usesuggK()
  })
  output$suggestedK <- renderUI({
    column(
      12,
      splitLayout(
        column(12,uiOutput('usesuggK')),

        fluidRow(
          conditionalPanel("input.model_or_data=='data'",{
            splitLayout(
              column(12,
                     conditionalPanel("input.usesuggK=='user-defined'",{
                       uiOutput("customKdata")
                     }),

                     conditionalPanel(
                       "input.usesuggK=='suggested'",
                       uiOutput("suggsKdata")
                     )
              ),
              column(12,
                     p(strong("Palette", style="color: #0D47A1")),
                     pickerInput(inputId = "hcdata_palette",
                                 label = NULL,
                                 choices = vals$colors_img$val,
                                 choicesOpt = list(content = vals$colors_img$img)))

            )

          }),

          conditionalPanel(
            "input.model_or_data=='som codebook'",  {
              splitLayout(
                column(12,
                       conditionalPanel("input.usesuggK=='user-defined'",{
                         uiOutput("customKmodel")
                       }),
                       conditionalPanel(
                         "input.usesuggK=='suggested'",
                         uiOutput("suggsKmodel")
                       )
                ),
                column(12,
                       p(strong("Palette", style="color: #0D47A1")),
                       pickerInput(inputId = "hcmodel_palette",
                                   label = NULL,
                                   choices = vals$colors_img$val,
                                   choicesOpt = list(content = vals$colors_img$img),
                                   options=list(container="body")
                       ))
              )

            }
          )
        ),cellWidths = c("40%","60%")


      )


    )
  })
  sugg_accRF_data <- reactive({
    if (input$smw_data_accRF %% 2)
    {
      rfs <- accRFdata()
      res <- rfs$acutable
      bp = smw_bp(res, ws = input$w_data_accRF)
      attr(bp, "suggmethod") <- c("(accRF)")
      vals$saved_kdataaccRF <- isolate(bp)
      bp
    } else {
      NULL
    }
  })
  sugg_accRF_model <- reactive({
    if (input$smw_model_accRF %% 2)
    {
      rfs <- accRFmodel()
      res <- rfs$acutable
      bp <- smw_bp(res, ws = input$w_model_accRF)
      attr(bp, "suggmethod") <- c("(accRF)")
      vals$saved_kmodelaccRF <- isolate(bp)
      bp
    } else {
      NULL
    }
  })
  sugg_WSS_data <- reactive({
    if (input$smw_data_WSS %% 2)
    {
      res <- WSSdata()
      bp <- smw_bp(res, ws = input$w_data_WSS)
      attr(bp, "suggmethod") <- c("(WSS)")
      vals$saved_kdataWSS <- isolate(bp)
      bp
    } else {
      NULL
    }
  })
  sugg_WSS_model <- reactive({
    if (input$smw_model_WSS %% 2)
    {
      res <- WSSmodel()
      bp <- smw_bp(res, ws = input$w_model_WSS)
      attr(bp, "suggmethod") <- c("(WSS)")
      vals$saved_kmodelWSS <- isolate(bp)
      bp

    } else {
      NULL
    }
  })
  output$plot_data_WSS <- renderUI({
    validate(need(input$getWSSdata %% 2,""))
    fluidRow(
      column(12,
             fluidRow(
               actionButton('downr_WSSdata',icon("fas fa-table"),icon("fas fa-download"), style="button_active"),
               actionButton('downp_WSSdata',icon("fas fa-image"),icon("fas fa-download"), style="button_active"),
             )),
      column(12,renderPlot(elbow_plot(WSSdata(), sugg = sugg_WSS_data())))
    )
  })
  output$plot_model_WSS <- renderUI({
    validate(need(input$getWSSmodel %% 2,""))
    fluidRow(
      column(12,
             fluidRow(
               actionButton('downr_WSSmodel',icon("fas fa-table"),icon("fas fa-download"), style="button_active"),
               actionButton('downp_WSSmodel',icon("fas fa-image"),icon("fas fa-download"), style="button_active"),
             )),
      column(12,renderPlot(elbow_plot(WSSmodel(), sugg = sugg_WSS_model())))
    )
  })
  output$plot_data_accRF <- renderUI({
    validate(need(input$getaccRFdata %% 2,""))
    fluidRow(
      column(12,
             fluidRow(
               actionButton('downr_accRFdata',icon("fas fa-table"),icon("fas fa-download"), style="button_active"),
               actionButton('downp_accRFdata',icon("fas fa-image"),icon("fas fa-download"), style="button_active"),
             )),
      column(12,renderPlot(plot_accuclus(accRFdata(), sugg = sugg_accRF_data())))
    )
  })
  output$plot_model_accRF <- renderUI({
    validate(need(input$getaccRFmodel %% 2,""))
    fluidRow(
      column(12,
             fluidRow(
               actionButton('downr_accRFmodel',icon("fas fa-table"),icon("fas fa-download"), style="button_active"),
               actionButton('downp_accRFmodel',icon("fas fa-image"),icon("fas fa-download"), style="button_active"),
             )),
      column(12,renderPlot(plot_accuclus(accRFmodel(), sugg = sugg_accRF_model())))
    )
  })
  observeEvent(input$downr_accRFdata,{
    vals$hand_down<-"screeplot_accRF data"
    showModal(downcenter())
  })
  observeEvent(input$downr_accRFmodel,{
    vals$hand_down<-"screeplot_accRF som"
    showModal(downcenter())
  })
  observeEvent(input$downr_WSSdata,{
    vals$hand_down<-"screeplot_WSS data"
    showModal(downcenter())
  })
  observeEvent(input$downr_WSSmodel,{
    vals$hand_down<-"screeplot_WSS som"
    showModal(downcenter())
  })
  output$input_accRF_data <- renderUI({
    fluidRow(
      tipify(
        strong("accRF data"),
        "Select the data to validate the optimum number of clusters"
      ),
      selectInput(
        "input_accRF_data",
        NULL,
        choices =    names(vals$saved_data),
        selectize=T
      )
    )
  })
  output$input_accRF_model <- renderUI({
    fluidRow(
      tipify(
        strong("accRF data"),
        "Select the data to validate the optimum number of clusters"
      ),
      selectInput(
        "input_accRF_model",
        NULL,
        choices =    names(vals$saved_data),
        selectize=T
      )
    )
  })
  accRFdata <- eventReactive(input$getaccRFdata, {
    get <- as.character(input$input_accRF_data)
    envi <- data.frame(vals$saved_data[[get]])
    machineRF(
      getdata_hc(),
      data = envi,
      k = input$K_accRF_data,
      type = "data",
      method = input$method.hc0,
      dist = input$disthc
    )
  })
  accRFmodel <- eventReactive(input$getaccRFmodel, {
    get <- as.character(input$input_accRF_model)
    envi <- data.frame(vals$saved_data[[get]])
    machineRF(
      getmodel_hc(),
      data = envi ,
      k = input$K_accRF_model,
      type = "som",
      method = input$method.hc0,
      dist = input$disthc,
      newcolhabs=vals$newcolhabs
    )
  })
  sugg <- reactive({
    if (input$smw_elbow %% 2)
    {
      smw_bp(elbow(), ws = input$window_elbow)
    } else {
      NULL
    }
  })
  K_accRF_data <- reactive({
    fluidRow(strong("k"),
             numericInput("K_accRF_data", NULL, value = Kdata(getdata_hc())))
  })
  K_accRF_model <- reactive({
    fluidRow(strong("k"),
             numericInput("K_accRF_model", NULL, value = Kmodel(getmodel_hc())))
  })
  K_WSS_data <- reactive({
    fluidRow(strong("k"),
             numericInput("K_WSS_data", NULL, value = Kdata(getdata_hc())))
  })
  K_WSS_model <- reactive({
    fluidRow(strong("k"),
             numericInput("K_WSS_model", NULL, value = Kmodel(getmodel_hc())))
  })
  output$K_accRF_data <- renderUI({
    K_accRF_data()
  })
  output$K_accRF_model <- renderUI({
    K_accRF_model()
  })
  output$K_WSS_data <- renderUI({
    K_WSS_data()
  })
  output$K_WSS_model <- renderUI({
    K_WSS_model()
  })
  output$clustering_panel <- renderUI({
    fluidRow(column(12,
                    column(
                      12,
                      uiOutput("hc_control")
                    ),
                    br(),
                    uiOutput("hc_panels")))
  })
  output$hc_panels<-renderUI({
    #req(input$model_or_data)
    #req(input$data_hc)
    fluidRow(style="margin-left:5px;",
             tabsetPanel(id="hc_tab",
                         tabPanel(strong("1. Dendrogram"),value="hc_tab1",
                                  column(12,actionButton('downp_dend',icon("fas fa-image"),icon("fas fa-download"), style="button_active")) ,
                                  column(12,uiOutput("Dendrogram"))),

                         tabPanel(
                           strong("2. Clustering Tools"),
                           value = "hc_tab2",
                           div(style = "background: white", tabsetPanel(
                             tabPanel(
                               strong("Scree plot"),value="hc_tab2_1",
                               column(12, style = "background: white",
                                      uiOutput('screeplot_control'))
                             )
                           ))
                         ),
                         tabPanel(
                           strong('3. Clustering results'),value="hc_tab3",
                           uiOutput("suggestedK"),
                           tabsetPanel(id = "hc_results",

                                       tabPanel(
                                         "3.1. Hcut",value ="Hcut",
                                         column(12,  style = "background: Snow;",
                                                uiOutput("phc"))
                                       ))

                         )
             ))
  })
  Tabs_HC <- reactive({
    if (anyNA(getdata_hc())) {
      column(
        12,
        strong(
          "The selected dataset contais missing values which are not allowed in this functionality. Please switch the dataset or go to the Upload menu for creating a new dataset without missing values.",
          style = "color: red;"
        )
      )
    }
  })
  output$new_fac_hc <- renderPrint({
    attr(getdata_hc(),"factors")
  })
  output$factor_bank_hc <- renderUI({
    column(12,
           br(),
           br(),
           column(
             12,
             strong("Factor bank", style = "color: Green;"),
             br(),
             div(verbatimTextOutput("new_fac_hc"), style = "width: 600;height:200px;overflow-y: scroll; overflow-x: scroll"),
             br(),
             br()
           ))
  })
  output$Tabs_HC <- renderUI({
    Tabs_HC()
  })
  observe({
    req(input$model_or_data)
    req(input$data_hc)
    if (input$model_or_data == "som codebook")
    {
      if(isFALSE(vals$insertbmu)){
        insertTab(
          inputId = "hc_results",
          tabPanel(
            "3.2. SOM codebook clusters",
            value = "bmu_tab",
            style = "background: Snow;",
            fluidRow(style = "background: Snow;",
                     uiOutput("pclus_control"))
          ),
          target = "Hcut",position="after",
        )
        vals$insertbmu<-T
      }
    }
  })
  observe({
    req(input$model_or_data)
    req(isTRUE(vals$insertbmu))
    if(input$model_or_data == "data"){
      removeTab("hc_results","bmu_tab")
      vals$insertbmu<-F
    }
  })
  output$screeplot_control <- renderUI(screeplot_control())
  output$hc_control <- renderUI({
    req(length(vals$saved_data)>0)
    column(12,
           splitLayout(cellWidths = c("20%","30%","25%","25%"),
                       uiOutput("choicesHC"),
                       uiOutput("data_hc"),
                       fluidRow(
                         conditionalPanel("input.model_or_data=='som codebook'",
                                          {
                                            column(12, uiOutput("somHC"))
                                          }),

                         conditionalPanel("input.model_or_data=='data'",
                                          column(12, uiOutput("disthc"))

                         )
                       ),

                       selectInput(
                         "method.hc0",
                         "method.hc",
                         choices = c("ward.D2", "ward.D", "single", "complete", "average"), selectize=T
                       )
           )
           ,uiOutput("saveHC")

    )




  })
  output$data_hc<-renderUI({
    column(12,selectInput("data_hc",
                          "Datalist::",
                          choices =    names(vals$saved_data),
                          selectize=T, selected=vals$cur_data))
  })
  output$choicesHC<-renderUI({

    radioButtons("model_or_data", "Clustering target", choices = choices_hc(), selected=c(choices_hc()[length(choices_hc())]))
  })
  output$saveHC<-renderUI({
    req(input$data_hc)

    fac<-as.list(attr(vals$saved_data[[input$data_hc]],"factors"))
    fac<-lapply(fac,function(x) as.character(x))
    vals$baghc<-!any(unlist(lapply(fac, function(x) identical(x, as.character(phc()$somC)))))

    if(isTRUE(vals$baghc)) {column(12,offset=8,
                                   absolutePanel(style="margin-top: 8px",popify(bsButton("tools_savehc", icon("fas fa-save"),style  = "button_active", type="action",value=FALSE),NULL,
                                                                                "Save clusters"
                                   )))} else{
                                     column(12,offset=8,
                                            absolutePanel(style="margin-top: 8px",em("Clusters saved in the Factor-Attributte"))



                                     )
                                   }
  })
  output$somHC<-renderUI({

    selectInput(
      "som_hc",
      "som codebook:",
      choices = names(attr(getdata_hc(),"som")),
      selectize=T
    )
  })
  observeEvent(input$tools_savehc,{
    if(input$tools_savehc %% 2) {
      vals$hand_save<-"Save Clusters"
      vals$hand_save2<-p(p(em(input$data_hc,style="color: gray"),strong("::"),em("Factor-Attribute",style="color: gray"), strong("::")))
      vals$hand_save3<-NULL
      showModal(
        hand_save_modal()
      )
    }
  })
  output$disthc <- renderUI({
    selectInput(
      "disthc",
      "distance",
      choices = c('bray', "euclidean", 'jaccard'),
      selectize=T
    )
  })
  sc_rf_elbow <- reactive({
    fluidRow(
      conditionalPanel("input.screeplot_type=='WSS'", {
        column(12,
               uiOutput("elbowcontrol"))
      }),
      conditionalPanel("input.screeplot_type=='accRF'", {
        column(12,
               uiOutput('rfloopcontrol'))
      })





    )
  })
  labhc <- reactive({
    req(input$labhc)
    as.character(attr(getdata_hc(),"factors")[rownames(getdata_hc()),as.character(input$labhc)])
  })
  output$hcdata_plot <- renderPlot({
    req(input$method.hc0)
    req(input$model_or_data)
    plot(cutdata(getdata_hc(), input$method.hc0),labels = as.character(labhc()),main = "Dendrogram")
    vals$pdend_plot<-recordPlot()
  })
  output$hcmodel_plot <- renderPlot({
    req(input$method.hc0)
    req(input$model_or_data)
    plot(cutm(getmodel_hc(), input$method.hc0), main = "Dendrogram")
  })
  hc0panel <- reactive({
    req(input$model_or_data)
    column(12,
           br(),
           column(
             12,
             conditionalPanel(
               "input.model_or_data=='data'",
               uiOutput('hclabs_control')
             ),
             conditionalPanel("input.model_or_data=='data'", {
               fluidRow(column(12, plotOutput("hcdata_plot")),
                        column(12, downloadButton('down_hcdata', 'Download')))

             }),
             conditionalPanel("input.model_or_data=='som codebook'", {
               fluidRow(column(12, plotOutput("hcmodel_plot")),
                        column(12, downloadButton('down_hcmodel', 'Download')))
             }),
           ))

  })
  output$hclabs_control <-renderUI({
      column(6,
             selectInput(
               "labhc",
               "Labels",
               choices = c(colnames(attr(getdata_hc(),"factors"))),
               selectize=T
             ))

    })
  output$Dendrogram <- renderUI({
    hc0panel()
  })
  downplot_center<-reactive({
    modalDialog(
      column(12,
             splitLayout(
               numericInput("fheight", "Height (cm)", min=2, max=15, step=1, value = 15),
               numericInput("fwidth", "Width (cm)", min=2, max=15, step=1, value = 20),
               selectInput("fres", "Res", choices=c("100","200","300"), selected = "100"),
               numericInput("pointsize", "pointsize",value=12,step=1),
               selectInput("fformat", "File type", choices=c("png","tiff","jpeg","pdf"), selected = "pdf", multiple = FALSE, selectize = TRUE),
               div(style="margin-top: 25px",downloadButton("bn_download",NULL, style="button_active")),

             ),

             column(12, withSpinner(type=8,color="SeaGreen",imageOutput("plotoutput")))
      ),
      title=p(strong("action:"),"download", vals$hand_plot),
      easyClose = T
    )
  })
  get_plot<-reactive({

  })
  fn_downloadf <- reactive({

    if(input$fformat=="png") filename <- paste0(vals$hand_plot,".png",sep="")
    if(input$fformat=="tiff") filename <- paste0(vals$hand_plot,".tif",sep="")
    if(input$fformat=="jpeg") filename <- paste0(vals$hand_plot,".jpg",sep="")
    if(input$fformat=="pdf") filename <- paste0(vals$hand_plot,".pdf",sep="")
    return(filename)
  })







  output$bn_download <- downloadHandler(
    filename = fn_downloadf,
    content = function(file) {
      fn_download()
      dev.off()
      file.copy(fn_downloadf(), file, overwrite=T)
    }
  )

  ## observes PLOT
  observeEvent(input$downp_summ_num,{
    vals$hand_plot<-"variable summary"
    showModal(downplot_center())
  })
  observeEvent(input$downp_stats_fac,{
    vals$hand_plot<-"factor summary"
    showModal(downplot_center())
  })
  observeEvent(input$downp_hist,{
    vals$hand_plot<-"histogram"
    showModal(downplot_center())
  })
  observeEvent(input$mds_downp,{
    vals$hand_plot<-vals$bag_submenu
    showModal(downplot_center())
  })
  observeEvent(input$pca_downp,{
    vals$hand_plot<-vals$bag_submenu
    showModal(downplot_center())
  })
  observeEvent(input$rda_downp,{
    vals$hand_plot<-vals$bag_submenu
    showModal(downplot_center())
  })
  observeEvent(input$downp_pchanges,{
    vals$hand_plot<-"Training plot"
    showModal(downplot_center())
  })
  observeEvent(input$downp_pcounts,{
    vals$hand_plot<-"Couting plot"
    showModal(downplot_center())
  })
  observeEvent(input$downp_pmatrix,{
    vals$hand_plot<-"uMatrix"
    showModal(downplot_center())
  })
  observeEvent(input$downp_bmu,{
    vals$hand_plot<-"BMUs"
    showModal(downplot_center())
  })
  observeEvent(input$downp_bmu_pred,{
    vals$hand_plot<-"BMUs predictions"
    showModal(downplot_center())
  })
  observeEvent(input$downp_pproperty,{
    vals$hand_plot<-"property plot"
    showModal(downplot_center())
  })
  observeEvent(input$downp_dend,{
    vals$hand_plot<-"Dendrogram"
    showModal(downplot_center())
  })
  observeEvent(input$downp_accRFdata,{
    vals$hand_plot<-"screeplot_accRF data"
    showModal(downplot_center())
  })
  observeEvent(input$downp_accRFmodel,{
    vals$hand_plot<-"screeplot_accRF som"
    showModal(downplot_center())
  })
  observeEvent(input$downp_WSSdata,{
    vals$hand_plot<-"screeplot_WSS data"
    showModal(downplot_center())
  })
  observeEvent(input$downp_WSSmodel,{
    vals$hand_plot<-"screeplot_WSS som"
    showModal(downplot_center())
  })
  observeEvent(input$downp_hcut,{
    vals$hand_plot<-"Hcut"
    showModal(downplot_center())
  })
  observeEvent(input$downp_pclus,{
    vals$hand_plot<-"codebook clusters"
    showModal(downplot_center())
  })
  observeEvent(input$downp_dt,{
    vals$hand_plot<-"Decision Tree"
    showModal(downplot_center())
  })
  observeEvent(input$downp_map,{
    vals$hand_plot<-"Map"
    if(isTRUE(input$surface_map)){
      vals$hand_plot<-"Surface map"
    }
    if(isTRUE(input$scatter_3d)){
      vals$hand_plot<-"Scatter 3D"
    }

    if(isTRUE(input$mantel_map)){
      vals$hand_plot<-"mantel plot"
    }
    if(isTRUE(input$stack_map)){
      vals$hand_plot<-"stacked raster map"
    }
    if(isTRUE(input$stack_scatter_3d)){
      vals$hand_plot<-"stacked scatter map"
    }

    showModal(downplot_center())
  })
  #outputOptions(output, "tbl_order", suspendWhenHidden = FALSE)
  observeEvent(input$downp_dp,{

    vals$hand_plot<-"dp"
    showModal(downplot_center())
  })
  observeEvent(input$downp_pw,{

    vals$hand_plot<-"segrda"
    showModal(downplot_center())
  })
  observeEvent(input$downp_we,{
    vals$hand_plot<-"we"
    showModal(downplot_center())
  })
  observeEvent(input$downp_confsom,{
    vals$hand_plot<-"Confusion Matrix SOM"
    showModal(downplot_center())
  })
  observeEvent(input$down_som_pred_results,{
    vals$hand_down<-"som predictions"
    showModal(downcenter())

  })
  observeEvent(input$down_pcodes_results,{
    vals$hand_down<-"pcodes"
    showModal(downcenter())

  })


## transformations

  getdata_upload<-reactive({
    req(input$data_upload)
    data.rares()
    data= data_cogs$df

    data
  })


  observe({

    data<-getdata_upload()
    factors<-attr(data,"factors")
    factors_o<-attr(vals$saved_data[[input$data_upload]],"factors")
    bagdata<-identical(as.matrix(data),as.matrix(vals$saved_data[[input$data_upload]]))
    bagfac<-identical(as.matrix(factors),as.matrix(factors_o))
    vals$bagdata<-c(bagdata,bagfac)
  })



 # onevent("mouseleave","main_panel",toggle("upload_tools_in"))
  #onevent("mouseleave","main_panel",toggle("upload_tools_in"))
 # onevent("mouseenter","upload_tools",shinyjs::addClass('data_change',selector="body",class="data_change1"))
 # onevent("mouseleave","upload_tools",shinyjs::removeClass('data_change',selector="body",class="data_change1"))
  #onevent("mouseenter","sidebar-main",shinyjs::addClass('data_change',selector="body",class="data_change0"))



  observeEvent(input$cogs,{
    req(isTRUE(input$cogs))

   showModal(
      div(
        id="change_modal",
        modalDialog({
          div(

            uiOutput("tools_upload"),

            uiOutput("histo")

          )
        },
        footer = NULL,
        size="s"
        )
      )
    )


  })
  output$change_summary <- renderPrint({

    ppsummary("----------------")
    ppsummary(paste("Missing values:",sum(is.na(attr(getdata_upload0(),"factors")))))
    ppsummary("----------------")
    str(attr(getdata_upload0(),"factors")[rownames(getdata_upload0()),,drop=F])
  })



  output$histo<-renderUI({

    #req(length(vals$histo)>0)

    req(is.data.frame(data_cogs$df))
    fixedPanel(id="histo",top = 0,left="100%",      uiOutput("track_change")

    )
  })



  output$track_change<-renderUI({
    req(is.data.frame(data_cogs$df))
    div(style="background: white",
        tags$style(
          paste(paste0("#transf_attr"),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
        ),
        tags$style(
          paste0("#transf_attr"),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
        ),


        div(
          column(12,style="background-color: white",inline(DT::dataTableOutput('transf_attr'))),
          div(
            id="switch_histo",
            bsButton("show_histo", span("show hist",icon("fas fa-signal")), type="toggle", style="show_histo")
          ),
          column(12,style="background-color: white",div(uiOutput("histo_plot")))
        )
        ,

    )
  })

  output$histo_plot<-renderUI({
    req(isTRUE(input$show_histo))
    column(12,
      renderPlot({

        plothists(data_cogs$df,vals$newcolhabs, len=6)
      }, width=350, height=300),
      absolutePanel(top=0,left=0,right = 0,actionLink("close_histo",icon("fas fa-window-close")))
    )
  })

  output$transf_attr<-DT::renderDataTable({
    table<-  attr(data_cogs$df, 'transf')
    rownames(table)<-c("Selected observations",
                       "Selected variables",
                       "Transformation",
                       "Cut",
                       "Scale",
                       "Center",
                       "NA.omit",
                       "Data imputation",
                       "Factor imputation")
    req(length(table)>0)
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))
  })

observeEvent(input$close_histo,{
  updateButton(session,'show_histo', value=F)
})
  observeEvent(input$tools_drop9,{
    updateButton(session,'cogs', value=F)
    removeModal()
  })

   output$tools_upload<-renderUI({
   # req(isTRUE(input$cogs_toggle))
    #req(length(vals$saved_data)>0)
   # req(input$data_upload)
    req(input$tabs!="menu_intro")


    div( id="upload_tools",

      useShinyjs(),
        div( id="tools_drop0",
             column(12,style="color:white; background: SeaGreen",
               #uiOutput("tour_cur"),

               div(class = "col-sm-6", style = "padding-left:0",
                   h4(style="text-align:left; color:white; background: SeaGreen",
                      inline(span("Pre-processing"))),
               ),
               div(class = "col-sm-6", style = "padding-right:0",
                   div(style="text-align:right; ",
                       bsButton("tools_drop9",icon("fas fa-window-close"),
                                style ="close_change")
                   )
               )

             ),
             div(
             inline(div(id="data_change",
                   pickerInput(
                     "data_upload",NULL,choices=names(vals$saved_data),  selected=vals$cur_data, options=list(container="body","style-base" = "form-control"),
                     width="200px"
                   )
        )),
      tags$style(".nav {z-index:999}"),

      span(id="tools_drop1",inline(dropdown(icon = icon("fas fa-list"), status ="button_change" , size="sm", width="400px",uiOutput("tool1"), inline = T, circle=F)),
      ),
      span(id="tools_drop2",inline(dropdown(icon = icon("fas fa-list fa-rotate-90",verify_fa = FALSE), size="sm",status ="button_change" , inputId = "dropID_selecvar",width="350px",inline = T, circle=F, uiOutput("selectVar")))),
      span(id="tools_drop3",inline(
        dropdown(icon = icon("fas fa-hammer"),size="sm", status ="button_change" ,  inline = T, circle=F,width="350px",
                       uiOutput("pop_transform")
        )
      )),
      span(id="tools_drop4",inline(dropdownButton(label=img(src=na_icon,size="sm",height='15',width='15'), status ="button_change" ,  inline = T, circle=F,width="350px",inputId="na_pop",uiOutput("pop_na")))),
      span(id="tools_drop5",inline(dropdownButton(label=img(src=split_icon,size="sm",height='20',width='20'), status ="button_change",inline=T, circle=F,width="300px",inputId='dropID_split',
                                           uiOutput("pop_split")
      ))),
      span(id="tools_drop6",inline(
        dropdownButton(
          icon = icon("fas fa-tags"),size="sm", status ="button_change",inline=T, circle=F,width="400px",inputId="dropID_tag",
          uiOutput("pop_tag"))
      )),
      span(id="tools_palette",bsButton("tools_drop7",icon("fas fa-palette"),
                       style ="button_change")),
      inline(
        uiOutput("bttn_save_changes", inline=T)
      ),



      bsTooltip('tools_drop1',"Select observations", placement = 'top',
                options=list(
                  delay = list(show=10, hide=0))),
      bsTooltip('tools_drop2',"Select variables", placement = 'top',
                options=list(
                  delay = list(show=10, hide=0) )),
      bsTooltip('tools_drop3',"Transformation", placement = 'top',
                options=list(
                  delay = list(show=10, hide=0) )),
      bsTooltip('tools_drop4',"Data imputation", placement = 'top',
                options=list(
                  delay = list(show=10, hide=0) )),
      bsTooltip('tools_drop5',"Data split", placement = 'top',
                options=list(
                  delay = list(show=10, hide=0) )),
      bsTooltip('tools_drop6',"Edit factors", placement = 'top',
                options=list(
                  delay = list(show=10, hide=0) )),
      bsTooltip('tools_palette',"Palette toolbox", placement = 'top',
                options=list(
                  delay = list(show=10, hide=0) )),
      bsTooltip('tools_drop8',"Save changes", placement = 'top',
                options=list(
                  delay = list(show=10, hide=0) )),
      bsTooltip('data_change',"Datalist", placement = 'top',
                options=list(
                  delay = list(show=10, hide=0) )),
      bsTooltip('tools_drop9',"Close and Discard changes", placement = 'left',
                options=list(
                  delay = list(show=10, hide=0) ))

    )))
  })
  seldata <- reactive({
    data <-  vals$saved_data[[input$data_upload]]
    factors<-attr(data,"factors")
    if(length(input$filter_data)>0) {
      if (input$filter_data != "none")
      { if(length(input$cutlevel_obs)>0){

        pic <-which(as.character(factors[, input$filter_data]) %in% as.character(input$cutlevel_obs))
        data = data[pic,, drop=F ]}}
    }
    if(length(input$filter_datalist)>0){
      if(input$filter_datalist != "none")
      {
        pic<-rownames(vals$saved_data[[input$filter_datalist]])
        data=na.omit(data[pic,,drop=F])

      }
    }

if(length(input$selecvar) >0){
  if (any(input$selecvar%in%colnames(data))) {
    pic<-which(colnames(data)%in%input$selecvar)
    if(length(pic)>0)
    data <- data[,pic, drop=F]
  }
}

    #if (length(input$selecobs) > 0) {data <- data[input$selecobs, ]}

    req(length(vals$seltree)>0)
    data[   vals$seltree,]
  })
  data.rares<-reactive({

    #vals$bagdata <- isolate(0)
    data = seldata()
    if (isTRUE(input$na.omit)) {data <- na.omit(data)}

    if (length(input$transf) > 0) {
      if (any(input$scale == "scale")) {
        if (any(input$center == "center")) {
          data = data.frame(scale(data, center = T))
        } else {
          data = data.frame(scale(data, center = F))
        }
      }

    }

    if(length(input$rareabund)>0){
      if(isTRUE(input$rareabund)){
        data = pctRares(data, input$pct_rare/100)
      }
    }
    if(length(input$rarefreq)>0){
      if(isTRUE(input$rarefreq)){
        data = pctPrev(data, input$pct_prev/100)
      }
    }
    if(length(input$raresing)>0){
      if(isTRUE(input$raresing)){
        data = singles(data)
      }
    }

    if (length(input$transf) > 0) {
      if (input$transf == "log2") {data = decostand(data, "log", na.rm = T, logbase = 2)}
      if (input$transf == "log10") {data = decostand(data, "log", na.rm = T, logbase = 10)}
      if (input$transf == "total") {data = decostand(data, "total", na.rm = T)}
      if (input$transf == "max") {data = decostand(data, "max", na.rm = T)}
      if (input$transf == "frequency") {data = decostand(data, "frequency", na.rm = T)}
      if (input$transf == "range") {data = decostand(data, "range", na.rm = T)}
      if (input$transf == "pa") {data = decostand(data, "pa", na.rm = T)}
      if (input$transf == "chi.square") {data = decostand(data, "chi.square", na.rm = T)}
      if (input$transf == "hellinger") {data = decostand(data, "hellinger", na.rm = T)}
      if (input$transf == "sqrt2") {data = sqrt(data)}
      if (input$transf == "sqrt4") {data = sqrt(sqrt(data))}
      if (input$transf == "log2(x+1)") {data = log2(data+1)}
      if (input$transf == "log10(x+1)") {data = log10(data+1)}
      if (input$transf == "BoxCox") {
        imp <- preProcess(data, method = "BoxCox")
        data <- predict(imp, data)
      }
      if (input$transf == "YeoJohnson") {
        imp <- preProcess(data, method = "YeoJohnson")
        data <- predict(imp, data)
      }
      if (input$transf == "expoTrans") {
        imp <- preProcess(data, method = "expoTrans")
        data <- predict(imp, data)
      }
      data = data.frame(data)
    }


    remove_IDs<-which(rowSums(is.na(data))==ncol(data))
    if(length(remove_IDs)>0){
      data<-data[-remove_IDs,]}
    data<-data_migrate(vals$saved_data[[input$data_upload]],data,input$data_upload)
    nrow_o<-attr(data,'nobs_ori')
    nrow_g<-nrow(data)
    ncol_o<-attr(data,'nvar_ori')
    ncol_g<-ncol(data)

    newattribs<-isolate(
      t(data.frame(
        Subobs=if(nrow_g<nrow_o){paste(nrow_o,"::",nrow_g)} else {0},
        Subvars=if(ncol_g<ncol_o){paste(paste(ncol_o,"::",ncol_g))}else{0},
        Transf = if(is.null(input$transf)){"None"}else{input$transf},
        Removed = if(is.null(input$rares)){"None"}else{input$rares},
        Scale = if(is.null(input$scale)){"None"}else{input$scale},
        Center = if(is.null(input$center)){"FALSE"}else{input$center},
        NA.omit = if(is.null(input$na.omit)){"FALSE"}else{input$na.omit},
        Data_imp="None",
        Factor_imp="None"
      ))
    )

    attr_data<-cbind(attr(data, "transf"),newattribs)
    colnames(attr_data)<-paste0("change", 1:ncol(attr_data))
    colnames(attr_data)[ncol(attr_data)]<-"current"


    attr(data, "transf") <-attr_data
    attr(data, "nobs_ori") <-nrow_o
    attr(data, "nvar_ori") <-ncol_o

    #if(nrow(attr(data, "transf"))==2){attr(data, "transf")<-attr(data, "transf")[-1,]}
    attr(data,"factors")<-attr(data,"factors")[rownames(data),,drop=F]
    attr(data,"coords")<-attr(data,"coords")[rownames(data),,drop=F]



    # validate(need(!any(unlist(lapply(vals$saved_data, function(x) identical(data,x)))),"you already have  the selected results in  your Data bank"))

    data_cogs$df<-data
    data
  })
  output$pop_split<-renderUI({
   fluidRow(
     column(12,h4("Split tools",style="border-bottom: 1px solid white;"),style=" background: #05668D; color: white"),
     column(12,
            column(12,
                   radioButtons("split_options",NULL,
                                choices =c("Random sampling","Balanced sampling"))
            ),
            conditionalPanel("input.split_options=='Balanced sampling'",{
              column(12,
                     selectInput("splitfactor","factor",choices=colnames(attr(vals$saved_data[[input$data_upload]],"factors"))))
            }),
            conditionalPanel("input.split_options=='Random sampling'|input.split_options=='Balanced sampling'",{
              column(12,
                     splitLayout(
                       numericInput("splitperc_data","p (%)",value=20,min=0,max=99,step=1),
                       numericInput("seed_test","seed",value=NA)))
            }),
            fluidRow(style="background: white",column(12,tipify(actionButton("addpart",label=icon("fas fa-plus")),"Add partition  as a factor in Factor-Attribute. You can later choose the factor which you want to use to constrain training and testing data. "))
            ),
            column(12,
                   splitLayout(
                     column(12,
                            strong("Partition"),
                            verbatimTextOutput("partition_preview")
                     ))
            )
     )
   )
  })




  output$fac_levels<- renderUI({
    data<-vals$saved_data[[input$data_upload]]
    lev<-levels(attr(data, 'data.factors')[,input$sel_ommitedfac])
    fluidRow(
      p(strong("Levels:"), length(lev)),
      renderPrint({
        lev
      })
    )

  })
  output$tool1<-renderUI({

    fluidRow(id="tool1",

      column(12,h4("Select/Remove observations:",tipify(icon("fas fa-question-circle"),"Tools for restricting the observations along all Data-Attribute, Factor-Attribute and Coords-Attribute (if exists)", options=list(container="body")),style="border-bottom: 1px solid white;"),style=" background: #05668D; color: white"),

      column(12,checkboxInput("na.omit", strong("NA.omit",pophelp(NULL,"check to remove all observations that contain any empty cases (NAs)")), value = F,)),
      column(12,
             div(
               div("Filter by factors",style="border-bottom: 1px solid gray; margin-top: 10px"),
               div(id='filter_obs',style="background: #F0F0F0",
                   shinyTree("tree", checkbox = TRUE,themeIcons=F,themeDots=T)))
             ),
      uiOutput("filterby_indobs")
      )
  })





  observeEvent(input$data_upload,{
    output$tree <- renderTree({
      factors<-attr(vals$saved_data[[input$data_upload]],"factors")
      lis<-get_faclevels(factors)

      df<-do.call(rbind,lapply(lis,function(x) {
        data.frame(parent=as.factor(rep(x[[2]],length(x[[1]]))),
                   child1=as.factor(x[[1]]))
      }))
      vals$tree<-gettree(df)

      vals$tree
    })


  })
observeEvent(input$tree,{
  factors<-attr(vals$saved_data[[input$data_upload]],"factors")
  vals$seltree<-get_selfactors(get_selected(input$tree),factors)
})





  output$cutlevel_obs<-renderUI({
    div(id="filter_cut",
        column(12,checkboxGroupInput('cutlevel_obs',NULL,levels(
          attr(vals$saved_data[[input$data_upload]],"factors")
        ))),


      bsTooltip("filter_cut","The factor level to restric the observations")


    )
  })
  output$data_create<-renderUI({
    req(input$hand_save=="create")
    data_store$df<-F

    res<-switch (vals$hand_save,
                 "Save new som in" = textInput("model_newname", NULL,paste("Som", length(attr(getdata_som(),"som"))+1)),
                 "Save changes"= textInput("data_newname", NULL,bag_name()),
                 "Include binary columns in the Data-Attribute"= textInput("data_newname", NULL,bag_name()),
                 "Include ordinal variables in the Data-Attribute"= textInput("data_newname", NULL,bag_name()),
                 "Save Clusters"= textInput("hc_newname", NULL,bag_hc()),
                 "Create data list from aggregation results"= textInput("agg_newname", NULL,bag_agg()),
                 "Add an Extra-Layer-Attribute to the Datalist"=textInput("extra_layer_newname", NULL,bag_extralayer()),

                 "Save diversity results"= textInput("div_newname", NULL,bag_divname()),
                 "Create a datalist with the variables selected in the Random Forest Explainer"= textInput("rf_newname", NULL,bag_rfname()),
                 "Create a datalist with the model errors"= textInput("rferrors_newname", NULL,bag_rferrosname()),
                 "Create a datalist with the prediction errors"= textInput("rf_pred_errors_newname", NULL,bag_rfpredname()),
                 "Create a datalist with the RF predictions"= textInput("rf_predictions_newname", NULL,bag_rfpredictions()),
                 "Create a datalist with the variables included in the N most frequent interactions in the RF"= textInput("rfinter_newname", NULL,bag_rfintername()),
                 "Create a datalist from the selected observations"= {
                   bag<-ncol(attr(getdata_upload(), "transf") )
                   textInput("selobs_newname", NULL,paste(gsub(".csv","",attr(getdata_upload(), "filename")) ,bag))
                 },
                 "Create factor using breakpoints from the dissimilarity profile"= textInput("bp_newname", NULL,paste("bp", vals$bagbp0+1)),
                 "add partition"= textInput("split_newname", NULL,bag_partition()),
                 "Save som predictions"= textInput("predsom_newname", NULL,predsom_name()),
                 "Save errors from som predictions (X)"= textInput("predsom_eX_newname", NULL,bag_sompred_eX()),
                 "Save errors from som predictions (Y)"= textInput("predsom_eY_newname", NULL,bag_sompred_eY()),

                 "Save new rf in"= textInput("rf_newname", NULL,bag_RFname()),
                 "Save interpolation model"=
                   {

                     textInput("interps_newname", NULL,bag_mapname())
                   },
                 "Save discrete model"=
                   {

                     textInput("discrete_newname", NULL,bag_mapname())
                   },
                 "Save raster"=
                   {

                     textInput("interps_newname", NULL,bag_mapname())
                   },
                 "Merge Datalists"= textInput("merge_newname", NULL,paste("Datalist_merged", length(vals$saved_data)+1))



    )
    data_store$df<-T
    res
  })
  output$toorder_tag <- renderUI({
    div(
      rank_list(
        text = "Drag the levels in any desired order",
        labels = levels(attr(vals$saved_data[[input$data_upload]],"factors")[,input$tag_edit]),
        input_id = "rank_list_2",
        class="custom-sortable"
      )

    )
  })
  output$pop_na2<-renderUI({
    data<-vals$saved_data[[input$data_upload]]
    factors<-attr(data,"factors")
    if(input$na_targ=="Data-Attribute"){
      validate(need(anyNA(data),"No missing values in the Data-Attribute"))
    }
    if(input$na_targ=="Factor-Attribute"){
      validate(need(anyNA(factors),"No missing values in the Factor-Attribute"))
    }
    div(
      span(
        span(id="na_method2",
             inline(uiOutput("na_methods"))
        ),
        tags$style("#na_method2 .selectize-input {margin-bottom: -12px;}"),
        inline(
          conditionalPanel("input.na_method=='knn'",
                           inline(numericInput("na_knn", "k",value=5, width="75px")))
        ),
        tipify(actionButton("go_na",img(src=na_icon,height='15',width='15'),style="margin-top:-5px "),"Impute missing values"),
        tipify(actionButton("undo_na",icon("fas fa-undo"),style="margin-top:-5px "),"Undo imputation"),
        uiOutput("na_warning"),
        uiOutput("imputation_help")
      )
    )
  })
  output$filterby_indobs<-renderUI({

    data<-vals$saved_data[[input$data_upload]]
    #data<-getdata_upload()

    column(12,  style = 'overflow-x: scroll;height:250px;overflow-y: scroll',
           div("Individual selection:",style="border-bottom: 1px solid gray; margin-top: 10px"),
           div(style="background: #F0F0F0",
           checkboxInput("check_obs",strong('Select/Unselect all'),T),
           checkboxGroupInput("selecobs",NULL,choices = rownames(data),selected = rownames(data))))
  })
  output$filterby_indvars<-renderUI({
    data<-vals$saved_data[[input$data_upload]]
    validate(need(ncol(data)<1000,"Individual selection not available for data with more than 1000 columns"))
    fluidRow(style="background: white; margin-top: 10px",
           column(12,
             p(strong("Individual selection:")),
             checkboxInput("check_fac",'Select/Unselect all',T),hr(),checkboxGroupInput("selecvar",NULL,choices = colnames(data),selected = colnames(data))
           ))
  })
  output$conversion_options<-renderUI({
    data<-vals$saved_data[[input$data_upload]]
    num<-data[,which(unlist(lapply(data, function(x) is.numeric(x)))), drop=F]
    if(length(attr(data,"data.factors"))>0){
      fac=attr(data,"data.factors") }else { fac=data.frame()}


    fluidRow(
      if(length(attr(data,"data.factors"))>0){
        fluidRow(
          column(12,strong("Factors were removed from the Data-Attribute")),
          column(12,"A Data-Attribute can handle only numeric variables. Use the 'Factor to Numeric' option to explore and include the ommited factors as numeric variables")
        )
      },
      if(ncol(num)>=1 & ncol(fac)>=1){
        radioButtons("convertion_type","Type:",choices=c("Numeric to Factor", "Factor to Numeric"))
      } else if(ncol(num)>=1 & !ncol(fac)>=1){
        radioButtons("convertion_type","Type:",choices=c("Numeric to Factor"))
      } else if(!ncol(num)>=1 & ncol(fac)>=1){
        radioButtons("convertion_type","Type:",choices=c("Factor to Numeric"))
      },
      conditionalPanel("input.convertion_type=='Numeric to Factor'",{
        splitLayout(style="height:200px;overflow-y: scroll",
                    checkboxGroupInput("vars_num","Select the variables:", choices=colnames(num), selected=NULL),
                    actionButton("convert_num", icon("fas fa-random"))
        )

      })
    )
  })
  output$conversion_tools<-renderUI({
    data<-vals$saved_data[[input$data_upload]]
    req(input$convertion_type=="Factor to Numeric")
    fluidRow(
      radioGroupButtons("hand_facs",NULL,
                        choiceValues   = list("Plot","Binary","Ordinal"), choiceNames=list(span("Explore the ommited factors"),span("Binary Tranformation",pophelp(NULL,"Creates a single binary column per factor level, with 1 indicating the class of that particular observation"), style="padding: 10px"), span("Ordinal Tranformation",pophelp(NULL,"Transform factors into ordinal variables"), style="padding: 10px"))),
      conditionalPanel("input.hand_facs=='Plot'",{
        fluidRow(
          column(12,
                 splitLayout(cellWidths = c('25%','25%',"50%"),
                             selectInput("sel_ommitedfac", strong("Factor", pophelp(NULL, "Select the factor to see its count")),choices=colnames(attr(data,"data.factors")), selectize=T),
                             column(12,
                                    strong("Palette"),
                                    pickerInput(inputId = "fac_palette",
                                                label = NULL,
                                                choices =     vals$colors_img$val[getgrad_col()],
                                                choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body")))
                             ,
                             column(12,style="height:100px;overflow-y: scroll",


                                    uiOutput("fac_levels"))

                 ),
                 h5(strong(
                   "Counting of factors:"
                 )),
                 renderPlot(pfactors(
                   attr(data, 'data.factors')[input$sel_ommitedfac], palette=input$fac_palette, newcolhabs=vals$newcolhabs
                 ))
          )
        )
      }),
      conditionalPanel("input.hand_facs=='Binary'",{
        fluidRow(
          splitLayout(cellWidths = c('40%','40%',"20%"),
                      column(12,style="height:200px;overflow-y: scroll;overflow-x: scroll",
                             checkboxGroupInput("to_classmat","Select the factors:", choices=colnames(attr(data,"data.factors")))
                      ),
                      column(12,style="height:200px;overflow-y: scroll;overflow-x: scroll",
                             uiOutput("binary_preview")),
                      column(12,popify(actionButton("insert_classMat",div(strong('Include new variables'),style='white-space: normal;'), icon=icon("fas fa-arrow-right"), width="100px"),NULL,"Click to insert the new variables in the Data-Attribute"))


          )

          #p("Any changes you make in this Panel affects the current Datalist, until you close the Panel"),



        )
      }),
      conditionalPanel("input.hand_facs=='Ordinal'",{
        data<-vals$saved_data[[input$data_upload]]
        fluidRow(
          column(12, selectInput("to_ordinal","Select the factors:", choices=colnames(attr(data,"data.factors"))) ) ,
          uiOutput("ordinal_out")

          #p("Any changes you make in this Panel affects the current Datalist, until you close the Panel"),



        )
      })



    )
  })
  output$toorder_in <- renderUI({
    fluidRow(
      rank_list(
        text = "Drag the levels in any desired order",
        labels = levels(attr(vals$saved_data[[input$data_upload]],"data.factors")[,input$to_ordinal]),
        input_id = "rank_list_1",
        class="custom-sortable"
      ),
      tags$style(
        HTML("
          .custom-sortable .rank-list-item {
            height: 25px;
          padding:5px;
           background-color: #BDB;
          border: 1px solid white;
          }
        ")
      )
    )
  })
  output$binary_preview<-renderUI({
    data<-vals$saved_data[[input$data_upload]]
    fluidRow(column(12,
                    h5(strong("New binary variables:")),
                    renderPrint({
                      req(length(input$to_classmat)>0)
                      colnames(getclassmat(attr(data, 'data.factors')[,input$to_classmat,drop=F]))})))
  })
  output$data_over<-renderUI({
    data_overwritte$df<-F
    req(input$hand_save=="over")
    validate(need(vals$hand_save!='Save som predictions',"This functionality is unavailable for saving SOM predictions."))
    res<-switch (vals$hand_save,
                 'Save new som in' = {
                   if(length(attr(getdata_som(),"som"))>0){
                     selectInput("model_over", NULL,choices=c(names(attr(getdata_som(),"som"))),selectize=T)
                   }else{
                     column(12,verbatimTextOutput("nosommodel"))
                   }
                 },
                 'Save changes' = selectInput("data_over", NULL,choices=c(names(vals$saved_data)),selected=input$data_upload),
                 'Include binary columns in the Data-Attribute' = selectInput("data_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Include ordinal variables in the Data-Attribute' = selectInput("data_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Save Clusters' = selectInput("hc_over", NULL,choices=c(colnames(attr(getdata_hc(),"factors"))),selectize=T),
                 'Create data list from aggregation results' = selectInput("agg_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Save diversity results' = selectInput("div_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Create a datalist with the variables selected in the Random Forest Explainer' = selectInput("rf_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Create a datalist with the model errors' = selectInput("rferrors_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Create a datalist with the prediction errors' = selectInput("rf_pred_errors_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Create a datalist with the RF predictions' = selectInput("rf_predictions_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Create a datalist with the variables included in the N most frequent interactions in the RF' = selectInput("rfinter_over", NULL,choices=c(names(vals$saved_data)),selectize=T),

                 'Save errors from som predictions (Y)' = selectInput("predsom_eY_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Save errors from som predictions (X)' = selectInput("predsom_eX_over", NULL,choices=c(names(vals$saved_data)),selectize=T),


                 'Create a datalist from the selected observations' = selectInput("selobs_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Add an Extra-Layer-Attribute to the Datalist' = selectInput("extra_layer_over", NULL,choices=c(names(attr(vals$saved_data[[input$data_map]],"extra_shape"))),selectize=T),
                 'Create factor using breakpoints from the dissimilarity profile' =  selectInput("bp_over", NULL,choices=c(colnames(attr(vals$saved_data[[input$data_upload]],"factors"))),selectize=T),
                 'add partition' = selectInput("split_over", NULL,choices=colnames(attr(vals$saved_data[[input$data_upload]],"factors")),selectize=T),
                 'Save som predictions' = {
                   if(length(attr(getdata_som(),"predictions"))>0){
                     selectInput("predsom_over", NULL,choices=c(names(attr(getdata_som(),"predictions"))),selectize=T)
                   }else{
                     column(12,verbatimTextOutput("nopredictions"))
                   }

                 },
                 'Save new rf in' = {
                   if(length(attr(getdata_rf(),"rf"))>0){
                     selectInput("rf_over", NULL,choices=c(names(attr(getdata_rfX(),"rf"))),selectize=T)
                   }else{
                     column(12,verbatimTextOutput("norfmodel"))
                   }
                 },
                 'Save interpolation model'= {
                   if(length(vals$saved_maps)>0){
                     selectInput("interps_over", NULL,choices=c(names(vals$saved_maps)),selectize=T, selected=input$saved_maps)
                   }else{
                     column(12,verbatimTextOutput("nointerpsmodel"))
                   }

                 },
                 'Save discrete model'= {
                   if(length(vals$saved_maps)>0){
                     selectInput("discrete_over", NULL,choices=c(names(vals$saved_maps)),selectize=T, selected=input$saved_maps)
                   }else{
                     column(12,verbatimTextOutput("nointerpsmodel"))
                   }

                 },
                 'Save raster'={
                   if(length(vals$saved_maps)>0){
                     selectInput("interps_over", NULL,choices=c(names(vals$saved_maps)),selectize=T, selected=input$saved_maps)
                   }else{
                     column(12,verbatimTextOutput("nointerpsmodel"))
                   }

                 },
                 "Merge Datalists"= selectInput("merge_over", NULL,choices=c(names(vals$saved_data)),selectize=T)
    )
    data_overwritte$df<-T
    res
  })
  getdata_split<-reactive({
    data<-vals$saved_data[[input$data_upload]]
    factors<-attr(data,"factors")
    fac<-factors[,input$splitfactor, drop=F]
    if(input$split_options=='Balanced sampling'){
      if (!is.na(input$seed_test)) {set.seed(input$seed_test)}
      res<- createDataPartition(unlist(fac),p=input$splitperc_data/100,list=F)
      res<-unlist(res)
    } else if(input$split_options=='Random sampling')
    {
      n_sample<-round(nrow(data)*input$splitperc_data/100)
      if (!is.na(input$seed_test)) {set.seed(input$seed_test)}
      res<-sample(1:nrow(data),n_sample)

    }

    res
  })
  get_partition<-reactive({
    data<-vals$saved_data[[input$data_upload]]
    factors<-attr(data,"factors")
    factors$Partition<-NA
    res<-  factors[,"Partition", drop=F]
    res[rownames(data)[as.vector(getdata_split())] ,"Partition"]<-"test"
    res[rownames(data)[-as.vector(getdata_split())] ,"Partition"]<-"training"
    res$Partition<-as.factor( res$Partition)
    res
  })
  getordinal_factors<-reactive({
    data=attr(vals$saved_data[[input$data_upload]],"data.factors")
    res<-data[,input$to_ordinal]
    res<-factor(res, labels=input$rank_list_1, levels=input$rank_list_1)
    res<-as.numeric(res)
    res

  })
  getlevels<-reactive({
    req(input$to_ordinal)
    data.frame(label=levels(attr(vals$saved_data[[input$data_upload]],"data.factors")[,input$to_ordinal]))
  })
  savebinary<-reactive({
    data=vals$saved_data[[input$data_upload]]
    data.factors<-attr(data, 'data.factors')
    data.factors[,input$to_classmat]<-NULL
    temp<-cbind(data,getclassmat(attr(data, 'data.factors')[,input$to_classmat,drop=F]))
    temp<-data_migrate(data,temp,input$data_newname)
    attr(temp, 'data.factors')<-data.factors
    if(input$hand_save=="create"){
      vals$saved_data[[input$data_newname]]<-temp
      vals$cur_data<-input$data_newname
    } else{
      vals$saved_data[[input$data_over]]<-temp
      vals$cur_data<-input$data_over
    }
    vals$new_facts<-NULL



  })
  saveordinal<-reactive({
    data=vals$saved_data[[input$data_upload]]
    data.factors<-attr(data, 'data.factors')
    data.factors[,input$to_ordinal]<-NULL
    temp<-cbind(data,vals$new_facts)
    temp<-data_migrate(data,temp,input$data_newname)
    attr(temp, 'data.factors')<-data.factors
    if(input$hand_save=="create"){
      vals$saved_data[[input$data_newname]]<-temp
      vals$cur_data<-input$data_newname
    } else{
      vals$saved_data[[input$data_over]]<-temp
      vals$cur_data<-input$data_over
    }
    vals$new_facts<-NULL

  })
  save_bpfac<-reactive({
    vals$bagbp0<-vals$bagbp0+1
    dp<-getDP()
    if(input$hand_save=="create"){
      attr(vals$saved_data[[input$data_upload]],"factors")[rownames(vals$splitBP),input$bp_newname]<-as.vector(vals$splitBP)
      attr(vals$saved_data[[input$data_upload]],"factors")[,input$bp_newname]<-as.factor(attr(vals$saved_data[[input$data_upload]],"factors")[,input$bp_newname])

    } else{
      attr(vals$saved_data[[input$data_upload]],"factors")[input$bp_over]<-as.vector(vals$splitBP)
      attr(vals$saved_data[[input$data_upload]],"factors")[,input$bp_over]<-as.factor(attr(vals$saved_data[[input$data_upload]],"factors")[,input$bp_over])

    }


  })
  bag_partition<-reactive({
    bag<-1
    name0<-paste("Partition")
    name1<-paste(name0,bag)
    if(name1%in%colnames(attr(vals$saved_data[[input$data_upload]],"factors")))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%colnames(attr(vals$saved_data[[input$data_upload]],"factors"))) break
      }
    }
    paste("Partition",bag)

  })
  savepart<-reactive({
    data<-getdata_upload()
    part<-get_partition()
    factors<-attr(data,"factors")
    if(input$hand_save=="create"){
      vals$bagpart0<-vals$bagpart0+1
      factors[,input$split_newname]<-part
      attr(vals$saved_data[[input$data_upload]],"factors")<-factors
      #vals$cur_data<-input$split_newname
      vals$cur_partsom<-input$split_newname
    } else {
      factors[,input$split_over]<-part
      attr(vals$saved_data[[input$data_upload]],"factors")<-factors
      # vals$cur_data<-input$split_over
      vals$cur_partsom<-input$split_over
    }
  })
  observeEvent(input$go_na,{
    data<-data_cogs$df
    transf<-attr(data,"transf")
    if(input$na_targ=="Data-Attribute"){
      data<-nadata(data=data,
                   na_method=input$na_method,
                   data_old=vals$saved_data[[input$data_upload]],
                   data_name=input$data_upload,
                   k=input$na_knn)
      transf<-attr(data,"transf")

      transf["Data_imp",'current']<-input$na_method
      attr(data,"transf")<-transf
    }

    data_cogs$df<-data
  })
  observeEvent(input$go_na,{
    data=data_cogs$df
    transf<-data.frame(attr(data,"transf"))
    if(input$na_targ=="Factor-Attribute"){
      data<-nafactor(data=data,
                     na_method=input$na_method
      )

      transf["Factor_imp",ncol(transf)]<-input$na_method
      attr(data,"transf")<-transf
    }
    data_cogs$df<-data

  })
  observeEvent(input$check_fac,{
    if(isTRUE(input$check_fac)){
      updateCheckboxGroupInput(session,
                               "selecvar",NULL,
                               choices = colnames(vals$saved_data[[input$data_upload]]),
                               selected = colnames(vals$saved_data[[input$data_upload]])
      )
    } else{
      updateCheckboxGroupInput(session,
                               "selecvar",NULL,
                               choices = colnames(vals$saved_data[[input$data_upload]])

      )
    }

  })
  observeEvent(input$check_obs,{
    if(isTRUE(input$check_obs)){
      updateCheckboxGroupInput(session,
                               "selecobs",NULL,
                               choices = rownames(vals$saved_data[[input$data_upload]]),
                               selected = rownames(vals$saved_data[[input$data_upload]])
      )
    } else{
      updateCheckboxGroupInput(session,
                               "selecobs",NULL,
                               choices = rownames(vals$saved_data[[input$data_upload]])

      )
    }

  })
  observeEvent(input$data_upload,{
    res<-try({ncol(vals$saved_data[[input$data_upload]][,input$selecvar])},T)
    if(class(res)=="try-error"){toggleDropdownButton('dropID_selecvar')
      update_selecvar$df<-T

    }
  })
  observeEvent(input$tag_order_apply,{
    factors<-attr(getdata_upload(),"factors")
    oldfac<-factors[,input$tag_edit]
    newfac<-factor(oldfac, labels=input$rank_list_2, levels=input$rank_list_2)
    factors[,input$tag_edit]<-newfac
    attr(vals$saved_data[[input$data_upload]],"factors")<-factors
    toggleDropdownButton('dropID_tag')
  })
  observeEvent(input$tag_edit_apply,{

    #input$tag_edit<-'season'
    # input$tag_edit_level<-"Spring"
    #input$tag_edit_newlevel<-'Primavera'
    factors<-attr(getdata_upload(),"factors")
    newfactor<-oldfactor<-factors[,input$tag_edit]
    newlevels<-oldlevels<-levels(oldfactor)
    piclevels<-which(oldlevels==input$tag_edit_level)
    newlevels[oldlevels]<-input$tag_edit_newlevel
    newfactor<-as.character(newfactor)
    newfactor[oldfactor==input$tag_edit_level]<-input$tag_edit_newlevel
    attr(vals$saved_data[[input$data_upload]],"factors")[,input$tag_edit]<- as.factor(newfactor)
    toggleDropdownButton('dropID_tag')

  })
  observeEvent(input$undo_na,{
    data_cogs$df<-vals$saved_data[[input$data_upload]]
  })
  observeEvent(input$data_upload,{
    if(length(input$selecvar)>0)
      updateCheckboxGroupInput(session,"selecvar", choices=colnames(vals$saved_data[[input$data_upload]]))

  })
  observeEvent(input$convert_factor,{


    showModal(
      div(id="fac_convert",modalDialog(

        column(12,style="background: white",
               column(12,
                      column(12,strong("Datalist:"),code(input$data_upload)),

                      uiOutput("conversion_options"),
                      uiOutput("conversion_tools"))

        ),
        title="Numeric/Factor conversion",
        size="xl",
        easyClose = T


      ))
    )

  })
  observeEvent(input$convert_num,{
    newnum<-data<-vals$saved_data[[input$data_upload]]
    newfac<-data[,input$vars_num, drop=F]
    newnum[,input$vars_num]<-NULL
    attr(newnum, "data.factors")<- if(length(attr(data, "data.factors"))>0){
      cbind(
        attr(data, "data.factors"),newfac
      )
    } else{
      newfac
    }
    vals$saved_data[[input$data_upload]]<-newnum



  })
  observeEvent(input$create_ordinal,{
    res<-getordinal_factors()
    data=attr(vals$saved_data[[input$data_upload]],"data.factors")
    if(is.null( vals$new_facts)){
      res0<-data.frame(matrix(NA,nrow=nrow(data)))
      colnames(res0)<-input$to_ordinal
      rownames(res0)<-rownames(data)
      vals$new_facts<-res0
    }
    vals$new_facts[input$to_ordinal]<-res
  })
  savechanges_comb<-reactive({
   # saveRDS(reactiveValuesToList(vals),"vals.rds")
   # saveRDS(reactiveValuesToList(input),"input.rds")
    #saveRDS(reactiveValuesToList(data_cogs),"data_cogs.rds")

   # vals<-readRDS("input.rds")
    #input<-readRDS("vals.rds")
   # data_cogs<-readRDS("data_cogs.rds")


    temp<-data_cogs$df
    data<-data_cogs$df
    data.factors<-attr(data,"data.factors")


    attr(temp, 'data.factors')<-data.factors
    if(input$hand_save=="create"){
      temp<-data_migrate(data,temp,input$data_newname)
      vals$saved_data[[input$data_newname]]<-temp
    } else{
      temp<-data_migrate(data,temp,input$data_over)
      vals$saved_data[[input$data_over]]<-temp

    }
    vals$new_facts<-NULL

  })
  bag_name<-reactive({
    bag<-ncol(attr(getdata_upload(), "transf") )
    name0<-paste(gsub(".csv","",attr(getdata_upload(), "filename")))


    name1<-paste0(name0," (",bag,")")
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste0(name0," (",bag,")")
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste0(name0," (",bag,")")

  })
## menu_explore
  getdata_upload0<-reactive({
    req(input$data_upload0)
    vals$saved_data[[input$data_upload0]]
  })
  output$rda_X<-renderUI({
    pickerInput("rda_X",span("X Data", tiphelp("Response data")), choices=names(vals$saved_data)[-which(names(vals$saved_data)%in%input$data_upload0)])
  })
  getDP<-reactive({
    smw<- vals$smw_dp
    dp_BPs<-if(input$dp_BPs==""){NULL} else{input$dp_BPs}
    dp_w<-if(is.na(input$dp_w)){NULL} else{input$dp_w}
    dp<-DP_smw()
    colnames(dp)[2]<-"SampleID"

    sim1o<-getord()
    yo<-data.frame(sim1o$yo)
    bps<-suppressWarnings(bp(dp))

    to_split<-c(1,rep(1:(length(bps)+1),
                      diff(c(1,bps, nrow(yo)))))
    splits<-lapply(split(yo,to_split),function(x) rownames(x))
    data_empty<-data.frame(attr(vals$saved_data[[input$data_upload0]],"factors")[,1,drop=F])
    data_empty[,1]<-as.numeric(data_empty[,1])
    for(i in 1:length(splits)){
      data_empty[splits[[i]],1]<-i
    }


    vals$splitBP<- data_empty
    dp
  })
  output$save_bp<-renderUI({
    if(!isFALSE(vals$splitBP)){
      if(!any(unlist(lapply(attr(vals$saved_data[[input$data_upload0]],"factors"), function (x) identical(x,as.vector(vals$splitBP)))))){
        popify(
          actionLink(
            'tools_saveBP',span("+ Save Breakpoints",icon("fas fa-save"))),"Save breakpoints from DP",
          "this action divides the observations according to the breakpoints and assigns a factor to each split", options=list(container="body")
        )

      }
    }

  })
  output$fac_bp<-renderUI({
    fluidRow(
      renderPrint(
        which(!duplicated(sort(attr(vals$saved_data[[input$data_upload0]],"factors")[,input$pw_sugg])))
      )
    )
  })
  segrda_symbol_factor <- reactive({
    req(input$segrda_symbol_factor)
    col<-getcolhabs(vals$newcolhabs,input$segrda_colpalette,2)
    req(col[1]!=col[2])

    if(col[1]!=col[2]){

      data = vals$saved_data[[input$data_upload0]]
      attr(data,"factors")[rownames(getord()$yo), input$segrda_symbol_factor]

    }else{NULL}
  })
  segrda_text_factor <- reactive({
    if(isFALSE(input$segrda_show_labels)){NULL} else{
      data = vals$saved_data[[input$data_upload0]]
      attr(data,"factors")[rownames(getord()$yo), input$segrda_labfactor]}

  })
  output$segrda_X<-renderUI({
    pickerInput("segrda_X",span("X Data", tiphelp("Response data")), choices=names(vals$saved_data)[-which(names(vals$saved_data)%in%input$data_upload0)])
  })
  observeEvent(input$revert_segrda,{
    cur<-input$data_upload0
    updatePickerInput(session,"data_upload0",NULL,choices=names(vals$saved_data), selected=input$segrda_X)
    updatePickerInput(session,"segrda_X",strong("Y Datalist:", tiphelp("Response data")), choices=names(vals$saved_data)[-which(names(vals$saved_data)%in%input$data_upload0)],selected=cur)

  })
  output$stats_pdesc<-renderUI({
    column(12,style="background: white;",
           column(12,style="margin-top:20px",strong("Descriptive statistics")),
           column(12,
                  splitLayout(
                    column(12,

                           p(strong("Aggregate:"),popify(a(icon("fas fa-question-circle")),
                                                         "Aggregate","The process involves two stages. First, collate individual cases of the Data-Attribute together with a grouping variable (unselected factors). Second, perform which calculation you want on each group of cases (selected factors)", options=list(container="body"))),

                           p(em(input$data_upload0,"::","Factor-Attribute::", style="color: gray")),
                           checkboxGroupInput('fac_descs', NULL, choices=colnames(
                             attr(getdata_upload0(),"factors")), selected = colnames(
                               attr(getdata_upload0(),"factors"))[1:(length(colnames(
                                 attr(getdata_upload0(),"factors")))-1)])

                    ),

                    column(12,
                           selectInput("spread_measures","function:",choices=c("sum","mean","median","var","sd","range","min","max"), selectize=T, selected="mean"),
                           popify(bsButton("tools_saveagg", icon("fas fa-file-signature"),style  = "button_active", type="action",value=FALSE, block=T),NULL,
                                  "Create datalist",options=list(container="body")
                           )
                    )
                  )
           ),


           column(12,style="overflow-x: scroll;height:250px;overflow-y: scroll;",uiOutput("desc_dataout"))
    )
  })

  output$panel_main<-renderUI({
    req(input$data_upload0)
    # border_alert<-border_alert()
    data<-vals$saved_data[[input$data_upload0]]
    if(is.null(vals$cur_desc_options)){vals$cur_desc_options<-'Summaries'}
    tabsetPanel(id='desc_options',selected = vals$cur_desc_options,
                tabPanel('Summaries',
                         uiOutput('summaries_out')),
                tabPanel('Histogram',
                         uiOutput("stats_phist")),
                tabPanel('Boxplot',
                         uiOutput("stats_cbox"),
                         fluidRow(
                           uiOutput('boxplot_out')
                         )),
                tabPanel('Aggregate',
                         uiOutput("stats_pdesc")),
                tabPanel('MDS',
                         fluidRow(
                           sidebarLayout(
                             sidebarPanel(
                               fluidRow(
                                 class="map_control_style",
                                 style="color: #05668D",
                                 uiOutput('omds_dist'),
                                 uiOutput('mds_options')

                               )
                             ),
                             mainPanel(
                               uiOutput("stats_cmds"),
                               uiOutput("stats_pmds")
                             )
                           )
                         )
                ),
                tabPanel('PCA',
                         fluidRow(
                           sidebarLayout(
                             sidebarPanel(
                               fluidRow(
                                 class="map_control_style",
                                 style="color: #05668D",
                                 uiOutput('opca_biplot'),
                                 uiOutput('pca_options')

                               )
                             ),
                             mainPanel(
                               uiOutput("stats_cpca"),
                               uiOutput("stats_ppca")
                             )
                           )
                         )
                ),
                tabPanel('RDA',
                         fluidRow(
                           div(uiOutput("stats_crda")),
                           sidebarLayout(
                             sidebarPanel(
                               fluidRow(
                                 class="map_control_style",
                                 style="color: #05668D",
                                 uiOutput('orda_options'),
                                 uiOutput('rda_options')

                               )
                             ),
                             mainPanel(
                               uiOutput("stats_rda")
                             )
                           )
                         )
                ),
                tabPanel('segRDA',
                         uiOutput("stats_csegrda")
                )
    )

  })
  output$stats_crda<-renderUI({
    validate(need(length(vals$saved_data)>1, "This functionality requires at least two datalist as explanatory and response data."))
    column(12,style="background: white",
           p(strong("Redundancy Analysis")),
           span(
             inline(
               span(style="width: 150px",
                    p(strong("Y Datalist:", style="color: #0D47A1"), span(actionLink("revert_rda",tipify(icon("fas fa-exchange-alt"),"invert selection")), style="margin-left: 20px")),
                    p(em(input$data_upload0),strong("~", style="color: #0D47A1"))
               )
             ),
             inline(uiOutput("rda_X"))
           )

    )
  })
  output$stats_csegrda<-renderUI({
    validate(need(!anyNA(getdata_upload0()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))

    validate(need(length(vals$saved_data)>1, "This functionality requires at least two datalist as explanatory and response data."))
    div(style="background: white",
        p(strong("Segmented Redundancy Analysis")),
        span(
          inline(
            span(style="width: 150px",
                 p(strong("Y Datalist:", style="color: #0D47A1"), span(actionLink("revert_segrda",tipify(icon("fas fa-exchange-alt"),"invert selection")), style="margin-left: 20px")),
                 p(em(input$data_upload0),strong("~", style="color: #0D47A1"))
            )
          ),
          inline(uiOutput("segrda_X"))
        ),
        column(12,
               tabsetPanel(id="segrda_panels",
                           tabPanel("OrdData",
                                    fluidRow(
                                      sidebarLayout(
                                        sidebarPanel(
                                          uiOutput("ord_side")),
                                        mainPanel(
                                          uiOutput("ord_segrda")
                                        ))
                                    )),
                           tabPanel("SMW",
                                    uiOutput("segrda_smw")

                           ),
                           tabPanel("DP",
                                    uiOutput("segrda_dp")

                           ),

                           tabPanel("pwRDA",
                                    p(strong("Piecewise RDA")),
                                    uiOutput("pw_out")))))
  })
  observeEvent(input$revert_rda,{
    cur<-input$data_upload0
    updatePickerInput(session,"data_upload0",NULL,choices=names(vals$saved_data), selected=input$rda_X)
    updatePickerInput(session,"rda_X",strong("Y Datalist:", tiphelp("Response data")), choices=names(vals$saved_data)[-which(names(vals$saved_data)%in%input$data_upload0)],selected=cur)


  })
  output$omds_dist<-renderUI({
    div(
      span("+ Distance:",
           inline(
             pickerInput("distance",NULL,choices = c("Choose one" = "", c('bray', "euclidean", 'jaccard')), selected=vals$cur_dist_mds, width="125px")
           )
      )
    )
  })
  output$opca_biplot<-renderUI({
    div(
      span("+",
           inline(
             checkboxInput('biplot', span("Biplot", pophelp(NULL,"show biplot arrows")), T, width="75px")
           )
      )
    )
  })
  output$orda_options<-renderUI({
    div(
      div(
        span("+",
             checkboxInput("rda_scale",span("Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=T, width = "100px")
        )
      ),

      div(
        span("+",
             inline(radioButtons("rda_view",NULL,choices=c(
               "Plot","Summary"),inline=T, width="150px"))
        )
      ),
      conditionalPanel("input.rda_view=='Summary'",{
        div(
          div(
            span(
              "+ Results:",
              inline(
                pickerInput("disp_rda_summ",NULL,choices=c('Variable scores','Observation scores','Linear constraints','Biplot','Importance (unconstrained)',"Importance (constrained)"), selected="Importance (unconstrained)", width="150px", options=list(container="body"))
              )
            )
          ),
          div(
            span("+ Axes",
                 inline(
                   numericInput("rda_axes",NULL, value=2, width="75px")
                 )
            )
          ),
          div(
            tipify(
              actionLink(
                'downcenter_rda',span("+ Download",icon("fas fa-download")), style="button_active"
              ),
              "Download selected results"
            )

          )
        )
      }),
      conditionalPanel("input.rda_view=='Plot'",{
        div(
          div(
            '+ Scaling:',
            inline(numericInput("rda_scaling",NULL, 2, width="45px", step=1, min=0, max=3)),tipify(icon("fas fa-question-circle",style="color: gray"),"Scaling for species and site scores. Either species (2) or site (1) scores are scaled by eigenvalues, and the other set of scores is left unscaled, or with 3 both are scaled symmetrically by square root of eigenvalues.", placement = "bottom", options=list(container="body"))
          ),
          div(span("+",checkboxInput('biplot_rda', span("Biplot", pophelp(NULL,"show biplot arrows")), T, width="75px"))),
          div(span("+",checkboxInput('sp_rda', span("Variables", pophelp(NULL,"Variables")), T, width="75px"))),
          conditionalPanel("input.sp_rda % 2",{
            column(12,
                   div(
                     '+ Number:',
                     inline(numericInput("rda_spnum",NULL, 10, width="45px", step=1)),tipify(icon("fas fa-question-circle",style="color: gray"),"Show N variables with the highest scores", placement = "bottom", options=list(container="body"))
                   ),
                   div(
                     "+ Display:",
                     span(
                       inline(
                         pickerInput("rda_sp_display",NULL,choices=c("Label","Shape"), width = "75px", options=list(container="body"))
                       ),
                       inline(
                         conditionalPanel("input.rda_sp_display=='Shape'",{
                           inline(pickerInput(
                             inputId = "rda_spshape",
                             label = NULL,
                             choices = df_symbol$val,
                             options=list(container="body"),
                             selected=df_symbol$val[8],
                             choicesOpt = list(content = df_symbol$img),width='35px'
                           ))
                         })
                       )
                     )
                   ),



                   div(
                     span("+ Variable Color:",
                          inline(
                            tipify(
                              pickerInput(
                                inputId = "rda_spcolor",
                                label = NULL,
                                selected=  vals$colors_img$val[getsolid_col()][4],
                                choices =   vals$colors_img$val[getsolid_col()],
                                choicesOpt = list(
                                  content =   vals$colors_img$img[getsolid_col()]
                                ),
                                width="75px",
                                options=list(container="body")
                              ), "Variable Color.")))
                   )
            )
          })
        )
      })

    )
  })
  output$mds_options<-renderUI({
    req(input$distance %in%c("bray","euclidean","jaccard"))
    div(
      div(
        span("+",
             inline(checkboxInput('mds_show_symbols',"Symbol" ,T, width='75px')))),
      conditionalPanel("input.mds_show_symbols % 2",{
        column(12,
               div(
                 span("+ Shape:",
                      inline(pickerInput(inputId = "mds_symbol",
                                         label = NULL,
                                         choices = df_symbol$val,
                                         options=list(container="body"),
                                         choicesOpt = list(content = df_symbol$img), width='75px')))),
               div(
                 span("+ Size:",
                      inline(numericInput("mds_cexpoint",NULL,value = 1,min = 0.1,max = 3,step = .1, width='75px')
                      ))
               ),
               div(
                 span("+ Color:",
                      inline(
                        tipify(
                          pickerInput(inputId = "mds_colpalette",label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='75px'), "Symbol palette. Choose a gradient to color observations by a factor")))),
               uiOutput("mds_fac_palette")
        )
      }),
      div(span("+",
               inline(checkboxInput('mds_show_labels',"Labels",F)
               ))),
      conditionalPanel("input.mds_show_labels % 2",{
        column(12,
               div(span("+ Factor:",
                        inline(tipify(pickerInput("mds_labfactor",NULL,choices = colnames(attr(getdata_upload0(),"factors")), width="125px"),  "label classification factor")
                        ))),
               div(span("+ Lab Color:",
                        inline(tipify(
                          pickerInput(
                            inputId = "mds_labcolor",
                            label = NULL,
                            selected=  vals$colors_img$val[12],choices =   vals$colors_img$val,choicesOpt = list(content =   vals$colors_img$img),width="75px",
                            options=list(container="body")
                          ),  "label classification factor"
                        )
                        ))),
               div(span("+ Lab adj:",
                        inline(
                          tipify(pickerInput("mds_labadj",NULL,choices=c(1:4), width="75px", options=list(containder="body")),  "a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.", placement = "right")
                        ))),
               div(span("+ Lab offset:",
                        inline(
                          tipify(numericInput("mds_offset",NULL,value = 0,step = .1, width="75px"),  "this value controls the distance (‘offset’) of the text label from the specified coordinate in fractions of a character width.")
                        ))),
               div(span("+ Size:",
                        inline(
                          tipify(numericInput("mds_cextext",NULL,value = 1,min = 0.1,max = 3,step = .1),  "label text size")
                        )))
        )
      }),
      div(
        actionLink(
          'mds_downp',span("+ Download",icon("fas fa-download")), style="button_active"
        )

      )

    )
  })
  output$mds_fac_palette<-renderUI({
    col<-getcolhabs(vals$newcolhabs,input$mds_colpalette,2)
    req(col[1]!=col[2])
    div(
      span("+ Factor:",
           inline(tipify(pickerInput("mds_symbol_factor",NULL,choices = rev(colnames(attr(getdata_upload0(),"factors"))), width='125px'), "symbol classification factor"))))
  })
  mds_symbol_factor <- reactive({
    req(input$mds_symbol_factor)
    col<-getcolhabs(vals$newcolhabs,input$mds_colpalette,2)
    if(col[1]!=col[2]){
      data = getdata_upload0()
      attr(data,"factors")[rownames(data), input$mds_symbol_factor]
    }else{NULL}
  })
  mds_text_factor <- reactive({
    if(isFALSE(input$mds_show_labels)){NULL} else{
      data = getdata_upload0()
      attr(data,"factors")[rownames(data), input$mds_labfactor]}
  })
  mds_symbol<-reactive({
    if(isFALSE(input$mds_show_symbols)){NA}else{as.numeric(input$mds_symbol)}
  })
  output$pca_options<-renderUI({
    div(
      div(
        span("+",
             inline(checkboxInput('pca_show_symbols',"Symbol" ,T, width='75px')))),
      conditionalPanel("input.pca_show_symbols % 2",{
        column(12,
               div(
                 span("+ Shape:",
                      inline(pickerInput(inputId = "pca_symbol",
                                         label = NULL,
                                         choices = df_symbol$val,
                                         options=list(container="body"),
                                         choicesOpt = list(content = df_symbol$img), width='75px')))),
               div(
                 span("+ Size:",
                      inline(numericInput("pca_cexpoint",NULL,value = 1,min = 0.1,max = 3,step = .1, width='75px')
                      ))
               ),
               div(
                 span("+ Color:",
                      inline(
                        tipify(
                          pickerInput(inputId = "pca_colpalette",label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='75px'), "Symbol palette. Choose a gradient to color observations by a factor")))),
               uiOutput("pca_fac_palette")
        )
      }),
      div(span("+",
               inline(checkboxInput('pca_show_labels',"Labels",F)
               ))),
      conditionalPanel("input.pca_show_labels % 2",{
        column(12,
               div(span("+ Factor:",
                        inline(tipify(pickerInput("pca_labfactor",NULL,choices = colnames(attr(getdata_upload0(),"factors")), width="125px"),  "label classification factor")
                        ))),
               div(span("+ Lab Color:",
                        inline(tipify(
                          pickerInput(
                            inputId = "pca_labcolor",
                            label = NULL,
                            selected=  vals$colors_img$val[12],choices =   vals$colors_img$val,choicesOpt = list(content =   vals$colors_img$img),width="75px",
                            options=list(container="body")
                          ),  "label classification factor"
                        )
                        ))),
               div(span("+ Lab adj:",
                        inline(
                          tipify(pickerInput("pca_labadj",NULL,choices=c(1:4), width="75px", options=list(containder="body")),  "a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.", placement = "right")
                        ))),
               div(span("+ Lab offset:",
                        inline(
                          tipify(numericInput("pca_offset",NULL,value = 0,step = .1, width="75px"),  "this value controls the distance (‘offset’) of the text label from the specified coordinate in fractions of a character width.")
                        ))),
               div(span("+ Size:",
                        inline(
                          tipify(numericInput("pca_cextext",NULL,value = 1,min = 0.1,max = 3,step = .1),  "label text size")
                        )))
        )
      }),
      div(
        actionLink(
          'pca_downp',span("+ Download",icon("fas fa-download")), style="button_active"
        )

      )

    )
  })
  output$pca_fac_palette<-renderUI({
    col<-getcolhabs(vals$newcolhabs,input$pca_colpalette,2)
    req(col[1]!=col[2])
    div(
      span("+ Factor:",
           inline(tipify(pickerInput("pca_symbol_factor",NULL,choices = rev(colnames(attr(getdata_upload0(),"factors"))), width='125px'), "symbol classification factor"))))
  })
  pca_symbol_factor <- reactive({
    req(input$pca_symbol_factor)
    col<-getcolhabs(vals$newcolhabs,input$pca_colpalette,2)
    if(col[1]!=col[2]){
      data = getdata_upload0()
      attr(data,"factors")[rownames(data), input$pca_symbol_factor]
    }else{NULL}
  })
  pca_text_factor <- reactive({
    if(isFALSE(input$pca_show_labels)){NULL} else{
      data = getdata_upload0()
      attr(data,"factors")[rownames(data), input$pca_labfactor]}

  })
  pca_symbol<-reactive({
    if(isFALSE(input$pca_show_symbols)){NA}else{as.numeric(input$pca_symbol)}
  })
  output$rda_options<-renderUI({
    req(input$rda_view=='Plot')
    div(
      div(
        span("+",
             inline(checkboxInput('rda_show_symbols',"Symbol" ,T, width='75px')))),
      conditionalPanel("input.rda_show_symbols % 2",{
        column(12,
               div(
                 span("+ Shape:",
                      inline(pickerInput(inputId = "rda_symbol",
                                         label = NULL,
                                         choices = df_symbol$val,
                                         options=list(container="body"),
                                         choicesOpt = list(content = df_symbol$img), width='75px')))),
               div(
                 span("+ Size:",
                      inline(numericInput("rda_cexpoint",NULL,value = 1,min = 0.1,max = 3,step = .1, width='75px')
                      ))
               ),
               div(
                 span("+ Color:",
                      inline(
                        tipify(
                          pickerInput(inputId = "rda_colpalette",label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='75px'), "Symbol palette. Choose a gradient to color observations by a factor")))),
               uiOutput("rda_fac_palette")
        )
      }),
      div(span("+",
               inline(checkboxInput('rda_show_labels',"Labels",F)
               ))),
      conditionalPanel("input.rda_show_labels % 2",{
        column(12,
               div(span("+ Factor:",
                        inline(tipify(pickerInput("rda_labfactor",NULL,choices = colnames(attr(getdata_upload0(),"factors")), width="125px"),  "label classification factor")
                        ))),
               div(span("+ Lab Color:",
                        inline(tipify(
                          pickerInput(
                            inputId = "rda_labcolor",
                            label = NULL,
                            selected=  vals$colors_img$val[12],choices =   vals$colors_img$val,choicesOpt = list(content =   vals$colors_img$img),width="75px",
                            options=list(container="body")
                          ),  "label classification factor"
                        )
                        ))),
               div(span("+ Lab adj:",
                        inline(
                          tipify(pickerInput("rda_labadj",NULL,choices=c(1:4), width="75px", options=list(containder="body")),  "a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.", placement = "right")
                        ))),
               div(span("+ Lab offset:",
                        inline(
                          tipify(numericInput("rda_offset",NULL,value = 0,step = .1, width="75px"),  "this value controls the distance (‘offset’) of the text label from the specified coordinate in fractions of a character width.")
                        ))),
               div(span("+ Size:",
                        inline(
                          tipify(numericInput("rda_cextext",NULL,value = 1,min = 0.1,max = 3,step = .1),  "label text size")
                        )))
        )
      }),
      div(
        actionLink(
          'rda_downp',span("+ Download",icon("fas fa-download")), style="button_active"
        )

      )

    )
  })
  output$rda_fac_palette<-renderUI({
    col<-getcolhabs(vals$newcolhabs,input$rda_colpalette,2)
    req(col[1]!=col[2])
    div(
      span("+ Factor:",
           inline(tipify(pickerInput("rda_symbol_factor",NULL,choices = rev(colnames(attr(getdata_upload0(),"factors"))), width='125px'), "symbol classification factor"))))
  })
  rda_symbol_factor <- reactive({
    req(input$rda_symbol_factor)
    col<-getcolhabs(vals$newcolhabs,input$rda_colpalette,2)
    if(col[1]!=col[2]){
      data = getdata_upload0()
      attr(data,"factors")[rownames(data), input$rda_symbol_factor]
    }else{NULL}
  })
  rda_text_factor <- reactive({
    if(isFALSE(input$rda_show_labels)){NULL} else{
      data = getdata_upload0()
      attr(data,"factors")[rownames(data), input$rda_labfactor]}

  })
  rda_symbol<-reactive({
    if(isFALSE(input$rda_show_symbols)){NA}else{as.numeric(input$rda_symbol)}
  })
  output$summaries_out<-renderUI({
    column(12,splitLayout(
      cellWidths = c('10%','90%'),
      column(12,
             fluidRow(style="margin-top: 15px",
                      radioGroupButtons("summ_options",NULL,choices=c("Data","Variables","Factors","Datalist"), status  ="button_active",justified =F,direction="vertical", selected=vals$curview_summ_options))),
      uiOutput("summ_out")))
  })
  output$boxplot_out<-renderUI({
    sidebarLayout(
      sidebarPanel(uiOutput("editbox")),
      mainPanel(uiOutput("stats_pbox"))
    )
  })
  output$data.summary<-renderTable({
    data<-getdata_upload0()
    psummary(data)
  },striped=T, spacing ="xs")
  output$plotoutput <- renderImage({

    fheight <- input$fheight
    fwidth <- input$fwidth
    fres <- as.numeric(input$fres)
    png(paste0(vals$hand_plot,".png",sep=""), height=fheight, width=fwidth, res=fres, units="cm", pointsize = input$pointsize)
    if(vals$hand_plot=="variable summary"){
      data=getdata_upload0()
      numerics<-data[,unlist(lapply(data,is.numeric))]
      replayPlot(vals$varplot)
    }
    if(vals$hand_plot=="factor summary"){
      pfac(res_pfac())
    }
    if(vals$hand_plot=="histogram"){
      phist(getdata_upload0())
    }
    if(vals$hand_plot=="boxplot"){replayPlot(vals$pbox_plot)}
    if(vals$hand_plot=="pca")   {replayPlot(vals$ppca_plot)}
    if(vals$hand_plot=="mds") {replayPlot(vals$pmds_plot)}
    if(vals$hand_plot=="Training plot"){pchanges(vals$som_results)}
    if(vals$hand_plot=="Couting plot"){  pcounts(vals$som_results)}
    if(vals$hand_plot=="uMatrix"){   pUmatrix(vals$som_results)}
    if(vals$hand_plot=="BMUs"){replayPlot(vals$bmus_plot)}
    if(vals$hand_plot=="BMUs predictions"){replayPlot(vals$bmus_pred_plot)}
    if(vals$hand_plot=="property plot") {replayPlot(vals$pprop_plot)}
    if(vals$hand_plot=="Dendrogram") {replayPlot(vals$pdend_plot)}

    if(vals$hand_plot=="screeplot_accRF data"){
      plot_accuclus(accRFdata(), sugg = sugg_accRF_data())}
    if(vals$hand_plot=="screeplot_accRF som"){
      plot_accuclus(accRFmodel(), sugg = sugg_accRF_model())}
    if(vals$hand_plot=="screeplot_WSS data"){
      elbow_plot(WSSdata(), sugg = sugg_WSS_data())}
    if(vals$hand_plot=="screeplot_WSS som"){
      elbow_plot(WSSmodel(), sugg = sugg_WSS_model())}

    if(vals$hand_plot=="Hcut"){hc_plot(phc())}

    if(vals$hand_plot=="codebook clusters"){replayPlot(vals$pclus_plot)}
    if(vals$hand_plot=="Minimal Depth distribution"){plot(vals$rfd_res)}

    if(vals$hand_plot=="Multi-way importance"){
      plot(vals$rfm_res)
    }
    if(vals$hand_plot=="RF - Partial Dependence"){
      plot(rfbiplot1$df)
    }


    if(vals$hand_plot=="Decision Tree") {replayPlot(vals$ptree_plot)}
    if(vals$hand_plot=="Map"){
      if(input$saved_maps=="new map"){plot(vals$map_res)} else{
        plot(vals$saved_maps[[input$saved_maps]])
      }
    }
    if(vals$hand_plot=="rda"){replayPlot(vals$rda_plot)}
    if(vals$hand_plot=="dp"){replayPlot(vals$plot_dp)}
    if(vals$hand_plot=="we"){replayPlot(vals$plot_we)}
    if(vals$hand_plot=="segrda"){replayPlot(vals$seg_rda_plot)}
    if(vals$hand_plot=="Confusion Matrix SOM"){plot(vals$conf_som)}
    if(vals$hand_plot=="Confusion Matrix - test RF"){plot(vals$conf_rf)}
    if(vals$hand_plot=="Confusion Matrix RF"){plot(vals$cm_rf)}
    if(vals$hand_plot=="Surface map"){replayPlot(persp_res$df)}
    if(vals$hand_plot=="mantel plot"){replayPlot(mantel_plot$df)}
    if(vals$hand_plot=="Scatter 3D"){replayPlot(scatter_out$df)}
    if(vals$hand_plot=="stacked raster map"){replayPlot(stack_out$df)}
    if(vals$hand_plot=="stacked scatter map"){replayPlot(ss3d_out$df)}
    if(vals$hand_plot=="RF interactions"){plot(rf_inter$df)}
    if(vals$hand_plot=="RF ranking comparations"){replayPlot(rf_rank$df)}
    if(vals$hand_plot=="RF measure comparations"){replayPlot(rf_rel$df)}


    dev.off()

    return(list(src = paste0(vals$hand_plot,".png",sep=""),
                contentType = "image/png",
                width = round((input$fwidth*as.numeric(input$fres))/2.54, 0),
                height = round((input$fheight*as.numeric(input$fres))/2.54, 0),
                alt = "plot"))
  },deleteFile=TRUE)
  output$stats_phist <- renderUI({
    fluidRow(
      column(12,actionButton('downp_hist',icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
      column(12,renderPlot(phist(getdata_upload0())))
    )
  })
  output$stats_var<-renderUI({
    data=getdata_upload0()
    factors<-data[,unlist(lapply(data,is.factor))]
    numerics<-data[,unlist(lapply(data,is.numeric))]

    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/50))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))

    fluidRow(
      column(12,
             splitLayout(cellWidths = c("10%","90%"),
                         column(12,actionButton('downp_summ_num',tipify(icon("fas fa-download"), "Download Plot"), style="button_active")),
                         column(12,selectInput('splitdata',"Variables:", choices=options_show)
                         ))
      ),
      column(12,renderPrint(class(getdata_upload0()))),
      column(12,plotOutput("summ_num", height = '700px'))
    )



  })
  output$stats_fac <- renderUI({
    column(12,
           column(12,

                  h5(strong("Factors:")),
                  h5(strong("Structure:")),
                  verbatimTextOutput('strlabels'),


           ),
           column(12,
                  column(12,actionButton('downp_stats_fac',icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
                  column(12,plotOutput("factorsplot"))))
  })
  output$psummary <- renderPrint({
    data=getdata_upload0()
    withProgress(message = "Calculating Data-Attribute summary ... Please, wait!",
                 min = 1,
                 max = 13,
                 {

                   nas=sum(is.na(unlist(data)))
                   incProgress(1)

                   n=data.frame(rbind(Param=paste('Missing values:', nas)))
                   incProgress(1)
                   a<-data.frame(rbind(Param=paste('nrow:', nrow(data)),paste('ncol:', ncol(data))))
                   incProgress(1)


                   ppsummary("-------------------")
                   incProgress(1)
                   ppsummary(n)
                   ppsummary("-------------------")
                   incProgress(1)
                   ppsummary(a)
                   ppsummary("-------------------")
                   incProgress(1)


                 })


  })
  output$summ_num<-renderPlot({
    data=getdata_upload0()
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/50))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))


    options_num_sel<-options_num[[which(options_show==input$splitdata)]]


    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    str_numerics(data)
    vals$varplot<-recordPlot()
  })
  output$summ_fac<-renderPlot({
    data=getdata_upload0()
    factors<-data[,unlist(lapply(data,is.factor))]
    str_factors(factors,newcolhabs=vals$newcolhabs)

  })
  mds.reactive <- reactive({
    req (input$distance %in%c("bray","euclidean","jaccard"))
    withProgress(message = "Running ...",
                 min = 1,
                 max = 1,
                 {
                   mds_data <- metaMDS (getdata_upload0(), distance = input$distance)
                 })
  })
  plot_mds<-reactive({
    validate(need(input$distance!='', "Select a distance measure for the mds"))
    mds_data = mds.reactive()
    if (exists("mds_data")) {
      pmds(
        mds_data = mds_data,
        key = mds_symbol_factor(),
        points =input$mds_show_symbols,
        text = input$mds_show_labels,
        palette = input$mds_colpalette,
        cex.points = input$mds_cexpoint,
        cex.text = input$mds_cextext,
        pch=mds_symbol(),
        keytext=mds_text_factor(),
        newcolhabs=vals$newcolhabs,
        textcolor=input$mds_labcolor,
        pos=input$mds_labadj,
        offset=input$mds_offset
      )
    }
    vals$pmds_plot<-recordPlot()
    res




  })
  output$mdscustom <- renderPlot({plot_mds()})
  output$stats_ppca <- renderUI({
    validate(need(!anyNA(getdata_upload0()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))


    column(12,renderPlot({

      suppressWarnings({
        ppca(
          getdata_upload0(),
          key = pca_symbol_factor(),
          points = input$pca_show_symbols,
          text = input$pca_show_labels,
          palette = input$pca_colpalette,
          cex.points = input$pca_cexpoint,
          cex.text = input$pca_cextext,
          pch=pca_symbol(),
          keytext=pca_text_factor(),
          biplot=input$biplot,
          newcolhabs=vals$newcolhabs,
          textcolor=input$pca_labcolor,
          pos=input$pca_labadj,
          offset=input$pca_offset
        )
      })
      vals$ppca_plot<-recordPlot()

    }))

  })
  output$stats_pmds <- renderUI({
    validate(need(!anyNA(getdata_upload0()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))
    res<-list(
      column(12,
             column(12,plotOutput("mdscustom")))
    )

    res
  })
  res_pfac<-reactive({
    attr(getdata_upload0(),"factors")[rownames(getdata_upload0()),,drop=F]
  })
  output$factorsplot<-renderPlot({
    pfac(res_pfac())
  })
  output$stats_data <- renderUI({
    column(
      12, style = "background: white;",
      fluidRow(

        column(12,
               h5(strong(
                 "numeric variables:"
               ))),
        column(6, verbatimTextOutput("psummary")),
        column(12,uiOutput("Locate_NA"))




      )
    )
  })
  output$Locate_NA<-renderUI({
    req(anyNA(unlist(getdata_upload0())))
    div(
      column(12,strong("Missing Values:")),
      column(12,
             div(
               tags$style('#missing_values td {padding: 0}'),
               inline(
                 DT::dataTableOutput("missing_values")
               )
             ))
    )
  })
  output$missing_values<-DT::renderDataTable({
    data=getdata_upload0()
    res0<-res<-which(is.na(data), arr.ind=TRUE)
    for(i in 1:nrow(res)){
      res0[i,1]<-rownames(data)[res[i,1]]
      res0[i,2]<-colnames(data)[res[i,2]]
    }
    colnames(res0)<-c("ID","Variable")
    rownames(res0)<-NULL
    res0
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 't'), rownames = F,class ='cell-border compact stripe')
  output$summ_out<-renderUI({
    column(12,switch (input$summ_options,
                      "Data" = uiOutput("stats_data"),
                      "Variables" = uiOutput("stats_var"),
                      "Factors" = uiOutput("stats_fac"),
                      "Datalist"=datalist_render(getdata_upload0())
    )
    )


  })
  plotbox<-reactive({
    res <- getbox()
    req(length(res)>0)
    req(input$box_palette)
    boxp<-pbox(res=res,
               palette=input$box_palette,
               varwidth=input$varwidth,
               coefic=1.5,
               lab_out=labbox(),
               show_outs=input$show_outs,
               lwd=as.numeric(input$box_lwd),
               ylim=c(input$ymin_box, input$ymax_box),
               main=input$titlebox,
               insetx=input$insetx,insety=input$insety,
               xlab.adj=input$box_xlab.adj,
               srt=input$box_srt,
               srty=input$box_srt_y,
               newcolhabs=vals$newcolhabs,
               showleg=input$showboxleg,
               mar_b=input$boxmar_b,
               mar_l=input$boxmar_l,
               mar_t=input$boxmar_t,
               mar_r=input$boxmar_r,
               xsize=input$box_xsize,
               ysize=input$box_ysize,
               tck=input$box_tick,
               ylab_text=input$box_ylab_text,
               lineylab=input$box_ylab_adj,
               cex_ylab=input$box_ylab_size,
               font_ylab=input$box_ylab_font,
               xlab_text=input$box_xlab_text,
               linexlab=input$box_xlab_adj,
               cex_xlab=input$box_xlab_size,
               font_xlab=input$box_xlab_font,
               horizontal=input$box_horiz
    )

    vals$pbox_plot<-recordPlot()
    boxp
  })
  labbox<-reactive({

    if(isTRUE(input$showout)){data.frame(attr(getdata_upload0(),"factors")[as.character(input$out_label)]
    )} else{
      NULL
    }
  })
  box_y_cur<-reactiveValues(df=1)
  observeEvent(input$box_y,{
    box_y_cur$df<-input$box_y
  })
  box_y_input <- reactive({
    data <- getdata_upload0()
    selectInput(
      'box_y',
      tipify(
        strong("Y ~"),
        " y is the data values to be split into groups according to the grouping variable", options = list(container="body")
      ),
      choices = colnames(data),
      selected= box_y_cur$df
    )
  })
  output$box_y_input <- renderUI({box_y_input()  })
  output$filter_box2 <- renderUI({
    filter_box2()
  })
  filter_box2_cur<-reactiveValues(df=1)
  observeEvent(input$filter_box2,{
    filter_box2_cur$df<-input$filter_box2
  })
  filter_box2 <- reactive({
    req(input$filter_box1)
    if (input$filter_box1 != "none") {
      data = getdata_upload0()
      labels <- attr(data,"factors")[rownames(data), input$filter_box1]
      selectInput('filter_box2',
                  "Class:",
                  choices = c(levels(as.factor(labels))),
                  selected=filter_box2_cur$df)
    }
  })
  filter_box1_cur<-reactiveValues(df=1)
  observeEvent(input$filter_box1,{
    filter_box1_cur$df<-input$filter_box1
  })
  output$filter_box1<-renderUI({
    selectInput('filter_box1',"Filter:",choices = c("none", colnames(attr(getdata_upload0(),"factors"))),selected=filter_box1_cur$df)
  })
  box_factor_cur<-reactiveValues(df=1)
  observeEvent(input$box_factor,{
    box_factor_cur$df<-input$box_factor
  })
  output$box_factor<-renderUI({
    selectInput('box_factor',"Factor:",choices = rev(colnames(attr(getdata_upload0(),"factors"))),selected=box_factor_cur$df)
  })
  output$title_boxplot<-renderUI({
    textInput("titlebox",NULL , value=titlebox(), width="150px")
  })
  output$editbox<-renderUI({
    req(input$box_factor)
    req(input$filter_box1)
    res<-na.omit(getbox())
    fluidRow(class="map_control_style",style="color: #05668D",
             div(span('+ Title:',
                      inline(
                        uiOutput("title_boxplot")
                      )
             )),
             div(span("+ Palette:",inline(
               pickerInput(inputId = "box_palette",
                           label = NULL,
                           choices = vals$colors_img$val,
                           choicesOpt = list(content = vals$colors_img$img), options=list(container="body"), width="75px")
             ))),
             div(span("+ Margins:",
                      tipify(numericInput("boxmar_b",NULL,value=5,step=0.1, width="30px"),"bottom", placement = "right"),
                      tipify(numericInput("boxmar_l",NULL,value=5,step=0.1, width="30px"),"left", placement = "right"),
                      tipify(numericInput("boxmar_t",NULL,value=5,step=0.1, width="30px"),"top", placement = "right"),
                      tipify(numericInput("boxmar_r",NULL,value=5,step=0.1, width="30px"),"right", placement = "right"))),

             div(span("+",inline(
               checkboxInput("box_horiz","Horizontal",F, width="95px")
             ))),
             div(span("+",inline(
               checkboxInput("show_outs","Outliers:",T, width="95px")
             ))),
             conditionalPanel("input.show_outs % 2",{
               column(12,span("+",inline(
                 checkboxInput("showout","Labels:",F, width="95px")
               ),inline(
                 conditionalPanel("input.showout % 2",{
                   div(
                     pickerInput("out_label",NULL,choices=colnames(attr(getdata_upload0(),"factors")), width="95px")
                   )
                 })
               )))
             }),

             div(span("+",inline(
               checkboxInput("varwidth","Varwidth",F, width="95px")
             ),tipify(icon("fas fa-question-circle"),"Drawn boxes with widths proportional to the square-roots of the number of observations in the groups", options =list(container="body")))),

             div(span("+ Line width:",inline(
               numericInput("box_lwd",NULL,value=1.2,step=0.1, width="75px")
             ))),
             column(12,style="border-top: 1px solid #05668D;border-bottom: 1px solid #05668D",
                    fluidRow(
                      div(uiOutput("box_xaxis")),
                      column(12,
                             div(span('+ Label:',
                                      inline(
                                        textInput("box_xlab_text",NULL , value=input$box_factor, width="120px")
                                      )
                             )),
                             div(span("+ Label size:",inline(
                               numericInput("box_xlab_size",NULL,value=1,step=.1, width="75px")
                             ))),
                             div(span("+ Label font:",inline(
                               pickerInput("box_xlab_font",NULL,choices=c("plain","bold","italic","bold_italic"), width="80px")
                             ))),
                             div(span("+ Label adjust:",inline(
                               numericInput("box_xlab_adj",NULL,value=2,step=.1, width="75px")
                             ))),
                             div(span("+ Axis size:",inline(
                               numericInput("box_xsize",NULL,value=1,step=.1, width="75px")
                             ))),
                             div(span("+ Axis rotation:",inline(
                               numericInput("box_srt",NULL,value=0,step=1, width="75px")
                             ))),
                             div(span("+ Axis lab-adjust:",inline(
                               numericInput("box_xlab.adj",NULL,value=0,step=1, width="55px")
                             ))))
                    )
             ),

             column(12,
                    fluidRow(
                      div(uiOutput('box_yaxis')),
                      column(12,
                             div(span('+ Label:',
                                      inline(
                                        textInput("box_ylab_text",NULL , value=input$box_y, width="120px")
                                      )
                             )),
                             div(span("+ Label size:",inline(
                               numericInput("box_ylab_size",NULL,value=1,step=.1, width="75px")
                             ))),

                             div(span("+ Label-font:",inline(
                               pickerInput("box_ylab_font",NULL,choices=c("plain","bold","italic","bold_italic"), width="80px")
                             ))),
                             div(span("+ Label adjust:",inline(
                               numericInput("box_ylab_adj",NULL,value=3,step=.1, width="75px")
                             ))),

                             div(span("+ Axis size:",inline(
                               numericInput("box_ysize",NULL,value=1,step=.1, width="75px")
                             ))),
                             uiOutput("box_y_rotation"),
                             div(span("+ min-value:",inline(
                               numericInput("ymin_box",NULL,value=floor(min(na.omit(res[,2]))),step=0.1, width="75px")
                             ))),
                             div(span("+ max-value:",inline(
                               numericInput("ymax_box",NULL,value=ceiling(max(na.omit(res[,2]))),step=0.5, width="75px")
                             )))
                      )
                    )
             ),
             column(12,style="border-top: 1px solid #05668D;",

                    span("+ Tick size:",inline(
                      numericInput("box_tick",NULL,value=-0.02,step=0.01, width="75px")
                    ))),
             column(12,style="border-top: 1px solid #05668D;",
                    fluidRow(
                      div(

                        span("+",inline(
                          checkboxInput("showboxleg","Show legend:",F, width="95px")
                        ))),
                      conditionalPanel("input.showboxleg % 2",{
                        div(
                          div(span("+ leg y:",inline(
                            numericInput("insety",NULL,value=0,step=0.1, width="75px")
                          ))),
                          div(span("+ leg x:",inline(
                            numericInput("insetx",NULL,value=0,step=0.1, width="75px")
                          )))
                        )
                      })
                    )),
             column(12,style="border-top: 1px solid #05668D;",
                    fluidRow(
                      actionLink(
                        'downp_box',span("+ Download",icon("fas fa-download")), style="button_active"
                      )

                    ))


    )

  })
  output$box_y_rotation<-renderUI({
    req(isFALSE(input$box_horiz))
    div(span("+ Axis rotation:",inline(
      pickerInput("box_srt_y",NULL,choices=c("horizontal","vertical"), width="75px")
    )))

  })
  observeEvent(input$box_horiz,{
    if(isTRUE(input$box_horiz)){

      #updateNumericInputIcon(session,"box_xlab.adj", value=200)
      updateNumericInputIcon(session,"box_xlab_adj", value=5)
      updateNumericInputIcon(session,"boxmar_l", value=7)

    }
  })
  output$box_xaxis<-renderUI({
    if(isFALSE(input$box_horiz)){
      span("+ X-axis:")} else{
        span("+ Y-axis:")
      }
  })
  output$box_yaxis<-renderUI({
    if(isFALSE(input$box_horiz)){
      span("+ Y-axis:")} else{
        span("+ X-axis:")
      }
  })
  observeEvent(input$downp_box,{
    vals$hand_plot<-vals$bag_submenu
    showModal(downplot_center())
  })
  titlebox<-reactive({
    if(isFALSE(input$box_horiz)){
      y<-input$box_y
      x<-input$box_factor
    } else{
      x<-input$box_y
      y<-input$box_factor

    }
    a<-paste(y,"~",x)
    if(input$filter_box1!='none'){b=paste0("[",input$filter_box1,"::",input$filter_box2,"]")} else{b=NULL}
    paste(a,b)
  })

  output$stats_rda<-renderUI({
    validate(need(!anyNA(getdata_upload0()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))

    column(12,style="background: white;",
           column(12,uiOutput("rda_out"))
    )
  })
  observe({
    input$revert_rda %% 2
    updateRadioGroupButtons(session,'desc_options',selected='RDA')
  })
  rda_model<-reactive({
    data<-getdata_upload0()
    x<-as.matrix(data)
    colnames(x)<-colnames(getdata_upload0())
    if(length(input$rda_X)>0){
      y<-na.omit(vals$saved_data[[input$rda_X]][rownames(x),,drop=F])
      colnames(y)<-colnames(vals$saved_data[[input$rda_X]])
      x<-na.omit(x[rownames(y),])
      model=rda(x~.,data=data.frame(y) ,scale=input$rda_scale)
    } else{model= rda(x,scale=input$rda_scale)}
    model})
  output$rda_plot<-renderPlot({
    prda(rda_model(),
         key = rda_symbol_factor(),
         points =input$rda_show_symbols,
         text = input$rda_show_labels,
         palette = input$rda_colpalette,
         cex.points = input$rda_cexpoint,
         cex.text = input$rda_cextext,
         pch=c(rda_symbol(),3),
         keytext=rda_text_factor(),
         biplot=input$biplot_rda,
         show.sp=input$sp_rda,
         n.sp=input$rda_spnum,
         sp.display=input$rda_sp_display,
         pch.sp=as.numeric(input$rda_spshape),
         col.sp=getcolhabs(vals$newcolhabs,input$rda_spcolor,1),
         textcolor=input$rda_labcolor,
         scaling=input$rda_scaling,
         newcolhabs=vals$newcolhabs,
         pos=input$rda_labadj,
         offset=input$rda_offset
    )
    vals$rda_plot<-recordPlot()
    rda

  })
  output$rda_out<-renderUI({
    req(input$rda_view)
    switch(input$rda_view,
           "Plot"=  plotOutput('rda_plot'),
           "Summary"=  verbatimTextOutput("rda_print"))
  })
  rda_summary<-reactive({
    res<-summary(rda_model())
    res<-switch(input$disp_rda_summ,
                "Variable scores"=res$species,
                "Observation scores"=res$sites,
                "Linear constraints"=res$constraints,
                "Biplot"=res$biplot,

                "Importance (unconstrained)"=res$cont$importance,
                "Importance (constrained)"=res$concont$importance

    )

    res[,1:input$rda_axes]
  })
  output$rda_print<-renderPrint({  rda_summary()})
  observeEvent(input$downcenter_rda,{
    vals$hand_down<-"rda"
    showModal(downcenter())
  })
  getord<-reactive({
    req(input$segrda_X)
    req(input$axis_ord_segrda)
    x<-as.matrix(getdata_upload0())
    colnames(x)<-colnames(getdata_upload0())
    y<-na.omit(as.matrix(vals$saved_data[[input$segrda_X]][rownames(x),,drop=F]))
    colnames(y)<-colnames(vals$saved_data[[input$segrda_X]])
    x<-na.omit(x[rownames(y),])
    if(isTRUE(input$segrda_ord)){
      sim1o<-OrdData(x=y,y=x, axis=input$axis_ord_segrda,scale=input$segrda_scale)} else{
        sim1o<-list()
        sim1o$xo<-y
        sim1o$yo<-x
      }
    sim1o

  })
  output$ord_segrda<-renderUI({
    req(isTRUE(input$segrda_ord))

    fluidRow(
      renderPlot({
        sim1o<-getord()
        xo<-sim1o$xo ## ordered explanatory matrix.
        yo<-sim1o$yo ## ordered community matrix (untransformed).
        x<-sim1o$y
        par(mfrow = c(1, 2), mgp = c(1, 1, 0), cex = 0.9)
        image(x, main = "Original response data", col = topo.colors(100), axes = F,
              xlab = "Observations", ylab = "Variable values")
        image(yo, main = "Ordered response data", col = topo.colors(100), axes = F,
              xlab = "Observations", ylab = "Variable values")
      })
    )
  })
  output$segrda_windows<-renderUI({
    inline(
      span(h5(strong("Pool:"), style="color: #0D47A1"),
           if(input$segrda_w%%2!=0){
             em("Window size must be even", style="color: gray")
           },
           div(inline(verbatimTextOutput("getpool")))
      )
    )
  })
  output$getpool<-renderPrint({
    getpool()
  })
  output$getpool2<-renderPrint({
    getpool()
  })
  observeEvent(input$include_window,{
    validate(need(input$segrda_w%%2==0, "Window size must be even"))
    vals$segrda_windows<-c(vals$segrda_windows,input$segrda_w)
  })
  observeEvent(input$remove_windows,{
    vals$segrda_windows<-NULL
  })
  output$smw_create<-renderUI({
    req(input$smw_default=='Create')
    fluidRow(
      inline(
        numericInput("segrda_w","window",value=if(is.null(vals$segrda_windows)){10}else{vals$segrda_windows[length(vals$segrda_windows)]+2}, step=2, min=4, width="75px")
      ),
      inline(
        actionButton("include_window",tipify(icon("fas fa-arrow-right"),"Include window size in the pool"), style="button_active")
      ),
      inline(uiOutput("segrda_windows")),
      inline(
        actionButton("remove_windows",tipify(icon("fas fa-eraser"),"restart"), style="button_active")
      )
    )

  })
  output$segrda_smw<-renderUI({
    div(
      div(
        inline(
          radioGroupButtons("smw_default","Window Pool",choices=c("Default","Create"), width = "150px")
        ),
        inline( uiOutput('smw_create')),
        inline(conditionalPanel("input.smw_default=='Default'",{
          inline(
            span(h5(strong("Pool:"), style="color: #0D47A1"),
                 div(inline(verbatimTextOutput("getpool2")))
            )
          )
        }))
      ),


      sidebarLayout(
        sidebarPanel(uiOutput('side_smw'),
                     br(),
                     div(style="margin-left: 20px",
                         actionButton("go_smw",strong( img(src=smw_icon,height='20',width='20'),"run SMW"), style="button_active")
                     )
        ),


        mainPanel( uiOutput("go_smw")),
      )


    )
  })
  output$side_smw<-renderUI({
    fluidRow( class="map_control_style",style="color: #05668D",
              div(
                span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"Dissimilarity index", options=list(container="body")),"+ Distance:"),
                     inline(
                       pickerInput("smw_dist",NULL, choices=c("bray","euclidean","manhattan","jaccard"), width="100px")
                     )
                )
              ),
              div(
                span(span(tipify(actionLink('smw_rand_help',icon("fas fa-question-circle")),"The type of randomization for significance computation. Click for details", options=list(container="body")),"+ Randomization:"),
                     inline(
                       pickerInput("smw_rand",NULL, choices=c("shift","plot"), width="70px")
                     )
                )
              ),
              div(
                span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"The number of randomizations", options=list(container="body")),"+ n.rand:"),
                     inline(
                       numericInput("smw_nrand",NULL, value=10, width="100px")
                     )
                )
              ),
              if(length(vals$smw_dp)>0){

                tipify(
                  actionLink(
                    'downp_we',span("+ Download",icon("fas fa-download")), style="button_active"
                  ),
                  "Download plot", options=list(container="body")
                )

              }

    )
  })
  bag_smw<-reactiveValues(df=F)
  observeEvent(input$go_smw,{
    bag_smw$df<-T
    req(!is.null(getpool()))
    sim1o<-getord()
    xo<-sim1o$xo ## ordered explanatory matrix.
    yo<-sim1o$yo ## ordered community matrix (untransformed)

    y=yo;ws=getpool(); dist=input$smw_dist;rand=input$smw_rand;n.rand=input$smw_nrand

    if (n.rand < 2) {
      stop("number of randomizations not alowed")
    }
    if (any(ws%%2 == 1)) {
      stop("all Window sizes must be enven")
    }
    rand <- match.arg(rand, c("shift", "plot"))
    argg <- c(as.list(environment()), list())
    smw <- list()


    withProgress(message = paste0("SMW analysis (", 1, "/", length(ws), "); w =",
                                  ws[1]),
                 min = 1,
                 max = length(ws),
                 {
                   for (j in 1:length(ws)) {
                     w1 <- ws[j]
                     DPtable <- smw.root2(yo, w1, dist)
                     OB <- DPtable[, 3]
                     rdp <- data.frame(rep(NA, length(OB)))
                     seq_yo<-1:nrow(yo)
                     withProgress(message="randomizing",min = 1,
                                  max = n.rand,{
                                    for (b in 1:n.rand) {
                                      if (rand == "shift") {

                                        comm.rand <- apply(yo, 2, function(sp) sp[sample(seq_yo)])
                                        rdp[b] <- smw.root2(data.frame(comm.rand),
                                                            w1, dist)[3]

                                      } else if (rand == "plot") {

                                        comm.rand <- t(apply(yo, 1, function(sp) sp[sample(seq_yo)]))
                                        rdp[b] <- smw.root2(data.frame(comm.rand),
                                                            w1, dist)[3]
                                      }

                                      incProgress(1)
                                    }
                                  })

                     rownames(rdp) <- DPtable[,1]
                     Dmean <- apply(rdp, 1, mean)
                     SD <- apply(rdp, 1, sd)
                     oem <- sum(Dmean)/(nrow(yo) - w1)
                     osd <- sum(SD)/(nrow(yo) - w1)
                     Dz <- (OB - oem)/osd
                     DPtable$zscore <- Dz
                     smw[[j]] <- list(dp = data.frame(DPtable), rdp = matrix(rdp),
                                      md = Dmean, sd = SD, oem = oem, osd = osd, params = argg)
                     class(smw[[j]]) <- c("smw")
                     incProgress(1, message=paste0("SMW analysis (", j+1, "/", length(ws), "); w =",
                                                   ws[j+1]))
                   }
                 })


    names(smw) <- paste("w", ws, sep = "")
    class(smw) <- c("smw")
    vals$smw_dp<-isolate(smw)

  })
  output$go_smw<-renderUI({
    validate(need(length(vals$smw_dp)==length(getpool()),"Please click 'run SMW' button"))
    validate(need(!is.null(getpool()),"The window pool is empty. Use the arrow button to include the window sizes"))
    validate(need(length(vals$smw_dp)>0,"Please click 'run SMW' button"))



    fluidRow(
      column(12,
             uiOutput("smw_warning"),
             renderPlot({



               if(length(getpool())>1){w.effect=TRUE
               main="Window size effect"} else{w.effect=F
               main="Dissimilary Profile"}

               suppressWarnings(plot( vals$smw_dp, w.effect =w.effect  , main=main))
               vals$plot_we<-recordPlot()

             })
      )
    )
  })
  observeEvent(input$go_smw,{
    output$smw_warning<-renderUI({
      req(is.null(getpool()))
      em("Pool is empty")
    })
  })
  max_seq<-reactive({
    validate(need(any(DP_smw()[,5]!='ns'),"DP without any significant dissimilarity value: no breakpoint could be determined."))

    df<-DP_smw()[,c(1,5)]
    colnames(df)<-c("a","b")
    new=transform(df, Counter = ave(a, rleid(b), FUN = seq_along))
    max_seq<-max(new[new[,2]=="*",3])
    attr(max_seq,"min")<-if(max_seq!=1){
      min( new[new[,2]=="*",3][new[new[,2]=="*",3]!=1])} else{
        min(new[new[,2]=="*",3])
      }



    max_seq
  })
  output$plot_dp<-renderPlot({
    max_seq<-max_seq()
    if(bag_smw$df==T){
      updateNumericInput(session,"dp_seq.sig", value=attr(max_seq,"min"))
      bag_smw$df<-F}

    validate(need(input$dp_seq.sig<=max_seq,paste(
      "DP shows '", sum(DP_smw()[,5]!="ns"),"' significant dissimilarity values but no breakpoint could be determined for seq.sig='",input$dp_seq.sig,"The maximum value for this input must be","max_seq"
    )))
    smw<- vals$smw_dp
    getDP()
    dp_BPs<-if(input$dp_BPs==""){NULL} else{input$dp_BPs}
    dp_w<-if(is.na(input$dp_w)){NULL} else{input$dp_w}
    par(cex=input$dp_cex)
    suppressWarnings(
      suppressMessages(
        plot(smw,w=dp_w, sig=input$dp_sig, z=input$dp_z,   BPs=dp_BPs,
             seq.sig=input$dp_seq.sig, bg= getcolhabs(vals$newcolhabs,input$dp_palette,nlevels(as.factor(vals$splitBP[,1]))),bg_alpha=input$dp_bg,cols=c(getcolhabs(vals$newcolhabs,input$dp_dcol,1),getcolhabs(vals$newcolhabs,input$dp_scol,1),getcolhabs(vals$newcolhabs,input$dp_bcol,1)))
      )
    )

    if(isTRUE(updp$df)){
      updateTabsetPanel(session,'segrda_panels','pwRDA')
      updp$df<-F
    }

    vals$plot_dp<-recordPlot()
  })
  DP_smw<-reactive({
    smw<- vals$smw_dp
    dp_BPs<-if(input$dp_BPs==""){NULL} else{input$dp_BPs}
    dp_w<-if(is.na(input$dp_w)){NULL} else{input$dp_w}
    suppressWarnings(
      suppressMessages(
        extract(smw,w=dp_w,
                index=input$dp_index,
                sig=input$dp_sig,
                z=input$dp_z,
                BPs=dp_BPs,
                seq.sig=input$dp_seq.sig)
      )
    )
  })
  output$dp_extract<-renderUI({
    dp<-getDP()
    fluidRow(
      column(12,renderPrint({dp}))
      #renderPrint({vals$splitBP})
    )
  })
  observeEvent(input$downcenter_dp_smw,{
    vals$hand_down<-"DP smw"
    showModal(downcenter())
  })
  getpool<-reactive({
    if(input$smw_default=="Default"){
      w<-1:(nrow(getdata_upload0())/2)
      w<-  w[which(( w %% 2) == 0)]
      w<-round(seq(10,w[length(w)], length.out=5))
      w[which(( w %% 2) != 0)]<-w[which(( w %% 2) != 0)]+1} else{
        w<-vals$segrda_windows
      }

    w

  })
  output$side_dp<-renderUI({

    validate(need(length(vals$smw_dp)>0,"You need to run SMW analysis first"))
    fluidRow(
      class="map_control_style",style="color: #05668D",
      div(
        div(
          span(
            "+ Display:", pickerInput("dp_view",NULL, choices=c("Plot", "DP results"), width="100px")
          )
        ),
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"A target window size from which results will be extracted. If empty return z-scores averaged over the set of window sizes", options=list(container="body")),'+ w:'),
             inline(
               numericInput("dp_w",NULL, value=NULL, width="75px")
             )
        )
      ),
      div(
        span(span(actionLink("dp_index_help",tipify(icon("fas fa-question-circle"),"The result to be extracted. Click for details", options=list(container="body"))),'+ index:'),
             inline(
               pickerInput("dp_index",NULL,choices=c("dp","rdp","md","sd","oem","osd","params"), width="75px")
             )
        )
      ),
      div(
        span(span(actionLink("dp_sig_help",tipify(icon("fas fa-question-circle"),"Significance test for detecting dissimilarity values that differs significantly from those appearing in a random pattern. Click for details", options=list(container="body"))),'+ sig'),
             inline(
               pickerInput("dp_sig",NULL,choices=c("z","sd","sd2","tail1"), width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"The critical value for the significance of z-values", options=list(container="body")),"+ z:"),
             inline(
               numericInput("dp_z",NULL, value=1.85,step=0.01, width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"Defines if the breakpoints should be chosen as those sample positions corresponding to the maximum dissimilarity in a sequence of significant values (max) or as those sample positions corresponding to the median position of the sequence (median). Defaults to BPs=max. If empty the breakpoints are not computed", options=list(container="body")),"+ BPs:"),
             inline(
               pickerInput("dp_BPs",NULL,choices=c("","max","median"), selected = "max", width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"The maximum length of consecutive, significant values of dissimilarity that will be considered in defining the community breakpoints", options=list(container="body")),"+ seq.sig:"),
             inline(
               numericInput("dp_seq.sig",NULL, value=3,step=1, width="75px", min=1)
             )
        )
      ),
      div(uiOutput("save_bp")),
      conditionalPanel("input.dp_view=='DP results'",{
        tipify(
          actionLink(
            'downcenter_dp_smw',span("+ Download",icon("fas fa-table")), style="button_active"
          ),
          "Download DP results", options=list(container="body")
        )
      }),
      conditionalPanel("input.dp_view=='Plot'",
                       div(
                         div(
                           span("+ Palette",
                                inline(
                                  pickerInput(inputId = "dp_palette",
                                              label = NULL,
                                              choices =     vals$colors_img$val[getgrad_col()],
                                              choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body"), width="75px")
                                )
                           )
                         ),

                         div(
                           span("+ size",
                                inline(
                                  numericInput("dp_cex", NULL,value=1,min=0.1,step=0.1, width="75px")
                                )
                           )
                         ),

                         div(
                           span("+ diss col",
                                inline(
                                  pickerInput(inputId = "dp_dcol",
                                              label = NULL,
                                              choices =   vals$colors_img$val[getsolid_col()],
                                              choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), options=list(container="body"), width="75px")
                                )
                           )
                         ),
                         div(
                           span("+ sig col",
                                inline(
                                  pickerInput(inputId = "dp_scol",
                                              label = NULL,
                                              choices =   vals$colors_img$val[getsolid_col()],
                                              choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), options=list(container="body"), selected=  vals$colors_img$val[getsolid_col()][4], width="75px")
                                )
                           )
                         ),
                         div(
                           span("+ bp col",
                                inline(
                                  pickerInput(inputId = "dp_bcol",
                                              label = NULL,
                                              choices =   vals$colors_img$val[getsolid_col()],
                                              choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), options=list(container="body"), selected=  vals$colors_img$val[getsolid_col()][3], width="75px")
                                )
                           )
                         ),

                         div(
                           span("+ bg",
                                inline(numericInput("dp_bg", NULL,value=0.5,min=0,max=1,step=0.05, width="75px"))
                           )
                         ),
                         tipify(
                           actionLink(
                             'downp_dp',span("+ Download",icon("fas fa-download"))
                           ),
                           "Download plot"
                         )
                       )
      )
    )

  })
  output$segrda_dp<-renderUI({
    validate(need(length(vals$smw_dp)>0,"You need to run SMW analysis first"))
    fluidRow(
      column(12,strong("Dissimilary Profile")),
      sidebarLayout(
        sidebarPanel(uiOutput(
          "side_dp"
        )),
        mainPanel(
          conditionalPanel("input.dp_view=='DP results'",{
            uiOutput("dp_extract")
          }),
          conditionalPanel("input.dp_view=='Plot'",{
            plotOutput("plot_dp")
          })
        )
      )





    )
  })
  observeEvent(input$tools_saveBP,{
    vals$hand_save<-"Create factor using breakpoints from the dissimilarity profile"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(
      hand_save_modal()
    )
  })
  observeEvent(input$dp_index_help,{
    showModal(
      modalDialog(
        column(12,
               p(strong("dp:"),span("The dissimilarity profile (DP) table containing significant discontinuities and suggested breakpoints")),
               p(strong("rdp:"),span("data frame containing the randomized DP;")),
               p(strong("md:"),span("mean dissimilarity of the randomized DP")),
               p(strong("sd:"),span("standard deviation for each sample position")),
               p(strong("ospan:"),span("overall expected mean dissimilarity;")),
               p(strong("osd:"),span("average standard deviation for the dissimilarities;")),
               p(strong("params:"),span("list with input arguments"))
        ),
        easyClose = T,
        size="m",
        title="index for extracting SMW results"
      )
    )

  })
  observeEvent(input$dp_sig_help,{
    showModal(
      modalDialog(
        column(12,
               p(strong("z:"),span("consider normalized dissimilarity (z-scores) discontinuities that exceed a z critical value")),
               p(strong("sd:"),span("consider dissimilarity discontinuities that exceed mean plus one standard deviation")),
               p(strong("sd2:"),span("consider dissimilarity discontinuities that exceed mean plus two standard deviation")),
               p(strong("tail1:"),span("Consider dissimilarity discontinuities that exceed 95 percent confidence limits"))

        ),
        easyClose = T,
        size="m",
        title="Significance test fort the SMW results"
      )
    )

  })
  observeEvent(input$inc_bp,{
    vals$bag_user_bp<-c(vals$bag_user_bp,input$pw_user)
  })
  output$user_bp<-renderPrint(vals$bag_user_bp)
  output$getDP<-renderPrint({
    req(!isFALSE(vals$splitBP))
    suppressWarnings(bp(getDP()))})
  output$pw_out<-renderUI({

    div(
      span(
        inline(
          div(
            inline(
              if(!isFALSE(vals$splitBP)){radioButtons("bp_pwrda","Breakpoints",c("suggested","user-defined"), selected="suggested", width="150px")}else{radioButtons("bp_pwrda","Breakpoints",c("user-defined"), width="150px")}
            )
          )
        ),
        inline(
          conditionalPanel("input.bp_pwrda=='suggested'",{
            inline(
              span(h5(strong("Breaks:"), style="color: #0D47A1"),
                   div(inline(verbatimTextOutput("getDP")))
              )
            )
          })
        ),
        inline(
          conditionalPanel("input.bp_pwrda=='user-defined'",{
            inline(
              div(
                inline(numericInput("pw_user", 'Breaks:',value=NULL, width="75px")),
                inline(tipify(actionButton("inc_bp",icon("fas fa-arrow-right")),"Type a number and click to include a breakpoint")),
                inline(verbatimTextOutput("user_bp")),
                inline(
                  actionButton("remove_breaks",tipify(icon("fas fa-eraser"),"restart"), style="button_active")
                )
              )
            )

          })
        ),


        inline(numericInput('pw_nrand', 'n.rand', value=99, width="75px")),
        inline(actionButton("run_pwrda",strong(img(src=pw_icon,height='20',width='20'),"run pwRDA"), style="button_active"))

      ),

      uiOutput('pwRDA_out')


    )
  })
  observeEvent(input$remove_breaks,{
    vals$bag_user_bp<-NULL
  })
  observeEvent(input$run_pwrda,{
    output$pwRDA_out<-renderUI({
      validate(need(length(getBP())>0, "No breakpoints found"))
      fluidRow(
        sidebarLayout(
          uiOutput("side_pw"),
          mainPanel(
            conditionalPanel("input.segrda_view=='Summary'",{
              verbatimTextOutput("segrda_print")
            }),
            conditionalPanel("input.segrda_view=='Plot'",
                             plotOutput("ppwRDA")
            ))
        )
      )
    })
  })
  output$side_pw<-renderUI({
    req(length(segrda_model()$rda.pw)>0)
    sidebarPanel(
      fluidRow(class="map_control_style",style="color: #05668D",
               div(
                 span("+",
                      inline(radioButtons("segrda_view",NULL,choices=c(
                        "Plot","Summary"),inline=T, width="150px"))
                 )
               ),
               conditionalPanel("input.segrda_view=='Summary'",{
                 div(
                   div(
                     span(
                       "+ Results:",
                       inline(
                         pickerInput("disegrda_sp_summ",NULL,choices=c('Summary stats','Variable scores','Observation scores','Linear constraints','Biplot','Importance (unconstrained)',"Importance (constrained)"), selected="Importance (unconstrained)", width="150px", options=list(container="body"))
                       )
                     )
                   ),
                   div(
                     span("+ Axes",
                          inline(
                            numericInput("segrda_axes",NULL, value=2, width="75px")
                          )
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         'downcenter_segrda',span("+ Download",icon("fas fa-download")), style="button_active"
                       ),
                       "Download selected results", options=list(container="body")
                     )

                   )
                 )
               }),
               conditionalPanel("input.segrda_view=='Plot'",{
                 div(
                   div(
                     '+ Scaling:',
                     inline(numericInput("segrda_scaling",NULL, 2, width="45px", step=1, min=0, max=3)),tipify(icon("fas fa-question-circle",style="color: gray"),"Scaling for species and site scores. Either species (2) or site (1) scores are scaled by eigenvalues, and the other set of scores is left unscaled, or with 3 both are scaled symmetrically by square root of eigenvalues.", placement = "bottom", options=list(container="body"))
                   ),
                   div(span("+",checkboxInput('segrda_biplot', span("Biplot", pophelp(NULL,"show biplot arrows")), T, width="75px"))),
                   div(span("+",checkboxInput('segrda_sp', span("Variables", pophelp(NULL,"Variables")), T, width="75px"))),
                   conditionalPanel("input.segrda_sp % 2",{
                     column(12,
                            div(
                              '+ Number:',
                              inline(numericInput("segrda_spnum",NULL, 10, width="45px", step=1)),tipify(icon("fas fa-question-circle",style="color: gray"),"Show N variables with the highest scores", placement = "bottom", options=list(container="body"))
                            ),
                            div(
                              "+ Display:",
                              span(
                                inline(
                                  pickerInput("segrda_sp_display",NULL,choices=c("Label","Shape"), width = "75px", options=list(container="body"))
                                ),
                                inline(
                                  conditionalPanel("input.segrda_sp_display=='Shape'",{
                                    inline(pickerInput(
                                      inputId = "segrda_spshape",
                                      label = NULL,
                                      choices = df_symbol$val,
                                      options=list(container="body"),
                                      selected=df_symbol$val[8],
                                      choicesOpt = list(content = df_symbol$img),width='35px'
                                    ))
                                  })
                                )
                              )
                            ),



                            div(
                              span("+ Variable Color:",
                                   inline(
                                     tipify(
                                       pickerInput(
                                         inputId = "segrda_spcolor",label = NULL,selected=  vals$colors_img$val[getsolid_col()][4],choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),       options=list(container="body"), width="75px"), "Variable Color.")))
                            )
                     )
                   }),

                   div(
                     span("+",
                          inline(checkboxInput('segrda_show_symbols',"Symbol" ,T, width='75px')))),
                   conditionalPanel("input.segrda_show_symbols % 2",{
                     column(12,
                            div(
                              span("+ Shape:",
                                   inline(pickerInput(inputId = "segrda_symbol",
                                                      label = NULL,
                                                      choices = df_symbol$val,
                                                      options=list(container="body"),
                                                      choicesOpt = list(content = df_symbol$img), width='75px')))),
                            div(
                              span("+ Size:",
                                   inline(numericInput("segrda_cexpoint",NULL,value = 1,min = 0.1,max = 3,step = .1, width='75px')
                                   ))
                            ),
                            div(
                              span("+ Color:",
                                   inline(
                                     tipify(
                                       pickerInput(inputId = "segrda_colpalette",label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='75px'), "Symbol palette. Choose a gradient to color observations by a factor")))),
                            uiOutput("fac_palette_pw")
                     )
                   }),
                   div(span("+",
                            inline(checkboxInput('segrda_show_labels',"Labels",F)
                            ))),
                   conditionalPanel("input.segrda_show_labels % 2",{
                     column(12,
                            div(span("+ Factor:",
                                     inline(tipify(pickerInput("segrda_labfactor",NULL,choices = colnames(attr(getdata_upload0(),"factors")), width="125px"),  "label classification factor")
                                     ))),

                            div(span("+ Lab Color:",
                                     inline(tipify(
                                       pickerInput(
                                         inputId = "segrda_labcolor",
                                         label = NULL,
                                         selected=  vals$colors_img$val[12],choices =   vals$colors_img$val,choicesOpt = list(content =   vals$colors_img$img),width="75px",
                                         options=list(container="body")
                                       ),  "label classification factor"
                                     )
                                     ))),
                            div(span("+ Lab adj:",
                                     inline(
                                       tipify(pickerInput("segrda_labadj",NULL,choices=c(1:4), width="75px", options=list(containder="body")),  "a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.", placement = "right")
                                     ))),
                            div(span("+ Lab offset:",
                                     inline(
                                       tipify(numericInput("segrda_offset",NULL,value = 0,step = .1, width="75px"),  "this value controls the distance (‘offset’) of the text label from the specified coordinate in fractions of a character width.")
                                     ))),
                            div(span("+ Size:",
                                     inline(
                                       tipify(numericInput("segrda_cextext",NULL,value = 1,min = 0.1,max = 3,step = .1),  "label text size")
                                     )))

                     )
                   }),
                   div(
                     tipify(
                       actionLink(
                         'downp_pw',span("+ Download",icon("fas fa-download")), style="button_active"
                       ), "Download plot"
                     ))
                 )
               })
      )
    )
  })
  output$fac_palette_pw<-renderUI({
    col<-getcolhabs(vals$newcolhabs,input$segrda_colpalette,2)
    req(col[1]!=col[2])

    div(
      span("+ Factor:",
           inline(tipify(pickerInput("segrda_symbol_factor",NULL,choices = rev(colnames(attr(getdata_upload0(),"factors"))), width='125px'), "symbol classification factor"))))
  })
  observeEvent(input$run_pwrda,{

    output$ppwRDA<-renderPlot({
      req(length(segrda_model()$rda.pw)>0)

      segrda<-prda(segrda_model()$rda.pw,
                   key = segrda_symbol_factor(),
                   points =input$segrda_show_symbols,
                   text = input$segrda_show_labels,
                   palette = input$segrda_colpalette,
                   cex.points = input$segrda_cexpoint,
                   cex.text = input$segrda_cextext,
                   pch=c(segrda_symbol(),3),
                   keytext=segrda_text_factor(),
                   biplot=input$segrda_biplot,
                   show.sp=input$segrda_sp,
                   n.sp=input$segrda_spnum,
                   sp.display=input$segrda_sp_display,
                   pch.sp=as.numeric(input$segrda_spshape),

                   col.sp=getcolhabs(vals$newcolhabs,input$segrda_spcolor,1),
                   textcolor=input$segrda_labcolor,
                   scaling=input$segrda_scaling,
                   newcolhabs=vals$newcolhabs,
                   pos=input$segrda_labadj,
                   offset=input$segrda_offset
      )
      vals$seg_rda_plot<-recordPlot()
      segrda

    })
  })
  segrda_symbol<-reactive({
    if(isFALSE(input$segrda_show_symbols)){NA}else{as.numeric(input$segrda_symbol)}
  })
  getBP<-reactive({
    if(input$bp_pwrda=="suggested"){
      bp(getDP())} else{
        vals$bag_user_bp
      }
  })
  segrda_model<-eventReactive(input$run_pwrda,{
    validate(need(length(getBP())>0, "No breakpoints found"))
    sim1o<-getord()

    model=suppressMessages(
      pwRDA2(sim1o$xo,sim1o$yo , BPs=getBP(),n.rand = input$pw_nrand)
    )
    model
  })
  segrda_summary<-reactive({
    res<-summary(segrda_model()$rda.pw)
    res<-switch(input$disegrda_sp_summ,
                "Summary stats"= segrda_model()$summ,
                "Variable scores"=res$species,
                "Observation scores"=res$sites,
                "Linear constraints"=res$constraints,
                "Biplot"=res$biplot,

                "Importance (unconstrained)"=res$cont$importance,
                "Importance (constrained)"=res$concont$importance

    )

    res[,1:input$segrda_axes]
  })
  output$segrda_print<-renderPrint({  segrda_summary()})
  observeEvent(input$downcenter_segrda,{
    vals$hand_down<-"segrda"
    showModal(downcenter())
  })
  observeEvent(input$tools_saveagg,{
    vals$hand_save<-"Create data list from aggregation results"
    vals$hand_save2<-if(!is.null(attr(aggreg(),"coords"))){p("The select Datalist contains a Coords-Attribute. Mean coordinates will be retrieved from the choosen factor group.")} else {NULL}
    vals$hand_save3<-NULL
    showModal(
      hand_save_modal()
    )
  })
  output$desc_facout<-renderUI({
    factors<-attr(getdata_upload0(),"factors")
    facs<-colnames(factors)


    colla<-factors[,facs[which(!facs%in%input$fac_descs)]]
    list(
      column(12,strong("Aggregated by:")),
      renderPrint(table(do.call(paste,args=c( colla, sep="_")))))
  })
  aggreg_reac<-reactiveValues(df=0)
  output$desc_dataout<-renderUI({
    data<-aggreg()
    aggreg_reac$df<-data
    factors<-attr(data,"factors")
    #aggregate(data,faclist,call(as.character(input$spread_measures)))
    list(
      DT::renderDataTable(cbind(factors,data),options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')
    )
  })
  aggreg<-reactive({
    data<-getdata_upload0()
    factors<-attr(data,"factors")[rownames(data),,drop=F]
    coords<-attr(data,"coords")
    df<- data.frame(aggregate(data,data.frame(factors[,input$fac_descs, drop=F]),get(input$spread_measures)))
    if(!is.null(coords)){
      coords<- data.frame(aggregate(coords,factors[,input$fac_descs, drop=F],mean ))
      coords<-coords[,which(unlist(lapply(coords,is.numeric))), drop=F]
      rownames(coords)<-rownames(df)
    }
    dfnum<-df[,which(unlist(lapply(df,is.numeric))), drop=F]
    dffac<-df[,which(unlist(lapply(df,is.factor))), drop=F]
    df<-dfnum
    df<-data_migrate(data,df,"Aggregated_results")
    rownames(dffac)<-rownames(df)


    attr(df,"factors")<-dffac
    attr(df,"coords")<-coords

    #if(anyNA(df)){df<-"Requires groups with at least two counts "}

    df
  })
  output$stats_pbox<-renderUI({
    fluidRow(
      column(12,renderPlot(plotbox()))
    )
  })
  output$strlabels <- renderPrint({

    ppsummary("----------------")
    ppsummary(paste("Missing values:",sum(is.na(attr(getdata_upload0(),"factors")))))
    ppsummary("----------------")
    str(attr(getdata_upload0(),"factors")[rownames(getdata_upload0()),,drop=F])
  })
  fn_download <- function()
  {

    fheight <- input$fheight
    fwidth <- input$fwidth
    fres <- as.numeric(input$fres)

    if(input$fformat=="pdf") fheight <- round(fheight*0.3937,2)
    if(input$fformat=="pdf") fwidth <- round(fwidth*0.3937,2)

    if(input$fformat=="png") png(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm", pointsize = input$pointsize)
    if(input$fformat=="tiff") tiff(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm",compression="lzw", pointsize = input$pointsize)
    if(input$fformat=="jpeg") jpeg(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm",quality=100, pointsize = input$pointsize)
    if(input$fformat=="pdf") pdf(fn_downloadf(), height=fheight, width=fwidth, pointsize = input$pointsize)

    switch (vals$hand_plot,
            "variable summary" = {
              data=getdata_upload0()
              numerics<-data[,unlist(lapply(data,is.numeric))]
              replayPlot(vals$varplot)
            },
            "factor summary" ={
              pfac(res_pfac())
            }
            ,
            "histogram" ={
              phist(getdata_upload0())
            },

            "boxplot" ={replayPlot(vals$pbox_plot)},
            "pca"= {replayPlot(vals$ppca_plot)},
            "mds"= {replayPlot(vals$pmds_plot)},
            "Training plot"={pchanges(vals$som_results)},
            "Couting plot"={  pcounts(vals$som_results)},
            "uMatrix"={   pUmatrix(vals$som_results)},
            "BMUs"={replayPlot(vals$bmus_plot)},
            "BMUs predictions"={replayPlot(vals$bmus_pred_plot)},
            "property plot"={replayPlot(vals$pprop_plot)},
            "Dendrogram"={replayPlot(vals$pdend_plot)},
            "screeplot_accRF data"={
              plot_accuclus(accRFdata(), sugg = sugg_accRF_data())},
            "screeplot_accRF som"={
              plot_accuclus(accRFmodel(), sugg = sugg_accRF_model())},
            "screeplot_WSS data"={
              elbow_plot(WSSdata(), sugg = sugg_WSS_data())},
            "screeplot_WSS som"={
              elbow_plot(WSSmodel(), sugg = sugg_WSS_model())},
            "Hcut"={hc_plot(phc())},
            "codebook clusters"= {replayPlot(vals$pclus_plot)},
            "Minimal Depth distribution"={plot(vals$rfd_res)},
            "Multi-way importance"={plot(vals$rfm_res)},
            "Decision Tree"={replayPlot(vals$ptree_plot)},
            "Map"={
              if(input$saved_maps=="new map"){plot(vals$map_res)} else{
                plot(vals$saved_maps[[input$saved_maps]])
              }
            },
            "rda"={replayPlot(vals$rda_plot)},
            "dp"={replayPlot(vals$plot_dp)},
            "we"={replayPlot(vals$plot_we)},
            "segrda"={replayPlot(vals$seg_rda_plot)},
            "Confusion Matrix SOM"={plot(vals$conf_som)},
            "Confusion Matrix - test RF"={plot(vals$conf_rf)},
            "Confusion Matrix RF"={plot(vals$cm_rf)},
            "Surface map"={replayPlot(persp_res$df)},
            "mantel plot"=replayPlot(mantel_plot$df),
            "Scatter 3D"=replayPlot(scatter_out$df),
            "stacked raster map"=replayPlot(stack_out$df),
            "RF interactions"=plot(rf_inter$df),
            "RF ranking comparations"=replayPlot(rf_rank$df),
            "RF measure comparations"=replayPlot(rf_rel$df),
            "RF - Partial Dependence"=plot(rfbiplot1$df)
    )
  }


  getbox <- reactive({
    req(input$box_factor)
    req(input$box_y)
    req(input$filter_box1)
    data=getdata_upload0()
    labels <- attr(data,"factors")
    pic<-1:nrow(data)
    x <- labels[input$box_factor]
    y <- data[input$box_y]

    if (input$filter_box1 != "none") {
      filtro <- as.character(input$filter_box1)
      filtro2 <- as.character(input$filter_box2)
      pic <- which(as.character(labels[, filtro]) == filtro2)


    }
    res = cbind(x,y)[pic,]
    res[,1]<-factor(res[,1])
    res
  })


  getdown<-reactive({
    switch(vals$hand_down,
           "data"=vals$saved_data[[input$data_bank]],
           "factors"=attr(vals$saved_data[[input$data_bank]],"factors"),
           "coords"=attr(vals$saved_data[[input$data_bank]],"coords"),
           "som"=combsom_down(),
           "rfdepth"=data.frame(mindeaphrf()[[2]]),
           "rfinter"=data.frame(rf_interactions_frame$df),
           "pcorr"=vals$pcorr_results,
           "screeplot_WSS data"=WSSdata(),
           "screeplot_WSS som"=WSSmodel(),
           "screeplot_accRF data"={
             rfs<-accRFdata()
             rfs$acutable},
           "screeplot_accRF som"={
             rfs<-accRFmodel()
             rfs$acutable},
           "rda"={data.frame(rda_summary())},
           "segRDA"={data.frame(segrda_summary())},
           "DP smw"={data.frame(DP_smw())},
           "som predictions"={data.frame(get_sompred_results())},
           "pcodes"={
             res=data.frame(vals$som_results$codes[[1]])
             rownames(res)<-paste("unit",1:nrow(res))
             res},
           "rf predictions"={data.frame(pred_rf())},
           "RF model classification errors"=data.frame(accu_rf_class(vals$RF_results)),
           "RF model regression errors"=data.frame(accu_rf_reg_model(vals$RF_results)),
           "RF regression - prediction errors"=data.frame(
             RFerror_reg(predall_rf(),RF_observed())
           ),
           "RF classification - prediction errors"=data.frame(
             RFerror_class(predall_rf(),RF_observed())
           ),
           "RF table classification"=data.frame(rf_tableClass()),
           "RF table Regression"=data.frame(rf_tableReg()),

    )
  })



  outputOptions(output, "side_map00", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_data_WSS", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_model_WSS", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_data_accRF", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_model_accRF", suspendWhenHidden = FALSE)

  ########################
  tunesom<-reactiveValues(
    finetopo=F,
    finesom=F,
    xdim=5,
    ydim=5,
    seed=NA,
    rlen=500,
    distmethod="BrayCurtis",
    toroidal="FALSE",
    neighbourhood.fct='bubble',
    a1=0.05,
    a2=0.01,
    r1=0,
    r2=0,
    mode="online",
    maxna=0.001,
    topo="hexagonal",
    sugtopo=T
  )


  observeEvent(  list(getdata_som(),input$sugtopo),{
    req(input$tabs=="menu_som")
    req(input$sugtopo)
    if(isTRUE(input$sugtopo)){
      dim = topo.reactive()
      tunesom$xdim<-dim[[2]]
      tunesom$ydim<-dim[[3]]
    }
  })
  observeEvent(input$xdim, {
    if (length(   names(vals$saved_data)) > 0) {
      dim = topo.reactive()
      xdim <- dim[[2]]
      if (input$xdim != xdim) {
        updateCheckboxInput(session, "sugtopo", NULL, FALSE)
      } else{
        updateCheckboxInput(session, "sugtopo", NULL, TRUE)
      }

    }
  })

  observeEvent(input$ydim, {
    if (length(   names(vals$saved_data)) > 0) {
      dim = topo.reactive()
      ydim <- dim[[3]]
      if (input$ydim != ydim) {
        updateCheckboxInput(session, "sugtopo", NULL, FALSE)
      } else {
        updateCheckboxInput(session, "sugtopo", NULL, TRUE)
      }


    }
  })

  topo.reactive <- reactive({
    req(input$tabs=="menu_som")
    req(input$som_test_pick)

    topology(getdata_som(), dist = input$distmethod)
  })

  getdata_som<-reactive({

req(input$tabs=="menu_som")
    req(input$data_som)
    data_o<-data<-vals$saved_data[[input$data_som]]
    factors<-attr(data,"factors")
    req(input$som_test_pick)
    if(length(input$som_test_pick)>0){
      if(input$som_test_pick!="None"){
        req(input$som_test_ref)
        req(input$som_test_pick%in%colnames(factors))
        fac<-factors[,input$som_test_pick]
        pic<-which(fac==input$som_test_ref)
        factors<-factors[pic,, drop=F]
        trains<-which(!rownames(data)%in%rownames(factors))
        data<-data[trains,]
        attr(data,"test_data")<-data_o[-trains,]
      }
    }

    validate(need(length(data)>0,"no data found"))
    attr(data,"test_data")<-data

 data
  })
  predsom.reactive<-reactive({
    checkpredsom()
    if(is.null(attr(vals$som_results,"supervisor"))){
      tabsetPanel(id="predsom_tab",
                  tabPanel(
                    strong("3.1. Results"),style="background: white",
                    value = "predsom_tab01",
                    uiOutput("pred_som_results")
                  ),

                  tabPanel(
                    strong("3.2. BMUs"),style="background: white",
                    value = "predsom_tab03",
                    uiOutput("pred_bmu")
                  ),
                  tabPanel(
                    strong("3.3. Property"),style="background: white",
                    value = "predsom_tab04",
                    uiOutput("pred_property"))

      )
    } else{
      tabsetPanel(id="predsom_tab",
                  tabPanel(
                    strong("3.1. Results"),style="background: white",
                    value = "predsom_tab01",
                    uiOutput("pred_som_results")
                  ),

                  tabPanel(
                    strong("3.2. BMUs"),style="background: white",
                    value = "predsom_tab03",
                    uiOutput("pred_bmu")
                  ),
                  tabPanel(
                    strong("3.3. Property"),style="background: white",
                    value = "predsom_tab04",
                    uiOutput("pred_property"))
                  ,
                  tabPanel(
                    strong("3.5. Confusion Matrix"),
                    value = "predsom_tab05",
                    style = "background: Snow;",
                    uiOutput("predsom_cm")
                  )
      )
    }
  })


  output$side_predsom_cm<-renderUI({
    fluidRow(class="map_control_style",style="color: #05668D",
             div(
               span("+ Palette:",
                    inline(
                      pickerInput(inputId = "cm_supsom_palette",
                                  label = NULL,
                                  choices =     vals$colors_img$val[getgrad_col()],
                                  choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body"),width='100px')
                    )
               )
             ),
             div(
               span("+ Y Datalist:",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values",
                                           options=list(container="body")),
                    inline(
                      pickerInput("predsom_newY",
                                  NULL,names(vals$saved_data[getobs_som()])
                                  ,width='100px')
                    )
               )
             ),
             div(
               span("+ Y variable:",
                    inline(
                      pickerInput("predsom_y",NULL, choices=rev(names(pred_som()$predictions[-1])),width='100px')
                    )
               )
             ),
             div(
               actionLink(
                 'downp_confsom',span("+ Download",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
               )

             )
    )
  })

  getobs_som<-reactive({
    datalist<-vals$saved_data
    datalist=lapply(datalist,function(x) attr(x,"factors"))

    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)%in%names(pred_som()$predictions[-1]))

      })
    )
    names(res0[res0==T])
  })

  output$predsom_cm<-renderUI({
    column(12, style = "background: Snow;",
           sidebarLayout(
             sidebarPanel(uiOutput("side_predsom_cm")),
             mainPanel( uiOutput("conf_som"))
           )
    )
  })

  output$predsom_tabs<-renderUI({
    validate(need(length(vals$som_results)>0,"No trained model in selected Datalist"))
    predsom.reactive()})
  output$conf_som<-renderUI({
    fluidRow(
      plotOutput("predsom_confusion"),
      renderPrint(confusionMatrix(get_predsom_cm()))

    )
  })

  get_predsom_cm<-reactive({
    factors<-attr(vals$saved_data[[input$predsom_newY]],"factors")
    obs<-if(input$sompred_type=="Datalist"){
      factors[,input$predsom_y]
    } else{attr(vals$som_results,"sup_test")[,input$predsom_y]}
    pred<- pred_som()$prediction[[input$predsom_y]]
    pred<-factor(pred, levels=levels(obs))
    conf<-table(obs,pred)
    conf
  })
  output$predsom_confusion<-renderPlot({
    conf<-get_predsom_cm()
    res<-plotCM(conf/sum(conf)*100, input$cm_supsom_palette,  newcolhabs=vals$newcolhabs)
    vals$conf_som<-res
    res
  })

  output$bmu_p_tools<-renderUI({
    column(12,
           fluidRow(
             popify(bsButton("downp_bmu_pred", span(icon("fas fa-download"),icon("fas fa-image")),style = "button_active"),NULL,"download BMU plot",
                    options=list(container="body")

             )
           )
    )
  })
  output$pred_bmu<-renderUI({
    column(12,style="background: white",
           p(strong(h4("Predictions - Best matching units"))),
           uiOutput("bmu_p_tools"),
           sidebarLayout(
             sidebarPanel(
               uiOutput("pop_bmu_p")
             ),  mainPanel(
               plotOutput('bmu_pCodes')
             ),
           ))


  })

  output$pop_bmu_p<-renderUI({
    fluidRow(
      class="map_control_style",style="color: #05668D",
      div(span("+ Display:",
               inline(radioButtons("bmu_p_dotlabel", NULL, choices = c("labels", "symbols"),selected=bmu_p_dotlabel$df, inline=T, width="100px"
               ))

      )),
      conditionalPanel("input.bmu_p_dotlabel=='labels'",{
        div(span("+ Labels:",inline(
          pickerInput("bmu_p_factors",NULL,choices = c(colnames(attr(getdata_som(),"factors"))),width = '75px')
        )))
      }),
      div(span("+ Training color:",inline(
        pickerInput(
          inputId = "bmu_p_training",
          label =NULL,
          choices =  vals$colors_img$val[getsolid_col()] ,
          choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
          selected=bmu_p_training$df, width = '75px'
        )
      )))
      ,
      div(span("+ Test color:", inline(
        pickerInput(inputId = "bmu_p_test",
                    label ="",
                    choices =  vals$colors_img$val[getsolid_col()] ,
                    choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
                    selected=bmu_p_test$df, width = '75px')
      ))),
      conditionalPanel("input.bmu_p_dotlabel=='symbols'",{
        div(span("+ Shape:", inline(
          pickerInput(inputId = "bmu_p_symbol",
                      label = NULL,
                      choices = df_symbol$val,
                      choicesOpt = list(content = df_symbol$img),

                      selected=bmu_p_symbol$df, width = '75px')
        )))
      })
      ,

      div(span("+ Size:", inline(tipify(
        numericInput("bmu_p_symbol_size",NULL,value = vals$bmu_p_symbol_size,min = 0.1,max = 3,step = .1, width = '75px'),"symbol size"
      )))),
      div(span("+ Background:",
               inline(
                 pickerInput(inputId = "bmu_p_bgpalette",
                             label = NULL,
                             choices = vals$colors_img$val,
                             choicesOpt = list(content = vals$colors_img$img),

                             selected=bmu_p_bgpalette$df, width = '75px')
               ))),

      div(span("+ Transparency:",inline(
        tipify(
          numericInput("bmu_p_bg_transp",NULL,
                       value=vals$bmu_p_bg_transp, min=0, max=1,step=0.1, width = '75px'),
          "Background transparency")
      ))),
      div(span(span('+ Border:'),inline(
        pickerInput(inputId = "bmu_p_border_grid",
                    label =NULL,
                    choices =  vals$colors_img$val[getsolid_col()] ,
                    choicesOpt = list(
                      content =  vals$colors_img$img[getsolid_col()] ),
                    selected= vals$colors_img$val[getsolid_col()] [7], width = '75px')
      )))
    )




  })
  bmu_pred_points<-reactive({
    if(bmu_p_dotlabel$df == 'symbols'){ T} else {F}
  })
  output$bmu_p_fac_control<-renderUI({
    fluidRow(
      splitLayout(
        column(12,
               p(strong("Test color", style="color: #0D47A1;")),
               tipify(
                 pickerInput(inputId = "bmu_p_test",
                             label =NULL,
                             choices =  vals$colors_img$val[getsolid_col()] ,
                             choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
                             selected= vals$colors_img$val[getsolid_col()] [4],
                             options=list(container="body")),
                 "Color of the testing observations",options=list(container="body"))),
        fluidRow(
          p(strong("Training color", style="color: #0D47A1;")),
          tipify(
            pickerInput(
              inputId = "bmu_p_training",
              label =NULL,
              choices =  vals$colors_img$val[getsolid_col()] ,
              choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
              selected= vals$colors_img$val[getsolid_col()] [2],
              options=list(container="body")),
            "Color of the training observations",
            options=list(container="body")))
      )
    )
  })

  bmu_p_factors_reac<-reactive({
    factors<-attr(getdata_som(),"factors")
    m<-vals$som_results
    if (length(input$bmu_p_factors)>0) {
      c(factors[rownames(getdata_som()), vals$bmu_p_factors],
        if(length(attr(m,"test"))>0) {factors[rownames(attr(m,"test")), vals$bmu_p_factors]})
    } else{c(factors[rownames(getdata_som()),1],
             if(length(attr(m,"test"))>0) {factors[rownames(attr(m,"test")), ]})}
  })
  BMUs_pred<-reactive({
    p<-pcorr(
      m=vals$som_results,
      npic = 0,
      pch = as.numeric(bmu_p_symbol$df),
      labels.ind = bmu_p_factors_reac(),
      cex =as.numeric(vals$bmu_p_symbol_size),
      bg_palette = as.character(bmu_p_bgpalette$df),
      factor.pal=as.character(bmu_p_training$df),
      points=bmu_pred_points(),
      legend=F,
      predict=pred_som(),
      alpha_bg=vals$bmu_p_bg_transp,
      border=getcolhabs(vals$newcolhabs,input$bmu_p_border_grid,1),

      pred_col=as.character(bmu_p_test$df),
      newcolhabs=vals$newcolhabs
    )
    vals$bmus_pred_plot<-recordPlot()
    p
  })
  output$bmu_pCodes<-renderPlot({vals$bmus_pred_plot})
  output$bmu_pCodes<-renderPlot({
    p<-BMUs_pred()
    vals$bmu_p_results<-data.frame(attr(p,"result"))
    p
  })
  re<-reactiveValues(df=NULL)
  output$som_type<-renderUI({
    req(input$som_tab=='som_tab1')
    span(
      strong("Type:", style="color: #0D47A1;"),
      inline(radioButtons("som_type", NULL, choices=c("Unsupervised", "Supervised"), inline=T, selected=vals$cur_somtype))
    )
  })
  output$som_datalist<-renderUI({
    div(
      class='choosechannel',
      inline(uiOutput("som_type")),
      uiOutput("som_inputs"),
      uiOutput("som_panels")
    )  })
  output$som_models<-renderUI({
    req(input$data_som)
    req(input$som_tab=="som_tab2"|input$som_tab=="som_tab3")
    choices<-if(isTRUE(vals$bagmodel)){
      c("new som (unsaved)", names(attr(vals$saved_data[[input$data_som]],"som")))
    } else{
      names(attr(vals$saved_data[[input$data_som]],"som"))
    }
    pickerInput(
      "som_models",
      strong("Som results:", tiphelp("SOM results. Click to see SOM results saved in the selected Datalist")),
      choices=choices,
      selected=vals$cur_train, width='150px')
  })
  output$save_som<-renderUI({
    req(input$som_models=="new som (unsaved)")
    req(input$som_tab=="som_tab2"|input$som_tab=="som_tab3")
    popify(bsButton("tools_savesom", div(icon("fas fa-save")),style  = "button_active", type="action",value=FALSE), NULL, "Save the som model in the Datalist",
    )
  })
  output$som_cur<-renderUI({
    req(input$som_tab=="som_tab2"|input$som_tab=="som_tab3")
    div(
      p(strong("X:"), em(input$data_som)),
      p(style="margin-top: -10px",strong("Y:"),em(input$som_type2,";"),em(attr(vals$som_results,"Method"))),
      p(style="margin-top: -10px",strong("Test Partition:"),em(attr(vals$som_results,"test_partition"))),
      p(style="margin-top: -10px",strong("Som-Attribute:"), em(input$som_models)),


    )
  })
  output$som_inputs<-renderUI({
    div(class='choosechannel',
        inline(uiOutput("som_datalist_y")),
        inline(uiOutput("som_y")),
        inline(uiOutput("som_y_sel")),
        inline(uiOutput("som_x")),
        inline(
          div(
            inline(uiOutput("som_partition"))
          )
        ),
        inline(uiOutput("som_models")),
        inline(uiOutput('save_som')),
        inline(uiOutput("som_cur"))

    )
  })
  observeEvent(input$data_som,{
    req(input$som_tab)
    re$df<-input$som_tab

  })

  observe({
    req(!is.null(re$df))
    if(length(attr(vals$saved_data[[input$data_som]],"som"))>0)
      updateTabsetPanel(session,"som_tab",re$df)

  })


  observeEvent(input$som_tab,{
    vals$cursomtab<-switch(input$som_tab,
                           "som_tab1"="som_tab1",
                           "som_tab2"="som_tab2",
                           "som_tab3"="som_tab3") })


  output$som_datalist_y<-renderUI({
    req(input$som_type=='Supervised')
    req(input$som_tab=="som_tab1")
    pickerInput("data_somY",span("Y Datalist:"), choices =names(vals$saved_data), selected=vals$cur_datasomY, width="250px", options=list(container="body"))
  })
  output$som_x<-renderUI({
    pickerInput("data_som",span("~Training data (X)::"), choices =names(vals$saved_data), selected=vals$cur_data, width="250px",options=list(container="body"))
  })
  output$som_y<-renderUI({
    req(input$som_tab)
    if(input$som_tab=="som_tab1"){
      req(input$som_type=='Supervised')
      pickerInput("som_type2","Y Attribute:", choices=c("Numeric","Factors"), selected=vals$cur_som_type2, width="100px")
    }
  })
  output$som_y_sel<-renderUI({
    req(length(input$som_tab)>0)
    if(input$som_tab=="som_tab1"){
      req(input$som_type=='Supervised')
      req(input$som_type2=='Factors')
      div(class = "choosechannel",pickerInput("selecfac",NULL, choices=colnames(attr(vals$saved_data[[input$data_somY]],"factors")), multiple = T, selected=colnames(attr(vals$saved_data[[input$data_somY]],"factors")), width="100px", inline=T))
    }
  })
  output$selecfac<-renderUI({
    column(12,
           checkboxGroupInput("selecfac", "Select the factors", choices=colnames(attr(vals$saved_data[[input$data_somY]],"factors"))))
  })
  choices_som_sup<-reactive({
    data<-vals$saved_data[[input$data_somY]]
    res<-switch (input$som_type2,
                 'Factors' = colnames(attr(data,"factors")),
                 'Numeric'=colnames(data))
    res
  })
  output$som_partition<-renderUI({
    req(input$data_som)
    req(input$som_tab)
    if(is.null(vals$cur_partsom)){vals$cur_partsom<-1}
    if(input$som_tab=="som_tab1"){
      div(
        inline(pickerInput("som_test_pick",strong('Partition:', tiphelp("choose a factor from Datalist X as reference for data partioning")), choices=c("None", colnames(attr(vals$saved_data[[input$data_som]],"factors"))),selected=vals$cur_partsom,width="150px",)),inline(uiOutput("som_test_ref"))
      )
    }
  })
  output$som_test_ref<-renderUI({

    req(input$som_test_pick!="None")
    req(input$data_som)
    if(is.null(vals$cur_testsom)){vals$cur_testsom<-1}
    req(input$som_test_pick%in%colnames(attr(vals$saved_data[[input$data_som]],"factors")))
    fac<-attr(vals$saved_data[[input$data_som]],"factors")[,input$som_test_pick]
    choices<-levels(fac)
    if(input$som_tab=="som_tab1"){
      pickerInput("som_test_ref",strong("Test reference", tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions")), choices=choices, selected=vals$cur_testsom,width="150px",)} else{NULL}
  })


  output$som_panels<-renderUI({
    req(length(vals$saved_data)>0)
    fluidRow(tabsetPanel(
      id = "som_tab",selected=vals$cursomtab,
      tabPanel(
        strong("1. Training"), value = "som_tab1",
        uiOutput("training_panel")
      ),
      tabPanel(value = "som_tab2",
               strong("2. Results"),
               uiOutput("results_panel")),
      tabPanel(value = "som_tab3",style="background: white",
               strong("3. Predict"),
               uiOutput('predsom_panel'),
               uiOutput('predsom_tabs')
      )
    )
    )})
  output$som_topo<-renderUI({
    column(12,
           br(),
           strong("1.1. Set the grid",
                  actionLink("somgridhelp", tipify(icon("fas fa-question-circle"), "Click for more details"))),
           column(12,
                  fluidRow(
                    column(12,
                           splitLayout(cellWidths = c('10%','90%'),
                                       checkboxInput("sugtopo", NULL, value =tunesom$sugtopo),
                                       p(style="margin-top: 9px",
                                         "suggested topology",
                                         tipify(actionLink(
                                           "sugtopohelp", icon("fas fa-question-circle")
                                         ), "Click for more details")))),
                    column(12,
                           withSpinner(type=8,color="SeaGreen",uiOutput("somgridtopo"))

                           ,
                           column(12,

                                  span(class="finesom_btn",
                                       bsButton("finetopo",tipify(span("Fine tuning*"), "show all parameters available"), type="toggle", value=tunesom$finetopo)
                                  ),
                                  uiOutput("finetopo_out"))))))
  })
  output$training_panel<-renderUI({
    fluidRow(id = "train_tab0",
             column(12,style = "background: white;",
                    column( 12,
                            splitLayout(
                              uiOutput("som_topo"),
                              column(12,br(),
                                     strong("2.2. Set the training parameters",
                                            actionLink("supersomhelp", tipify(
                                              icon("fas fa-question-circle"), "Click for more details"
                                            ))),
                                     column(12, style = "background: white;",
                                            fluidRow(uiOutput("somcontrol")



                                            ))
                              )
                            )
                    )
             )
    )
  })


  observeEvent(input$finesom,{
    tunesom$finesom<-input$finesom
  })

  output$results_panel<-renderUI({
    validate(need(length(vals$som_results)>0,"No trained model in selected Datalist"))


    validate(need(!is.null(vals$saved_data),"no data found"))
    div(style = "background: WhiteSmoke;",
        tabsetPanel(id="som_res",
                    tabPanel(value= 'train_tab1',"3.1. Parameters",
                             column(12, style = "background: white; margin-top: 10px",

                                    column(12,
                                           splitLayout(uiOutput('som_errors'),
                                                       div(
                                                         p(popify(bsButton("down_pcodes_results", span(icon("fas fa-download"),icon("fas fa-table")),style = "button_active"),NULL,"download codebook results",options=list(container="body"))),
                                                         p(popify(downloadButton("down_kohonen_results", icon("fas fa-braille"),style = "button_active"),NULL,"download kohonen object as rds file containing the object of class kohonen with components",
                                                                  options=list(container="body")))))),
                                    column(12, style="margin-top: 10px",splitLayout(cellWidths = c("50%","10%"),div(p(strong("Training Parameters")),tableOutput("train.summary")))))),
                    tabPanel("3.2. Changes",value = "train_tab2",
                             column(12,
                                    style = "background: white;",
                                    column(12,actionButton('downp_pchanges',icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
                                    column(12,plotOutput('pchanges')))
                    ),
                    tabPanel("3.3. Couting", value = "train_tab3",
                             column(12,style = "background: white;",
                                    column(12,actionButton('downp_pcounts',icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
                                    column(12, plotOutput('pcounts')))),
                    tabPanel("3.4. Umatrix", value = "train_tab4",
                             column(12,style = "background: white;",
                                    column(12,actionButton('downp_pmatrix',icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
                                    column(12, plotOutput('pUmatrix')))),
                    tabPanel("3.5. BMUs", value = "train_tab5",
                             uiOutput("pcorr_control")),
                    tabPanel("3.6. Property", value = "train_tab6",
                             column(12,
                                    style = "background: white;",br(),column(12, strong("Property",tipify(icon("fas fa-question-circle"),"Show areas for high values (red) and low values (blue) for the selected variable"))),
                                    column(12, uiOutput("var_pproperty")),
                                    column(12,
                                           column(12,actionButton('downp_pproperty',icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
                                           column(12,plotOutput("pproperty"))))
                    )
        )
    )
  })

  output$predsom_panel<-renderUI({
    validate(need(length(vals$som_results)>0,"No trained model in selected Datalist"))
    data=getdata_som()
    test<-attr(vals$som_results,"test_partition")

    column(12,style="background: white",
           splitLayout(cellWidths = c("30%",'40%','10%'),
                       if(test!="None"){
                         radioButtons("sompred_type","New data (X):",choices=c("Partition","Datalist"), inline=T)
                       } else{ radioButtons("sompred_type","New data (X):",choices=c("Datalist"),inline=T) },
                       conditionalPanel("input.sompred_type =='Datalist'",{
                         div(selectInput("predsom_new","Datalist",choices=names(vals$saved_data[getobs_somX()])))})

           )
    )
  })

  getobs_somX<-reactive({
    datalist<-vals$saved_data
    m<-vals$som_results
    res0<-unlist(
      lapply(datalist, function (x){
        res<-colnames(x)%in%colnames(m$data[[1]])
        sum(res)==ncol(m$data[[1]])
      })
    )
    names(res0[res0==T])
  })


  output$predsom_var<-renderUI({
    selectInput("predsom_var","variable",colnames(test_som()))
  })
  test_som<-reactive({
    pred_tab<-if(input$sompred_type=="Partition"){attr(vals$som_results,"test")}
    if(input$sompred_type=="Datalist"){
      pred_tab<-vals$saved_data[[input$predsom_new]]}
    pred_tab
  })

  checkpredsom<-reactive({
    req(input$predsom_new)
    check<-vals$saved_data[[input$data_som]]
    res<-res0<-lapply(vals$saved_data,function(x) match_col(x,check) )
    res<-names(which(unlist(res)==T))
    choices=names(unlist(c(res0[res0==T],res0[ res0==F])))
    #validate(need(isTRUE(res0[[input$predsom_new]]),paste("Variables from Datalist",input$predsom_new, "incompatible with the Training data", input$data_som)))
    choices
  })

  output$pred_som_results<-renderUI({

    #validate(need(res[[input$predsom_new]],"Incompatible variables"))

    column(12,style="background: white",
           uiOutput("pred_som_results_control")

    )
  })
  output$pred_som_results_control<-renderUI({
    som_pred<-pred_som()
    div(
      #renderPrint(som_pred),
      uiOutput("som_pred_results")
    )
  })
  cur_predsom_res1<-reactiveValues(df="Data predictions (X)")
  cur_predsom_res2<-reactiveValues(df="Data predictions (X)")
  cur_predsom_res3<-reactiveValues(df="Data predictions (X)")
  observeEvent(input$som_results,{
    if(attr(vals$som_results,"Method")=="Unsupervised"){
      cur_predsom_res1$df<-vals$som_results
    } else{
      if(attr(vals$som_results,"som_type2")=="Numeric"){
        cur_predsom_res2$df<-vals$som_results
      } else{
        cur_predsom_res3$df<-vals$som_results
      }

    }
  })
  output$som_pred_results<-renderUI({
    if(attr(vals$som_results,"Method")=="Unsupervised"){
      choices<-c("Data predictions (X)", "Obs errors (X)","Var errors (X)","BMUs","Neuron predictions (X)")
      selected= cur_predsom_res1$df
    } else{
      if(attr(vals$som_results,"som_type2")=="Numeric"){
        choices<-c("Data predictions (X)","Data predictions (Y)","Obs errors (X)", "Obs errors (Y)","Var errors (X)", "Var errors (Y)","BMUs","Neuron predictions (X)","Neuron predictions (Y)")
        selected= cur_predsom_res2$df
      } else{
        choices<-c("Data predictions (X)","Data predictions (Y)","BMUs","Neuron predictions (X)" ,"Neuron predictions (Y)")
        selected= cur_predsom_res3$df
      }
    }
    column(12,
           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 class="map_control_style",
                 style="color: #05668D",
                 div(
                   h5("+ Results:"),
                   column(12,radioButtons("predsom_results",NULL,choices, selected=selected))
                 ),
                 div(
                   tipify(
                     actionLink(
                       'down_som_pred_results',span("+ Download",icon("fas fa-download")), style="button_active"
                     ),
                     "Download selected results"
                   )
                 ),
                 uiOutput("savesom_pred")
               )),
             mainPanel(
               uiOutput("predsom_newY2"),
               uiOutput("pred_som_results_out")
             )

           )


    )
  })
  output$predsom_newY2<-renderUI({
    req(attr(vals$som_results,"Method")!="Unsupervised")
    req(input$predsom_results%in%c("Obs errors (Y)"))
    span("+ Y Datalist:",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values",
                                options=list(container="body")),
         inline(
           pickerInput("predsom_newY2",
                       NULL,names(vals$saved_data[getobs_som2()]),
                       inline=T)
         )
    )
  })

  errors_somX_sp<-reactive({
    m<-vals$som_results
    predX<- correctsom()$predX

    observed<-newdata_som()

    rmse_res<- mse_res<-mape_res<-mae_res<-list()
    for(i in 1:ncol(observed))
    {
      mape_res[[i]]<-mape(observed[,i],predX[,i])
      mse_res[[i]]<-mse(observed[,i],predX[,i])
      rmse_res[[i]]<-rmse(observed[,i],predX[,i])
      mae_res[[i]]<-mae(observed[,i], predX[,i])
    }
    rmse<-data.frame(RMSE=do.call(c,rmse_res))
    mae<-data.frame(MAE=do.call(c,mae_res))
    mape<-data.frame(MAPE=do.call(c,mape_res))
    mse<-data.frame(MSE=do.call(c,mse_res))
    rownames(mape)<- rownames(mse)<-rownames(rmse)<-  rownames(mae)<-colnames(observed)
    data.frame(mae=mae,mse=mse,rmse=rmse)

  })
  errors_somY_sp<-reactive({
    m<-vals$som_results
    predY<- correctsom()$predY

    observed<-newdata_somY()
    rmse_res<- mse_res<-mape_res<-mae_res<-list()
    for(i in 1:ncol(observed))
    {
      mape_res[[i]]<-mape(observed[,i],predY[,i])
      mse_res[[i]]<-mse(observed[,i],predY[,i])
      rmse_res[[i]]<-rmse(observed[,i],predY[,i])
      mae_res[[i]]<-mae(observed[,i], predY[,i])
    }
    rmse<-data.frame(RMSE=do.call(c,rmse_res))
    mae<-data.frame(MAE=do.call(c,mae_res))
    mape<-data.frame(MAPE=do.call(c,mape_res))
    mse<-data.frame(MSE=do.call(c,mse_res))
    rownames(mape)<- rownames(mse)<-rownames(rmse)<-  rownames(mae)<-colnames(observed)
    data.frame(mae=mae,mse=mse,rmse=rmse)

  })
  errors_somX<-reactive({
    m<-vals$som_results
    predX<- correctsom()$predX

    observed<-newdata_som()
    res0<-matrix(NA,nrow(observed),3, dimnames = list(c(rownames(observed)),c('mae','mse','rmse')))


    for(i in 1:nrow(observed))
    {

      res0[i,"mae"]<-mae(as.numeric(observed[i,]),as.numeric(predX[i,]))
      res0[i,"mse"]<-mse(as.numeric(observed[i,]),as.numeric(predX[i,]))
      res0[i,"rmse"]<-rmse(as.numeric(observed[i,]),as.numeric(predX[i,]))


    }
    res0

  })
  errors_somY<-reactive({
    m<-vals$som_results
    predX<- correctsom()$predY

    observed<-newdata_somY()
    res0<-matrix(NA,nrow(observed),3, dimnames = list(c(rownames(observed)),c('mae','mse','rmse')))


    for(i in 1:nrow(observed))
    {

      res0[i,"mae"]<-mae(as.numeric(observed[i,]),as.numeric(predX[i,]))
      res0[i,"mse"]<-mse(as.numeric(observed[i,]),as.numeric(predX[i,]))
      res0[i,"rmse"]<-rmse(as.numeric(observed[i,]),as.numeric(predX[i,]))


    }
    res0

  })
  newdata_somY<-reactive({
    req(input$sompred_type)
    req(input$predsom_newY2)
    m<-vals$som_results
    newdata= if(input$sompred_type=="Partition"){
      attr(m,"test")} else{
        vals$saved_data[[input$predsom_newY2]]
      }
    newdata
  })

  getobs_som2<-reactive({
    datalist<-vals$saved_data
    m<-vals$som_results


    res0<-unlist(
      lapply(datalist, function (x){
        res<-colnames(x)%in%colnames(m$data[[2]])
        sum(res)==ncol(m$data[[2]])
      })
    )
    names(res0[res0==T])
  })



  output$savesom_pred<-renderUI({
    req(input$predsom_results=='Data predictions (X)'|input$predsom_results=='Data predictions (Y)'|input$predsom_results=='Obs errors (Y)'|input$predsom_results=='Obs errors (X)')
    div(
      tipify(
        actionLink(
          'tools_savesom_pred',span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
        ),
        "Create a datalist  with the selected prediction", options=list(container="body")
      )
    )
  })

  correctsom<-reactive({
    som_pred<-pred_som()
    m<-vals$som_results
    newdata=as.matrix(newdata_som())
    pred_som<-kohonen:::predict.kohonen(m,newdata=newdata, whatmap = 1)
    res<-get_correct_predsom(m,pred_som,newdata)
    res
  })
  output$pred_som_results_out<-renderUI({
    validate(need(!anyNA(newdata_som()),""))
    column(12,
           #renderPrint({correctsom()}),
           div(uiOutput("corrected_predsom_warning")),
           div(uiOutput("dt_predsom_res_va"))
    )})


  output$dt_predsom_res_va<-renderUI({
    req(length(get_sompred_results())>0)
    validate(need(ncol(get_sompred_results())<1000,"Too big. download the table to view it"))
    fluidRow(
      tags$style('#DT_predsom_res td {padding: 0}'),
      inline(
        DT::dataTableOutput("DT_predsom_res")
      )
    )


  })
  output$DT_predsom_res<-DT::renderDataTable({
    req(length(get_sompred_results())>0)
    req(ncol(get_sompred_results())<1000)
    get_sompred_results()
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')
  get_sompred_results<-reactive({
    req(input$predsom_results)
    som_pred<-pred_som()
    m<-vals$som_results
    obs=attr(m,"sup_test")

    predX<- correctsom()$predX
    predY<- correctsom()$predY
    if(length(predY)>0){
      #colnames(predY)<-gsub("Y.","", colnames(predY))
    }



    res<-switch(input$predsom_results,
                'BMUs'={
                  bmu<-data.frame(bmu_test=som_pred$unit.classif)
                  rownames(bmu)<-rownames(predX)
                  bmu
                },
                'Data predictions (X)'={predX},
                'Neuron predictions (X)'={som_pred$unit.predictions[[1]]},
                'Data predictions (Y)'={predY},
                "Neuron predictions (Y)"={ neuY<-data.frame(do.call(cbind,som_pred$unit.predictions[-1]))},
                "Obs errors (X)"={data.frame(errors_somX())},
                "Obs errors (Y)"={data.frame(errors_somY())},
                "Var errors (X)"={data.frame(errors_somX_sp())},
                "Var errors (Y)"={data.frame(errors_somY_sp())}

    )
    res
  })




  newdata_som<-reactive({
    req(input$sompred_type)
    m<-vals$som_results
    newdata= if(input$sompred_type=="Partition"){
      attr(m,"test")} else{
        vals$saved_data[[input$predsom_new]]
      }
    newdata
  })
  pred_som<-reactive({
    validate(need(!anyNA(newdata_som()),"NAs not allowed in the prediction Datalist"))
    m<-vals$som_results
    som_pred<-kohonen:::predict.kohonen(m,newdata = as.matrix(newdata_som()), whatmap = 1)
    #names(som_pred$predictions)<-c("X","Y")
    #names(som_pred$unit.classif)<-rownames(som_pred$predictions[[1]])
    attr(som_pred,"coords")<-attr(m,"coords")
    som_pred
  })

  predsom_war<-reactiveValues(df=F)
  output$corrected_predsom_warning<-renderUI({
    req(input$sompred_type=="Partition")
    #req(length(attr(vals$som_results,"test"))>0)
    newx<-nrow(attr(correctsom(),"newx"))
    req(!is.null(newx))
    req(newx>0)
    req(input$predsom_results=='Data predictions (X)'|input$predsom_results=='Data predictions (Y)')
    fluidRow("samples", strong(paste0(rownames(attr(correctsom(),"newx")), collapse="; ")),"were allocated to units which no training data were mapped, leading to NA values in the predictions.To avoid the NA values, predictions for these observations were estimated using the neighboring units."
    )
  })






  output$pred_property<-renderUI({



    column(12,style="background: white",

           uiOutput("predsom_var"),
           plotOutput("predsom_property"))
  })


  output$predsom_property<-renderPlot({
    req( input$predsom_var)
    som_pred<-pred_som()
    plot(vals$som_results, type = "property", main = input$predsom_var, property = som_pred$unit.predictions[[1]][,input$predsom_var], palette.name=coolBlueHotRed,cex=0.5,shape="straight",keepMargins = TRUE)})

  observeEvent(input$sugtopo,{
    tunesom$sugtopo<-input$sugtopo
  })

  observeEvent(input$topo,{
    tunesom$topo<-input$topo
  })
  observeEvent(input$maxna,{
    tunesom$maxna<-input$maxna
  })
  observeEvent(input$mode,{
    tunesom$mode<-input$mode
  })
  observeEvent(input$r2,{
    tunesom$r2<-input$r2
  })
  observeEvent(input$r1,{
    tunesom$r1<-input$r1
  })
  observeEvent(input$a2,{
    tunesom$a2<-input$a2
  })
  observeEvent(input$a1,{
    tunesom$a1<-input$a1
  })
  observeEvent(input$neighbourhood.fct,{
    tunesom$neighbourhood.fct<-input$neighbourhood.fct
  })

  observeEvent(input$toroidal,{
    tunesom$toroidal<-input$toroidal
  })

  observeEvent(input$distmethod,{
    tunesom$distmethod<-input$distmethod
  })

  observeEvent(input$xdim,{
    tunesom$xdim<-input$xdim
  })
  observeEvent(input$ydim,{
    tunesom$ydim<-input$ydim
  })
  observeEvent(input$seed,{
    tunesom$seed<-input$seed
  })
  observeEvent(input$rlen,{
    tunesom$rlen<-input$rlen
  })


  somcontrol<-reactive({
    fluidRow(
      column(12,
             div(br()),
             div(br()),
             splitLayout(
               style="margin-top: 10px",
               selectInput('distmethod',strong("dist.fcts",tipify(icon("fas fa-question-circle"),"Distance measure between each neuron and input data")),
                           choices = c("BrayCurtis","euclidean","sumofsquares","manhattan",
                                       "tanimoto"),selected=tunesom$distmethod),
               numericInput("rlen",strong("rlen",tipify(icon("fas fa-question-circle"),"The number of times the complete dataset will be presented to the network")),value = tunesom$rlen,min = 1,step = 1),
               numericInput("seed", strong("seed",tipify(icon("fas fa-question-circle"),"A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the som analysis.")), value =tunesom$seed, min=0, step=1)),
             uiOutput("trainsom")))
  })

  output$trainsom<-renderUI({
    if(input$distmethod=="BrayCurtis"){
      validate(need(!anyNA(getdata_som()),"NA values not allowed with BrayCurtis distance"))
    }
    column(12,
           column(9,align = "center",
                  br(),
                  popify(actionButton("trainSOM", h4(icon("fas fa-braille"),"train SOM",
                                                     icon("fas fa-arrow-circle-right")), style = "background: #05668D; color: white"),"TrainSOM","Train the Data-Attribute from the selected Datalist")),
           column(3, align = "right", actionButton("resetsom", "reset")),

           column(12,
                  span(class="finesom_btn",
                       tipify(bsButton("finesom","Fine tuning*", type="toggle", value=tunesom$finesom),"show all parameters available")
                  )),
           column(12,uiOutput("finetuning_som"))
    )

  })

  observeEvent(input$data_som,{
    if(anyNA(getdata_som())){
      updateSelectInput(session,"distmethod")
    }
  })

  output$finetuning_som<-renderUI({
    req(isTRUE(input$finesom))
    div(id="finesom_out",
        splitLayout(
          numericInput(inputId = "a1",label = "a1",value = tunesom$a1,step = 0.01),
          numericInput(inputId = "a2",label = "a2",value = tunesom$a2,step = 0.01),
          numericInput(inputId = "r1",label = "r1",value = tunesom$r1),
          numericInput(inputId = "r2",label = "r2",value = tunesom$r2,step = 0.01 )),
        splitLayout(selectInput("mode", "mode", choices = c("online","batch", "pbatch"), selected=tunesom$mode),
                    numericInput("maxna","maxNA.fraction",value = tunesom$maxna,step = 0.01)))

  })
  output$somcontrol<-renderUI({
    fluidRow(
      somcontrol()
    )
  })
  topocontrol<-reactive({
    fluidRow(id = "topocontrol",
             splitLayout(cellWidths = c('30%','30%','40%'),
                         numericInput('xdim',"xdim",value = tunesom$xdim,min = 0,step = 1),
                         numericInput("ydim","ydim",value = tunesom$ydim,min = 0,step = 1),
                         selectInput("topo","topo",choices = c("hexagonal", "rectangular"),selected=tunesom$topo)),
             column(10, plotOutput("showgrid", height = "150px")),
             column(2, align = "right", actionButton("resettopo", "reset"))

    )
  })
  observeEvent(input$finetopo,{
    tunesom$finetopo<-input$finetopo
  })

  output$finetopo_out<-renderUI({
    req(isTRUE(input$finetopo))
    splitLayout(id="finetopo_out",
                selectInput("neighbourhood.fct",label = "neighbourhood.fct",choices = c("bubble","gaussian"),selected=tunesom$neighbourhood.fct),
                selectInput("toroidal",label = "toroidal",choices = c(F, T), selected=tunesom$toroidal))

  })


  output$somgridtopo<-renderUI({topocontrol()})
  toroidal<-reactive({
    switch (tunesom$toroidal,
            'TRUE' = TRUE,
            "FALSE" = FALSE)
  })



  observe({
    req(input$xdim)
    req(input$ydim)
    tunesom$r1<-as.vector(quantile(unit.distances(
      kohonen::somgrid(input$xdim,input$ydim,topo = input$topo,toroidal = toroidal(), neighbourhood.fct=input$neighbourhood.fct)), 2 / 3))
  })








  cutsom.reactive<-reactive({
    req(input$method.hc0)
    req(input$hcmodel_palette)
    m<-getmodel_hc()
    somC<-cutsom(m, picK(), method.hc = input$method.hc0, palette=input$hcmodel_palette,newcolhabs=vals$newcolhabs)
    somC
  })
  bmu_factors_reac<-reactive({
    factors<-attr(getdata_som(),"factors")
    if (length(input$bmu_factors)>0) {
      factors[rownames(getdata_som()), vals$cur_bmu_factors]
    } else{factors[,1]}
  })
  var_corr_codes.reactive<-reactive({
    if(input$var_corr_codes=="Highest correlations"){res="var"}
    if(input$var_corr_codes=="Clockwise-correlations"){res="cor"}
    res
  })
  train.summary<-reactive({
    m<-vals$som_results
    traindata<-data.frame(m$data[[1]])
    mean = round(mean(unlist(traindata)), 2)
    n.obs = nrow(traindata)
    n.variables = ncol(traindata)
    summ<-m$grid[-1]
    summ$neighbourhood.fct<-as.character(summ$neighbourhood.fct)
    summ<-do.call(rbind, summ)
    mode<-attr(m,"mode")
    alpha = paste0(m$alpha, collapse = "; ")
    radius = paste0(round(m$radius, 3), collapse = "; ")
    # user.weights = m$user.weights
    maxNA.fraction = m$maxNA.fraction
    dist.fcts = m$dist.fcts
    if(length(dist.fcts)>1){
      dist.fcts<-paste0(dist.fcts[1],dist.fcts[2], sep="/")
    }


    Parameters<-
      rbind(
        n.obs,
        n.variables,
        summ,
        alpha,
        radius,
        #user.weights,
        maxNA.fraction,
        dist.fcts,
        mode
      )
    data.frame(Parameters)
  })
  output$train.summary<-renderTable({
    train.summary()
  }, rownames = T, colnames = F,striped=T, spacing ="xs")

  savesompred<-reactive({

    data=vals$saved_data[[input$data_som]]
    som_pred<-pred_som()
    predX<- correctsom()$predX
    predX<-data_migrate(data,predX,input$predsom_newname)
    factors<-attr(data,"factors")[rownames(predX),]
    coords<-attr(som_pred,"coords")[rownames(predX),]
    attr(predX,"factors")<-factors
    attr(predX,"coords")<-coords
    newsaveX<-paste0(input$predsom_newname,"_X")
    saved_sompred$df[[newsaveX]]<-1
    vals$saved_data[[newsaveX]]<-predX

    if(input$hand_save=="create"){
      if(attr(vals$som_results,"Method")!="Unsupervised"){
        predY<- correctsom()$predY
        if(attr(vals$som_results,"som_type2")=="Numeric"){
          newsave<-paste(input$predsom_newname,"_Y")
          predY<-data_migrate(data,predY,newsave)
          vals$saved_data[[newsave]]<-predY
        } else{

          attr(vals$saved_data[[newsaveX]],"factors")<-predY

        }
      }
    } else {
      pred_data<-data.frame(pred_data)
      pred_data<-data_migrate(data,pred_data,input$predsom_over)
      factors<-attr(pred_data,"factors")[rownames(pred_data),]
      factors[,input$predsom_over]<-pred_factors
      coords<-attr(test_data,"coords")[rownames(pred_data),]
      attr(pred_data,"factors")<-factors
      attr(pred_data,"coords")<-coords
      attr(pred_data,"transf")<-attr(data,"transf")
      vals$saved_data[[input$predsom_over]]<-pred_data}
    updateTabsetPanel(session,'som_tab','som_tab3')
  })

  savesom<-reactive({
    temp<-vals$som_results
    bmu<-temp$unit.classif
    names(bmu)<-rownames(temp$data[[1]])
    bmu<-as.factor(bmu)
    vals$bagmodel<-FALSE
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$model_newname
      attr(vals$saved_data[[input$data_som]],"som")[input$model_newname]<-c(temp,attr(vals$saved_data[[input$data_som]],"som")[[input$model_newname]])
      attr(vals$saved_data[[input$data_som]],"factors")[names(bmu),paste0('bmu_',input$model_newname)]<-bmu
      cur<-input$model_newname
    } else{
      temp<-list(temp)
      names(temp)<-input$model_over
      attr(vals$saved_data[[input$data_som]],"som")[input$model_over]<-temp
      attr(vals$saved_data[[input$data_som]],"factors")[names(bmu),input$model_over]<-bmu
      cur<-input$model_over

    }
    vals$cur_train<-cur
    updateTabsetPanel(session, "som_tab", vals$cursomtab)
    updatePickerInput(session,
                      "som_models",
                      choices=c(names(attr(vals$saved_data[[input$data_som]],"som"))),
                      selected=cur)})
  output$nopredictions<-renderPrint({
    cat("no predictions in \n'",input$data_som,"' \n to overwritte")
  })
  output$nosommodel<-renderPrint({
    cat("no som model in \n'",input$data_som,"' \n to overwritte")
  })
  observeEvent(input$trainSOM, {
    vals$bagmodel<-T
    vals$cur_train<-"new som (unsaved)"
    updateTabsetPanel(session, "som_tab", "som_tab2")
    updateTabsetPanel(session, "som_tab", "train_tab2")
    # updateSelectInput(session,"som_models",                      choices=c("new som (unsaved)", names(attr(vals$saved_data[[input$data_som]],"som"))))
    updateSelectInput(session,"som_models",
                      selected="new som (unsaved)")
  })
  observeEvent(input$resetsom, {
    tunesom$rlen=500
    tunesom$distmethod="BrayCurtis"
    tunesom$seed=1
    tunesom$a1=0.05
    tunesom$a2=0.01
    tunesom$r1=0
    tunesom$r2=0
    tunesom$mode="online"
    tunesom$maxna=0.001
  })
  observeEvent(input$resettopo, {
    tunesom$sugtopo=T

    tunesom$toroidal="FALSE"
    tunesom$topo="hexagonal"
    tunesom$neighbourhood.fct='bubble'
  })

  observeEvent(input$tools_savesom,{
    if(input$tools_savesom %% 2){
      vals$hand_save<-"Save new som in"
      vals$hand_save2<-column(12,fluidRow(em(input$data_som, style="color:gray"),strong("::"), em("Som-Attribute", style="color:gray"),strong("::")
      ))
      vals$hand_save3<-em("Both 'create' and 'overwrite' options add the resulting BMU to the 'Factor-Attribute' of the corresponding Datalist."
      )
      showModal(
        hand_save_modal())}
  })

  observeEvent(input$trainSOM,{
    data = getdata_som()
    data_o<-vals$saved_data[[input$data_som]]
    test<-which(!rownames(data_o)%in%rownames(data))
    withProgress(message = "Running som... the time taken will depend on the size of the data and the training.",
                 min = 1,
                 max = 1,
                 {
                   if(input$som_type=='Unsupervised')
                   {
                     if(is.na(input$seed)==F){set.seed(input$seed)}
                     m<-try(supersom(
                       list(X=as.matrix(data)),
                       grid = somgrid(
                         input$xdim,
                         input$ydim,
                         topo = input$topo,
                         toroidal = toroidal(),
                         neighbourhood.fct=tunesom$neighbourhood.fct
                       ),
                       rlen = input$rlen,
                       dist.fcts = input$distmethod,
                       alpha = c(tunesom$a1, tunesom$a2),
                       radius = c(tunesom$r1, tunesom$r2),
                       mode = tunesom$mode,
                       maxNA.fraction = tunesom$maxna
                     ))

                   } else {
                     sup_test<-get_sup_som_test()
                     if(is.na(input$seed)==F){set.seed(input$seed)}
                     m<-try(xyf(
                       X=as.matrix(data),
                       Y=if(input$som_type2=="Numeric") {as.matrix(vals$saved_data[[input$data_somY]][rownames(data),])} else{
                         get_supsom_list()
                       },
                       #whatmap = 1,
                       grid = somgrid(
                         input$xdim,
                         input$ydim,
                         topo = input$topo,
                         toroidal = toroidal(),
                         neighbourhood.fct=tunesom$neighbourhood.fct
                       ),
                       rlen = input$rlen,
                       dist.fcts = input$distmethod,
                       alpha = c(tunesom$a1, tunesom$a2),
                       radius = c(tunesom$r1, tunesom$r2),
                       mode = tunesom$mode,
                       maxNA.fraction = tunesom$maxna
                     ))
                     attr(m,"sup_test")<-sup_test
                     attr(m,"supervisor")<-input$data_somY

                   }






                   #updateTabItems(session, "tabs", "training")
                   #updateTabsetPanel(session, "som_tab", "results")
                   #updateTabsetPanel(session, "som_tab", "results")
                   if (class(m) != "kohonen")
                   {
                     validate(paste(m[[1]], "Please decrease your alpha (learning rate)"))
                   }
                   attr(m,'mode')<-input$mode

                   names(m$unit.classif)<-rownames(m$data[[1]])

                   if(input$som_test_pick!="None"){
                     attr(m,"test_partition")<-paste(input$som_test_pick,"::",input$som_test_ref)
                     attr(m,"test")<-data_o[test,]

                   } else{

                     attr(m,"test_partition")<-"None"

                   }
                   attr(m,"Method")<-if(input$som_type=="Unsupervised"){"Unsupervised"} else{
                     input$data_somY
                   }


                   attr(m,"Datalist")<-input$data_som
                   attr(m,"Partition")<-paste("Test data:",input$som_test_pick,"::",input$som_test_ref)
                   attr(m,"som_type2")<-input$som_type2
                   if(input$som_type=="Supervised"){
                     if(input$som_type2=="Factors"){attr(m,"faclist")<-attr(get_supsom_list(),"faclist")}}
                   attr(m,"coords")<-attr(data,"coords")
                   vals$som_unsaved<-m

                   m


                 })

  })
  observeEvent(input$resettopo, {
    output$somgridtopo<-renderUI({topocontrol()})})
  observe({
    req(input$som_models)
    if(input$som_models=="new som (unsaved)"){
      vals$som_results<-vals$som_unsaved
    } else{
      vals$som_results<-attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]}
  })
  observeEvent(input$data_som,{
    updateTabsetPanel(session,'som_tab', 'som_tab1')})
  observeEvent(input$som_test_ref,{
    updateTabsetPanel(session,'som_tab', 'som_tab1')})
  observeEvent(input$som_test_pick,{
    updateTabsetPanel(session,'som_tab', 'som_tab1')})
  observeEvent(input$tools_savesom_pred,{

    if(input$predsom_results=="Data predictions (X)"|input$predsom_results=="Data predictions (Y)")  {

      vals$hand_save<-"Save som predictions"
      vals$hand_save2<-NULL
      mess<-"Creates a Datalist with the SOM predictions"
      if(attr(vals$som_results,"Method")!="Unsupervised"){
        if(attr(vals$som_results,"som_type2")=="Numeric")
        {
          mess<-paste(mess,"The defined name will be automatically followed by the '_X' tag. Y (numerical) predictions will also be automatically saved, with the '_Y' tag instead" )
        } else{
          mess<-paste(mess,"The defined name will be automatically followed by the '_X' tag. The Y predictions (factors) will be assigned to the Factor-Attribute of the created datalist" )
        }
      } else{
        mess<-paste(mess,"The defined name will be automatically followed by the '_X' tag")
      }
      vals$hand_save3<-em(mess)}

    if(input$predsom_results=="Obs errors (X)"){
      vals$hand_save<-"Save errors from som predictions (X)"
      vals$hand_save2<-NULL
      vals$hand_save3<-NULL
    }
    if(input$predsom_results=="Obs errors (Y)"){
      vals$hand_save<-"Save errors from som predictions (Y)"
      vals$hand_save2<-NULL
      vals$hand_save3<-NULL
    }
    showModal(hand_save_modal())
  })


  #### PREVIEW-PLOT

  ## DOWNLOAD FUNCTION


mybooks<-readRDS('vals.rds')
for (var in names(mybooks)) {    vals[[var]] <- mybooks[[var]]  }
updateTextInput(session, "tabs", value = mybooks$cur_tab)


}

#object.size(mybooks$saved_data)

#res<-do.call(rbind,lapply(mybooks,object.size))
