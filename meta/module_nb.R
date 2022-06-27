
plot_dens_nb<-function(m,var="Chl",newcolhabs,palette="viridis",lwd=1,xlab="Variables"){
  opar<-par(no.readonly=TRUE)
  layout(matrix(c(1,2),nrow=1),widths = c("70","30"))
  colors<-getcolhabs(newcolhabs,palette,length(m$levels))
  tables<-m$finalModel$tables[[var]]
  if(lapply(m$finalModel$tables,class)[[1]]=="table"){
    par(mar=c(5,5,5,3),las=1)
    colors<-getcolhabs(newcolhabs,palette,length(levels(m$trainingData[[var]])))
    spineplot(tables,col=colors,las=1,ylab="",xlab=paste("Class:",attr(m,"supervisor")),off=1,axes =T,yaxlabels=F)
    par(mar=c(0,0,5,0),xpd=T)
    plot.new()
    legend(0,1,legend=levels(m$trainingData[[var]]),pch=15,col=colors,bty="n")
    on.exit(par(opar),add=TRUE,after=FALSE)

  } else{

    par(mar=c(5,5,5,0))
    res<-lapply(tables,function(x)
      c(
        minx=min(x$x),
        maxx=max(x$x),
        miny=min(x$y),
        maxy=max(x$y)
      ))
    res<-do.call(rbind,res)
    minx<-min(res[,1])
    maxx<-max(res[,2])
    miny<-min(res[,3])
    maxy<-max(res[,4])
    plot(m$finalModel$tables[[var]][[1]],xlim=c(minx,maxx),ylim=c(miny,maxy),type="n",main=var)
    i=1
    for(i in 1:length(tables)){
      lines(tables[[i]],col=colors[i],lwd=lwd)
    }

    par(mar=c(0,0,5,0),xpd=T)
    plot.new()
    legend(0,1,legend=m$levels,lty=1,col=colors,bty="n")
    on.exit(par(opar),add=TRUE,after=FALSE)
  }


}


module_ui_nb <- function(id){

  ns <- NS(id)
  tagList(
    includeCSS("meta/styles_mod.css"),
    column(12,style="background: white",
      uiOutput(ns("nb_inputs")),
      uiOutput(ns("war_nb")),
      column(12,
             tabsetPanel(id = ns("nb_tab"),
                         tabPanel(strong("1. Training"),value='nb_tab1',
                                  uiOutput(ns("nb_params")))

             )
      )

    )
  )

}

# Server
module_server_nb <- function (input,output,session,vals,df_colors,newcolhabs ){
  ns <- session$ns

  insert_nb_resutls<-reactiveValues(df=F)
  observe({
    req(input$data_nbX)
    req(isFALSE(insert_nb_resutls$df))
    req(length(vals$nb_unsaved)>0|length(attr(vals$saved_data[[input$data_nbX]],"nb"))>0)
    insertTab('nb_tab',target='nb_tab1',
              tabPanel(strong("2. Results"),value='nb_tab2',
                       uiOutput(ns("results_nb"))),

    )
    insertTab('nb_tab',target='nb_tab2',
              tabPanel(strong("3. Predict"),value='nb_tab3',
                       uiOutput(ns("predict_nb")))
    )
    insert_nb_resutls$df<-T
  })



  getnewdatanb<-reactive({
    datalist<-vals$saved_data
    Xattr<-attr(vals$nb_results,'inputs')$Xattr
    if(Xattr=="Data-Attribute"){
      datalist_comp<-lapply(
        vals$saved_data,function (x){
          colnames(x)<-gsub(" ",".",colnames(x))
          x
        }

      )

      datalist_comp<-vals$saved_data
    }
    if(Xattr=="Factor-Attribute"){
      datalist_comp<-lapply(
        vals$saved_data,function (x){
          x<-attr(x,"factors")
          colnames(x)<-gsub(" ",".",colnames(x))
          x
          }

      )
    }


    m<-vals$nb_results
    Y<-which(colnames(m$trainingData)==".outcome")
    res0<-unlist(
      lapply(datalist_comp,function (x){
        res<-colnames(x)%in%colnames(m$trainingData[-Y])
        sum(res)==ncol(m$trainingData[-Y])
      })
    )


    names(datalist[res0==T])
  })



  get_supervisor_nb <- reactive({
    data <- vals$saved_data[[input$data_nbY]]
    labels <- attr(data,"factors")
    labels[input$nb_sup]
  })


  output$predict_nb<-renderUI({
    column(12,style="background: white", uiOutput(ns("nb_pred_type")),
           tabsetPanel(id="prednb_tab",
                       tabPanel(
                         strong("3.1. Results"), value="prednb_tab01",
                         fluidRow(style="background: white",
                                  uiOutput(ns("nb_tab3_1")))),
                       tabPanel(
                         strong("3.2. Errors"),
                         fluidRow(style="background: white",
                                  uiOutput(ns("nb_tab3_2")))
                       )

           )
    )
  })

  output$nb_pred_type<-renderUI({
    choices=if(length(attr(vals$nb_results,'test'))==1){c("Datalist")
    } else{c("Partition","Datalist")}
    div(inline(radioButtons(ns("nbpred_which"),"New data (X):",choices=choices,inline=T, width="200px")), inline(uiOutput(ns("datalist_pred_nb"))))
  })

  output$datalist_pred_nb<-renderUI({
    req(input$nbpred_which =='Datalist')
    div(
      pickerInput(ns("prednb_new"),"Datalist",names(vals$saved_data[getnewdatanb()]), width = '300px')
    )
  })




  get_cm_pred<-reactive({
    obs<-get_nb_prederrors()$obs
    pred<-get_nb_prederrors()$pred
    conf<-table(obs,pred)
    conf
  })

  output$confusion_nb_pred<-renderUI({
    conf<-get_cm_pred()
    column(12,
           renderPlot({
             res<-plotCM(conf/sum(conf)*100,input$nbpalette_pred, newcolhabs=vals$newcolhabs)
             vals$nb_cm_pred<-res
             res
           }))

  })
  output$confusion_nb2_pred<-renderPrint(
    confusionMatrix(get_cm_pred()))




  output$nb_tab3_2<-renderUI({
    sup_test<-attr(vals$nb_results,"supervisor")
    supname<-paste0("Observed Y [",sup_test,"]")
    sidebarLayout(
      sidebarPanel(
        fluidRow(class="map_control_style",style="color: #05668D",
                 div(
                   span("+",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
                        span(supname),
                        inline(pickerInput(ns("prednb_newY"),
                                           NULL,names(vals$saved_data[getobsnb()]),width="200px"))
                   )
                 ),
                 div(
                   span(tipify(icon("fas fa-question-circle"),"Confusion Matrix Palette"),"+ Palette",
                        inline(
                          pickerInput(inputId = ns("nbpalette_pred"),
                                      label = NULL,
                                      choices =     vals$colors_img$val,
                                      choicesOpt = list(content =     vals$colors_img$img),options=list(container="body"),width="75px")
                        )
                   )
                 ),
                 div(tipify(actionLink(ns("downp_cmnb_pred"),span("+ Donwload",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"Download plot"))

        )
      ),
      mainPanel(
        tags$style(
          paste(paste0("#",ns('nb_tab_errors0_test')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
        ),
        tags$style(
          paste0("#",ns('nb_tab_errors0_test')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
        ),
        div(
          p(strong("Prediction stats:")),
          inline(DT::dataTableOutput(ns('nb_tab_errors0_test')))
        ),
        div(
          p(strong("Confusion matrix:")),
          uiOutput(ns("confusion_nb_pred")),
          verbatimTextOutput(ns("confusion_nb2_pred"))
        )






      )
    )
  })

pred_test_nb<-reactive({
  obs<-get_nb_prederrors()$obs
  pred<-get_nb_prederrors()$pred
  table<-data.frame(t(postResample(pred,obs)))
  table
})

  output$nb_tab_errors0_test<-DT::renderDataTable({
    table<-pred_test_nb()
    table<-round(table,3)
    rownames(table)<-NULL

    DT::datatable(table,options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't',rownames = TRUE,class ='compact cell-border'))

  })
  get_nb_prederrors<-reactive({
    m<-vals$nb_results
    pred<-pred_nb()[,1]
   # x_t<-attr(m,"test")
    obs<-if(input$nbpred_which=="Partition"){
   attr(m,"sup_test")} else{
        attr(vals$saved_data[[input$prednb_newY]],"factors")[,attr(vals$nb_results,"supervisor")]
      }
    return(
      list(
        obs=obs,
        pred=pred
      )
    )
  })

  getobsnb<-reactive({
    sup_test<-attr(vals$nb_results,"supervisor")
    datalist<-vals$saved_data
    datalist=lapply(datalist,function(x)attr(x,"factors"))
    m<-vals$nb_results
    res0<-unlist(
      lapply(datalist,function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })

  output$nb_tab3_1<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('nb_create_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")),style="button_active"
                       ),
                       "Create a datalist with the NB predictions",options=list(container="body")
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         ns('down_nb_tab3_1'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style="button_active"
                       ),
                       "Download Table",options=list(container="body")
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('nbtab_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('nbtab_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          inline(DT::dataTableOutput(ns('nbtab_pred')))


        )
      )
    )
  })


  observeEvent(input$down_nb_tab3_1,{
    vals$hand_down<-"NB - predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })

  output$nbtab_pred<-DT::renderDataTable({
    table<-vals$nbtab_pred<-pred_nb()
    DT::datatable(table,options=list(pageLength = 20,info = FALSE,lengthMenu = list(c(20,-1),c( "20","All")),autoWidth=T,dom = 'lt'))

  },rownames = TRUE,class ='compact cell-border')

  test_nb<-reactive({
    req(input$nbpred_which)

    Xattr<-attr(vals$nb_results,'inputs')$Xattr
    req(length(Xattr)>0)
    if(input$nbpred_which=="Datalist"){
      req(input$prednb_new)
      if(Xattr=="Data-Attribute"){

        pred_tab<-vals$saved_data[[input$prednb_new]]

      }
      if(Xattr=="Factor-Attribute"){

        pred_tab<-attr(vals$saved_data[[input$prednb_new]],"factors")
      }

    } else   if(input$nbpred_which=="Partition"){
      pred_tab<-attr(vals$nb_results,"test")
    }
    colnames(pred_tab)<-gsub(" ",".",colnames(pred_tab))
    pred_tab

  })

  pred_nb<-reactive({
    validate(need(!anyNA(test_nb()),"NAs not allowed in the prediction Datalist"))
    m<-vals$nb_results


    nb_pred <- predict(m$finalModel,newdata = test_nb())
    res<-data.frame(Predictions= nb_pred)
    rownames(res)<-rownames(test_nb())
    #colnames(res)<-attr(vals$nb_results,"supervisor")

    # attr(res,"obs")<-test_nb()
    res



  })

  observeEvent(input$data_nbY,{
    vals$cur_data_nbY<-input$data_nbY
  })
  observeEvent(input$nb_attribute,{
    vals$cur_nb_attribute<-input$nb_attribute
  })
  observeEvent(input$prednb_new,{
    vals$prednb_new<-input$prednb_new
  })

  observeEvent(input$nbpred_which,{
    vals$nbpred_which<-input$nbpred_which
  })
  output$nb_inputs <- renderUI({
    if(is.null(vals$cur_nb_attribute)){vals$cur_nb_attribute<-1}
    if(is.null(vals$cur_data_nbY)){vals$cur_data_nbY<-1}
    #req(length(vals$saved_data)>0)
    div(style="color: #05668D;",
             if(input$nb_tab=="nb_tab1"){
               div(
                 div(class="well3",
                     span(strong("X:"),
                          inline(uiOutput(ns('data_nbX_out'))),"::",
                          inline(
                            div(
                              div("Attribute:"),
                              div(pickerInput(ns("nb_attribute"),NULL,choices =  c("Factor-Attribute","Data-Attribute"),width="150px",selected=vals$cur_nb_attribute))
                            )
                          )


                     )
                 ),

                 div(class="well3",
                      span(strong("Y:"),
                           inline(
                             div(
                               div("Datalist:"),
                               div(pickerInput(ns("data_nbY"),NULL,choices =  names(vals$saved_data),width="150px",selected=vals$cur_data_nbY))
                             )
                           ),
                           "::",
                           inline(uiOutput(ns('nb_supervisor'))),

                           inline(uiOutput(ns("nb_test_part"))),
                           "::",
                           inline(uiOutput(ns("nb_partition")))
                      )
                 )


               )
             } else {
               column(12,
                      inline( uiOutput(ns('data_nbX_out'))),
                      inline(uiOutput(ns("nb_results_menu"))),
                      inline(uiOutput(ns("savenb_button"))),
                      inline(uiOutput(ns("rmnb_button"))),
               )
             }
    )
  })


  output$nb_params<- renderUI({
    column(12,class="well2",
           div(
             uiOutput(ns("teste"))
           ),

           div(class="map_control_style",style="color: #05668D; margin-top: 20px",
               div(

                 tipify(icon("fas fa-question-circle",style="color: gray"),"one or multiple numeric values for Laplace correction (if a vector,comma delimited between 0 and 1)",options=list(container="body")),
                 "+ fL",
                 textInput(ns('fL_nb'),NULL,value ='1',width="200px")
               ),
               div(
                 tipify(icon("fas fa-question-circle",style="color: gray"),"one or multiple numeric values for Bandwidth Adjustment (larger numbers mean more flexible density estimate; if a vector must be comma delimited between 0 and 1)",options=list(container="body")),
                 "+ Bandwidth Adjustment",
                 textInput(ns('bandw_nb'),NULL,value ='1',width="200px")

               ),
               div(
                 tipify(icon("fas fa-question-circle"),textseed(),options = list(container="body")),
                 "+ seed:",
                 inline(
                   numericInput(ns("seednb"),NULL,value = NULL,width="122px")
                 )
               ),

               div(popify(icon("fas fa-question-circle"),NULL,
                          HTML(paste0(
                            div(HTML(paste0(strong("repeatedcv:")," repeated k-fold cross-validation;"))),
                            div(HTML(paste0(strong("boot:")," bootstraping;"))),

                            div(HTML(paste0(strong("LOOCV:")," Leave one out;"))),
                            div(HTML(paste0(strong("LGOCV:")," Leave-group out")))

                          )),options=list(container="body")

               ),
               "+ Resampling method:",
               pickerInput(ns("nb_res_method"),NULL,choices=list("repeatedcv","boot","LOOCV","LGOCV"),width = "100px")
               ),
               uiOutput(ns("nb_resampling"))
           ),

           column(12,style='white-space: normal;',
                  uiOutput(ns("nb_war"))
           ),

           column(12,align = "center",
                  popify(actionButton(ns("trainNB"),h4(img(src=nb_icon,height='20',width='20'),"train Naive Bayes",icon("fas fa-arrow-circle-right")),style = "background:  #05668D; color: white"),NULL,"Click to run")
           ),

    )
  })


  output$nb_resampling<-renderUI({
    div(
      inline(
        if(input$nb_res_method=='cv'|input$nb_res_method=='repeatedcv')
        {
          div(

            tipify(icon("fas fa-question-circle"),"number of folds",options=list(container="body")),"+ cv:",
            numericInput(ns("cvnb"),NULL,value = 5,width="140px")

          )
        }
      ),
      div(
        inline(
          if(input$nb_res_method=='repeatedcv')
          {
            inline(
              div(tipify(icon("fas fa-question-circle"),"the number of complete sets of folds to compute",options=list(container="body")),
                  "+ repeats:",
                  numericInput(ns("repeatsnb"),NULL,value = 1,width="109px")
              )
            )

          }
        )
      ),
      inline(
        if(input$nb_res_method=='boot'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the number of resampling iterations",options=list(container="body")),
              "+ number:",
              numericInput(ns("cvnb"),NULL,value = 5,width="100px")

            )
          )
        }
      ),
      inline(
        if(input$nb_res_method=='LGOCV'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the training percentage",options=list(container="body")),
              "+ percentage:",
              numericInput(ns("pleavenb"),NULL,value = 10,width="100px")
            )
          )
        }
      )
    )
  })




  output$nb_supervisor <- renderUI({
    req(input$data_nbY)

    if(is.null(vals$cur_nb_sup)){vals$cur_nb_sup<-1}
    data <- vals$saved_data[[input$data_nbY]]
    labels <- attr(data,"factors")
    choices<-rev(colnames(labels))
    div(well="class2",
      div("Variable"),
      div(tipify(pickerInput(
        ns("nb_sup"),
        NULL,
        choices =choices ,
        width="150px",selected=vals$cur_nb_sup
      ),"The response vector"))
    )

  })
  observeEvent(input$nb_sup,{
    vals$cur_nb_sup<-input$nb_sup
  })
  output$data_nbX_out<-renderUI({
    if(is.null(vals$cur_data)){vals$cur_data<-1}
    div(
      div("~ Training Datalist:"),
      pickerInput(ns("data_nbX"),NULL,choices =names(vals$saved_data),width="150px",selected=vals$cur_data)
    )
  })
  observeEvent(input$data_nbX,{
    vals$cur_data<-input$data_nbX
  })
  output$nb_test_part<-renderUI({
    req(input$data_nbX)
    if(is.null(vals$cur_nb_test_partition)){vals$cur_nb_test_partition<-1}
    div(
      div(span("Partition:",tiphelp("choose a factor as reference for the partition"))),
      div(pickerInput(ns("nb_test_partition"),NULL,choices=c("None",colnames(attr(vals$saved_data[[input$data_nbY]],"factors"))),width="150px",selected=vals$cur_nb_test_partition))
    )
  })
  observeEvent(input$nb_test_partition,{
    vals$cur_nb_test_partition<-input$nb_test_partition
  })
  output$nb_partition<-renderUI({
    req(input$nb_test_partition!="None")
    req(input$data_nbY)
    if(is.null(vals$cur_testdata_nb)){vals$cur_testdata_nb<-1}
    fac<-attr(vals$saved_data[[input$data_nbY]],"factors")[,input$nb_test_partition]
    choices<-levels(fac)
    div(
      div("::",span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training,and can be be later used to generate predictions"))),
      pickerInput(ns("testdata_nb"),NULL,choices=choices,width="200px",selected=vals$cur_testdata_nb)
    )
  })
  observeEvent(input$testdata_nb,{
    vals$cur_testdata_nb<-input$testdata_nb
  })
  output$nb_results_menu<-renderUI({
    if(is.null(vals$cur_nb_models)){vals$cur_nb_attribute<-1}
    div(pickerInput(ns("nb_models"),strong("NB results:",tiphelp("Random Forest Results. Click to select nb results saved in the Training Datalist (X).")),choices= names(attr(vals$saved_data[[input$data_nbX]],"nb")),width="200px",selected = vals$cur_nb_models)
    )
  })
  observeEvent(input$nb_models,{
    vals$cur_nb_models<-input$nb_models
  })
  output$rmnb_button<-renderUI({
    req(input$nb_models!="new nb (unsaved)")
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_rmnb"),icon("far fa-trash-alt")),"Remove the nb model from the training Datalist (X)")
    )

  })



  get_parts_nb<-reactive({
    req(input$data_nbY)
    req(input$testdata_nb)
    partition<- attr(vals$saved_data[[input$data_nbY]],"factors")[,input$nb_test_partition]
    test<-which(partition==input$testdata_nb)
    train<-which(partition!=input$testdata_nb)
    list(train=train,test=test)
  })

  getdata_nbX<-reactive({
    req(input$data_nbX)

    if(input$nb_attribute=="Factor-Attribute"){
      data<-attr(vals$saved_data[[input$data_nbX]],"factors")
    } else{
      data=vals$saved_data[[input$data_nbX]]
    }
    data
  })


  getdata_nbY<-reactive({
  data <- vals$saved_data[[input$data_nbY]]
  labels <- attr(data,"factors")
   labels<-labels[,input$nb_sup]
   labels
})



  observeEvent(input$trainNB,{
    try({


      x<-x_o<-getdata_nbX()
      y<-y_o<-getdata_nbY()
      output$nb_war<-renderUI({
        column(12,style="color: red",align="center",
               if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
               if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
        )
      })
      validate(need(anyNA(x)==F,"NAs not allowed in X"))
      validate(need(anyNA(y)==F,"NAs not allowed in Y"))

      if(input$nb_test_partition!="None"){
        parts<-get_parts_nb()
        train<-parts$train
        test<-parts$test
        x<-getdata_nbX()[train,]
        y<-as.factor(getdata_nbY()[train])
      }

      if(input$nb_attribute=="Factor-Attribute"){
        if(input$data_nbY==input$data_nbX){
          x[input$nb_sup]<-NULL
        }
      }

      if(input$nb_attribute=="Data-Attribute"){
        usekernel=T
      } else{usekernel=c(T,F)}


      bandw_nb<-as.numeric(unlist(strsplit(input$bandw_nb,",")))
      fL_nb<-as.numeric(unlist(strsplit(input$fL_nb,",")))
      grid <- expand.grid(fL=c(fL_nb),usekernel = c(usekernel),adjust=c(bandw_nb))


      seed<-if (!is.na(input$seednb)){ input$seednb} else{
        NULL
      }
      nb_search<-input$nb_search

      colnames(x)<-gsub(" ",".",colnames(x))
      colnames(x_o)<-gsub(" ",".",colnames(x_o))

      if (!is.na(input$seednb)){set.seed(input$seednb)}
      withProgress(message = "Running Naive Bayes ...",
                   min = 1,
                   max = 1,
                   {
                     NB<-train(x,y,'nb',

                               trControl=trainControl(

                                 method = input$nb_res_method,
                                 number = input$cvnb,
                                 repeats = input$repeatsnb,
                                 p=input$pleavenb/100,
                                 savePredictions = "final"

                               ),
                               tuneGrid=grid
                     )
                     attr(NB,"test_partition")<-paste("Test data:",input$nb_test_partition,"::",input$testdata_nb)
                     attr(NB,"Y")<-paste(input$data_nbY,"::",input$nb_sup)
                     attr(NB,"Datalist")<-paste(input$data_nbX)

                     vals$nb_unsaved<-NB

                     if(input$nb_test_partition!="None"){
                       attr(vals$nb_unsaved,'test')<-x_o[test,]
                       attr(vals$nb_unsaved,"sup_test")<-y_o[test]
                     } else{ attr(vals$nb_unsaved,'test')<-c("None")}
                     vals$bag_nb<-T
                     attr(vals$nb_unsaved,"supervisor")<-input$nb_sup
                     attr(vals$nb_unsaved,"inputs")<-list(
                       Ydatalist=input$data_nbY,
                       Y=input$nb_sup,
                       Xdatalist=input$data_nbX,
                       Xattr=input$nb_attribute
                     )
                     updateTabsetPanel(session,"nb_tab","nb_tab2")
                   })
      beep(10)
      attr(vals$saved_data[[input$data_nbX]],"nb")[['new nb (unsaved)']]<-vals$nb_unsaved
      vals$cur_nb_models<-"new nb (unsaved)"
      vals$bag_nb<-T
      #saveRDS(vals$nb_unsaved,"nb.rds")



    })
  })


  output$results_nb<-renderUI({
    req(length(vals$nb_results))
    m<-vals$nb_results
    Y<-which(colnames(m$trainingData)==".outcome")
    nvars<-ncol(m$trainingData[,-Y])
    req(length(nvars)>0)
    val<-if(nvars>10){10} else{
      nvars
    }
    column(12,
           div(
             p(
               strong("Model:"),em(getmodel_nb())
             )
           ),
           tabsetPanel(
             tabPanel("1. Summary",
             renderPrint(m)
                      ),
             tabPanel("2. Training error",
                      uiOutput(ns("nb_tab_2_2"))
                      ),
             tabPanel(if(lapply(m$finalModel$tables,class)[[1]]=="table"){"3. Mosaic plot"} else {"3. Density plot"},
                      uiOutput(ns("nb_tab_2_3"))
             ),
             tabPanel("4. Variable importance",
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(
                            class="map_control_style",style="color: #05668D",
                            div("+ Number of variables:",
                              numericInput(ns("varImp_n_nb"),NULL,value=val,step=1,max=nvars,width="60px")
                            ),
                            div(
                              tipify(
                                actionLink(
                                  ns('nb_varImp'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style="button_active"
                                ),
                                "Download Table (importances)",options=list(container="body")
                              )
                            ),
                            div(
                              tipify(
                                actionLink(
                                  ns('nb_varImp_plot'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")),style="button_active"
                                ),
                                "Download plot",options=list(container="body")
                              )
                            )
                          )
                        ),
                        mainPanel(  uiOutput(ns("nb_tab_2_4")))
                      )

             ),
             tabPanel("5. Confusion matrix",
                      uiOutput(ns("nb_tab_2_5"))
             )
           )

    )
  })

output$nb_tab_2_3<-renderUI({
  m<-vals$nb_results
  Y<-which(colnames(m$trainingData)==".outcome")
  choices=colnames(m$trainingData[-Y])
  sidebarLayout(
    sidebarPanel(
      fluidRow(class="map_control_style",style="color: #05668D",

               div("+ Variable",
                   pickerInput(ns("nb_dens_var"),NULL,choices=rev(choices),width="150px")
                   ),
               div(
                 span("+ Palette",
                      inline(
                        pickerInput(inputId = ns("nb_dens_palette"),
                                    label = NULL,
                                    choices =     vals$colors_img$val,
                                    choicesOpt = list(content =     vals$colors_img$img),options=list(container="body"),width="75px")
                      )
                 )
               ),
               div(
                 "+ Line width",
                 inline(numericInput(ns("nb_dens_lwd"),NULL,1,step=0.1,width="75px"))
               ),
               div(tipify(actionLink(ns("downp_nb_dens"),span("+ Donwload",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"Download plot"))

      )
    ),
    mainPanel(
      uiOutput(ns("nb_densplot"))
    )
  )
})


output$nb_densplot<-renderUI({
  renderPlot({
    plot_dens_nb(vals$nb_results,var=input$nb_dens_var,palette=input$nb_dens_palette,lwd=input$nb_dens_lwd,newcolhabs=vals$newcolhabs)
    vals$nb_densplot<-recordPlot()
  })
})
  output$nb_tab_2_2<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('nb_create_errors_train'),span("+ Create Datalist",icon("fas fa-file-signature")),style="button_active"
                       ),
                       "Create a datalist with the NB training errors",options=list(container="body")
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         ns('nb_down_errors_train'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style="button_active"
                       ),
                       "Download Table",options=list(container="body")
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('nb_tab_errors0_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('nb_tab_errors0_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          tags$style(
            paste(paste0("#",ns('nb_tab_errors_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('nb_tab_errors_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          div(
            p(strong("Global:")),
            inline(DT::dataTableOutput(ns('nb_tab_errors0_train')))
          ),

          div(
            p(strong("Observations:")),
            inline(DT::dataTableOutput(ns('nb_tab_errors_train')))
          )



        )
      )
    )
  })

  observeEvent(input$downp_cmnb,{
    vals$hand_plot<-"NB - Confusion Matrix"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)})

  observeEvent(input$downp_cmnb_pred,{
    vals$hand_plot<-"NB - Confusion Matrix (predictions)"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)})

  observeEvent(input$nb_varImp_plot,{
    vals$hand_plot<-"NB - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
  })
  observeEvent(input$downp_nb_dens,{
    vals$hand_plot<-"NB - DM plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
  })





  observeEvent(input$nb_down_errors_train,{
    vals$hand_down<-"NB - training errors (observations)"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })

  observeEvent(input$nb_varImp,{
    vals$hand_down<-"NB - variable importance"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })



  output$nb_tab_errors0_train<-DT::renderDataTable({
    m<-vals$nb_results
    table<- m$results[rownames(m$bestTune),]
    DT::datatable(table,options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't',rownames = TRUE,class ='compact cell-border'))

  })

  output$nb_tab_errors_train<-DT::renderDataTable({
    m<-vals$nb_results
    table<-accu_rf_class(m)
    vals$nb_down_errors_train<-table
    table
    DT::datatable(table,options=list(pageLength = 20,info = FALSE,lengthMenu = list(c(20,-1),c( "20","All")),autoWidth=T,dom = 'lt'),rownames = TRUE,class ='compact cell-border')

  })

  output$nb_tab_2_4<-renderUI({
    renderPlot({
      m<-vals$nb_results
      saveRDS(reactiveValuesToList(vals),"vals.rds")

      X2<-X <- varImp(m)
      vals$nb_varImp <-data.frame(X2$importance)
      X2$importance<-X2$importance[1:input$varImp_n_nb,]
      colnames(X2$importance)<-m$levels
      vals$nb_varImp_plot<-res<-plot(X2)
      res
    })
  })


output$nb_tab_2_5<-renderUI({
 # validate(need(is.factor( vals$nb_results$finalModel$y),"Confusion matrices are only valid for classification (factor)models."))
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        class="map_control_style",style="color: #05668D",
        div(
          span(
            "+ Type: ",
            pickerInput(inputId = ns("nb_cm_type"),
                        label = NULL,
                        choices = c("Resampling","Optimal Model"),

                        width="100px"
            )
          )
        ),
        div(
          span(
            "+ Palette",
            pickerInput(inputId = ns("nbpalette"),
                        label = NULL,
                        choices = vals$colors_img$val,
                        choicesOpt = list(
                          content = vals$colors_img$img
                        ),
                        options=list(container="body"),
                        selected=vals$colors_img$val[1],
                        width="75px"
            )
          )
        ),
        div(
          popify(actionLink(ns("downp_cmnb"),span("+ Download",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"download CM plot")
        ),
        div(
          popify(actionLink(ns("dowcenter_cmnb"),span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style  = "button_active"),NULL,"download CM plot")
        )

      )

    ),
    mainPanel(
      plotOutput(ns("confusion_nb")),
      verbatimTextOutput(ns("confusion_nb2"))
    )
  )

})
observeEvent(input$dowcenter_cmnb,{
  vals$hand_down<-"nb_cm"
  module_ui_downcenter("downcenter")
  mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
})

output$confusion_nb <- renderPlot({
  m<-vals$nb_results
  if(input$nb_cm_type=="Resampling"){
    res<-plotCM(m,input$nbpalette, newcolhabs=vals$newcolhabs)
  } else{
    cm<-table(predict(vals$nb_results),vals$nb_results$trainingData[,'.outcome'])
    res<-plotCM(cm,input$nbpalette,vals$newcolhabs)
    }
  vals$cm_nb<-res
  res
})
output$confusion_nb2<-renderPrint({
  res<-if(input$nb_cm_type=="Resampling"){
    confusionMatrix(vals$nb_results$pred$pred,vals$nb_results$pred$obs)
  } else{
    confusionMatrix(predict(vals$nb_results),vals$nb_results$trainingData[,'.outcome'])
  }
  vals$nb_cm<-res$table
  res

})


  observe({
    req(input$nb_models)
    if(input$nb_models=="new nb (unsaved)"){
      vals$nb_results<-vals$nb_unsaved } else{
        vals$nb_results<-attr(vals$saved_data[[input$data_nbX]],"nb")[[input$nb_models]][[1]]
      }
  })






  getmodel_nb<-reactive({

    attri<-attr(vals$nb_results,'inputs')
    res<-c(attri$Ydatalist,
           attri$Y,
           attri$Xdatalist,
           attri$Xattr)
    model<-  paste( paste0(res[1],"::",res[2]),"~",paste0(res[3],"::",res[4]))
    model

  })

  observeEvent(input$tools_rmnb,{
    attr(vals$saved_data[[input$data_nbX]],"nb")[input$nb_models]<-NULL
  })


  ###
  observeEvent(input$tools_savenb,{
    if(input$tools_savenb %% 2){
      vals$hand_save<-"Save NB model in"
      vals$hand_save2<-column(12,fluidRow(em(input$data_nbX,style="color:gray"),strong("::"),em("NB-Attribute",style="color:gray"),strong("::")
      ))
      vals$hand_save3<-NULL
      module_ui_dc("datacreate")
      mod_downcenter <- callModule(module_server_dc,"datacreate", vals=vals,dataX=input$data_nbX)

      }
  })
  observeEvent(input$nb_create_errors_train,{
    vals$hand_save<-"Create Datalist: NB training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    module_ui_dc("datacreate")
    mod_downcenter <- callModule(module_server_dc,"datacreate", vals=vals,dataX=input$data_nbX)
  })
  observeEvent(input$nb_create_predictions,{
    vals$hand_save<- "Create Datalist: NB predictions"
    vals$hand_save2<-"Posterior probabilities will be saved as Data-Attribute,and final classifications as Factor-Attribute"
    vals$hand_save3<-NULL
    module_ui_dc("datacreate")
    mod_downcenter <- callModule(module_server_dc,"datacreate", vals=vals,dataX=input$data_nbX)
  })




  output$savenb_button<-renderUI({
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_savenb"),icon("fas fa-save")),"Save the nb model in the training Datalist (X)")
    )
  })


}
