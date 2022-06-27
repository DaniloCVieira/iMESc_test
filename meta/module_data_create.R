
module_ui_dc<- function(id){
  ns <- NS(id)


}


# Server
module_server_dc <- function (input, output, session,vals, dataX ){
  ns <- session$ns
  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  newname<-reactiveValues(df=0)
  output$data_create<-renderUI({
    req(newname$df!=0)
    data_store$df<-F
    req(input$hand_save=="create")
    res<-textInput(ns("newdatalist"), NULL, newname$df)
    data_store$df<-T
    res
  })

  output$data_over<-renderUI({
    data_overwritte$df<-F
    req(input$hand_save=="over")
    res<-selectInput(ns("over_datalist"), NULL,choices=c(names(vals$saved_data)))
    data_overwritte$df<-T
    res
  })

  get_newname<-reactive({
    newname$df<-switch(
      vals$hand_save,
      ##RF
      "Save RF model in"= {name_saverf()},
      "Create Datalist: RF frequent interactions"={ name_saverf_int()},
      "Create Datalist: RF top variables"={ name_saverf_top()},
      "Create Datalist: RF prediction errors"= { name_saverf_predErr()},
      "Create Datalist: RF predictions"= { name_saverf_pred()},
      "Create Datalist: RF training errors"= { name_saverf_err()},
      "Save NB model in"= { name_save_nb()},
      "Create Datalist: NB training errors -obs"={ name_nb_train_errors()},
      "Create Datalist: NB predictions"={ name_nb_pred()},
      "Save SVM model in"= { name_save_svm()},
      "Create Datalist: SVM training errors -obs"={name_svm_train_errors()},
      "Create Datalist: SVM predictions"={ name_svm_pred()}



    )})



  ## action functions
  observeEvent( input$data_confirm,{
    removeModal()
    switch(
      vals$hand_save,
      "Create Datalist: RF top variables"= {datalistrf()},
      "Create Datalist: RF frequent interactions"= {datalistrfinter()},
      "Create Datalist: RF prediction errors"= {datalistrf_prederrors()},
      "Create Datalist: RF predictions"= {datalistrf_predicions()},
      "Create Datalist: RF training errors"= {datalistrferrors()},
      "Save RF model in"= {saverf()},
      "Save NB model in"= {savenb()},
      "Create Datalist: NB training errors -obs"=nb_create_training_errors(),
      "Create Datalist: NB predictions"={nb_create_pred()},


      "Save SVM model in"= {savesvm()},
      "Create Datalist: SVM training errors -obs"=svm_create_training_errors(),
      "Create Datalist: SVM predictions"={svm_create_pred()},

    )
  })



  ## RF
  saverf<-reactive({
    req(vals$cur_rf_models=='new rf (unsaved)')
    temp<-vals$RF_results
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$newdatalist
      attr(vals$saved_data[[dataX]],"rf")[[input$newdatalist]]<-c(temp,attr(vals$saved_data[[dataX]],"rf")[[input$newdatalist]])
      cur<-input$newdatalist
    } else{
      temp<-list(temp)
      names(temp)<-input$over_datalist
      attr(vals$saved_data[[dataX]],"rf")[input$over_datalist]<-temp
      cur<-input$over_datalist
    }
    attr(vals$saved_data[[dataX]],"rf")[['new rf (unsaved)']]<-NULL
    vals$bag_rf<-F
    vals$cur_rf_models<-cur
    vals$cur_rf<-cur

  })
  savenb<-reactive({
    req(vals$cur_nb_models=='new nb (unsaved)')
    temp<-vals$nb_results
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$newdatalist
      attr(vals$saved_data[[dataX]],"nb")[[input$newdatalist]]<-c(temp,attr(vals$saved_data[[dataX]],"nb")[[input$newdatalist]])
      cur<-input$newdatalist
    } else{
      temp<-list(temp)
      names(temp)<-input$over_datalist
      attr(vals$saved_data[[dataX]],"nb")[input$over_datalist]<-temp
      cur<-input$over_datalist
    }
    attr(vals$saved_data[[dataX]],"nb")['new nb (unsaved)']<-NULL
    vals$bag_nb<-F
    vals$cur_nb_models<-cur
    vals$cur_nb<-cur
  })
  savesvm<-reactive({
    req(vals$cur_svm_models=='new svm (unsaved)')
    temp<-vals$svm_results
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$newdatalist
      attr(vals$saved_data[[dataX]],"svm")[[input$newdatalist]]<-c(temp,attr(vals$saved_data[[dataX]],"svm")[[input$newdatalist]])
      cur<-input$newdatalist
    } else{
      temp<-list(temp)
      names(temp)<-input$svm_over
      attr(vals$saved_data[[dataX]],"svm")[input$svm_over]<-temp
      cur<-input$svm_over
    }
    attr(vals$saved_data[[dataX]],"svm")['new svm (unsaved)']<-NULL
    vals$bag_svm<-F
    vals$cur_svm_models<-cur
    vals$cur_svm<-cur
  })
  datalistrf<-reactive({
    datao<-vals$saved_data[[dataX]]
    temp<- datao[,vals$rf_sigs$variable]
    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
      vals$cur_data<-input$newdatalist
    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
      vals$cur_data<-input$over_datalist
    }
  })
  datalistrfinter<-reactive({
    a<-vals$rf_interactions_frame[1:vals$rfinter_k2,"variable"]
    b<-as.character(vals$rf_interactions_frame[1:vals$rfinter_k2,"root_variable"])
    pic<-unique(c(a,b))

    temp<- vals$saved_data[[dataX]][, pic]
    temp<-data_migrate( vals$saved_data[[dataX]],temp,"newdata_rf")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
      vals$cur_data<-input$newdatalist
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
      vals$cur_data<-input$over_datalist
    }
  })
  datalistrf_prederrors<-reactive({
    temp<-vals$rf_prederrors
    datao<-if(vals$rfpred_which=="Datalist"){
      vals$saved_data[[vals$predrf_newY]]
    } else{ vals$saved_data[[dataX]]}

    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  datalistrf_predicions<-reactive({
    temp<-vals$rf_pred_out
    temp[,1]<-as.numeric( temp[,1])
    datao<-if(vals$rfpred_which=="Datalist"){
      vals$saved_data[[vals$predrf_new]]
    } else{ vals$saved_data[[dataX]]}
    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  datalistrferrors<-reactive({
    temp<-switch(vals$RF_results$modelType,
                 "Regression"=data.frame(accu_rf_reg_model(vals$RF_results)),
                 "Classification" =data.frame(accu_rf_class(vals$RF_results)))
    temp<-data_migrate( vals$saved_data[[dataX]],temp,"newdata_rf")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }
  })


  nb_create_training_errors<-reactive({
    temp<-vals$nb_down_errors_train
    temp<-data_migrate(vals$saved_data[[dataX]],temp,"newdatalist")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }
  })
  nb_create_pred<-reactive({
    temp<-vals$nbtab_pred[-1]
    datao<-if(vals$nbpred_which=="Datalist"){
      vals$saved_data[[vals$prednb_new]]
    } else{ vals$saved_data[[dataX]]}
    temp<-data_migrate(datao,temp,"newdatalist")
    attr(temp,"factors")<-cbind(attr(temp,"factors")[rownames(temp),],vals$nbtab_pred[1])

    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }
  })
  svm_create_training_errors<-reactive({
    temp<-vals$svm_down_errors_train
    temp<-data_migrate(vals$saved_data[[dataX]],temp,"newdatalist")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }
  })
  svm_create_pred<-reactive({
    temp<-vals$svmtab_pred[-1]
    datao<-if(vals$svmpred_which=="Datalist"){
      vals$saved_data[[vals$predsvm_new]]
    } else{ vals$saved_data[[dataX]]}
    temp<-data_migrate(datao,temp,"newdatalist")
    attr(temp,"factors")<-cbind(attr(temp,"factors")[rownames(temp),],vals$svmtab_pred[1])

    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }
  })

  ## NB
  ## names functions

  name_saverf<-reactive({
    name0<-paste0("RF (",attr(vals$RF_results,"Y"),'~',dataX,")")
    bag<-1
   # name0<-paste0("rf")
    name1<-paste(name0,bag)
    if(name1%in%names(attr(vals$saved_data[[dataX]],"rf"))){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(attr(vals$saved_data[[dataX]],"rf"))) break
      }}
    paste(name0,bag)
  })
  name_saverf_top<-reactive({
    bag<-1
    name0<-paste("RF_sigs")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("RF_sigs",bag)
  })
  name_saverf_int<-reactive({
    bag<-1
    name0<-paste("rf_inter")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste(name0,bag)
  })
  name_saverf_err<-reactive({

    bag<-1
    name0<-paste("RFmodel_errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("RFmodel_errors",bag)

  })
  name_saverf_predErr<-reactive({

    bag<-1
    name0<-paste("RFpred_errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("RFpred_errors",bag)

  })
  name_saverf_pred<-reactive({
    bag<-1
    name0<-paste("RF_predictions")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste(name0,bag)

  })

  name_save_nb<-reactive({
    bag<-1
    name0<-paste0("NB")
    name1<-paste(name0,bag)
    if(name1%in%names(attr(vals$saved_data[[dataX]],"nb"))){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(attr(vals$saved_data[[dataX]],"nb"))) break
      }}
    paste(name0,bag)
  })
  name_nb_train_errors<-reactive({
    bag<-1
    name0<-paste0("NB training errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  name_nb_pred<-reactive({
    bag<-1
    name0<-paste0("NB predictions")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })

  name_save_svm<-reactive({
    bag<-1
    name0<-paste0("SVM")
    name1<-paste(name0,bag)
    if(name1%in%names(attr(vals$saved_data[[dataX]],"svm"))){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(attr(vals$saved_data[[dataX]],"svm"))) break
      }}
    paste(name0,bag)
  })
  name_svm_train_errors<-reactive({
    bag<-1
    name0<-paste0("SVM training errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  name_svm_pred<-reactive({
    bag<-1
    name0<-paste0("SVM predictions")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })

  ## saving functions

   ## create functions



  ## svm
  ## names functions

  ## saving functions

  ## create functions









  ### modals
  output$databank_storage<-renderUI({
    newname$df<-0
    get_newname()
    column(12,
           fluidRow(
             column(12,p(strong("action:"),em("*",vals$hand_save,style="color: SeaGreen")), p(vals$hand_save2,style="color: gray")),
             column(12,vals$hand_save3),
             column(12,style='margin-top: 10px; margin-left: 10px',
                    splitLayout(cellWidths = c("30%","70%"),
                                radioButtons(ns("hand_save"),NULL,
                                             choiceNames= list(div(style="height: 50px","create"),
                                                               div(style="height: 50px","overwrite")),
                                             choiceValues=list('create',"over")),
                                column(12,div(style="height: 50px",
                                              withSpinner(type=8,color="SeaGreen",uiOutput(ns("data_create")))),
                                       div(style="height: 50px",
                                           withSpinner(type=8,color="SeaGreen",uiOutput(ns("data_over")))))
                    ))

           )
    )
  })
  output$save_confirm<-renderUI({
    req(isTRUE(data_store$df)|isTRUE(data_overwritte$df))
    actionButton(ns("data_confirm"),strong("confirm"))
  })

 showModal({


   modalDialog(
     withSpinner(type=8,color="SeaGreen",uiOutput(ns("databank_storage"))),
     title=strong('Databank storage',icon("fas fa-warehouse")),
     footer=column(12,
                   fluidRow(tipify(bsButton(ns("preview_button"),icon("fas fa-eye"), block=F),"Preview"), modalButton(strong("cancel")),
                            inline(uiOutput(ns("save_confirm")))
                   )
     ),
     size="l",
     easyClose = T
   )
 })

  }
