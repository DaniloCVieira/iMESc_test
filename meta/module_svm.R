module_name<-"SVM"
paste("Create Datalist:",module_name, 'training errors -obs')
paste("Save",module_name, 'model in')
paste("Create Datalist:",module_name, 'predictions')



svmGrid <- function(x, y, len = NULL, search = "grid") {
  library(kernlab)
  ## This produces low, middle and high values for sigma
  ## (i.e. a vector with 3 elements).
  sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
  ## To use grid search:
  if(search == "grid") {
    out <- expand.grid(sigma = mean(as.vector(sigmas[-2])),
                       C = 2 ^((1:len) - 3))
  } else {
    ## For random search, define ranges for the parameters then
    ## generate random values for them
    rng <- extendrange(log(sigmas), f = .75)
    out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])),
                      C = 2^runif(len, min = -5, max = 8))
  }
  out
}
plot_dens_svm<-function(m,var="Chl",newcolhabs, palette="viridis", lwd=1, xlab="Variables"){
  opar<-par(no.readonly=TRUE)
  layout(matrix(c(1,2), nrow=1), widths = c("70","30"))
  colors<-getcolhabs(newcolhabs,palette,length(m$levels))
  tables<-m$finalModel$tables[[var]]
  if(lapply(m$finalModel$tables, class)[[1]]=="table"){
    par(mar=c(5,5,5,3), las=1)
    colors<-getcolhabs(newcolhabs,palette,length(levels(m$trainingData[[var]])))
    spineplot(tables, col=colors, las=1,ylab="", xlab=paste("Class:",attr(m,"supervisor")),off=1, axes =T,yaxlabels=F)
    par(mar=c(0,0,5,0), xpd=T)
    plot.new()
    legend(0,1, legend=levels(m$trainingData[[var]]), pch=15, col=colors, bty="n")
    on.exit(par(opar),add=TRUE,after=FALSE)

  } else{

    par(mar=c(5,5,5,0))
    res<-lapply(tables, function(x)
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
    plot(m$finalModel$tables[[var]][[1]], xlim=c(minx,maxx), ylim=c(miny,maxy), type="n", main=var)
    i=1
    for(i in 1:length(tables)){
      lines(tables[[i]], col=colors[i], lwd=lwd)
    }

    par(mar=c(0,0,5,0), xpd=T)
    plot.new()
    legend(0,1, legend=m$levels, lty=1, col=colors, bty="n")
    on.exit(par(opar),add=TRUE,after=FALSE)
  }


}


module_ui_svm <- function(id){

  ns <- NS(id)
  tagList(
    includeCSS("meta/styles_mod.css"),
    column(12,style="background: white",
      uiOutput(ns("svm_inputs")),
      uiOutput(ns("war_svm")),
      column(12,
             tabsetPanel(id = ns("svm_tab"),
                         tabPanel(strong("1. Training"),value='svm_tab1',
                                  uiOutput(ns("svm_params")))

             )
      )

    )
  )

}

# Server
module_server_svm <- function (input, output, session,vals,df_colors,newcolhabs ){
  ns <- session$ns

  insert_svm_resutls<-reactiveValues(df=F)




  output$predict_svm<-renderUI({
    column(12,style="background: white", uiOutput(ns("svm_pred_type")),
           tabsetPanel(id="predsvm_tab",
                       tabPanel(
                         strong("3.1. Results"), value="predsvm_tab01",
                         fluidRow(style="background: white",
                                  uiOutput(ns("svm_tab3_1")))),
                       tabPanel(
                         strong("3.2. Errors"),
                         fluidRow(style="background: white",
                                  uiOutput(ns("svm_tab3_2")))
                       )

           )
    )
  })

  output$svm_pred_type<-renderUI({
    choices=if(length(attr(vals$svm_results,'test'))==1){c("Datalist")
    } else{c("Partition","Datalist")}
    div(inline(radioButtons(ns("svmpred_which"),"New data (X):",choices=choices,inline=T, width="200px")), inline(uiOutput(ns("datalist_pred_svm"))))
  })

  output$datalist_pred_svm<-renderUI({
    req(input$svmpred_which =='Datalist')
    div(
      pickerInput(ns("predsvm_new"),"Datalist",names(vals$saved_data[getnewdatasvm()]), width = '300px')
    )
  })



  output$confusion_svm_pred<-renderUI({
    conf<-get_cm_pred()
    column(12,
           renderPlot({
             res<-plotCM(conf/sum(conf)*100, input$svmpalette_pred,  newcolhabs=vals$newcolhabs)
             vals$svm_cm_pred<-res
             res
           }))

  })
  output$confusion_svm2_pred<-renderPrint({
    confusionMatrix(get_cm_pred())
  })
  output$svm_tab3_2<-renderUI({
    sup_test<-attr(vals$svm_results,"supervisor")
    supname<-paste0("Observed Y [",sup_test,"]")
    sidebarLayout(
      sidebarPanel(
        fluidRow(class="map_control_style",style="color: #05668D",
                 div(
                   span("+",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
                        span(supname),
                        inline(pickerInput(ns("predsvm_newY"),
                                           NULL,names(vals$saved_data[getobssvm()]), width="200px"))
                   )
                 ),
                 div(
                   span(tipify(icon("fas fa-question-circle"),"Confusion Matrix Palette"), "+ Palette",
                        inline(
                          pickerInput(inputId = ns("svmpalette_pred"),
                                      label = NULL,
                                      choices =     vals$colors_img$val,
                                      choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"), width="75px")
                        )
                   )
                 ),
                 div(tipify(actionLink(ns("downp_cmsvm_pred"), span("+ Donwload",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"Download plot"))

        )
      ),
      mainPanel(
        tags$style(
          paste(paste0("#",ns('svm_tab_errors0_test')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
        ),
        tags$style(
          paste0("#",ns('svm_tab_errors0_test')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
        ),
        div(
          p(strong("Prediction stats:")),
          inline(DT::dataTableOutput(ns('svm_tab_errors0_test')))
        ),
        div(
          p(strong("Confusion matrix:")),
          uiOutput(ns("confusion_svm_pred")),
          verbatimTextOutput(ns("confusion_svm2_pred"))
        )
      )
    )
  })
  output$svm_tab_errors0_test<-DT::renderDataTable({
    table<-pred_test_svm()
    table<-round(table,3)
    rownames(table)<-NULL
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))
  })
  output$svm_tab3_1<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('svm_create_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                       ),
                       "Create a datalist with the svm predictions", options=list(container="body")
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         ns('down_svm_tab3_1'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                       ),
                       "Download Table", options=list(container="body")
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('svmtab_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('svmtab_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          inline(DT::dataTableOutput(ns('svmtab_pred')))
        )
      )
    )
  })
  output$svmtab_pred<-DT::renderDataTable({
    table<-vals$svmtab_pred<-pred_svm()
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'))

  }, rownames = TRUE,class ='compact cell-border')


  output$svm_inputs <- renderUI({
    if(is.null(vals$cur_svm_method)){vals$cur_svm_method<-"svmLinear"}
    if(is.null(vals$cur_data_svmY)){vals$cur_data_svmY<-1}
    if(is.null(vals$cur_svm_type)){vals$cur_svm_type<-"Classification"}
    #req(length(vals$saved_data)>0)
    div(style="color: #05668D;",
             if(input$svm_tab=="svm_tab1"){
               div(
                 div(class='well3',
                     strong("Type:"),inline(radioButtons(ns('svm_type'),NULL,choices=c("Classification","Regresion"),inline =T, selected=vals$cur_svm_type))
                 ),
                 div(class="well3",
                     span(strong("X:"),
                          inline(uiOutput(ns('data_svmX_out'))),
                          inline(
                            div(
                              div("Method:"),
                              pickerInput(ns("svm_method"),NULL,choices=c("svmLinear","svmRadial"), width="150px", selected=vals$cur_svm_method)
                            )
                          )
                     )
                 ),
                 div(class="well3",
                     span(strong("Y:"),
                          inline(
                            uiOutput(ns("data_svmY_out"))
                          ),
                          "::",
                          inline(uiOutput(ns('svm_Y'))),

                          inline(uiOutput(ns("svm_partition"))),
                          "::",
                          inline(uiOutput(ns("svm_test_ref")))
                     )
                 )


               )
             } else {
               column(12,
                      inline( uiOutput(ns('data_svmX_out'))),
                      inline(uiOutput(ns("svm_results_menu"))),
                      inline(uiOutput(ns("savesvm_button"))),
                      inline(uiOutput(ns("rmsvm_button"))),
               )
             }
    )
  })
  output$data_svmY_out<-renderUI({
    div(
      div("Datalist:"),
      div(pickerInput(ns("data_svmY"),NULL,choices =  names(vals$saved_data), width="150px", selected=vals$cur_data_svmY))
    )
  })
  output$svm_Y <- renderUI({
    req(input$data_svmY)
    req(input$svm_type)

    if(is.null(vals$cur_svm_sup)){vals$cur_svm_sup<-1}
    data <- vals$saved_data[[input$data_svmY]]
    labels <- attr(data,"factors")
    choices<-if(input$svm_type=="Classification"){rev(colnames(labels))} else{
      colnames(data)
    }

    div(well="class2",
        div("Variable"),
        div(tipify(pickerInput(
          ns("svm_sup"),
          NULL,
          choices =choices ,
          width="150px", selected=vals$cur_svm_sup
        ),"The response vector"))
    )

  })
  output$svm_partition<-renderUI({
    req(input$data_svmX)
    if(is.null(vals$cur_svm_test_partition)){vals$cur_svm_test_partition<-1}
    div(
      div(span("Partition:",tiphelp("choose a factor as reference for the partition"))),
      div(pickerInput(ns("svm_test_partition"),NULL, choices=c("None", colnames(attr(vals$saved_data[[input$data_svmY]],"factors"))), width="150px", selected=vals$cur_svm_test_partition))
    )
  })
  output$svm_test_ref<-renderUI({
    req(input$svm_test_partition!="None")
    req(input$data_svmY)
    if(is.null(vals$cur_testdata_svm)){vals$cur_testdata_svm<-1}
    fac<-attr(vals$saved_data[[input$data_svmY]],"factors")[,input$svm_test_partition]
    choices<-levels(fac)
    div(
      div("::",span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions"))),
      pickerInput(ns("testdata_svm"),NULL, choices=choices, width="200px", selected=vals$cur_testdata_svm)
    )
  })
  output$data_svmX_out<-renderUI({
    if(is.null(vals$cur_data)){vals$cur_data<-1}
    div(
      div("~ Training Datalist:"),
      pickerInput(ns("data_svmX"),NULL,choices =names(vals$saved_data),width="150px", selected=vals$cur_data)
    )
  })

  output$svm_params<- renderUI({
    column(12,class="well2",
           div(class="map_control_style",style="color: #05668D; margin-top: 20px",
               div(
                 div(
                   tipify(icon("fas fa-question-circle"),"Tuning search", options = list(container="body")),
                   "+ search:",
                   inline(
                     pickerInput(ns("svm_search"), NULL, choices = c("grid","random","user-defined"), width="110px")
                   )
                 ),
                 uiOutput(ns("svm_search_length")),
               ),
               uiOutput(ns("svm_grid"))
               ,
               div(
                 tipify(icon("fas fa-question-circle"),textseed(), options = list(container="body")),
                 "+ seed:",
                 inline(
                   numericInput(ns("seedsvm"), NULL, value = NULL, width="122px")
                 )
               ),

               div(popify(icon("fas fa-question-circle"),NULL,
                          HTML(paste0(
                            div(HTML(paste0(strong("repeatedcv:")," repeated k-fold cross-validation;"))),
                            div(HTML(paste0(strong("boot:")," bootstraping;"))),

                            div(HTML(paste0(strong("LOOCV:")," Leave one out;"))),
                            div(HTML(paste0(strong("LGOCV:")," Leave-group out")))

                          )), options=list(container="body")

               ),
               "+ Resampling method:",
               pickerInput(ns("svm_res_method"),NULL, choices=list("repeatedcv","boot","LOOCV","LGOCV"), width = "100px")
               ),
               uiOutput(ns("svm_resampling"))
           ),

           column(12,style='white-space: normal;',
                  uiOutput(ns("svm_war"))
           ),

           column(12, align = "center",
                  popify(actionButton(ns("trainsvm"), h4(img(src=svm_icon,height='20',width='20'),"train Support Vector Machine",icon("fas fa-arrow-circle-right")), style = "background:  #05668D; color: white"),NULL,"Click to run")
           ),

    )
  })
  output$svm_grid<-renderUI({
    req(input$svm_search=="user-defined")
    div(
      div(
        tipify(icon("fas fa-question-circle",style="color: gray"),"one or multiple numeric values for the cost regularization parameter. This parameter controls the smoothness of the fitted function, essentially higher values for C lead to less smooth functions (if a vector, comma delimited)", options=list(container="body")),
        "+ Cost",
        textInput(ns('cost_svm'), NULL, value ='1', width="200px")),
      if(input$svm_method=="svmRadial") {
        div(
          tipify(icon("fas fa-question-circle",style="color: gray"),"one or multiple numeric values for bandwidth of kernel function. If the sigma value is very small, then the decision boundary is highly non-linear. If a vector must be comma delimited between 0 and 1)", options=list(container="body")),
          "+ Sigma",
          textInput(ns('sigma_svm'), NULL, value ='1', width="200px")

        )
      }
    )
  })
  output$svm_search_length<-renderUI({
    req(input$svm_search!='user-defined')
    if(is.null(vals$cur_svm_tuneLength)){vals$cur_svm_tuneLength<-5}

    div(
      tipify(icon("fas fa-question-circle"),"The maximum number of mtry- combinations that will be generated", options = list(container="body")),
      "+ tuneLength:",
      inline(
        numericInput(ns("svm_tuneLength"), NULL, value = vals$cur_svm_tuneLength, width="82px")
      )
    )
  })
  output$svm_resampling<-renderUI({
    div(
      inline(
        if(input$svm_res_method=='cv'|input$svm_res_method=='repeatedcv')
        {
          div(

            tipify(icon("fas fa-question-circle"),"number of folds", options=list(container="body")),"+ cv:",
            numericInput(ns("cvsvm"), NULL, value = 5,width="140px")

          )
        }
      ),
      div(
        inline(
          if(input$svm_res_method=='repeatedcv')
          {
            inline(
              div(tipify(icon("fas fa-question-circle"),"the number of complete sets of folds to compute", options=list(container="body")),
                  "+ repeats:",
                  numericInput(ns("repeatssvm"),NULL, value = 1,width="109px")
              )
            )

          }
        )
      ),
      inline(
        if(input$svm_res_method=='boot'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the number of resampling iterations", options=list(container="body")),
              "+ number:",
              numericInput(ns("cvsvm"), NULL, value = 5,width="100px")

            )
          )
        }
      ),
      inline(
        if(input$svm_res_method=='LGOCV'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the training percentage", options=list(container="body")),
              "+ percentage:",
              numericInput(ns("pleavesvm"), NULL, value = 10,width="100px")
            )
          )
        }
      )
    )
  })


  output$svm_results_menu<-renderUI({
    if(is.null(vals$cur_svm_models)){vals$cur_svm_models<-1}
    div(pickerInput(ns("svm_models"),strong("svm results:", tiphelp("Random Forest Results. Click to select svm results saved in the Training Datalist (X).")), choices= names(attr(vals$saved_data[[input$data_svmX]],"svm")), width="200px", selected = vals$cur_svm_models)
    )
  })
  output$rmsvm_button<-renderUI({
    req(input$svm_models!="new svm (unsaved)")
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_rmsvm"), icon("far fa-trash-alt")), "Remove the svm model from the training Datalist (X)")
    )

  })
  output$svm_tab_2_1<-renderUI({
    m<-vals$svm_results
    div(
      div(popify(downloadButton(ns("down_svm_results"),style = "button_active"),NULL,"download model as rds file",
                 options=list(container="body"))),
      renderPrint(m)
    )
  })
  output$down_svm_results <- {
    downloadHandler(
      filename = function() {
        paste0("SVM","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$nb_results,file)
      })
  }
  observeEvent(input$svm_tab2,{
    vals$svm_tab2<-input$svm_tab2
  })

  output$results_svm<-renderUI({
    if(is.null(vals$svm_tab2)){vals$svm_tab2<-'svm_tab2_1'}
    #saveRDS(vals$svm_results,"svm.rds")
    req(length(vals$svm_results))
    m<-vals$svm_results
    #m<-readRDS("svm.rds")
    Y<-which(colnames(m$trainingData)==".outcome")
    nvars<-ncol(m$trainingData[,-Y])
    req(length(nvars)>0)
    val<-if(nvars>10){10} else{
      nvars
    }
    column(12,
           div(
             p(
               strong("Model:"),em(getmodel_svm())
             )
           ),
           tabsetPanel(id=ns("svm_tab2"),selected=vals$svm_tab2,
                       tabPanel("1. Summary",value="svm_tab2_1",
                                uiOutput(ns("svm_tab_2_1"))
                       ),
                       tabPanel("2. Training error",value="svm_tab2_2",
                                uiOutput(ns("svm_tab_2_2"))
                       ),
                       tabPanel("3. Variable importance",value="svm_tab2_3",
                                sidebarLayout(
                                  sidebarPanel(
                                    fluidRow(
                                      class="map_control_style",style="color: #05668D",
                                      div("+ Number of variables:",
                                          numericInput(ns("varImp_n_svm"),NULL, value=val, step=1, max=nvars, width="60px")
                                      ),
                                      div(
                                        tipify(
                                          actionLink(
                                            ns('svm_varImp'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                                          ),
                                          "Download Table (importances)", options=list(container="body")
                                        )
                                      ),
                                      div(
                                        tipify(
                                          actionLink(
                                            ns('svm_varImp_plot'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
                                          ),
                                          "Download plot", options=list(container="body")
                                        )
                                      )
                                    )
                                  ),
                                  mainPanel(  uiOutput(ns("svm_tab_2_3")))
                                )

                       ),
                       tabPanel("4. Confusion matrix",value="svm_tab2_4",
                                uiOutput(ns("svm_tab_2_4"))
                       )
           )

    )
  })

  output$svm_tab_2_2<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('svm_create_errors_train'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                       ),
                       "Create a datalist with the svm training errors", options=list(container="body")
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         ns('svm_down_errors_train'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                       ),
                       "Download Table", options=list(container="body")
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('svm_tab_errors0_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('svm_tab_errors0_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          tags$style(
            paste(paste0("#",ns('svm_tab_errors_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('svm_tab_errors_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          div(
            p(strong("Global:")),
            inline(DT::dataTableOutput(ns('svm_tab_errors0_train')))
          ),

          div(
            p(strong("Observations:")),
            inline(DT::dataTableOutput(ns('svm_tab_errors_train')))
          )



        )
      )
    )
  })
  output$svm_tab_errors0_train<-DT::renderDataTable({
    m<-vals$svm_results
    table<- m$results[rownames(m$bestTune),]
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })
  output$svm_tab_errors_train<-DT::renderDataTable({
    m<-vals$svm_results
    table<-accu_rf_class(m)
    vals$svm_down_errors_train<-table
    table
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })
  output$svm_tab_2_3<-renderUI({

    renderPlot({
      m<-vals$svm_results

      X <- varImp(m)
      vals$svm_varImp <-data.frame(X$importance)
      vals$svm_varImp_plot<-res<-plot(X,input$varImp_n_svm)
      res
    })
  })
  output$svm_tab_2_4<-renderUI({

    # validate(need(is.factor( vals$svm_results$finalModel$y), "Confusion matrices are only valid for classification (factor) models."))
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          class="map_control_style",style="color: #05668D",
          div(
            span(
              "+ Type: ",
              pickerInput(inputId = ns("svm_cm_type"),
                          label = NULL,
                          choices = c("Resampling","Optimal Model"),

                          width="100px"
              )
            )
          ),
          div(
            span(
              "+ Palette",
              pickerInput(inputId = ns("svmpalette"),
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
            popify(actionLink(ns("downp_cmsvm"), span("+ Download",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"download CM plot")
          ),
          div(
            popify(actionLink(ns("dowcenter_cmsvm"), span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style  = "button_active"),NULL,"download CM plot")
          )

        )

      ),
      mainPanel(
        plotOutput(ns("confusion_svm")),
        verbatimTextOutput(ns("confusion_svm2"))
      )
    )


  })

  observeEvent(input$dowcenter_cmsvm,{
    vals$hand_down<-"svm_cm"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  output$confusion_svm <- renderPlot({
    m<-vals$svm_results
    if(input$svm_cm_type=="Resampling"){
      res<-plotCM(m, input$svmpalette,  newcolhabs=vals$newcolhabs)
    } else{
      cm<-table(predict(vals$svm_results),vals$svm_results$trainingData[,'.outcome'])
      res<-plotCM(cm,input$svmpalette,vals$newcolhabs)
    }
    vals$cm_svm<-res
    res
  })
  output$confusion_svm2<-renderPrint({
    res<-if(input$svm_cm_type=="Resampling"){
      confusionMatrix(vals$svm_results$pred$pred,vals$svm_results$pred$obs)
    } else{
      confusionMatrix(predict(vals$svm_results),vals$svm_results$trainingData[,'.outcome'])
    }
    vals$svm_cm<-res$table
    res
  })



  output$savesvm_button<-renderUI({
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_savesvm"), icon("fas fa-save")), "Save the svm model in the training Datalist (X)")
    )
  })

  getnewdatasvm<-reactive({
    datalist<-vals$saved_data

    datalist_comp<-lapply(
      vals$saved_data, function (x) {
        colnames(x)<-gsub(" ",".", colnames(x))
        x
      }

    )

    datalist_comp<-vals$saved_data




    m<-vals$svm_results
    Y<-which(colnames(m$trainingData)==".outcome")
    res0<-unlist(
      lapply(datalist_comp, function (x){
        res<-colnames(x)%in%colnames(m$trainingData[-Y])
        sum(res)==ncol(m$trainingData[-Y])
      })
    )


    names(datalist[res0==T])
  })
  get_supervisor_svm <- reactive({
    m<-vals$svm_results
    att<-if(m$modelType=="Classification")
    {
      data <- vals$saved_data[[input$data_svmY]]
      labels <- attr(data,"factors")
      labels[input$svm_sup]
    } else{
      data <- vals$saved_data[[input$data_svmY]]
      data[input$svm_sup]
    }

  })
  get_cm_pred<-reactive({
    obs<-get_svm_prederrors()$obs
    pred<-get_svm_prederrors()$pred
    conf<-table(obs, pred)
    conf
  })
  pred_test_svm<-reactive({
    obs<-get_svm_prederrors()$obs
    pred<-get_svm_prederrors()$pred
    table<-data.frame(t(postResample(pred,obs)))
    table
  })
  get_svm_prederrors<-reactive({
    m<-vals$svm_results
    pred<-pred_svm()[,1]
    # x_t<-attr(m,"test")
    obs<-if(input$svmpred_which=="Partition"){
      attr(m,"sup_test")} else{
        attr(vals$saved_data[[input$predsvm_newY]],"factors")[,attr(vals$svm_results,"supervisor")]
      }
    return(
      list(
        obs=obs,
        pred=pred
      )
    )
  })
  getobssvm<-reactive({
    sup_test<-attr(vals$svm_results,"supervisor")
    datalist<-vals$saved_data
    datalist=lapply(datalist,function(x) attr(x,"factors"))
    m<-vals$svm_results
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })
  test_svm<-reactive({
    req(input$svmpred_which)


    if(input$svmpred_which=="Datalist"){
      req(input$predsvm_new)
      pred_tab<-vals$saved_data[[input$predsvm_new]]
    } else   if(input$svmpred_which=="Partition"){
      pred_tab<-attr(vals$svm_results,"test")
    }
    colnames(pred_tab)<-gsub(" ",".", colnames(pred_tab))
    pred_tab

  })
  pred_svm<-reactive({
    validate(need(!anyNA(test_svm()),"NAs not allowed in the prediction Datalist"))
    m<-vals$svm_results


    svm_pred <- predict(m$finalModel,newdata = test_svm())
    res<-data.frame(Predictions= svm_pred)
    rownames(res)<-rownames(test_svm())
    #colnames(res)<-attr(vals$svm_results,"supervisor")

    # attr(res,"obs")<-test_svm()
    res



  })
  get_parts_svm<-reactive({
    req(input$data_svmY)
    req(input$testdata_svm)
    partition<- attr(vals$saved_data[[input$data_svmY]],"factors")[, input$svm_test_partition]
    test<-which(partition==input$testdata_svm)
    train<-which(partition!=input$testdata_svm)
    list(train=train,test=test)
  })
  getdata_svmX<-reactive({
    req(input$data_svmX)
    data=vals$saved_data[[input$data_svmX]]
    data
  })
  getdata_svmY<-reactive({
    req(input$data_svmY)
    req(input$svm_sup)

    data <- vals$saved_data[[input$data_svmY]]
    if(input$svm_type=="Classification"){
      labels <- attr(data,"factors")
      labels<-labels[,input$svm_sup]
      labels
    } else{
      data<-data[,input$svm_sup]
      data
    }

  })
  getmodel_svm<-reactive({
    m<-vals$svm_results
    att<-if(m$modelType=="Classification"){
      "Factor-Attribute"
    } else{ "Data-Attribute"}
    attri<-attr(m,'inputs')
    res<-c(attri$Ydatalist,
           att,
           attri$Y,
           attri$Xdatalist)
    model<-  paste( paste0(res[1],"::",res[2],"::",res[3]),"~",paste0(res[4]))
    model

  })

  observe({
    req(input$data_svmX)
    req(isFALSE(insert_svm_resutls$df))
    req(length(vals$svm_unsaved)>0|length(attr(vals$saved_data[[input$data_svmX]],"svm"))>0)
    insertTab('svm_tab',target='svm_tab1',
              tabPanel(strong("2. Results"),value='svm_tab2',
                       uiOutput(ns("results_svm"))),

    )
    insertTab('svm_tab',target='svm_tab2',
              tabPanel(strong("3. Predict"),value='svm_tab3',
                       uiOutput(ns("predict_svm")))
    )
    insert_svm_resutls$df<-T
  })
  observeEvent(input$predsvm_new,{
    vals$predsvm_new<-input$predsvm_new
  })
  observeEvent(input$svmpred_which,{
    vals$svmpred_which<-input$svmpred_which
  })
  observeEvent(input$svm_down_errors_train,{
    vals$hand_down<-"SVM - training errors (observations)"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$svm_varImp,{
    vals$hand_down<-"SVM - variable importance"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$down_svm_tab3_1,{
    vals$hand_down<-"SVM - predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$data_svmY,{
    vals$cur_data_svmY<-input$data_svmY
  })
  observeEvent(input$svm_type,{
    vals$cur_svm_type<-input$svm_type
  })
  observeEvent(input$svm_tuneLength,{
    vals$cur_svm_tuneLength<-input$svm_tuneLength
  })
  observeEvent(input$svm_method,{
    vals$cur_svm_method<-input$svm_method
  })
  observeEvent(input$svm_sup,{
    vals$cur_svm_sup<-input$svm_sup
  })
  observeEvent(input$data_svmX,{
    vals$cur_data<-input$data_svmX
  })
  observeEvent(input$svm_test_partition,{
    vals$cur_svm_test_partition<-input$svm_test_partition
  })
  observeEvent(input$testdata_svm,{
    vals$cur_testdata_svm<-input$testdata_svm
  })
  observeEvent(input$svm_models,{
    vals$cur_svm_models<-input$svm_models
  })
  observeEvent(input$trainsvm,{
    x<-x_o<-getdata_svmX()
    y<-y_o<-getdata_svmY()

    output$svm_war<-renderUI({
      column(12,style="color: red",align="center",
             if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
             if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
      )
    })
    validate(need(anyNA(x)==F,"NAs not allowed in X"))
    validate(need(anyNA(y)==F,"NAs not allowed in Y"))
    req(input$svm_test_partition)
    if(input$svm_test_partition!="None"){
      parts<-get_parts_svm()
      train<-parts$train
      test<-parts$test
      x<-getdata_svmX()[train,]
      y<-getdata_svmY()[train]
    }
    #readrd(y,"y.rds")
    #saveRDS(x,"x.rds")
    #saveRDS(reactiveValuesToList(input),"input.rds")
    #y<-readRDS("y.rds")
    #x<-readRDS("x.rds")
    #input<-readRDS("input.rds")
    if(input$svm_search=="user-defined"){
      nb_search<-"grid"
      cost_svm<-as.numeric(unlist(strsplit(input$cost_svm,",")))
      if(input$svm_method!="svmLinear"){

        sigma_svm<-as.numeric(unlist(strsplit(input$sigma_svm,",")))
        grid <- expand.grid(C=c(cost_svm),sigma=c(sigma_svm))
      } else{
        grid <- expand.grid(C=c(cost_svm))
      }


    } else{
      grid<-svmGrid(x,y, len=input$svm_tuneLength, input$svm_search)
      if(input$svm_method=="svmLinear"){
        grid<-grid["C"]
      }
    }

    seed<-if (!is.na(input$seedsvm)) { input$seedsvm} else{
      NULL
    }
    svm_search<-input$svm_search

    colnames(x)<-gsub(" ",".", colnames(x))
    if (!is.na(input$seedsvm)) {set.seed(input$seedsvm)}
    withProgress(message = "Running Support Vector Machine ...",
                 min = 1,
                 max = 1,
                 {
                   svm<-train(x,y,input$svm_method,

                              trControl=trainControl(

                                method = input$svm_res_method,
                                number = input$cvsvm,
                                repeats = input$repeatssvm,
                                p=input$pleavesvm/100,
                                savePredictions = "final"

                              ),
                              tuneGrid=grid
                   )
                   attr(svm,"test_partition")<-paste("Test data:",input$svm_test_partition,"::",input$testdata_svm)
                   attr(svm,"Y")<-paste(input$data_svmY,"::",input$svm_sup)
                   attr(svm,"Datalist")<-paste(input$data_svmX)

                   vals$svm_unsaved<-svm

                   if(input$svm_test_partition!="None"){
                     attr(vals$svm_unsaved,'test')<-x_o[test,]
                     attr(vals$svm_unsaved,"sup_test")<-y_o[test]
                   } else{ attr(vals$svm_unsaved,'test')<-c("None")}
                   vals$bag_svm<-T
                   attr(vals$svm_unsaved,"supervisor")<-input$svm_sup
                   attr(vals$svm_unsaved,"inputs")<-list(
                     Ydatalist=input$data_svmY,
                     Y=input$svm_sup,
                     Xdatalist=input$data_svmX
                   )
                   updateTabsetPanel(session,"svm_tab","svm_tab2")
                 })
    beep(10)
    attr(vals$saved_data[[input$data_svmX]],"svm")[['new svm (unsaved)']]<-vals$svm_unsaved
    vals$cur_svm_models<-"new svm (unsaved)"
    vals$bag_svm<-T
    #saveRDS(vals$svm_unsaved,"svm.rds")
  })
  observeEvent(input$downp_cmsvm,{
    vals$hand_plot<-"SVM - Confusion Matrix"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(input$downp_cmsvm_pred,{
    vals$hand_plot<-"SVM - Confusion Matrix (predictions)"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(input$svm_varImp_plot,{
    vals$hand_plot<-"SVM - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_svm_dens,{
    vals$hand_plot<-"SVM - DM plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observe({
    req(input$svm_models)
    if(input$svm_models=="new svm (unsaved)"){
      vals$svm_results<-vals$svm_unsaved } else{
        vals$svm_results<-attr(vals$saved_data[[input$data_svmX]],"svm")[[input$svm_models]][[1]]
      }
  })
  observeEvent(input$svm_models,{
    vals$cur_svm<-input$svm_models
  })
  observeEvent(input$tools_rmsvm,{
    attr(vals$saved_data[[input$data_svmX]],"svm")[input$svm_models]<-NULL
  })
  observeEvent(input$tools_savesvm,{
    if(input$tools_savesvm %% 2){
      vals$hand_save<-"Save SVM model in"
      vals$hand_save2<-column(12,fluidRow(em(input$data_svmX, style="color:gray"),strong("::"), em("svm-Attribute", style="color:gray"),strong("::")
      ))
      vals$hand_save3<-NULL
      module_ui_dc("datacreate")
      mod_downcenter <- callModule(module_server_dc, input$data_svmX,  vals=vals,dataX=input$data_svmX)

    }
  })
  observeEvent(input$svm_create_errors_train,{
    vals$hand_save<-"Create Datalist: SVM training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    mod_downcenter <- callModule(module_server_dc, input$data_svmX,  vals=vals,dataX=input$data_svmX)
  })
  observeEvent(input$svm_create_predictions,{
    vals$hand_save<- "Create Datalist: SVM predictions"
    vals$hand_save2<-"Posterior probabilities will be saved as Data-Attribute, and final classifications as Factor-Attribute"
    vals$hand_save3<-NULL
    module_ui_dc("datacreate")
    mod_downcenter <- callModule(module_server_dc, input$data_svmX,  vals=vals,dataX=input$data_svmX)
  })

}
