module_name<-"rf"
paste("Create Datalist:",module_name, 'training errors -obs')
paste("Save",module_name, 'model in')
paste("Create Datalist:",module_name, 'predictions')

getdown<-reactive({
  switch(vals$hand_down,
         "rfdepth"=data.frame(attr(vals$RF_results,"mindepth")[[2]]),
         "rfinter"=data.frame(rf_interactions_frame$df),
         "RF: predictions"={data.frame( vals$rf_pred_out)},
         "RF: ClassErrors"=data.frame(accu_rf_class(vals$RF_results)),
         "RF: RegErrors"=data.frame(accu_rf_reg_model(vals$RF_results)),
         "RF: RegErrors-pred"=data.frame(vals$rf_prederrors),
         "RF: ClassErrors-pred"=data.frame(vals$rf_prederrors)
  )
})

##

module_ui_rf <- function(id){

  ns <- NS(id)
  tagList(
    tags$head(tags$style(
      HTML("
           .well3 {
 min-height: 20px;
   padding-left:5px;
    padding-top:5px;
    padding-bottom:5px;
    margin-bottom: 5px;
    background-color: #f5f5f5;
    border: 1px solid #e3e3e3;
    border-radius: 4px;
    -webkit-box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%);
    box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%)
}"))),
column(12,
       uiOutput(ns("rf_inputs")),
       uiOutput(ns("war_rf")),
       div(
         tabsetPanel(id = ns("rf_tab"),
                     tabPanel(strong("1. Training"),value='rf_tab1',
                              uiOutput(ns("rf_params"))),
                     tabPanel(
                       strong("2. Results"),value="rf_tab2",
                       uiOutput(ns("RF_results"))
                     ),
                     tabPanel(strong("3. Predict"),value="rf_tab3",style="background: white",
                              uiOutput(ns("RF_predictions"))
                     )

         )
       )
)
  )

  }

# Server
module_server_rf <- function (input, output, session,vals,df_colors,newcolhabs ){
  ns <- session$ns

observeEvent(input$rf_tab,{
  vals$rf_tab<-input$rf_tab
})

  output$rf_inputs <- renderUI({
    if(is.null(vals$cur_data_rfY)){vals$cur_data_rfY<-1}
    if(is.null(vals$cur_rf_type)){vals$cur_rf_type<-"Classification"}
    #req(length(vals$saved_data)>0)
    fluidRow(style="color: #05668D;",

             if(input$rf_tab=="rf_tab1"){
               column(12,
                      uiOutput(ns("train_test")),
                 div(class='well3',
                     strong("Type:"),inline(radioButtons(ns('rf_type'),NULL,choices=c("Classification","Regresion"),inline =T, selected=vals$cur_rf_type))
                 ),
                 div(class="well3",
                     span(strong("X:"),
                          inline(uiOutput(ns('data_rfX_out')))




                     )
                 ),

                 div(class="well3",
                     span(strong("Y:"),
                          inline(
                            div(
                              div("Datalist:"),
                              div(pickerInput(ns("data_rfY"),NULL,choices =  names(vals$saved_data), width="150px", selected=vals$cur_data_rfY))
                            )
                          ),
                          "::",
                          inline(uiOutput(ns('rf_supervisor'))),

                          inline(uiOutput(ns("rf_test_part"))),
                          "::",
                          inline(uiOutput(ns("rf_partition")))
                     )
                 )


               )
             } else {
               column(12,
                      inline( uiOutput(ns('data_rfX_out'))),
                      inline(uiOutput(ns("rf_results_menu"))),
                      inline(uiOutput(ns("saverf_button"))),
                      inline(uiOutput(ns("rmrf_button"))),
               )
             }
    )
  })
  output$rf_params <- renderUI({
    column(12,class="well2",

           div(class="map_control_style",style="color: #05668D; margin-top: 20px",

               div(
                 tipify(icon("fas fa-question-circle"),"Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times", options = list(container="body")),
                 "+ ntree:",
                 inline(
                   numericInput(ns("ntree"), NULL, value = 50, width="120px")
                 )
               ),
               div(
                 tipify(icon("fas fa-question-circle"),"how the mtry parameter (i.e. the number of variables randomly sampled as candidates at each split) is determined", options = list(container="body")),
                 "+ search:",
                 inline(
                   pickerInput(ns("rf_search"), NULL, choices = c("grid","random","user-defined"), width="110px")
                 )
               ),

               uiOutput(ns("rf_mtry"))
               ,

               uiOutput(ns("rf_grid_search")),

               div(
                 tipify(icon("fas fa-question-circle"),textseed(), options = list(container="body")),
                 "+ seed:",
                 inline(
                   numericInput(ns("seedrf"), NULL, value = NULL, width="122px")
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
               pickerInput(ns("res_method"),NULL, choices=list("repeatedcv","boot","LOOCV","LGOCV"), width = "100px")
               ),
               div(
                 uiOutput(ns("rf_resampling"))
               )
           ),

           column(12,style='white-space: normal;',
                  uiOutput(ns("rf_war"))
           ),

           column(12, align = "center",
                  uiOutput(ns("train_RF_button"))
           ),

    )
  })

  output$rf_resampling<-renderUI({
    div(
      if(input$res_method=='cv'|input$res_method=='repeatedcv'){
        div(
          tipify(icon("fas fa-question-circle"),"number of folds", options=list(container="body")),"+ cv:",
          numericInput(ns("cvrf"), NULL, value = 5,width="140px")

        )
      },
      if(input$res_method=='repeatedcv'){
        inline(
          div(tipify(icon("fas fa-question-circle"),"the number of complete sets of folds to compute", options=list(container="body")),
              "+ repeats:",
              numericInput(ns("repeatsrf"),NULL, value = 1,width="109px")
          )
        )},
      if(input$res_method=='boot'){
        inline(
          div(
            tipify(icon("fas fa-question-circle"),"the number of resampling iterations", options=list(container="body")),
            "+ number:",
            numericInput(ns("cvrf"), NULL, value = 5,width="100px")

          )
        )

      },
      if(input$res_method=='LGOCV'){
        inline(
          div(
            tipify(icon("fas fa-question-circle"),"the training percentage", options=list(container="body")),
            "+ percentage:",
            numericInput(ns("pleaverf"), NULL, value = 10,width="100px")
          )
        )
      }
    )


  })
  observeEvent(input$predrf_tab,{
    vals$rftab3<-input$predrf_tab
  })

  
  output$train_RF_button<-renderUI({
    if(input$rf_search=="user-defined" ){
      req(length(vals$mtry_pool)>0)
    }

    popify(actionButton(ns("trainRF"), h4(icon("fas fa-tree"),icon("fas fa-tree", style = "margin-left: -10px;"),icon("fas fa-tree", style = "margin-left: -10px;"),"train RF",icon("fas fa-arrow-circle-right")), style = "background:  #05668D; color: white"),NULL,"Click to run")
  })
  output$RF_predictions<-renderUI({
    column(12,style="background: white", uiOutput(ns("predictions_rf")),
           tabsetPanel(id=ns("predrf_tab"), selected=vals$rftab3,
                       tabPanel(
                         strong("3.1. Results"), value="rftab3_1",
                         fluidRow(style="background: white",
                                  uiOutput(ns("predictions_rf_out")))),
                       tabPanel(
                         strong("3.2. Tree predictions"), value="rftab3_2",
                         tabsetPanel(
                           tabPanel("3.2.1. Histograms",
                             div(style="background: white",
                                 inline(uiOutput(ns("rf_tree_show"))),
                                 inline(uiOutput(ns("rf_tree_pred"))),
                                 plotOutput(ns("summ_trees"), height = '700px'))
                           ),
                           tabPanel("3.2.2. wilcox.test",
                                    uiOutput(ns('wilcox_rf')))
                         )),
                       tabPanel(
                         strong("3.3. Errors"), value="rftab3_3",
                         fluidRow(style="background: white",
                                  uiOutput(ns("rf_errors")))
                       ),
                       tabPanel(
                         strong("3.4. Confusion Matrix"), value="rftab3_4",
                         fluidRow(style="background: white",
                                  uiOutput(ns("rf_metrics")))
                       )
           )
    )
  })

  get_wilcox<-eventReactive(input$wilcox_go,{
# vals<-readRDS("vals2.rds")
#input<-readRDS("input.rds")  
#names(vals$saved_data)
#input$predrf_newY_tree<-"meio_c2"
 
#rfs<-attr(vals$saved_data[[input$predrf_new]],'rf')
#m<-rfs[[6]][[1]]
 #vals$RF_results<-m
 #input$rf_reftest<-"Estacao"
  
#min_depth_frame <- min_depth_distribution(m$finalModel)


    m<-vals$RF_results
    var<-attr(m,"supervisor")
    model_data<-vals$saved_data[[input$predrf_new]]
    obs_data<-vals$saved_data[[input$predrf_newY_tree]]
    if(input$rf_reftest=="rownames"){
      ref1<-rownames(model_data)
      ref2<-rownames(obs_data)
      ref1<-ref2
    } else{
      
      fac_c1<-attr(model_data,'factors')
      ref1<-fac_c1[,input$rf_reftest]
      fac_c2<-attr(obs_data,"factors")
      ref2<-fac_c2[,input$rf_reftest]
      levels(ref1)<-levels(ref2)
      
    }


   
    lo<-split(obs_data[var],ref2)
    pred <- predict(m$finalModel,newdata = model_data, predict.all=T)$individual
    pred_interval=0.05
    pred.rf.int <- data.frame(
      t(apply(pred, 1, function(x) {
        c(quantile(x, c(pred_interval/2,   1-(pred_interval/2))))
      }))
    )
    
    
    lpred<-split(pred,ref1)
    lp<-split(pred.rf.int,ref1)
    lop<-mapply(list, lp,lo,lpred,SIMPLIFY=FALSE)

    x<-lop[[1]]
    res<-do.call(rbind,lapply(lop,function(x){
      interval<-x[[1]]
      obs<-unlist(x[[2]])
      cbind(x[[2]],do.call(rbind,lapply(obs, function(xx){

        res<-if(isTRUE(between(xx,interval[[1]],interval[[2]] ))){"as expected"} else{
          if(xx<=interval[[1]]){"lower"} else if(xx>=interval[[1]]){"higher"} else{"as expected"}
        }
        v1= xx
        a<-x[[3]]
        rest<-wilcox.test(a, y = NULL,
                          alternative = "two.sided",
                          mu = v1, paired = FALSE, exact = NULL, correct = TRUE,
                          conf.int = T, conf.level = 0.95,
                          tol.root = 1e-4)
        
        interval<-data.frame(interval)
        interval$q_class<-res

     
        
        statistic=rest$statistic[[1]]
        p.value=rest$p.value[[1]]
        null.value=rest$null.value[[1]]
        alternative=rest$alternative[[1]]
        w_conf1=rest$conf.int[[1]]
        w_conf2=rest$conf.int[[2]]
        estimate=rest$estimate[[1]]

        wil_res<-data.frame(statistic,
                            p.value,
                            null.value,
                            w_conf1,
                            w_conf2,
                            estimate)
        sig= wil_res$p.value<0.05
        conf2<-sort(c(w_conf1,w_conf2))
        w_class<-if(isTRUE(between(xx,conf2[[1]],conf2[[2]] ))){"as expected"} else{
          if(xx<=conf2[[1]]){"lower"} else if(xx>=conf2[[2]]){"higher"} else{"as expected"}
        }

        wil_res$sig<-sig
        wil_res$w_class<-w_class
        wil_res<-cbind(wil_res,interval)
        wil_res
      })))
    }))
    
colnames(res)[c(10,11)]<-c("q0.025",'q0.975')
    rownames(res)<-rownames(obs_data)
    res$q_class<-factor(res$q_class, levels=c("lower","as expected","higher"), labels=c("lower","as expected","higher"))
    res$w_class<-factor(res$w_class, levels=c("lower","as expected","higher"), labels=c("lower","as expected","higher"))
    res$sig<-as.factor(res$sig)
    vals$rf_treetest<-res
    vals$rf_treetest
  })
  observeEvent(input$predrf_newY_tree,{
    vals$predrf_newY_tree<-input$predrf_newY_tree
  })

  output$wilcox_rf<-renderUI({
   validate(need(input$rfpred_which=='Datalist',"This feature currently does not support predictions from the partition"))

    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("wilcox_side"))
      ),
      mainPanel(style="background: white",
                div(style="margin-top: 15px",
                    
                    tags$style(
                      paste(paste0("#",ns('rf_trees_out')),"td {
                    padding-left: 4px;
                    padding-right: 4px;
                    padding-top: 2px;
                    padding-bottom: 2px;
                     text-align: left;
                     font-size:12px}")
                    ),
                    tags$style(
                      paste0("#",ns('rf_trees_out')),"th {
                padding-left: 4px;
                    padding-right: 4px;
                    padding-top: 2px;
                    padding-bottom: 2px;
                     text-align: left;
                     font-size:12px}"
                    ),

                inline(
                  DT::dataTableOutput(ns("rf_trees_out"))
                )
                )
      )
    )
  })
  output$pick_rfobs<-renderUI({
    sup_test<-attr(vals$RF_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("predrf_newY_tree"),
                       NULL,names(vals$saved_data[getobsRF()]), width="150px",selected=vals$predrf_newY_tree
           ))
    )
  })
  output$wilcox_side<-renderUI({
 
    div(
      uiOutput(ns("pick_rfobs")),
      uiOutput(ns("predrf_newY_ref")),
      div(actionButton(ns('wilcox_go'),"Run Wilcox.test")),
      div(actionLink(ns('wilcox_gdownp'),"Download table")),
      div( actionLink(ns('wilcox_gcreate'),"Create Datalist"))
      
      
    )
  })
  observeEvent(input$wilcox_gcreate,{
    
    vals$hand_save<-"wilcox_create"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    mod_downcenter <- callModule(module_server_dc, input$predrf_newY_tree,  vals=vals,dataX=input$predrf_newY_tree)
    
  })
  observeEvent(input$wilcox_gdownp,{
    
    vals$hand_down<-"rf_wilcox"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
    
  })

  output$rf_trees_out<-DT::renderDataTable({
    table<-get_wilcox()
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'))
    
  })
  
  output$rf_tree_show<-renderUI({
    numericInput(ns("nhist_tree"),"show", 25, width = "100px")
  })
  output$rf_tree_pred<-renderUI({
    req(input$nhist_tree)
    req(vals$RF_results$modelType=="Regression")
    
    data=t(predall_rf()$individual)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    
    div(
      inline(pickerInput(ns('splitdata_trees'),"Observations:", choices=options_show, width="200px")),
      actionButton(ns('downp_summ_trees'),tipify(icon("fas fa-download"), "Download Plot"), style="button_active")
    )
  })
  
  output$summ_trees<-renderPlot({
    req(input$nhist_tree)
    data=t(predall_rf()$individual)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
    
    
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    str_numerics(data)
    vals$vartrees<-recordPlot()
  })
  observeEvent(input$rftab2,{
    vals$cur_rftab2<-input$rftab2
  })

  output$RF_results<-renderUI({
    if(is.null(vals$cur_rftab2)){vals$cur_rftab2<-"rftab2_1"}
    validate(need(length(vals$RF_results)>0,"Please train a RF model in "))
    div(
      tabsetPanel(
        id=ns("rftab2"),selected =vals$cur_rftab2,
        tabPanel(value="rftab2_1",
                 strong("2.1. Summary"),
                 column(12,style="background: white",
                   uiOutput(ns("rfsummary")),
                   splitLayout(
                     div(renderPrint(gettrain(vals$RF_results))),
                     div(renderPrint(vals$RF_results$results))
                   ),
                   plotOutput(ns("rf_mtry_plot"))

                 )),
        tabPanel(value="rftab2_2",
                 strong("2.2. Training error "),
                 uiOutput(ns("rf_table"))),
        tabPanel(strong("2.3. RandomForest Explainer"),value='rftab2_3',
                 uiOutput(ns("RFexp"))),
        tabPanel(value="rftab2_4",
                 strong("2.4. Confusion Matrix"),
                 uiOutput(ns("rftab2_4_res")))


      )
    )
  })
  output$rf_mtry_plot<-renderPlot({
    req(length(vals$RF_results)>0)
    m<-vals$RF_results
    res<-m$results

    plot(res[,2]~res$mtry, type="n", col="darkblue", las=1, xlab="# Randomly Selected Predictors",ylab=paste(colnames(res)[2],resampName(m,F)), pch=16, ann=F, axes=F, xaxp=NULL)
    grid()
    par(new=T)
    plot(res[,2]~res$mtry, type="b", col="darkblue", las=1, xlab="# Randomly Selected Predictors",ylab=paste(colnames(res)[2],resampName(m,F)), pch=16)
    abline(v=res[which(res[,1]==m$finalModel$mtry),1], lty=2,col="red")


  })
  output$rftab2_4_res <- renderUI({
    validate(need(is.factor( vals$RF_results$finalModel$y), "Confusion matrices are only valid for classification (factor) models."))
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          class="map_control_style",style="color: #05668D",
          div(
            span(
              "+ Type: ",
              pickerInput(inputId = ns("rf_cm_type"),
                          label = NULL,
                          choices = c("Resampling","Optimal Model"),

                          width="100px"
              )
            )
          ),
          div(
            span(
              "+ Palette",
              pickerInput(inputId = ns("rfpalette"),
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
            popify(actionLink(ns("downp_cmrf"), span("+ Download",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"download CM plot")
          ),
          div(
            popify(actionLink(ns("dowcenter_cmrf"), span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style  = "button_active"),NULL,"download CM table")
          )

        )

      ),
      mainPanel(
        plotOutput(ns("CM")),
        verbatimTextOutput(ns("Confusion_RF"))
      )
    )
  })

  output$CM <- renderPlot({
    if(input$rf_cm_type=="Resampling"){
    m<-vals$RF_results} else{
      m<-vals$RF_results$finalModel
    }
    attr(m,'title')<- attr(getConfusion(m),'title')
    res<-plotCM(m, input$rfpalette,  newcolhabs=vals$newcolhabs)
    vals$cm_rf<-res
    res
  })

  observeEvent(input$rftab2_3,{
    vals$rftab2_3<-input$rftab2_3
  })

  output$Confusion_RF<-renderPrint({
    res<-if(input$rf_cm_type=="Resampling"){
      res<-confusionMatrix(vals$RF_results$pred$pred,vals$RF_results$pred$obs)
    } else{
      confusionMatrix(predict(vals$RF_results$finalModel),vals$RF_results$trainingData[,1])
    }
    vals$rf_cm<-data.frame(res$table)
    res
    })
  output$RFexp<-renderUI({
    if(is.null(vals$rftab2_3)){vals$rftab2_3<-"rftab2_3_1"}
    div(
        tabsetPanel(
          id=ns('rftab2_3'),selected =vals$rftab2_3,
          tabPanel(value="rftab2_3_1",
            span("2.3.1. Measures"),
            div(
                uiOutput(ns("rf_measure"))
            )
          ),
          tabPanel(value="rftab2_3_2",
            span("2.3.2. Min Depth Distr."),

            div(
                uiOutput(ns("rf_mindepth"))
            )
          ),
          tabPanel(value="rftab2_3_3",
            span("2.3.3. Multi-way"),
            div(
                uiOutput(ns("rf_multi"))
            )
          ),
          tabPanel(value="rftab2_3_4",
                   "2.3.4. Relationships",
                   uiOutput(ns("rf_relations"))

          ),


          tabPanel(
            value="rftab2_3_5",
            span("2.3.5. Interactions"),
            div(
                tabsetPanel(
                  tabPanel(span("2.3.5.1. Variable Interactions"),
                           div(uiOutput(ns("rftab2_3_5_1")))),
                  tabPanel(span("2.3.5.2. Biplot Interactions"),
                           div(uiOutput(ns("rftab2_3_5_2"))))
                )

            )
          )
        )

    )

  })
  output$rftab2_3_5_2<-renderUI({
    sidebarLayout(
      sidebarPanel(
        strong("Partial Dependence",tipify(icon("fas fa-question-circle"),"Plot partial dependence of two variables(i.e., marginal effects) for the randomForest", options =list(container="body")), style="font-size: 16px"),
        uiOutput(ns("side_rf_grid"))),
      mainPanel(style="background: white",
        withSpinner(type=8,color="SeaGreen",div(style="margin-top: 10px",plotOutput(ns("rfbiplot1"))))
      )
    )
  })
  output$rf_interbiplot2<-renderUI({
    frame<-attr(vals$RF_results,"interframe")
    pic<-which(frame[,1]==frame[,2])
    choices<-frame$interaction[-pic]

    if(input$rf_useinter=="Interaction Frame"){
      div(
        span("+ Interaction:",
             div(
               pickerInput(ns("rf_interaction"), NULL, choices=choices, width="200px",options =list(container="body"))
             )
        )
      )
    } else{
      div(
        div(
          span("+ Variable 1",
               inline(
                 pickerInput(ns("rf_grid_var1"), NULL, choices=c(colnames(vals$RF_results$trainingData[-1])), width="100px")
               )
          )
        ),
        div(
          span("+ Variable 2",
               inline(
                 pickerInput(ns("rf_grid_var2"), NULL, choices=c(colnames(vals$RF_results$trainingData[-1])), width="100px", selected=colnames(vals$RF_results$trainingData[-1])[2])
               )
          )
        )
      )
    }
  })
  output$side_rf_grid<-renderUI({

    fluidRow(class="map_control_style",style="color: #05668D",

             div(
               span("+ Use",
                    inline(
                      pickerInput(ns("rf_useinter"), NULL, choices=c("Interaction Frame","Custom Variables"), width="150px", selected=colnames(vals$RF_results$trainingData[-1])[2])
                    )
               )
             ),
             uiOutput(ns("rf_interbiplot2")),
             if(vals$RF_results$modelType=="Classification"){
               div(
                 span("+ Class",
                      inline(
                        pickerInput(ns("rf_biplotclass"), NULL, choices=levels(vals$RF_results$trainingData[,1]), width="150px")

                      )
                 )
               )
             },
             div(
               span(span(tipify(icon("fas fa-question-circle"),"Integer giving the number of equally spaced points to use for the continuous variables", options =list(container="body")),'+ Grid resolution'),
                    inline(
                      numericInput(ns('rfbi_grid'),NULL,5, width="75px")
                    )
               )
             ),
             uiOutput(ns(
               "pd_plot_custom"
             ))


    )
  })

  output$pd_plot_custom<-renderUI({
    var1=picvars_pd()$var1
    var2=picvars_pd()$var2
    div(
      class="well3",

      div(
        span("+ Title:",
             inline(
               textInput(ns("title_pd"),NULL, value=NULL, width="200px")
             )
        )
      ),
      div(
        span("+ X label:",
             inline(
               textInput(ns("xlab_pd"),NULL, value=var1, width="200px")
             )
        )
      ),
      div(
        span("+ Y label:",
             inline(
               textInput(ns("ylab_pd"),NULL, value=var2, width="200px")
             )
        )
      ),


      div(
        span("+ Palette:",
             inline(
               pickerInput(inputId = ns("pd_palette"),
                           label = NULL,
                           selected=vals$colors_img$val[2],
                           choices =vals$colors_img$val[getgrad_col()],
                           choicesOpt = list(
                             content =vals$colors_img$img[getgrad_col()]),
                           options=list(container="body"),
                           width='100px')
             )
        )
      ),
      div(
        span("+ Size:",
             inline(
               numericInput(ns("pd_size_plot"),NULL, value=10, width="75px")
             )
        )
      ),
      div(
        actionLink(ns('downp_rfbi'),span("+ Download plot",icon("fas fa-image"),icon("fas fa-download")), style="button_active")
      )
    )
  })
  picvars_pd<-reactive({
req(input$rf_useinter)
    if(input$rf_useinter=='Interaction Frame'){
      req(input$rf_interaction)
      #isolate( saveRDS(reactiveValuesToList(input),"input.rds"))
      # isolate( saveRDS(reactiveValuesToList(vals),"vals.rds"))
      #
      pic<-which(attr(vals$RF_results,"interframe")$interaction==input$rf_interaction)
      var1<-attr(vals$RF_results,"interframe")$variable[pic]
      var2<-as.character(attr(vals$RF_results,"interframe")$root_variable[pic])
    }else{
      #req(input$rf_grid_var1)
      var1<-input$rf_grid_var1
      var2<-input$rf_grid_var2
    }
    list(var1=var1,var2=var2)
  })

  output$rfbiplot1<-renderPlot({
    req(input$rf_useinter)
    req(input$rfbi_grid)
    req(input$ylab_pd)
    req(input$xlab_pd)
    req(input$pd_size_plot)
    req(input$pd_palette)
     #req(input$rfbi_prob)
#isolate(saveRDS(reactiveValuesToList(vals),"vals.rds"))
#isolate(saveRDS(reactiveValuesToList(input),"input.rds"))
  #input<-readRDS("input.rds")
 # vals<-readRDS("vals.rds")
    m<-vals$RF_results
    var1=picvars_pd()$var1
    var2=picvars_pd()$var2
    if(vals$RF_results$modelType=="Classification"){
      req(input$rf_biplotclass)
      pd <- pdp::partial(m, pred.var = c(var1,var2), type="classification",which.class=input$rf_biplotclass,
                         grid.resolution=input$rfbi_grid,

                         prob=T,
                         chull=T)
      p1 <- autoplot(pd, contour = TRUE, main = paste("Class:",input$rf_biplotclass),legend.title = "Partial\ndependence (prob)")
    } else{
      pd<-pdp::partial(m, pred.var = c(var1,var2), type="regression",grid.resolution=input$rfbi_grid,
                       chull=T)


      p1 <- autoplot(pd, contour = TRUE, main = "",legend.title = "Partial\ndependence")}
    p1<- p1 + scale_fill_gradientn(colours =scale_color_2(input$pd_palette,vals$newcolhabs),name="Partial\ndependence (prob)")
    p1<-p1+theme_bw(base_size = input$pd_size_plot)
    p1<-p1+ ggtitle(input$title_pd)
    p1<-p1+labs(y = input$ylab_pd, x=input$xlab_pd)


    vals$rfbiplot1<-p1
    p1
  })
  output$rf_relations<-renderUI({

    validate(need(length(attr(vals$RF_results,"mindepth"))>0,"Use '+ Click to Measure Importance' in '2.3.1 Measures' panel before generating the plot  "))
    tabsetPanel(
      tabPanel(
        span("2.3.4.1. Between importances"),
        div(style="background: white",
            div(
              span(
                strong("Compare measures",tipify(icon("fas fa-question-circle"),"Plot selected importance measures pairwise against each other", options =list(container="body")), style="font-size: 16px")
              )
            ),
            uiOutput(ns("rf_rel"))

        )
      ),
      tabPanel(
        span("2.3.4.2. Between rankings"),
        div(style="background: white",
            div(
              span(

                strong("Compare rankings",tipify(icon("fas fa-question-circle"),"Plot against each other rankings of variables according to various measures of importance", options =list(container="body")), style="font-size: 16px")

              )
            ),
            uiOutput(ns("rf_rank"))
        )
      )
    )
  })
  output$rf_mindepth<-renderUI({
    validate(need(length(attr(vals$RF_results,"mindepth"))>0,"Use '+ Click to Measure Importance' in '2.3.1 Measures' panel before generating the plot  "))
    sidebarLayout(
      sidebarPanel(
        strong("Minimal depth distribution",tipify(icon("fas fa-question-circle"),"Plot the distribution of minimal depth in a random forest", options =list(container="body")), style="font-size: 16px"),
        uiOutput(ns("side_rf_mindeph"))),
      mainPanel(
        style="background: white",
        div(style="margin-top: 10px",
          plotOutput(ns("prf"))
        ))
    )
  })
  output$rf_measure<-renderUI({
    sidebarLayout(
      sidebarPanel(
        div(
          strong("Measures",tipify(icon("fas fa-question-circle"),"Creates a data frame with various measures of importance of variables in a random forest", options =list(container="body")), style="font-size: 16px"),
          uiOutput(ns("rf_sidemeasure"))
        )
      ),
      mainPanel(style="background: white",
        div(style="margin-top: 15px",

            tags$style(
              paste(paste0("#",ns('rf_measure_out')),"td {
                    padding-left: 4px;
                    padding-right: 4px;
                    padding-top: 2px;
                    padding-bottom: 2px;
                     text-align: left;
                     font-size:12px}")
            ),
            tags$style(
              paste0("#",ns('rf_measure_out')),"th {
                padding-left: 4px;
                    padding-right: 4px;
                    padding-top: 2px;
                    padding-bottom: 2px;
                     text-align: left;
                     font-size:12px}"
            ),
          inline(
            DT::dataTableOutput(ns("rf_measure_out"))
          )
        )
      )
    )
  })
  output$rf_measure_out<-DT::renderDataTable(data.frame( attr(vals$RF_results,"mindepth")[[2]]),options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')
  output$rf_sidemeasure<-renderUI({
    fluidRow(
      class="map_control_style",style="color: #05668D",
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The sample of trees on which conditional mean minimal depth is calculated", options =list(container="body")),'+ mean_sample'),
             inline(
               pickerInput(ns('rfmeasure_mean_sample'),NULL,choices=c("all_trees", "top_trees", "relevant_trees"),selected='top_trees', width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle")," the measures of importance to be used", options =list(container="body")),'+ measures:'),
             p(
               fluidRow(
                 style="margin-left: 15px",
                 inline(checkboxGroupInput(ns('rf_measures'),NULL,choices=c(rf_inputsmeasure()),selected=rf_inputsmeasure(),width="140px"))

               )
             )
        )
      ),
      div(
        (actionLink(ns('go_rfplot'),span("+ Click to Measure Importance ",icon("fas fa-arrow-circle-right")), style = "animation: glowing3 1000ms infinite;"))
      ),
      if(length(attr(vals$RF_results,"mindepth"))>0){
        div(
          tipify(
            actionLink(
              ns('downcenter_rfdepth'),span("+ Download Results",icon("fas fa-table")), style="button_active"
            ),
            "Download Minimal Depth distribution results", options=list(container="body")
          )
        )
      }




    )
  })
  output$rftab2_3_5_1<-renderUI({
    validate(need(length(attr(vals$RF_results,"mindepth"))>0,"Use '+ Click to Measure Importance' in '2.3.1 Measures' panel before generating the plot  "))
    sidebarLayout(
      sidebarPanel(
        strong("Variable interactions",tipify(icon("fas fa-question-circle"),"Investigate interactions with respect to the  set of most important variables", options =list(container="body")), style="font-size: 16px"),
        div(
          radioGroupButtons("rf_intermode","show",choices=c("Plot","Table"))
        ),
        uiOutput(ns("rf_sideinter"))),
      mainPanel(style="background: white",
                div(style="margin-top: 10px",
                    conditionalPanel("input.rf_intermode=='Plot'",{
                      plotOutput(ns("rf_inter_out"))
                    }),

                    conditionalPanel("input.rf_intermode=='Table'",{
                      fluidRow(
                        tags$style(
                          paste(paste0("#",ns('rf_inter_tab')),"td {
                    padding-left: 4px;
                    padding-right: 4px;
                    padding-top: 2px;
                    padding-bottom: 2px;
                     text-align: left;
                     font-size:12px}")
                        ),
                    tags$style(
                      paste0("#",ns('rf_inter_tab')),"th {
                padding-left: 4px;
                    padding-right: 4px;
                    padding-top: 2px;
                    padding-bottom: 2px;
                     text-align: left;
                     font-size:12px}"
                    ),
                        inline(
                          DT::dataTableOutput(ns("rf_inter_tab"))
                        )
                      )
                    })

                )
        )
    )
  })

  observe({
    #req(is.null(attr(vals$RF_results,"interframe")))
    req(input$rfinter_k)
    req(input$rfinter_mean_sample)
    req(input$uncond_mean_sample)
    req(input$rfinter_k2)
    req(length(attr(vals$RF_results,"mindepth"))>0)
    withProgress(message = "Running ...",
                 min = 1,
                 max = 1,
                 {
                   importance_frame<- attr(vals$RF_results,"mindepth")[[2]]
                   vars <- important_variables(importance_frame, k = input$rfinter_k, measures = c("mean_min_depth", "no_of_trees"),ties_action="draw")
                   if(class(vals$RF_results)[1]=="randomForest"){
                     forest=vals$RF_results } else {forest=vals$RF_results$finalModel}
                   interactions_frame <- suppressWarnings(min_depth_interactions(forest, vars, mean_sample=input$rfinter_mean_sample, uncond_mean_sample=input$uncond_mean_sample))
                   vals$rf_interactions_frame
                   res<-interactions_frame[ order(interactions_frame$occurrences,decreasing=T),]

                   if(input$rf_models=="new rf (unsaved)"){
                     attr(vals$RF_results,"interframe")<-res
                   } else{
                     attr(attr(vals$saved_data[[input$data_rfX]],"rf")[[input$rf_models]][[1]],"interframe")<-res
                   }
                   updateTabsetPanel(session,"rftab2","rftab2_3")
                   updateTabsetPanel(session,"rftab2_3","rftab2_3_5")
                 })
  })

  output$rf_inter_tab<-DT::renderDataTable(data.frame(attr(vals$RF_results,"interframe")),options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = F,class ='cell-border compact stripe')
  output$rf_inter_out<-renderPlot({
    req(!is.null(attr(vals$RF_results,"interframe")))
    req(input$rfinter_k2)
    k2<-input$rfinter_k2
    if(is.na(k2)){
      k2<-NULL
    }
    p<-plot_min_depth_interactions(attr(vals$RF_results,"interframe"),k=k2)
    vals$rf_inter<-p
    p


  })
  output$rf_sideinter<-renderUI({
    fluidRow(
      class="map_control_style",style="color: #05668D",

      div(
        span(span(tipify(icon("fas fa-question-circle"),"The number of variables to extract", options =list(container="body")),'+ k'),
             inline(
               numericInput(ns('rfinter_k'),NULL,value=5,step=1, width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The sample of trees on which conditional mean minimal depth is calculated", options =list(container="body")),'+ mean_sample'),
             inline(
               pickerInput(ns('rfinter_mean_sample'),NULL,choices=c("all_trees", "top_trees", "relevant_trees"),selected='top_trees', width="80px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The sample of trees on which unconditional mean minimal depth is calculated", options =list(container="body")),'+ unc_mean_sample'),
             inline(
               pickerInput(ns('uncond_mean_sample'),NULL,choices=c("all_trees", "top_trees", "relevant_trees"),selected='mean_sample', width="45px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The number of best interactions to plot, if empty then all plotted", options =list(container="body")),'+ N inter'),
             inline(
               numericInput(ns('rfinter_k2'),NULL,value=30,step=1, width="75px")
             )
        )
      ),
      div(
        tipify(
          actionLink(
            ns('create_rfinter'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
          ),
          "Create a datalist with the variables included in the N most frequent interactions in the RF", options=list(container="body")
        )
      ),

      div(
        (actionLink(ns('rfinter_downp'),span("+ Download plot",icon("fas fa-image"),icon("fas fa-download")), style="button_active"))
      ),
      div(
        tipify(
          actionLink(
            ns('downcenter_rfinter'),span("+ Download Interaction Results",icon("fas fa-table")), style="button_active"
          ),
          "Download Interaction frame", options=list(container="body")
        )
      )

    )
  })
  observeEvent(input$rfinter_k2,
               vals$rfinter_k2<-input$rfinter_k2)
  output$rf_rank<-renderUI({
    div(
      renderPlot({


        vals$rf_rank<- plot_importance_rankings( attr(vals$RF_results,"mindepth")[[2]])
        vals$rf_rank


      })
    )
  })
  output$rf_rel<-renderUI({
    div(
      renderPlot({
        withProgress(message = "Running ...",
                     min = 1,
                     max = 1,
                     {
                       vals$rf_rel<-plot_importance_ggpairs( attr(vals$RF_results,"mindepth")[[2]])
                       vals$rf_rel

                     })

      })
    )
  })

  observeEvent(input$sigprf,{
    vals$cur_sigprf<-input$sigprf})
  observeEvent(input$rf_depth,{
    vals$cur_rf_depth<-input$rf_depth})

  observeEvent(input$depth_min_no_of_trees,{
    vals$cur_depth_min_no_of_trees<-input$depth_min_no_of_trees})


  observeEvent(input$depth_mean_sample,{
    vals$cur_depth_mean_sample<-input$depth_mean_sample})
  output$side_rf_mindeph<-renderUI({
    if(is.null(vals$cur_mdd_palette)){vals$cur_mdd_palette<-vals$colors_img$val[1]}
    if(is.null(vals$cur_rf_depth)){vals$cur_rf_depth<-T}
    if(is.null(vals$cur_depth_mean_sample)){vals$cur_depth_mean_sample<-'"top_trees"'}
    if(is.null(vals$cur_sigprf)){   vals$cur_sigprf<-0.05}

    if(is.null(vals$cur_sigprf)){   vals$cur_sigprf<-0.05}
    if(is.null(vals$cur_depth_min_no_of_trees)){   vals$cur_depth_min_no_of_trees<-0}

    fluidRow(class="map_control_style",style="color: #05668D",

             div(class="well3",
               div(
                 span("+",
                      inline(
                        checkboxInput(ns("rf_depth"), "Only significant variables:", vals$cur_rf_depth, width="75px")
                      )
                 )
               ),
               inline(uiOutput(ns("npicrf"))),
               div(
                 span(span(tipify(icon("fas fa-question-circle"),"The minimal number of trees in which a variable has to be used for splitting to be used for plotting", options =list(container="body") ),'+ Min_n_trees:'),
                      inline(
                        numericInput(ns('depth_min_no_of_trees'),NULL,value=vals$cur_depth_min_no_of_trees,step=1, width="75px")
                      )
                 )
               ),
               div(
                 span(span( tipify(icon("fas fa-question-circle"),"The sample of trees on which mean minimal depth is calculated", options =list(container="body")),'+ Mean_sample:'),
                      inline(
                        pickerInput(ns('depth_mean_sample'),NULL,choices=c( "top_trees","all_trees","relevant_trees"), selected= vals$cur_depth_mean_sample,width='85px')
                      )
                 )
               ),
               div(
                 span(span(tipify(icon("fas fa-question-circle"),"Significance level", options =list(container="body")),'+ Sig:'),
                      inline(
                        numericInput(ns("sigprf"), NULL, vals$cur_sigprf, width="75px", step=.05, max=1, min=0)
                      )
                 )
               ),
               div(
                 tipify(
                   actionLink(
                     ns('create_rf'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                   ),
                   "Create a datalist with the variables selected in the Random Forest Explainer.", options=list(container="body")
                 )
               )
             ),
             div(class="well3",
                 div(
                   span("+ Title:",
                        inline(
                          textInput(ns("title_prf"),NULL, value=paste0(
                            attr(vals$RF_results,"Y"),"~",attr(vals$RF_results,"Datalist")
                          ), width="200px")
                        )
                   )
                 ),
                 div(
                   span("+ X label:",
                        inline(
                          textInput(ns("xlab_prf"),NULL, value="Number of Trees", width="200px")
                        )
                   )
                 ),
                 div(
                   span("+ Y label:",
                        inline(
                          textInput(ns("ylab_prf"),NULL, value="Variable", width="200px")
                        )
                   )
                 ),


                 div(
                   span("+ Palette:",
                        inline(
                          pickerInput(
                            inputId = ns("mdd_palette"),
                            label = NULL,
                            selected=vals$cur_mdd_palette,
                            choices =vals$colors_img$val[getgrad_col()],
                            choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body"),width='100px'
                          )
                        )
                   )
                 ),

                 div(
                   span("+ Size:",
                        inline(
                          numericInput(ns("labrfsize"),NULL, value=10, width="75px")
                        )
                   )
                 ),
                 div(
                   span("+ Text-in size:",
                        inline(
                          numericInput(ns("size_mmd"),NULL, value=4, width="75px")
                        )
                   )
                 ),

                 div(
                   actionLink(
                     ns('downp_prf'),span("+ Download Plot",icon("fas fa-image"),icon("fas fa-download")), style="button_active"
                   )
                 )
             )
    )

  })

  observeEvent(input$mdd_palette,{
    vals$cur_mdd_palette<-input$mdd_palette
  })
  observeEvent(input$n_var_rf,{
    vals$cur_n_var_rf<-input$n_var_rf})


  output$npicrf <- renderUI({
    req (isFALSE(input$rf_depth))
    if(is.null(vals$cur_n_var_rf)){vals$cur_n_var_rf<-10}
    span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"The maximal number of variables with lowest mean minimal depth to be used for plotting", options =list(container="body")),'+ N var:'),
         inline(
           numericInput(ns("n_var_rf"),
                        NULL,
                        value = vals$cur_n_var_rf,
                        step = 1, width='45px')
         )
    )


  })
  output$rf_mtry<-renderUI({
    data<-getdata_rfX()
    req(input$rf_search=='user-defined')
    div(
      div(
        tipify(icon("fas fa-question-circle"),"the number of variables randomly sampled as candidates at each split", options = list(container="body")),
        "+ mtry:",
        inline(
          numericInput(ns("mtry"), NULL, value = 2, width="100px", max=ncol(data))
        ),

        inline(actionButton(ns("mtry_include"),tipify(icon("fas fa-arrow-right"),"Include mtry in the grid search", options = list(container="body")))),

        inline(span(verbatimTextOutput(ns("mtry_grid")))),
        inline(actionButton(ns("remove_mtry"),tipify(icon("fas fa-eraser"),"restart mtry"), style="button_active"))
      )


    )



  })
  output$rf_grid_search<-renderUI({
    req(input$rf_search!='user-defined')
    div(
      tipify(icon("fas fa-question-circle"),"The maximum number of mtry- combinations that will be generated", options = list(container="body")),
      "+ tuneLength:",
      inline(
        numericInput(ns("tuneLength"), NULL, value = 5, width="82px")
      )
    )




  })
  output$mtry_grid<-renderPrint({vals$mtry_pool})




  output$rf_supervisor <- renderUI({
    req(input$data_rfY)
    req(input$rf_type)

    if(is.null(vals$cur_rf_sup)){vals$cur_rf_sup<-1}
    data <- vals$saved_data[[input$data_rfY]]
    labels <- attr(data,"factors")
    choices<-if(input$rf_type=="Classification"){rev(colnames(labels))} else{
      colnames(data)
    }

    div(well="class2",
        div("Variable"),
        div(tipify(pickerInput(
          ns("rf_sup"),
          NULL,
          choices =choices ,
          width="150px", selected=vals$cur_rf_sup
        ),"The response vector"))
    )

  })
  observeEvent(input$rf_sup,{
    vals$cur_rf_sup<-input$rf_sup
  })
  output$data_rfX_out<-renderUI({
    if(is.null(vals$cur_data)){vals$cur_data<-1}
    div(
      div("~ Training Datalist:"),
      pickerInput(ns("data_rfX"),NULL,choices =names(vals$saved_data),width="150px", selected=vals$cur_data)
    )
  })
  observeEvent(input$data_rfX,{
    vals$cur_data<-input$data_rfX
  })
  output$rf_test_part<-renderUI({
    req(input$data_rfX)
    if(is.null(vals$cur_rf_test_partition)){vals$cur_rf_test_partition<-1}
    div(
      div(span("Partition:",tiphelp("choose a factor as reference for the partition"))),
      div(pickerInput(ns("rf_test_partition"),NULL, choices=c("None", colnames(attr(vals$saved_data[[input$data_rfY]],"factors"))), width="150px", selected=vals$cur_rf_test_partition))
    )
  })
  observeEvent(input$rf_test_partition,{
    vals$cur_rf_test_partition<-input$rf_test_partition
  })
  output$rf_partition<-renderUI({
    req(input$rf_test_partition!="None")
    req(input$data_rfY)
    if(is.null(vals$cur_testdata_rf)){vals$cur_testdata_rf<-1}
    fac<-attr(vals$saved_data[[input$data_rfY]],"factors")[,input$rf_test_partition]
    choices<-levels(fac)
    div(
      div("::",span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions"))),
      pickerInput(ns("testdata_rf"),NULL, choices=choices, width="200px", selected=vals$cur_testdata_rf)
    )
  })
  observeEvent(input$testdata_rf,{
    vals$cur_testdata_rf<-input$testdata_rf
  })
  output$rf_results_menu<-renderUI({
    div(pickerInput(ns("rf_models"),strong("rf results:", tiphelp("Random Forest Results. Click to select rf results saved in the Training Datalist (X).")), choices= c(names(attr(vals$saved_data[[input$data_rfX]],"rf"))), width="300px", selected = vals$cur_rf_models)
    )
  })
  observeEvent(input$rf_models,{
    vals$cur_rf_models<-input$rf_models
  })
  
  observeEvent(input$trainRF,{
     req(input$rf_type)
    t<-try({
      envi<-envi_o<-getdata_rfX()
      sup<-sup_o<-get_supervisor()
      
      if(input$rf_test_partition!="None"){
        parts<-get_parts_rf()
        train<-parts$train
        test<-parts$test
        envi<-getdata_rfX()[names(train),]
        sup<-get_supervisor()[names(train),, drop=F]
      }
      
      
      if (input$rf_type == 'Classification') {
        sup[1] <- as.factor(sup[, 1])
      }
      seed<-if (!is.na(input$seedrf)) { input$seedrf} else{
        NULL
      }
      
      
      
      join <- na.omit(cbind(sup, envi[rownames(sup), ,drop=F]))
      envi <- join[-1]
      somC <- join[1]
      if(input$rf_search=="user-defined"){
        rf_search<-"grid"
        validate(need(length(vals$mtry_pool)>0,"Requires at least one mtry value"))
        tuneGrid=data.frame(mtry=vals$mtry_pool)
      } else{
        rf_search<-input$rf_search
        tuneGrid=NULL
      }
      withProgress(message = "Running rf the time taken will depend on the random forest tuning",
                   min = 1,
                   max = 1,
                   {
                     RF = wrapRF(
                       envi,
                       data.frame(somC),
                       supervisor = "clusters",
                       prev.idw = F,
                       seed = seed,
                       ntree = input$ntree,
                       
                       trainControl.args = list(
                         method = input$res_method,
                         number = input$cvrf,
                         repeats = input$repeatsrf,
                         p=input$pleaverf/100,
                         savePredictions = "final",
                         search=rf_search
                       ),
                       tuneLength=input$tuneLength,
                       tuneGrid=tuneGrid
                     )
                     attr(RF,"test_partition")<-paste("Test data:",input$rf_test_partition,"::",input$testdata_rf)
                     attr(RF,"Y")<-paste(input$data_rfY,"::",input$rf_sup)
                     attr(RF,"Datalist")<-paste(input$data_rfX)
                  
                     if(input$rf_test_partition!="None"){
                       attr(RF,'test')<-data.frame(envi_o[test,])
                       attr(RF,"sup_test")<-sup_o[test,]
                     } else{ attr(RF,'test')<-c("None")}
                     
                     attr(RF,"supervisor")<-input$rf_sup
                     vals$cur_rftab2<-"rftab2_1"
                     updateTabsetPanel(session,"rf_tab","rf_tab2")
                     updateTabsetPanel(session,"rftab2","rftab2_1")
                     vals$rf_unsaved<-RF
                     
                   })
      attr(vals$saved_data[[input$data_rfX]],"rf")[['new rf (unsaved)']]<-vals$rf_unsaved
      vals$cur_rf_models<-"new rf (unsaved)"
      
      beep(10)
      
    })
    
    if("try-error" %in% class(t)){
      output$rf_war<-renderUI({
        column(12,em(style="color: gray",
                     "Error in training the RF model. Check if the number of observations in X and Y are compatible"
        ))
      })
      
    } else{
      output$rf_war<-NULL
    }
  })
  
  observeEvent(input$data_rfY,{
    vals$cur_data_rfY<-input$data_rfY
  })



  output$saverf_button<-renderUI({
    req(input$rf_models=="new rf (unsaved)")
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_saverf"), icon("fas fa-save")), "Save the rf model in the training Datalist (X)")
    )
  })
  output$rmrf_button<-renderUI({
    req(input$rf_models!="new rf (unsaved)")
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_rmrf"), icon("far fa-trash-alt")), "Remove the rf model from the training Datalist (X)")
    )

  })
  output$rf_errors<-renderUI({
    #saveRDS(reactiveValuesToList(vals),"vals.rds")
    #saveRDS(reactiveValuesToList(input),"input.rds")
    #vals<-readRDS('vals.rds')
    #input<-readRDS('input.rds')
    column(12,
           sidebarLayout(
             sidebarPanel(
               uiOutput(ns("RF_prederrors_side"))),
             mainPanel(
               div(
                 tags$style(
                   paste(paste0("#",ns('rf_tab_errors0_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
                 ),
                 tags$style(
                   paste0("#",ns('rf_tab_errors0_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
                 ),
                 tags$style(
                   paste(paste0("#",ns('rf_tab_errors_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
                 ),
                 tags$style(
                   paste0("#",ns('rf_tab_errors_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
                 ),
                 div(
                   p(strong("Global:")),
                   inline(DT::dataTableOutput(ns('rf_tab_errors0_pred')))
                 ),
                 hr(),
                 div(
                   p(strong("Observations:")),
                   inline(DT::dataTableOutput(ns('rf_tab_errors_pred')))
                 )
               )
             )
           )


    )
  })

  rf_global_pred<-reactive({

    if(vals$RF_results$modelType=="Regression"){
      obs_reg<-RF_observed()

      pred_reg<-apply(predall_rf()$individual,1,mean)
      stat<-postResample(pred_reg,obs_reg)
      stat
    } else{
      obs_class<-RF_observed()
      pred_class<-apply(predall_rf()$individual,1, function(x){
        res<-table(x)
        names(res)[ which.max(res)]})

      stat<-postResample(pred_class,obs_class)


      actual<-as.numeric(as.character(pred_class)==as.character(obs_class))

      auc<-  Metrics::auc(actual,pred_class)
      c(stat,AUC=auc,use.names=T)
    }

  })



  output$rf_tab_errors0_pred<-DT::renderDataTable({
    req(input$rf_round_3_2)
    req(length(rf_global_pred())>0)
    table<-round(  data.frame(as.list(rf_global_pred())),input$rf_round_3_2)
    req(length(table)>0)
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })

  output$rf_tab_errors_pred<-DT::renderDataTable({
    req(input$rf_round_3_2)

    table<-round(rf_prederrors(),input$rf_round_3_2)
    rownames(table)<-rownames(pred_rf())
    vals$rf_prederrors<-table
    req(length(table)>0)
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'))


  })

  RF_observed<-reactive({
    req(input$rfpred_which)
    if(vals$RF_results$modelType=="Regression"){
      observed<-if(input$rfpred_which=="Datalist"){
        req(input$predrf_newY)
        factors<-vals$saved_data[[input$predrf_newY]]
        factors[,attr(vals$RF_results,"supervisor")]
      } else{
        attr(vals$RF_results,"sup_test")
      }
    } else{
      observed<-if(input$rfpred_which=="Datalist"){
        req(input$predrf_newY)
        factors<-attr(vals$saved_data[[input$predrf_newY]],"factors")
        factors[,attr(vals$RF_results,"supervisor")]
      } else{
        attr(vals$RF_results,"sup_test")
      }
    }
    observed

  })
  observeEvent(input$predrf_newY,{
    vals$predrf_newY<-input$predrf_newY
  })
  output$predrf_newY_out<-renderUI({
    req(input$rfpred_which=='Datalist')
    sup_test<-attr(vals$RF_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
      pickerInput(ns("predrf_newY"),
                NULL,names(vals$saved_data[getobsRF()]), width="150px",selected=vals$predrf_newY
    ))
  })
  observeEvent(input$predrf_newY_tree,
               vals$predrf_newY_tree<-input$predrf_newY_tree)
  output$RF_prederrors_side<-renderUI({

    fluidRow( class="map_control_style",style="color: #05668D",
              uiOutput(ns("predrf_newY_out")),
   
              div(
                span(
                  "+ Round:",
                  numericInput(ns("rf_round_3_2"),NULL,value=3,width="75px")
                )
              ),
              div(
                tipify(
                  actionLink(
                    ns('down_rf_errors'),span("+ Download",icon("fas fa-download")), style="button_active"
                  ),
                  "Download error results",options=list(container="body")
                )

              ),
              div(
                tipify(
                  actionLink(
                    ns('create_rfreg'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                  ),
                  "Create a datalist with the prediction errors", options=list(container="body")
                )
              )
    )

  })

  

  output$predrf_newY_ref<-renderUI({

    factors<-attr(vals$saved_data[[input$predrf_newY_tree]],"factors")
    choices=c('rownames',colnames(factors))
    div(
      span("+ ref",pickerInput(ns('rf_reftest'),NULL,choices, width="100px", selected=vals$rf_reftest))
      
    )})
  
  observeEvent(input$rf_reftest,{
    vals$rf_reftest<-input$rf_reftest
  })
  rf_prederrors<-reactive({
    
    pred<-predall_rf()
    obs<-RF_observed()
    req(length(pred)>0)
    req(length(obs)>0)
    temp<-switch(vals$RF_results$modelType,
                 "Regression"=RFerror_reg(pred,obs),
                 "Classification" =data.frame(RFerror_class(pred,obs)))
    temp
  })


  output$RF_prederrors<-renderUI({
    div(
      if(input$rf_type=='Classification'){
        div(
          renderPrint({
            vals$rf_prederrors<-rf_prederrors()
            vals$rf_prederrors

          })
        )
      } else{div(

        div("rmse:", em("Root Mean square error")),
        div("mse:", em("Mean square error")),
        div("mae:", em("Mean absolute error")),
        div("mape:", em("Mean absolute percentage error")),
        renderPrint({
          vals$rf_prederrors<-rf_prederrors()
          vals$rf_prederrors

        })

      )}

    )
  })
  output$rf_metrics<-renderUI({
    validate( need(vals$RF_results$modelType=="Classification","Confusion matrices are only valid for classification models"))



    fluidRow(
      column(12, style = "background: Snow;",
             splitLayout(
               column(12,
                      strong("Palette"),
                      pickerInput(inputId = ns("cm_suprf_palette"),
                                  label = NULL,
                                  choices =     vals$colors_img$val[getgrad_col()],
                                  choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body"))
               ),
               fluidRow(style="margin-top: 20px",
                        popify(bsButton(ns("downp_confrf"), span(icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"download CM plot",
                               options=list(container="body")

                        )
               )
             ),
             uiOutput(ns("conf_rf"))
      ),

    )



  })
  output$conf_rf<-renderUI({
    pred_rf<-pred_rf()

    obs<-if(input$rfpred_which=="Datalist"){
      factors<-attr(vals$saved_data[[input$predrf_new]],"factors")
      factors[,attr(vals$RF_results,"supervisor")]
    } else{
      attr(vals$RF_results,"sup_test")
    }
    conf<-table(obs, pred_rf[,1])

    column(12,

           renderPlot({
             res<-plotCM(conf/sum(conf)*100, input$cm_suprf_palette,  newcolhabs=vals$newcolhabs)
             vals$conf_rf<-res
             res
           }),


           renderPrint(
             confusionMatrix(conf)

           ))

  })
  output$predictions_rf_out<-renderUI({
    envi<-getdata_rfX()
    sup =get_supervisor()

    sidebarLayout(
      sidebarPanel(
        fluidRow(class="map_control_style",style="color: #05668D",
                 div(
                   tipify(
                     actionLink(
                       ns('create_rfreg_pred'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                     ),
                     "Create a datalist with the RF predictions", options=list(container="body")
                   )
                 ),
                 div(
                   tipify(
                     actionLink(
                       ns('down_rf_pred_results'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                     ),
                     "Download Table", options=list(container="body")
                   )
                 )

        )
      ),
      mainPanel(
        renderPrint({
          vals$rf_pred_out<-pred_rf()
          vals$rf_pred_out
        })
      )
    )


  })
  output$predictions_rf<-renderUI({

    data=getdata_rfX()
    test<-attr(vals$RF_results,"test")
    column(12,style="background: white",
           splitLayout(
             cellWidths = c("30%",'40%','10%'),
             uiOutput(ns("rf_pred_type")),
             uiOutput(ns("predrf_new_out"))
           ),


    )
  })

  output$predrf_new_out<-renderUI({
    req(input$rfpred_which =='Datalist')
    div(selectInput(ns("predrf_new"),"Datalist",names(vals$saved_data[getnewdataRF()]), selected=vals$predrf_new))
  })

  observeEvent(input$predrf_new,{
    vals$predrf_new<-input$predrf_new
  })

  observeEvent(input$rfpred_which,{
    vals$rfpred_which<-input$rfpred_which
  })


  output$rf_pred_type<-renderUI({
    if(length(attr(vals$RF_results,'test'))==1){
      radioButtons(ns("rfpred_which"),"New data (X):",choices=c("Datalist"),inline=T)
    } else{ radioButtons(ns("rfpred_which"),"New data (X):",choices=c("Partition","Datalist"), inline=T, selected = vals$rfpred_which)}
  })


  output$rf_sidemulti<-renderUI({
    fluidRow(
      class="map_control_style",style="color: #05668D",
      div(
        span("+",
             inline(checkboxInput(ns("rf_sigmulti"), "Only significant variables", T))
        )
      ),
      inline(
        div(
          span(
            inline(uiOutput(ns("multi_no_labels")))
          )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The measure of importance to be shown on the X axis", options =list(container="body")),'+ X:'),
             inline(pickerInput(ns('multi_x_measure'),NULL,choices=c("mean_min_depth","accuracy_decrease"), selected="mean_min_depth",width="140px"))
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The measure of importance to be shown on the Y axis", options =list(container="body")),'+ Y:'),
             inline(pickerInput(ns('multi_y_measure'),NULL,choices=c("times_a_root","gini_decrease"), selected="times_a_root",width="140px"))
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"Exclude text labels that overlap too many things", options =list(container="body")),'+ max.overlaps:'),
             inline(numericInput(ns('max.overlaps'),NULL,10, width="40px"))
        )
      ),

      div(
        span(span(tipify(icon("fas fa-question-circle"),"Significance level", options =list(container="body")),'+ Sig:'),
             inline(numericInput(ns("sigmrf"), NULL, 0.05, width="75px"))
        )
      ),

      div(
        (actionLink(ns('downp_mrf'),span("+ Download plot",icon("fas fa-image"),icon("fas fa-download")), style="button_active"))
      )

    )
  })
  output$rf_multi<-renderUI({
    validate(need(length(attr(vals$RF_results,"mindepth"))>0,"Use '+ Click to Measure Importance' in '2.3.1 Measures' panel before generating the plot  "))
    sidebarLayout(
      sidebarPanel(
        strong("Multi-way importance",tipify(icon("fas fa-question-circle"),"Plot two measures of importance of variables in a random fores", options =list(container="body")), style="font-size: 16px"),
        uiOutput(ns("rf_sidemulti"))),
      mainPanel(
        style="background: white",
        div(style="margin-top: 10px",plotOutput(ns("rf_multi_out")))
        )
    )
  })
  output$rf_multi_out<-renderPlot({
    rf_multi()
  })
  output$multi_no_labels<- renderUI({
    req(isFALSE(input$rf_sigmulti))
    div(
      span(span(tipify(icon("fas fa-question-circle"),"The approximate number of best variables (according to all measures plotted) to be labeled (more will be labeled in case of ties)", options =list(container="body")),'+ No_of_labels'),
           inline(
             numericInput(ns('multi_no_of_labels'),NULL,value=10,step=1, width="75px")
           )
      )
    )

  })
  output$rf_tab_errors0_train<-DT::renderDataTable({
    m<-vals$RF_results
    table<- m$results[rownames(m$bestTune),]
    rownames(table)<-"Optimal model"
    table<-round(table,input$rf_round_2_2)
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })
  output$rf_table <- renderUI({
    div(
      tags$style(
        paste(paste0("#",ns('rf_tab_errors0_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
      ),
      tags$style(
        paste0("#",ns('rf_tab_errors0_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
      ),
      tags$style(
        paste(paste0("#",ns('rf_tab_errors_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
      ),
      tags$style(
        paste0("#",ns('rf_tab_errors_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
      ),
      div(
        sidebarLayout(
          sidebarPanel( width = 3,
            fluidRow( class="map_control_style",style="color: #05668D",
                      div(
                        span(
                          "+ Round:",
                          numericInput(ns("rf_round_2_2"),NULL,value=3,width="75px")
                        )
                      ),
                      div(
                        tipify(
                          actionLink(
                            ns('downcenter_intererror'),span("+ Download Obs. Errors",icon("fas fa-download")), style="button_active"
                          ),
                          "Download error results",options=list(container="body")
                        )

                      ),
                      div(
                        tipify(
                          actionLink(
                            ns('create_rfreg_errorinter'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                          ),
                          "Create a datalist with the observation errors", options=list(container="body")
                        )
                      )
            )
          ),
          mainPanel( width = 9,style="background: white",
            div(style="margin-top: 5px",
              div(
                p(strong("Global:")),
                inline(DT::dataTableOutput(ns('rf_tab_errors0_train')))
              ),
              hr(),
              div(
                p(strong("Observations:")),
                inline(DT::dataTableOutput(ns('rf_tab_errors_train')))
              )


            ))
        )
      )
    )
  })
  output$rf_tab_errors_train<-DT::renderDataTable({
    m<-vals$RF_results
    table<-if(vals$RF_results$modelType=="Regression"){
      accu_rf_reg_model(m)} else{accu_rf_class(m)}
    vals$rf_down_errors_train<-table
    table<-round(table,input$rf_round_2_2)
    table
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })



  output$rfsummary <- renderUI({
    req(length(vals$RF_results)>0)
    div(
      inline(
        div(class="well3",
           div(style="margin: 5px",
             div(strong('Training data (X):'),em(attr(vals$RF_results,"Datalist"))),
             div(strong('Independent variable (Y):'),em(attr(vals$RF_results,"Y"))),
             div(strong('Partition:'),em(attr(vals$RF_results,"test_partition")))



           )
           )
      ),
      inline(
        div(popify(downloadButton(ns("down_rf_results"), span(icon("fas fa-tree"),icon("fas fa-tree", style = "margin-left: -10px;"),icon("fas fa-tree", style = "margin-left: -10px;")),style = "button_active"),NULL,"download rf results as rds file",
                   options=list(container="body")))
      )


      )
  })
  output$down_rf_results <- {
    downloadHandler(
      filename = function() {
        paste0("rf","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$RF_results,file)
      })
  }
  output$prf <- renderPlot({
    p<-prf.reactive()
    vals$rfd_res<-p
    p
  })

  rfmodal <- function() {
    modalDialog(
      textrf(),
      title = h4(strong("Random Forest")),
      footer = modalButton("close"),
      size = "m",
      easyClose = TRUE
    )
  }

  getdata_rfY<-reactive({
    req(input$data_rfY)
    req(input$rf_sup)

    data <- vals$saved_data[[input$data_rfY]]
    if(input$rf_type=="Classification"){
      labels <- attr(data,"factors")
      labels<-labels[,input$rf_sup]
      labels
    } else{
      data<-data[,input$rf_sup]
      data
    }

  })

  get_parts_rf<-reactive({
    req(input$data_rfY)
    req(input$testdata_rf)
    req(input$rf_test_partition)
    factors<-attr(vals$saved_data[[input$data_rfY]],"factors")
   if(input$rf_test_partition!='None'){
    partition<- factors[, input$rf_test_partition]
    test<-which(partition==input$testdata_rf)
    names(test)<-rownames(factors)[test]
    train<-which(partition!=input$testdata_rf)
    names(train)<-rownames(factors)[train]

    list(train=train,test=test)} else{
      list(train=NULL,test=NULL)
    }

  })





  observe({
    req(input$rf_models)
    if(input$rf_models=="new rf (unsaved)"){
      vals$RF_results<-vals$rf_unsaved } else{
        rf_attr<-attr(vals$saved_data[[input$data_rfX]],"rf")
        if(is.list(rf_attr[[input$rf_models]][[1]])){
          vals$RF_results<-rf_attr[[input$rf_models]][[1]]
        } else{
          vals$RF_results<-rf_attr[[input$rf_models]]
        }
      }

  })
  observe({
    req(!input$trainRF %% 2)
    req(input$rf_search=='user-defined')
    if(input$rf_search=="user-defined" & !length(vals$mtry_pool)>0) {
      output$rf_war<-renderUI({
        column(12,em(style="color: gray",
                     "Requires at least one mtry value when 'search' is 'user-defined'"
        ))
      })
    } else {
      output$rf_war<-renderUI({
        NULL
      })
    }
  })
  observeEvent(input$go_rfplot,{
    if(input$rf_models=="new rf (unsaved)"){
      attr(vals$RF_results,"mindepth")<-mindeaphrf()
    } else{
      attr(attr(vals$saved_data[[input$data_rfX]],"rf")[[input$rf_models]][[1]],"mindepth")<-mindeaphrf()
    }
    updateTabsetPanel(session,"rftab2","rftab2_3")
  })
  observeEvent(input$rf_type,{
    vals$cur_rf_type<-input$rf_type
  })
  observeEvent(input$rfhelp, {
    showModal(rfmodal())
  })

  observeEvent(input$rf_sup,{vals$cur_rfsup<-input$rf_sup})

  observeEvent(input$rf_test_partition,{vals$cur_partrf<-input$rf_test_partition
  })
  observeEvent(input$data_rfX,{
    vals$cur_data=input$data_rfX
  })
  observeEvent(input$rf_tab,{
    vals$currftab<-switch(input$rf_tab,
                          "rf_tab1"="rf_tab1",
                          "rf_tab2"="rf_tab2",
                          "rf_tab3"="rf_tab3")
  })
  observeEvent(input$mtry_include,{
    vals$mtry_pool<-c(vals$mtry_pool,input$mtry)
  })
  observeEvent(input$remove_mtry,{
    vals$mtry_pool<-c()
  })
  observeEvent(input$rf_type,{
    updateTabsetPanel(session,"rf_tab", selected="rf_tab1")
  })

  observeEvent(input$testdata_rf,{
    updateTabsetPanel(session,"rf_tab",'rf_tab1')
  })
  observeEvent(input$tools_rmrf,{
    attr(vals$saved_data[[input$data_rfX]],"rf")[input$rf_models]<-NULL
  })
  observeEvent(input$tools_saverf,{
    #isolate(saveRDS(reactiveValuesToList(vals),"vals.rds"))
    #isolate(saveRDS(reactiveValuesToList(input),"input.rds"))
    req(input$rf_models=="new rf (unsaved)")
    vals$hand_save<-"Save RF model in"
    vals$hand_save2<-column(12,fluidRow(em(input$data_rfX, style="color:gray"),strong("::"), em("RF-Attribute", style="color:gray"),strong("::")
    ))
    vals$hand_save3<-NULL

    mod_downcenter <- callModule(module_server_dc, input$data_rfX,  vals=vals,dataX=input$data_rfX)
  })







  ## downplot
  observeEvent(input$downp_cmrf,{
    vals$hand_plot<-"Confusion Matrix RF"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(input$downp_confrf,{
    vals$hand_plot<-"Confusion Matrix - test RF"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(input$downp_prf,{
    vals$hand_plot<-"Minimal Depth distribution"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(input$downp_mrf,{
    vals$hand_plot<-"Multi-way importance"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(input$downp_rfbi,{
    vals$hand_plot<-"RF - Partial Dependence"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(input$rfinter_downp,{
    vals$hand_plot<-"RF interactions"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(input$rfcomp_rank_downp,{
    vals$hand_plot<-"RF ranking comparations"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(input$rfcomp_meas_downp,{
    vals$hand_plot<-"RF measure comparations"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})


  ## create
  observeEvent(input$create_rfinter,{
    vals$hand_save<-"Create Datalist: RF frequent interactions"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    mod_downcenter <- callModule(module_server_dc, input$data_rfX,  vals=vals,dataX=input$data_rfX)
  })
  observeEvent(input$create_rf,{
    vals$hand_save<-"Create Datalist: RF top variables"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    mod_downcenter <- callModule(module_server_dc, input$data_rfX,  vals=vals,dataX=input$data_rfX)
  })
  observeEvent(input$create_rfreg,{
    vals$hand_save<-"Create Datalist: RF prediction errors"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
 
    mod_downcenter <- callModule(module_server_dc, input$data_rfX,  vals=vals,dataX=input$data_rfX)
  })
  observeEvent(input$create_rfreg_pred,{
    vals$hand_save<-"Create Datalist: RF predictions"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    mod_downcenter <- callModule(module_server_dc, input$data_rfX,  vals=vals,dataX=input$data_rfX)
  })
  observeEvent(input$create_rfreg_errorinter,{
    vals$hand_save<-"Create Datalist: RF training errors"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    mod_downcenter <- callModule(module_server_dc, input$data_rfX,  vals=vals,dataX=input$data_rfX)
  })


  ## downcenter
  observeEvent(input$dowcenter_cmrf,{
    vals$hand_down<-"rf_cm"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })

   observeEvent(input$downcenter_rfdepth,{
    vals$hand_down<-"rfdepth"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$downcenter_rfinter,{
    vals$hand_down<-"rfinter"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$down_rf_pred_results,{
    vals$hand_down<-"rf_predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$downcenter_intererror,{
    pic<-switch(vals$RF_results$modelType,
                "Regression"='rf_reg_obsErrs',
                "Classification" ='rf_class_obsErrs')
    vals$hand_down<-pic
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$down_rf_errors,{
    pic<-switch(vals$RF_results$modelType,
                "Regression"='rf_reg_predErrs',
                "Classification" ='rf_class_predErrs')
    vals$hand_down<-pic
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })


  predall_rf<-reactive({
    validate(need(!anyNA(test_rf()),"NAs not allowed in the prediction Datalist"))
    m<-vals$RF_results
    #factors<-attr(vals$saved_data[[input$predrf_new]],"factors")

    rf_pred <- predict(m$finalModel,newdata = test_rf(), predict.all=T)
    rf_pred
  })
  getdata_rfX <- reactive({
    req(input$data_rfX)
    data=vals$saved_data[[input$data_rfX]]
    vals$saved_labels<-attr(data, "factors")
    data
  })
  getgrad_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    grad<-names(res1[res1==F])
    pic<-which(vals$colors_img$val%in%grad)
    pic
  })
  rf_inputsmeasure<-reactive({
    if(vals$RF_results$modelType=="Classification"){
      choices=c("mean_min_depth",
                "accuracy_decrease",
                "gini_decrease" ,
                "no_of_nodes",
                "times_a_root")
    } else{
      choices=c('mean_min_depth', 'mse_increase', 'node_purity_increase', 'no_of_nodes', 'times_a_root')
    }
  })
  mindeaphrf<- reactive({
    req(length(vals$RF_results)>0)
    withProgress(message = "Running ...",
                 min = 1,
                 max = 1,
                 {
                   res <- multipimp(vals$RF_results,measures=c(input$rf_measures,'p_value','no_of_trees'), mean_sample=input$rfmeasure_mean_sample)
                 })
    res
  })
  get_supervisor <- reactive({
    req(input$rf_type)
    req(input$data_rfY)
    if (input$rf_type == 'Classification') {
      data <- vals$saved_data[[input$data_rfY]]
      labels <- attr(data,"factors")
      labels[input$rf_sup]
    } else {

      data <- vals$saved_data[[input$data_rfY]]
      data[input$rf_sup]
    }

  })
  test_rf<-reactive({
    req(input$rfpred_which)
    pred_tab<-if(input$rfpred_which=="Partition"){attr(vals$RF_results,"test")} else if(input$rfpred_which=="Datalist"){
      req(input$predrf_new)
      pred_tab<-vals$saved_data[[input$predrf_new]]
    }
    pred_tab

  })
  pred_rf<-reactive({
    validate(need(!anyNA(test_rf()),"NAs not allowed in the prediction Datalist"))
    m<-vals$RF_results
        rf_pred <- predict(m$finalModel,newdata = test_rf())
    res<-data.frame(Predictions= rf_pred)
    rownames(res)<-rownames(test_rf())
    colnames(res)<-attr(vals$RF_results,"supervisor")

    # attr(res,"obs")<-test_rf()
    res



  })
  getnewdataRF<-reactive({
    datalist<-vals$saved_data

    m<-vals$RF_results
    res00<-unlist(lapply(
      datalist, function(x){
        ncol(x)==ncol(m$trainingData[-1])
      }
    ))
    datalist<-datalist[res00==T]
    res0<-unlist(
      lapply(datalist, function (x){
        res<-colnames(x)%in%colnames(m$trainingData[-1])
        sum(res)==ncol(m$trainingData[-1])
      })
    )
    names(res0[res0==T])
  })
  getobsRF<-reactive({
    sup_test<-attr(vals$RF_results,"supervisor")

    datalist<-vals$saved_data
    if(vals$RF_results$modelType=="Classification"){
      datalist=lapply(datalist,function(x) attr(x,"factors"))}

    m<-vals$RF_results
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })
  rf_multi<-reactive({

    if(!isTRUE(input$rf_sigmulti)){
      req(input$multi_no_of_labels)
    }
    p<-suppressWarnings(
      prf_multi(
        attr(vals$RF_results,"mindepth"), sigs =if (input$rf_sigmulti == TRUE) {TRUE} else {input$multi_no_of_labels},
        input$sigmrf,
        x_measure = input$multi_x_measure,
        y_measure = input$multi_y_measure,
        max.overlaps =input$max.overlaps
      )
    )
    vals$rfm_res<-p
    p

  })
  prf.reactive <- reactive({

    if(!isTRUE(input$rf_depth)){
      req(input$n_var_rf)
    }
    mean_scale<-if(isFALSE(input$mean_scale)){F}else{T}
    res<-prf( attr(vals$RF_results,"mindepth"), sigs =     if (input$rf_depth == TRUE) { TRUE} else {input$n_var_rf},
             input$sigprf,
             size_plot=input$labrfsize,
             min_no_of_trees = input$depth_min_no_of_trees,
             mean_sample = input$depth_mean_sample,
             newcolhabs=vals$newcolhabs,
             palette=input$mdd_palette,
             size_mmd=input$size_mmd)
    res<- res +
      ggtitle(input$title_prf)+
      labs(x = input$ylab_prf, y=input$xlab_prf)


    vals$rf_sigs<-attr(res,"sigs")

    res
  })
  getrf_errors<-reactive({

    pred<-pred_rf()
    newpred<-pred[,1]
    names(newpred)<-rownames(pred)
    predicted<-newpred
    factors<-vals$saved_data[[input$predrf_new]]
    observed<-if(input$rfpred_which=="Datalist"){
      factors[,attr(vals$RF_results,"supervisor")]
    } else{
      attr(vals$RF_results,"sup_test")
    }

    accu_rf_reg(observed,predicted)


  })


  }
