
module_ui_downcenter <- function(id){
  ns <- NS(id)


}

# Server
module_server_downcenter <- function (input, output, session,vals ){
  ns <- session$ns
  getdown<-reactive({
    switch(vals$hand_down,
           'nb_stats_class'=data.frame(vals$nbtable_class),
           'rf_stats_class'=data.frame(vals$rftable_class),
           'rf_stats_reg'=data.frame(vals$rftable_reg),
           'svm_stats_class'=data.frame(vals$svmtable_class),
           'svm_stats_reg'=data.frame(vals$svmtable_reg),

           "rfdepth"=data.frame(attr(vals$RF_results,"mindepth")[[2]]),
           "rfinter"=data.frame(rf_interactions_frame$df),
           "rf_predictions"={data.frame( vals$rf_pred_out)},
           "rf_class_obsErrs"=data.frame(accu_rf_class(vals$RF_results)),
           "rf_reg_obsErrs"=data.frame(accu_rf_reg_model(vals$RF_results)),
           "rf_reg_predErrs"=data.frame(vals$rf_prederrors),
           "rf_class_predErrs"=data.frame(vals$rf_prederrors),
           "rf_cm"=data.frame(vals$rf_cm),
           "rf_wilcox"=data.frame(vals$rf_treetest),


           "NB - training errors (observations)"=   data.frame(vals$nb_down_errors_train),
           "NB - predictions"=data.frame(vals$nbtab_pred),
           "NB - variable importance"=data.frame(vals$nb_varImp),
           "NB - table"=data.frame(vals$nb_tableClass),
           "nb_cm"=data.frame(vals$nb_cm),

           "SVM - training errors (observations)"=   data.frame(vals$svm_down_errors_train),
           "SVM - predictions"=data.frame(vals$svmtab_pred),
           "SVM - variable importance"=data.frame(vals$svm_varImp),
           "SVM - table"=data.frame(vals$svm_tableClass),
           "svm_cm"=data.frame(vals$svm_cm)

           )
  })

  output$csv_format<-renderUI({
    req(input$down_type=='.csv')
    splitLayout(
      column(12,
             radioButtons(ns("down_sep"),strong("sep",tipify(icon("fas fa-question-circle"),"the field separator string. Values within each row of x are separated by this string.", options=list(container="body"))),
                          choiceValues =list(",",";"),
                          choiceNames =list(
                            "comma",
                            "semicolon"
                          )
             )),
      column(12,
             uiOutput(ns('down_sep_semi')) ,
             
             uiOutput(ns('down_sep_comma'))
            
      )

    )
  })
  
  cur_down_dec<-reactiveValues(df=NULL)
  observeEvent(input$down_dec,{
    cur_down_dec$df<-input$down_dec
  })
  output$down_sep_semi<-renderUI({
    req(input$down_sep==';')
    radioButtons(ns("down_dec"),strong("dec",tipify(icon("fas fa-question-circle"), "the string to use for decimal points in columns", options=list(container="body"))),
                 choiceValues =list(".",","),
                 choiceNames=list(
                   "dot","comma"), selected=cur_down_dec$df)
    
  })
  
  cur_down_dec<-reactiveValues(df=NULL)
  observeEvent(input$down_dec,{
    cur_down_dec$df<-input$down_dec
  })
  output$down_sep_comma<-renderUI({
    req(input$down_sep==',')
    column(12,
           radioButtons(ns("down_dec"),strong("dec",tipify(icon("fas fa-question-circle"), "the string to use for decimal points in columns", options=list(container="body"))),
                        choiceValues =list("."),
                        choiceNames=list("dot")
           ))
    
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

  showModal({

    modalDialog(

      column(12,
             h5(strong(vals$hand_down)),
             splitLayout(cellWidths = c("30%","70%"),
                         column(12,
                                radioButtons(ns("down_type"),strong("format",tipify(icon("fas fa-question-circle"),"file extension", options=list(container="body"))),c(".xlsx",".csv"))),
                         uiOutput(ns("csv_format"))
             ),
             column(12,
                    downloadButton(ns("download_action"),NULL,icon=icon("fas fa-download"),style="width: 50%"))

      )

      ,

      title=h4(icon("fas fa-download"),strong("Download")),
      size="m",
      easyClose = T

    )
  })

}
