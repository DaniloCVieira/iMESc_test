
module_ui_figs<- function(id){
  ns <- NS(id)


}

# Server
module_server_figs <- function (input, output, session,vals ){
  ns <- session$ns

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

    switch (
      vals$hand_plot,
      "Confusion Matrix RF"={plot(vals$cm_rf)},
      "Confusion Matrix - test RF"={plot(vals$conf_rf)},
      "Minimal Depth distribution"={plot(vals$rfd_res)},
      "Multi-way importance"={plot(vals$rfm_res)},
      "RF interactions"={plot(vals$rf_inter)},
      "RF - Partial Dependence"=plot(vals$rfbiplot1),
      "RF ranking comparations"=plot(rf_rank$df),
      "RF measure comparations"=plot(rf_rel$df),


      "NB - Confusion Matrix"={plot(vals$cm_nb)},
      "NB - Variable Importance plot"=plot(vals$nb_varImp_plot),
      "NB - Confusion Matrix (predictions)"=plot(vals$nb_cm_pred),
      "NB - DM plot"=replayPlot(vals$nb_densplot),

      "SVM - Confusion Matrix"={plot(vals$cm_svm)},
      "SVM - Variable Importance plot"=plot(vals$svm_varImp_plot),
      "SVM - Confusion Matrix (predictions)"=plot(vals$svm_cm_pred),
      "SVM - DM plot"=replayPlot(vals$svm_densplot)
    )


  }


  output$plotoutput <- renderImage({

    fheight <- input$fheight
    fwidth <- input$fwidth
    fres <- as.numeric(input$fres)
    png(paste0(vals$hand_plot,".png",sep=""), height=fheight, width=fwidth, res=fres, units="cm", pointsize = input$pointsize)
    switch(
      vals$hand_plot,
      "Confusion Matrix RF"={plot(vals$cm_rf)},
      "Confusion Matrix - test RF"={plot(vals$conf_rf)},
      "Minimal Depth distribution"={plot(vals$rfd_res)},
      "Multi-way importance"={plot(vals$rfm_res)},
      "RF interactions"={plot(vals$rf_inter)},
      "RF - Partial Dependence"=plot(vals$rfbiplot1),
      "RF ranking comparations"=plot(rf_rank$df),
      "RF measure comparations"=plot(rf_rel$df),


      "NB - Confusion Matrix"={plot(vals$cm_nb)},
      "NB - Variable Importance plot"=plot(vals$nb_varImp_plot),
      "NB - Confusion Matrix (predictions)"=plot(vals$nb_cm_pred),
      "NB - DM plot"=replayPlot(vals$nb_densplot),
      "SVM - Confusion Matrix"={plot(vals$cm_svm)},
      "SVM - Variable Importance plot"=plot(vals$svm_varImp_plot),
      "SVM - Confusion Matrix (predictions)"=plot(vals$svm_cm_pred),
      "SVM - DM plot"=replayPlot(vals$svm_densplot)
    )


    dev.off()

    return(list(src = paste0(vals$hand_plot,".png",sep=""),
                contentType = "image/png",
                width = round((input$fwidth*as.numeric(input$fres))/2.54, 0),
                height = round((input$fheight*as.numeric(input$fres))/2.54, 0),
                alt = "plot"))
  },deleteFile=TRUE)
  output$bn_download <- downloadHandler(
    filename = fn_downloadf,
    content = function(file) {
      fn_download()
      dev.off()
      file.copy(fn_downloadf(), file, overwrite=T)
    }
  )
  fn_downloadf <- reactive({

    if(input$fformat=="png") filename <- paste0(vals$hand_plot,".png",sep="")
    if(input$fformat=="tiff") filename <- paste0(vals$hand_plot,".tif",sep="")
    if(input$fformat=="jpeg") filename <- paste0(vals$hand_plot,".jpg",sep="")
    if(input$fformat=="pdf") filename <- paste0(vals$hand_plot,".pdf",sep="")
    return(filename)
  })

  observeEvent(input$fheight,{
    vals$fheight<-input$fheight
  })
  observeEvent(input$fwidth,{
    vals$fwidth<-input$fwidth
  })
  observeEvent(input$fres,{
    vals$fres<-input$fres
  })
  observeEvent(input$pointsize,{
    vals$pointsize<-input$pointsize
  })
  observeEvent(input$fformat,{
    vals$fformat<-input$fformat
  })




  showModal({
    if(is.null(vals$fheight)){vals$fheight<-15}
    if(is.null(vals$fwidth)){vals$fwidth<-20}
    if(is.null(vals$fres)){vals$fres<-"100"}
    if(is.null(vals$pointsize)){vals$pointsize<-12}
    if(is.null(vals$fformat)){vals$fformat<-"pdf"}
    modalDialog(



      column(12,
             splitLayout(
               numericInput(ns("fheight"), "Height (cm)", min=2, max=15, step=1, value = vals$fheight),
               numericInput(ns("fwidth"), "Width (cm)", min=2, max=15, step=1, value = vals$fwidth),
               selectInput(ns("fres"), "Res", choices=c("100","200","300"), selected = vals$fres),
               numericInput(ns("pointsize"), "pointsize",value=vals$pointsize,step=1),
               selectInput(ns("fformat"), "File type", choices=c("png","tiff","jpeg","pdf"), selected =vals$fformat, multiple = FALSE, selectize = TRUE),
               div(style="margin-top: 25px",downloadButton(ns("bn_download"),NULL, style="button_active")),

             ),

             column(12, withSpinner(type=8,color="SeaGreen",imageOutput(ns("plotoutput"))))
      ),
      title=p(strong("action:"),"download", vals$hand_plot),
      easyClose = T

    )
  })

}
