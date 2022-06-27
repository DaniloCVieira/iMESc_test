module_ui_new <- function(id){

  ns <- NS(id)
  tagList(
    div(
      id=ns("data_change"),
      uiOutput(ns("pick"))

    )
  )

}
# Server
module_server_new <- function (input, output, session,vals){
  ns <- session$ns

output$pick<-renderUI({
  pickerInput(
    ns("data_upload"),NULL,choices=names(vals$saved_data), options=list(container="body","style-base" = "form-control"),
    width="200px",selected=vals$cur_data
  )
})

observeEvent(input$data_upload,{
  vals$cur_data<-input$data_upload
})

  }


