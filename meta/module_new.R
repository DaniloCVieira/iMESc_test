module_ui_new <- function(id){

  ns <- NS(id)
  tagList(
    div(
      id=ns("data_change"),
      pickerInput(
        ns("data_upload"),NULL,choices=names(vals$saved_data),  selected=vals$cur_data, options=list(container="body","style-base" = "form-control"),
        width="200px"
      )

    )
  )

}

# Server
module_server_new <- function (input, output, session,vals,newcolhabs ){

  observeEvent(input$data_upload,{
    vals$cur_data<-input$data_upload
  })

  }

getwd()
