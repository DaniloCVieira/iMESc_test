




lapply(droplist[-which(droplist==id)], function(x){
  shinyjs::toggleDropdownButton(x)
})

hide_others<-function(id){
  req(isTRUE(last_cog()))
  lapply(droplist[-which(droplist==id)], function(x){
    shinyjs::hide(x)
  })
  toggleDropdownButton(id)
}

observeEvent(input$last_btn,{
  req(length(last_btn$equal)==2)
  if(last_btn$equal[1]==last_btn$equal[2]){
    toggled$df<-swi(toggled$df)

    last_btn$equal[1]<-"none"}
})
