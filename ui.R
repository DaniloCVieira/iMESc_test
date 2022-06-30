ui <- function(request){

  shinydashboardPlus::dashboardPage(

    skin = "blue",
    shinydashboardPlus::dashboardHeader(),


    dashboardSidebar(
      extendShinyjs(
        text = "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}",
functions = 'collapse'
      ),

useShinyjs(),
use_cicerone(),
introjsUI(),
width = 160,

sidebarMenu(   id = "tabs",
  div(

    id = "sidebar-main",class="needed",
    div(
      style = "background-color:  #05668D; font-size: 13px",
      class = "sidebar-menu",
      menuItem(
        div(
          div(img(
            src = b64,
            height = '32',
            width = '32',style ="padding: 0px; margin: 0px",
          ), "iMESc",style = "color: white; font-size: 22px; font-family: 'Alata';padding: 0px; margin: 0px"), style = "margin-left: -5px; color: white; font-size: 14px; white-space: normal; margin-top: -6px;margin-bottom: -6px"
        ),
        tabName = "menu_intro"
      ),
      menuItem(
        div(icon("fas fa-database"), HTML('&nbsp;'), "Data Bank", style = "margin-left: -8px; color: white; font-size: 14px; margin-top: -2px;margin-bottom: -2px;white-space: normal;", id="menu_data",
            class='.databank_page'),
        tabName = "menu_upload"
      ),
      menuItem(
        div(icon("fas fa-binoculars"), HTML('&nbsp;'), "Descriptive tools", style = "margin-left: -8px; color: white; font-size: 14px; margin-top: -2px;margin-bottom: -2px;white-space: normal;"),
        tabName = "menu_explore"
      ),
      menuItem(
        div(
          img(src=nb_icon,height='20',width='20'),
          HTML('&nbsp;'),
          "Naive Bayes",
          style = "margin-left: -8px; color: white; font-size: 14px; margin-top: -2px;margin-bottom: -2px;"
        ),
        tabName = "menu_nb"
      ),
      menuItem(
        div(
          img(src=svm_icon,height='20',width='20'),
          HTML('&nbsp;'),
          "Support Vector",
          p("Machine", style = "margin-left: 30px"),
          style = "margin-left: -8px; color: white; font-size: 14px; margin-top: -2px;margin-bottom: -2px;"
        ),
        tabName = "menu_svm"
      ),
      menuItem(
        div(
          icon("fas fa-braille"),
          HTML('&nbsp;'),
          "Self-Organizing",
          p("Maps", style = "margin-left: 30px"),
          style = "margin-left: -8px; color: white; font-size: 14px; margin-top: -2px;margin-bottom: -2px;"
        ),
        tabName = "menu_som"
      ),
      menuItem(
        div(
          icon("fas fa-network-wired"),
          HTML('&nbsp;'),
          "Hierarchical",
          p("Clustering", style = "margin-left: 30px"),
          style = "margin-left: -8px; color: white; font-size: 14px; margin-top: -2px;margin-bottom: -2px;"
        ),
        tabName = "menu_hc"
      ),
      menuItem(
        div(
          icon("fas fa-tree"),
          icon("fas fa-tree", style = "margin-left: -8px;"),icon("fas fa-tree", style = "margin-left: -8px;"),
          HTML('&nbsp;'),
          "Random Forest"
          ,
          style = "margin-left: -8px; color: white; font-size: 14px; margin-top: -2px;margin-bottom: -2px;"
        ),
        tabName = "menu_rf"
      ),


      menuItem(
        div(icon("fas fa-map"), HTML('&nbsp;'), "Spatial tools", style = "margin-left: -8px; color: white; font-size: 14px; margin-top: -2px;margin-bottom: -2px;"),
        tabName = "menu_maps"
      ),

      menuItem(
        div(
          icon("fas fa-pencil-ruler"),
          HTML('&nbsp;'),
          "Diversity tools",
          style = "margin-left: -8px; color: white; font-size: 14px; margin-top: -2px;margin-bottom: -2px;"
        ),
        tabName = "menu_div"
      ),
      div(
        tags$style(HTML(
          ".footer {
        margin-left: 5px;
        font-size: 12px;
             position: absolute;
             bottom: 0;
             width: 100%;
             height: 40px; /* Set the fixed height of the footer here */

           }")),
        div(class="footer",
            em("Last update:"),last_update,
            #p(em("by: Danilo C Vieira")),
            style="background: transparent; color: white"),

      )
    )


  )
)
    ),

dashboardBody(
  div(class="needed",
    fluidRow(id="header_app",
             class='myClass',' An Interactive Machine Learning App for Environmental Science')
  ),
  tags$head(tags$script(HTML(js_getid))),
  tags$head(tags$script(HTML('
        $(document).on("keydown", function (e) {
        Shiny.onInputChange("lastkeypresscode", e.keyCode);
        });
        '))),

  uiOutput('tools_upload'),
  uiOutput("change_head"),
  uiOutput("change_background"),
  div(id="main_panel",class="needed",
   

           includeCSS("styles.css"),
    tags$style(".pretty.p-default input:checked~.state label:after {background-color: SeaGreen !important;}"),
tags$script("$(document).on('click', '.name-opt a', function (evt) {
  evt.preventDefault(); Shiny.setInputValue('name',this.dataset.name);
});"
),

tags$script("$(document).on('shiny:connected', function(event) {
var myWidth = $(window).width();
Shiny.onInputChange('shiny_width',myWidth)

});"),

tags$script("$(document).on('shiny:connected', function(event) {
var myHeight = $(window).height();
Shiny.onInputChange('shiny_height',myHeight)

});"),

fluidPage(

  useShinyjs(),
  add_busy_spinner(spin = "hollow-dots",height="20px", color="yellow",width="20px", margins = c(20, 100)),


  div(id="teste",class="needed",
    style = "margin-left: -15px; margin-right:-15px;",
    uiOutput("menutitle"),
    tabsetPanel(
      type = "hidden",
      tabPanel("intro",
               tabItems(
                 tabItem(
                   tabName = "menu_intro",
                   tabsetPanel(
                     tabPanel(value="intro1",
                              strong("Welcome"),
                              textintro(),

                              column(12,
                                     column(12,
                                            #h4(strong("Take a tour",style="color: #05668D"))
                                            #bsButton(inputId = "tour_databank",label = span(img(src=tutor_icon,height='40',width='40'),"Start a tour"),style="button_active3")
                                     ))
                     ),
                     tabPanel(strong("Authors"),value="intro2", textauthors()),
                     tabPanel(strong("Workflow"),value="intro3",
                              column(12,br())
                     ),
                     tabPanel(
                       strong("Running offline"),value="intro4",
                       column(12,
                              column(12, br(), textoffline()))),
                     tabPanel(
                       strong("Version"),value="intro5",
                       column(12, style="margin-top 25px",
                              h4(style="margin-top 25px",span("iMESc", style="font-family: 'Alata', sans-serif;")),
                              p(version),
                              p(strong("Last update:"),last_update)
                              ,
                              p(em("developed by Danilo C Vieira in  R, version 4.0.5; RStudio, version 1.4, and Shiny package, version 1.6.0."))

                       )),
                     tabPanel(
                       strong("Refereces"),value="intro5",
                       column(12,
                              column(12, br(), textrefs()))),
                     tabPanel(
                       strong("Packages"),value="intro5",
                       column(12,
                              verbatimTextOutput({"package_refs"}))
                     )
                   )),
                 # training panel

                 tabItem(tabName = "menu_nb",
                         uiOutput("menu_nb_out")),
                 tabItem(tabName = "menu_svm",
                         uiOutput("menu_svm_out")),
                 tabItem(tabName = "menu_upload",
                         uiOutput("menu_bank_out")),
                 tabItem(tabName = "menu_explore",
                         uiOutput("menu_upload_out")),
                 tabItem(tabName = "menu_som",
                         column(12,uiOutput('som_datalist'))),
                 tabItem(tabName = "menu_hc",
                         uiOutput('clustering_panel')),
                 tabItem(
                   tabName = "menu_rf",
                   uiOutput('menu_rf_out')),
                 tabItem(tabName = "menu_dt",
                         uiOutput("dt_panel")
                 ),
                 tabItem(  tabName = "menu_maps",
                           column(12,uiOutput("map_panel"))

                 ),
                 tabItem(tabName = "menu_div",
                         column(12,
                                uiOutput("divtabs"))
                 )
               )
      )
    )

    )
)
  )
)
  )
}
