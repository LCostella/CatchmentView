library(networkD3)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinyalert)
library(shinyBS)
library(shinyjs)
useShinyalert()

# tags$head(tags$script(src="teste.js"),
#           tags$style(HTML(".fa { font-size: 12px; }"))
# )

ui <- dashboardPage(title="CatchmentView",
  dashboardHeader(title = HTML('<img src="logo_catch2.png"  height="40" width="180">'),
                  tags$li(class = "dropdown", tags$a(
                    textOutput("Refresh1"))),
                  tags$li( HTML("<a id=\"home\" href=\"#\" class=\"action-button\">
                        <i class=\"fa fa-home\"></i> </a>"),
                    class = "dropdown"),
                  
                  tags$li(
                  HTML("<a id=\"contact\" href=\"#\" class=\"action-button\">
                        <i class=\"fa fa-envelope-o fa-fw\"></i> </a>"), class = "dropdown"),
                 
                  tags$li( HTML("<a id=\"info\" href=\"#\" class=\"action-button\">
                        <i class=\"fa fa-info-circle\"></i> </a>"),
                           class = "dropdown")
  ),
                
  
  
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "IPv4 vs IPv6",
      tabName = "dashboard",
      icon = icon("dashboard"),
      HTML("<br>"),
      selectInput(
        "select6x4",
        label = h3("Select root"),
        choices = list(
          "B" = 'B',
          "C" = 'C',
          "F" = 'F',
          "I" = 'I',
          "K" = 'K',
          "L" = 'L'
        ),
        selected = 'K'
      ),
      
      dateInput(
        "date6x4",
        label = h3("Date"),
        value = "2018/01/01",
        min = "2018/01/01",
        max = "2018/03/31"
      ),
      sliderInput(
        "slider_hours6x4",
        "Hour:",
        min = 0,
        max = 23,
        value = 12,
        step = 1
      ),
      
      menuItem(
        " Advanced Probe Selection",
        tabName = "dashboard",
        icon = icon("angle-right"),
        HTML("<br>"),
        actionBttn(
          inputId = "btn_probe",
          size = "sm",
          label = "Probe IDs",
          style = "bordered",
          icon = icon("plus"),
          color = "primary",
          block = TRUE
        ),
        actionBttn(
          inputId = "btn_asn",
          size = "sm",
          label = "ASN IDs",
          style = "bordered",
          icon = icon("plus"),
          color = "primary",
          block = TRUE
        ),
        actionBttn(
          inputId = "btn_location",
          size = "sm",
          label = "Location ",
          style = "bordered",
          icon = icon("plus"),
          color = "primary",
          block = TRUE
        ),
        HTML("<br>")
      ),
      HTML("<br>"),
      HTML("<br>"),
      #actionButton("action6x4", label = "Ready"),
      actionBttn(
        inputId = "action6x4",
        size = "sm",
        label = "Ready ",
        style = "unite",
        color = "primary",
        block = TRUE
      ),
      HTML("<br>")
    ),
    
    menuItem(
      "Temporal",
      tabName = "widgets",
      icon = icon("th"),
      selectInput(
        "selectTemp",
        label = h3("Select root"),
        choices = list(
          "B" = 'B',
          "C" = 'C',
          "F" = 'F',
          "I" = 'I',
          "K" = 'K',
          "L" = 'L'
        ),
        selected = 'K'
      ),
      
      radioButtons(
        "radioTemp",
        label = h3("IP version"),
        choices = list("IPv4" = 4, "IPv6" = 6),
        selected = 4
      ),
      
      dateInput(
        "temp",
        label = h3("Date"),
        value = "2018/01/01",
        min = "2018/01/01",
        max = "2018/03/31"
      ),
      sliderInput(
        "slider_hoursTemp",
        "Hour:",
        min = 0,
        max = 23,
        value = 12,
        step = 1
      ),
      
      menuItem(
        "Advanced Probe Selection",
        tabName = "dashboard",
        icon = icon("angle-right"),
        HTML("<br>"),
        actionBttn(
          inputId = "btn_probeTemp",
          size = "sm",
          label = "Probe IDs",
          style = "bordered",
          icon = icon("plus"),
          color = "primary",
          block = TRUE
        ),
        actionBttn(
          inputId = "btn_asnTemp",
          size = "sm",
          label = "ASN IDs",
          style = "bordered",
          icon = icon("plus"),
          color = "primary",
          block = TRUE
        ),
        actionBttn(
          inputId = "btn_locationTemp",
          size = "sm",
          label = "Location ",
          style = "bordered",
          icon = icon("plus"),
          color = "primary",
          block = TRUE
        )
      ),
      HTML("<br>"),
      HTML("<br>"),
      a(
        href = "#NEW",
        target = "_blank",
        actionBttn(
          inputId = "actionTemp",
          size = "sm",
          label = "Ready ",
          style = "unite",
          color = "primary",
          block = TRUE
        ),
        HTML("<br>")
      )
    )
  )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    useShinyjs(),
    useShinyalert(),
    #bsModal("bs","Select Groups","reloadleo", DT::dataTableOutput("groupslist"),actionButton("attlist", "OK"),tags$head(tags$style(".modal-footer{ display:none}"))),
    
    fluidRow(
      valueBoxOutput("ProbesStat"),
      valueBoxOutput("map"),
      valueBoxOutput("set")
    ),
    
    bsModal(
      'modal_bnt_probe',
      'Select probes by IDs',
      trigger = 'btn_probe',
      textAreaInput(
        "captionProbe",
        "Semi-collon separated list of IDs",
        width = "500px",
        placeholder = "Example: 9999;9898. Leave empty to use all probes"
      ),
      bsButton("okProbe", style = "primary", label = "OK"),
      bsButton("clearProbe", style = "default", label = "CLEAR")
    ),
    
    bsModal(
      'modal_bnt_asn',
      'Select Autonomous Systems by number',
      'btn_asn',
      textAreaInput(
        "captionAsn",
        "Semi-collon separated list of IDs",
        width = "500px",
        placeholder = "Example: 9999;9898. Leave empty to use all probes"
      ),
      bsButton("okAsn", style = "primary", label = "OK"),
      bsButton("clearAsn", style = "default", label = "CLEAR")
    ),
    
    bsModal(
      'modal_bnt_location',
      'Select Location Filter',
      'btn_location',
      selectInput(
        "selectLocation",
        h3("Location Filter"),
        choices = list(
          "None" = 1,
          "Globe Region" = 2,
          "Country" = 3
        ),
        selected = 1
      ),
      conditionalPanel(
        "input.selectLocation== 2 ",
        selectInput(
          'selectRegion',
          label = HTML("<p>Globe Region <a id=\"infoGlobeRegion\" href=\"#\" class=\"action-button\">
     <i class=\"fa fa-info-circle\"></i> </a></p>"),
          
          choices = list(
            "WorldWide" = 1,
            "South-East" = 2,
            "North-East" = 3,
            "South-Central" = 4,
            "North-Central" = 5,
            "West" = 6
          ),
          selected = 1
        )
      ),
      conditionalPanel(
        "input.selectLocation == '3'",
        selectizeInput(
          'countries',
          'Select countries',
          choices = countries$name,
          multiple = TRUE
        )
      ),
      
      
      bsButton("okLocation", style = "primary", label = "OK"),
      bsButton("clearLocation", style = "default", label = "CLEAR")
    ),
    
    bsModal(
      'modal_bnt_probeTemp',
      'Select probes by IDs',
      trigger = 'btn_probeTemp',
      textAreaInput(
        "captionProbeTemp",
        "Semi-collon separated list of IDs",
        width = "500px",
        placeholder = "Example: 9999;9898. Leave empty to use all probes"
      ),
      bsButton("okProbeTemp", style = "primary", label = "OK"),
      bsButton("clearProbeTemp", style = "default", label = "CLEAR")
    ),
    
    bsModal(
      'modal_bnt_asnTemp',
      'Select Autonomous Systems by number',
      'btn_asnTemp',
      textAreaInput(
        "captionAsnTemp",
        "Semi-collon separated list of IDs",
        width = "500px",
        placeholder = "Example: 9999;9898. Leave empty to use all probes"
      ),
      bsButton("okAsnTemp", style = "primary", label = "OK"),
      bsButton("clearAsnTemp", style = "default", label = "CLEAR")
    ),
    
    bsModal(
      'modal_bnt_locationTemp',
      'Select Location Filter',
      'btn_locationTemp',
      selectInput(
        "selectLocationTemp",
        h3("Location Filter"),
        choices = list(
          "None" = 1,
          "Globe Region" = 2,
          "Country" = 3
        ),
        selected = 1
      ),
      conditionalPanel(
        "input.selectLocationTemp== 2 ",
        selectInput(
          'selectRegionTemp',
          "Globe Region",
          choices = list(
            "WorldWide" = 1,
            "South-East" = 2,
            "North-East" = 3,
            "South-Central" = 4,
            "North-Central" = 5,
            "West" = 6
          ),
          selected = 1
        )
      ),
      conditionalPanel(
        "input.selectLocationTemp == '3'",
        selectizeInput(
          'countriesTemp',
          'Select countries',
          choices = countries$name,
          multiple = TRUE
        )
      ),
      
      
      bsButton("okLocationTemp", style = "primary", label = "OK"),
      bsButton("clearLocationTemp", style = "default", label = "CLEAR")
    ),
    
    fluidRow(column(
      12, tags$div(id = "space", style = "height:150px;")
    )),
    
    
    fluidRow(
      column(3),
      column(6,
             tags$div(
               id = "about",
               box(
                 id="kika",
                 verbatimTextOutput("final_text"),
                 status = "primary",
                 solidHeader = TRUE,
                 width = 12,
                 title = "CatchmentView",
                 footer = HTML("<a id='contact' href='#'> Contact us </a>"),
                 HTML("<b> This is a  beta version of a visualization tool to assist the understanding of the differences between IPv4 and IPv6 anycast services </b> <a id='info' href='#'> See more </a>")
                 
                
                                 
                          
               )
               )),
      column(3),
      tags$div(
        id = "all",
        style = "height:1000px;width:98%;float:left;margin:1%;" ,
        box(
          title = "Temporal",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          height = 1000,
          forceNetworkOutput("all", height = "1000px")
        )
      ),
      tags$div(
        id = "v4",
        style = "height:1000px;width:48%;float:left;margin:1%;" ,
        box(
          title = "IPv4",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          height = 1000,
          forceNetworkOutput("v4", height = "1000px")
        )
      ),
      tags$div(
        id = "v6",
        style = "height:1000px;width:48%;float:left;margin: 1%;",
        box(
          title = "IPv6",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          height = 1000,
          forceNetworkOutput("v6", height = "1000px")
        )
      )
               )
    
             )
  
  )
