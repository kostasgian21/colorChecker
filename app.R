library(shiny)
library(hues)
library(ggplot2)
library(sortable)
library(gplots)
library(dichromat)
library(plotrix)

# shinylive::export(appdir = "C:/Users/kogi/Documents/repos/QRapps/colorChecker", destdir = "docs")


desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}

# overload the swatch function from 'hues' library
swatchKG <- function (x,y=x) 
{
  if (length(x)<1) {
    x=c('violet','purple','lightblue','grey40','brown','khaki1','orange','pink','blue','green','darkgreen','grey60','red','black','yellowgreen','gold','deeppink','orange3','greenyellow','cyan','yellow','white')
    y=c('_intent','_unintent','inj_trans','_otherncd','msk','skin','diab_ckd','_subs','_mental','_neuro','digest','resp','cvd','_neo','nutrition','mater_neonat','_infect','_ntd','_enteric_all','_ri','_hiv_std','COVID-19')
  }
  barplot(rep(0.5, length(x)), col = rev(x), space = 0.01, axes = FALSE, 
          names.arg = rev(y), cex.names = 1.2, horiz = T, las = 1)
  return(invisible(NULL))
}

# # colDistance among simulated colors
# colDistance <- function (x,y,...) 
# {
#   l <- list(...)
#   if (l=="normal" || length(l)==0) {
#     return(sum((col2rgb(x)-col2rgb(y))^2))
#   }
#   return(sum((col2rgb(dichromat::dichromat(x, type = l[[1]]))-col2rgb(dichromat::dichromat(y, type = l[[1]])))^2))
# }

rank_list_basic <- rank_list(
  text = "Swap colors",
  labels = as.list(scan(text = gsub(" ",",","violet purple lightblue grey40 brown khaki1 orange pink blue green darkgreen grey60 red black yellowgreen gold deeppink orange3 greenyellow cyan yellow white"), what = "", sep = ",")),
  input_id = "rank_list_basic",
  options = sortable_options(swap = TRUE)
)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Check colour palettes"),
  
  tabsetPanel(               
    tabPanel("Play and swap",
             sidebarLayout(
               sidebarPanel(
                 rank_list_basic,width = 2
               ),
               mainPanel(
                 plotOutput("plot3"),width = 10
               )
             )
    ),
    tabPanel("Check colours",
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               sidebarPanel(
                 textInput("fn", label = "Paste colour names or hex strings.",value = "red green black"),
                 
                 helpText("Paste color names or color hex strings and separate them using commas or single spaces. You can use the paste(cols,collapse = \" \") command, where cols is a vecto rwith your colors.Eg, copy this: violet purple lightblue grey40 brown khaki1 orange pink blue green darkgreen grey60 red black yellowgreen gold deeppink orange3 greenyellow cyan yellow white"),
                 
                 textInput("fn2", label = "Paste cause labels",value = "cvd _neuro _neo"),
                 
                 helpText("Paste cause names and separate them using commas or single spaces. Do as with colours. Eg, copy this: _intent _unintent inj_trans _otherncd msk skin diab_ckd _subs _mental _neuro digest resp cvd _neo nutrition mater_neonat _infect _ntd _enteric_all _ri _hiv_std COVID-19"),
                 
                 # Horizontal line ----
                 
                 
                 sliderInput("walkers",
                             "Saturation reduction:",
                             value = 1,
                             min = 0.001,
                             max = 1),
                 helpText("Choose saturation reduction value."),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 
                 ############
                 
                 
                 
               ),
               
               
               
               
               
               
               # Main panel for displaying outputs ----
               mainPanel(
                 # Output: Tabset w/ plot, summary, and table ----
                 plotOutput("plot2"),
                 verbatimTextOutput("colsNew")
               )
               
             )
    )
  )
)




# Define server logic for random distribution app ----
server <- function(input, output) {
  
  
  output$plot2 <- renderPlot({
    par(mfrow=c(1,4))
    
    cols=gsub(" ",",",input$fn)
    cols <- scan(text = cols, what = "", sep = ",")
    
    pal <- col2hex(cols)
    cols <- desat(pal, input$walkers)
    
    yy=gsub(" ",",",input$fn2)
    yy <- scan(text = yy, what = "", sep = ",")
    
    if (length(yy) != length(cols) ) {
      yy=cols
    }
    
    
    swatchKG(cols,yy)
    swatchKG(dichromat(cols, type = "protan"),yy)
    swatchKG(dichromat(cols, type = "deutan"),yy)
    swatchKG(dichromat(cols, type = "tritan"),yy)
  })
  ########
  
  output$plot3 <- renderPlot({
    par(mfrow=c(1,4))
    cols=unlist(input$rank_list_basic)
    cols=gsub(" ",",",cols)
    cols <- scan(text = cols, what = "", sep = ",")
    
    pal <- col2hex(cols)
    cols <- desat(pal, input$walkers)
    
    yy=gsub(" ",",","_intent _unintent inj_trans _otherncd msk skin diab_ckd _subs _mental _neuro digest resp cvd _neo nutrition mater_neonat _infect _ntd _enteric_all _ri _hiv_std COVID-19")
    yy <- scan(text = yy, what = "", sep = ",")
    
    if (length(yy) != length(cols) ) {
      yy=cols
    }
    
    
    swatchKG(cols,yy)
    swatchKG(dichromat(cols, type = "protan"),yy)
    swatchKG(dichromat(cols, type = "deutan"),yy)
    swatchKG(dichromat(cols, type = "tritan"),yy)
  })
  
  output$colsNew <- renderPrint({ 
    cols=gsub(" ",",",input$fn)
    cols <- scan(text = cols, what = "", sep = ",")
    
    pal <- col2hex(cols)
    cols <- desat(pal, input$walkers)
    
    recols=sapply(cols, color.id)
    recols=sapply(recols,"[[",1)
    print("Here are the new, saturated colors")
    recols
  })
  
  
}

# Create Shiny app ----
shinyApp(ui, server)