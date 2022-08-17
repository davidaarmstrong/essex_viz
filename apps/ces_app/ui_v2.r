## Make .Rmd that explains output for each panel and then link to the relevant html.
library(stargazer)
library(DT)
library(htmltools)
library(plotly)
library(ggplot2)
library(colourpicker)
library(rintrojs)


dv.names <- c("Vote Liberal"  = "vote_lib", "Vote Conservative" = "vote_con", 
              "Vote NDP" = "vote_ndp", "Vote BQ" = "vote_bloc", 
              "Vote Green" = "vote_green", "Vote for Incumbent" = "vote_incumbent", 
              "Vote Turnout" = "turnout")
blocks <- list()
blocks[[1]] <- c("gender", "agegrp", "cathol", "relig", "educ", "union", "year_fac", "province")
blocks[[2]] <- c("continent", "market", "moral", "cynicism")
blocks[[3]] <- c("pid", "pidng", "pidnb", "pidnbg")
blocks[[4]] <- c("retroper", "retrocan")
blocks[[5]] <- c("sp_defence", "sp_envir", "immig", "usties", "jobspriv", 
                 "blame", "poorgap", "stayhome", "dowomen")
blocks[[6]] <- c("leader_con", "leader_lib", "leader_ndp", "leader_bloc", "leader_incumbent")

nblocks <- list()
nblocks[[1]] <- c("Gender", "Age Group", "Catholic", "Religion", "Schooling", "Union Household", "Year", "Region")
nblocks[[2]] <- c("Continentalism", "Market Liberalism", "Moral Tradition", "Political Cynicism")
nblocks[[3]] <- c("Party ID", "Party ID (No Green)", "Party ID (No BQ)", "Party ID (No Green or BQ)")
nblocks[[4]] <- c("Personal Retrospective", "National Retrospective")
nblocks[[5]] <- c("Defense Spend", "Environment Spend", "More Immigration",
                  "Ties to USA", "Private Sector Create Jobs", "Blame Self for Failure", 
                  "Reduce Income Inequality", "Women Stay Home", "Done for Women")
nblocks[[6]] <- c("Feelings: Conservative", "Feelings: Liberal", "Feelings: NDP", "Feelings: BQ", "Feelings: Incumbent Leader")
b <- do.call("c", blocks)
n <- do.call("c", nblocks)

chc <- do.call("c", blocks)
names(chc) <- do.call("c", nblocks)
chc <- chc[order(names(chc))]



val_num_vars <- c("none", "market", "moral", "cynicism", "continent", 
                  "leader_bloc", "leader_lib", "leader_ndp", "leader_con")
val_num_names <- c(" ", "Market Liberalism", "Moral Traditionalism", "Political Cynicism", "Continentalism", 
                   "Feeling Thermometer: BQ Leader", "Feeling Thermometer: Liberal Leader", 
                   "Feeling Thermometer: NDP Leader", "Feeling Thermometer: Conservative")

val_strat_vars <- c("none", "gender", "agegrp", "cathol", "relig", "educ", 
                    "union", "year", "province", "pid", "retroper", 
                    "retrocan", "sp_defence", "sp_envir", "immig", "vote", 
                    "usties", "jobspriv", "blame", "poorgap", "stayhome", "dowomen")
val_strat_names <- c("None", "Gender", "Age Group", "Catholic", "Religion", "Schooling", 
                     "Union Household", "Year", "Region", "Party ID", "Personal Retrospective Economy", 
                     "National Retrospective Economy", "Defense Spending", "Environment Spending", "More Immigration?", "Vote", 
                     "Ties to USA", "Private Sector Create Jobs", "Blame Self for Failure", 
                     "Reduce Income Inequality", "Women Stay Home", "Done for Women")
num_chc <- val_num_vars
names(num_chc) <- val_num_names
strat_chc <- val_strat_vars
names(strat_chc) <- val_strat_names
num_chc <- num_chc[order(names(num_chc))]
strat_chc <- strat_chc[order(names(strat_chc))]
wsc.none <- which(strat_chc == "none")
strat_chc <- strat_chc[c(wsc.none, (1:length(strat_chc))[-wsc.none])]


shinyUI(fluidPage(
  introjsUI(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$style(HTML("
      .dataTables_filter{
         visibility: hidden;
      }
      .dataTables_length{
         visibility: hidden;
      }              
      #plotw{
        width:75px;
      }
      #ploth{
        width:75px;
      }
      #xtoffw{
        width:75px;
      }
      #xtoffh{
        width:75px;
      }
      #ytoffw{
        width:75px;
      }
      #ytoffh{
        width:75px;
      }
      #rotlabs{
        width:150px;
      }
      #xtitle{
        width:150px;
      }
      #ytitle{
        width:150px;
      }
      #plotCtrls{
        opacity: 0;
      }
      table{
        font-size: 2rem;  
      }
      h3, .h3 {
        font-size: 2.15rem;
        font-weight: bold;
      }
.myButton {
	box-shadow:inset 0px 1px 3px 0px #3dc21b;
	background:linear-gradient(to bottom, #44c767 5%, #5cbf2a 100%);
	background-color:#44c767;
	border-radius:5px;
	border:1px solid #18ab29;
	display:inline-block;
	cursor:pointer;
	color:#ffffff;
	font-family:Arial;
	font-size:1.75rem;
	font-weight:bold;
	padding:7px 7px;
	text-decoration:none;
	text-shadow:0px -1px 0px #2f6627;
}
.myButton:hover {
	background:linear-gradient(to bottom, #5cbf2a 5%, #44c767 100%);
	background-color:#5cbf2a;
}
.myButton:active {
	position:relative;
	top:1px;
}
    #stratdes, #desvar, #nbins, #dv, #rowvar, #colvar, .option{
      font-size: 1.75rem;
    }
    .selectize-input{
      padding: 10px 10px; 
    }
    .introjs-tooltip {
      max-width: 650px;
      min-width: 350px;
      font-size: 2rem;
    }
    .introjs-disabled {
     display: none !important;
    }
    ")), 
      tags$script('
  function myFunction() {
    var i;
    var x = document.getElementById("plotCtrls");
    var y = document.querySelector("#ctrlButton");
    if (x.style.opacity == 0) {
      x.style.opacity = 1;
      y.innerText = "Hide Plot Controls"
    } else {
      x.style.opacity = 0;
      y.innerText = "Show Plot Controls"
    }
  }; 
  
  window.addEventListener("load", function() {
    let checkbox = document.getElementById("cmactive");
    checkbox.addEventListener("change", function() {
      if(this.checked) {
          this.parentElement.style.display="none";
      } else {
      }
    });
  });
  var checkbox = document.getElementById("cmactive");
  checkbox.addEventListener( "change", function() {
});
')),
  
  # App title ----
#  titlePanel( 
div(style="display:inline-block; width:250px; text-align: center; vertical=align: middle",
    a(href="https://en.wikipedia.org/wiki/File:Can-vote-stub.svg", img(src="vote-icon.png", height="100px"))),
div(style="display:inline-block; width=400px; float:right",
    img(src="ces-logo-wide.png", width="300px")),
#    ),

  # Sidebar layout with input and output definitions ----
  
  tabsetPanel(
    tabPanel("Descriptives", 
              div(style="display:inline-block; width=225px; vertical-align: middle",
              selectInput('desvar', 'Summarise Variable', num_chc, selectize=TRUE, multiple=FALSE, selected=NULL, width = "350px")),
          div(style="display:inline-block; width=225px; vertical-align: middle",
              selectInput('stratdes', 'Group By', strat_chc, selectize=TRUE, multiple=FALSE, selected="none", width = "350px")), 
          div(style="display:inline-block; width=225px; vertical-align: middle",
                              actionButton("btn","Walk Me Through", class="myButton")),                  
           uiOutput("desInst"),   
           dataTableOutput("Descriptive", width=100), 
           htmltools::tags$br(), 
          div(class="row", 
              div(class="column", 
                plotlyOutput("descPlot", width=600, height=400)), 
                uiOutput("binInput"), 
              div(class="column", style="width: 50px"), 
          div(class="column",
              plotOutput("facplot", width=400, height=400))), 
          uiOutput("sumhr"), 
          uiOutput("sumexp")
          
  ),
  

  
  tabPanel("Cross-Tabs", 
            div(style="display:inline-block; width=225px; vertical-align:middle",
              selectInput('rowvar', 'Row Variable', strat_chc, selectize=TRUE, multiple=FALSE, selected="none", width="350px")),
            div(style="display:inline-block; width=225px; vertical-align:middle",
              selectInput('colvar', 'Column Variable', strat_chc, selectize=TRUE, multiple=FALSE, selected="none", width="350px")),
            div(style="display:inline-block; width=225px; vertical-align:middle",
                actionButton("btn2","Walk Me Through", class="myButton")),                  
             uiOutput("xtabInst"), 
              br(), 
              div(class="row", 
                    div(class="column", 
                        dataTableOutput("Xtab"), 
                        htmltools::br(), 
                        uiOutput("chisq")),
               div(class = "column", 
                   plotlyOutput("mosPlot", width=600, height=600)
                   )),
            # uiOutput("sumhr")
            uiOutput("xtexp")
  ),
  tabPanel("Models", 
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select the random distribution type ----
      #Add to your UI:
      #actionButton("browser", "browser"),
      # tags$script("$('#browser').hide();"),
      div(style="text-align: center; padding-bottom:20px", actionButton("btn3","Walk Me Through", class="myButton")),
      selectInput('dv', 'Dependent Variable', dv.names, selectize=FALSE, selected="vote_lib"),
      selectInput('mod1', 'Model 1', chc, selectize=TRUE, multiple=TRUE, selected=NULL),
      checkboxInput('cmactive', 'Add Comparison Model', FALSE),
      uiOutput("compmod"), 
      uiOutput("varinst"), 
      radioButtons("varby", "Vary Effect by ...", 
                   c("None"="none", "Year" = "year", "Region" = "region"), 
                   selected="none"),
      div(id="subset", 
      checkboxGroupInput(inputId="provs", "Regions", 
          choices=c("Atlantic", "Quebec", "Ontario", "West"), 
          selected=c("Atlantic", "Quebec", "Ontario", "West")),
      checkboxGroupInput(inputId="years", "Years", 
          choices=c(2004, 2006, 2008, 2011, 2015, 2019), 
          selected=c(2004, 2006, 2008, 2011, 2015, 2019))), 
      # radioButtons("type", "Effect Type", 
      #              c("Average Case"="aveCase", "Average Effect" = "aveEff"), 
      #              selected="aveCase"), 
      width=2),

    # Main panel for displaying outputs ----
    mainPanel(
  # Output: Tabset w/ plot, summary, and table ----
  # Add to your UI:
  # actionButton("browser", "browser"),
                  tabsetPanel(type = "tabs",
                          tabPanel("Model Output", 
                                   uiOutput("modelInst"), 
                                   div(align="center", id="modOutput",  
                                       uiOutput("mo1"),
                                       br(), 
                                       uiOutput("mfs"), 
                                       uiOutput("mo2")
                                   ), 
                                   uiOutput("modexp")
                                   
                          ),
                          tabPanel("Plot", 
                             uiOutput("plotInst"),
                             div(class="row", 
                              div(class="column", style="display:inline-block; min-width: 650px",
                                 plotlyOutput("p1")
                              ),
                              div(class="column", style="width:200px",
                                uiOutput("btnOutput"),
                                div(id = "plotCtrls",
                                div(id = "colPickers", 
                                  uiOutput("cp1"),
                                  uiOutput("cp2")),
                                div(id="legPos", 
                                  uiOutput("lhp"), 
                                  uiOutput("lvp")),
                                uiOutput("plotdimx"),
                                div(class="row", id="plotSizeCtrl", style="width:200px padding:0 margin:0", 
                                    HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
                                    uiOutput("ploth"),
                                    HTML("&nbsp;"),
                                    uiOutput("plotw")),
                                uiOutput("xtoffdimx"),
                                div(class="row", id="xto", style="width:200px padding:0 margin:0", 
                                    HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
                                    uiOutput("xtoffh"),
                                    HTML("&nbsp;"),
                                    uiOutput("xtoffw")),
                                uiOutput("ytoffdimx"),
                                div(class="row", id = "yto", style="width:200px padding:0 margin:0", 
                                    HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
                                    uiOutput("ytoffh"),
                                    HTML("&nbsp;"),
                                    uiOutput("ytoffw")), 
                                uiOutput("rotlabs"),
                                div(id="axTitles", 
                                uiOutput("xtitle"), 
                                uiOutput("ytitle")))
                                  ) 
                                )), 

                         tabPanel("Predicted Probabilities", 
                             uiOutput("predprobInst"), 
                             div(id="pp1a", style="width: 100%; min-height:350px; display:block",  
                             uiOutput("ppTab1"), 
                             div(align="center", 
                             dataTableOutput("t1a", width="50%")
                             )),
                             div(id="pp1b", 
                               uiOutput("ppDiff1"), 
                               div(align="center", 
                                 DTOutput("t1b", width="80%")
                             )),
                             uiOutput("ppTab2"), 
                            div(align="center",
                             DTOutput("t2a", width="50%")
                             ),
                            uiOutput("ppDiff2"), 
                             div(align="center", 
                                 DTOutput("t2b", width="80%")
                             ))
                    ), 
  width=10))), 
  tabPanel("Documentation", 
  includeMarkdown("Documentation.md")             
)), 
hr(), 
HTML("<p>This app was developed as a collaborative effort between the Centre for 
Computational and Quantitative Social Science and the Consortium on Electorial 
Democracy.  This is meant as a teaching tool for students who may not have the 
technical skills to do the analysis on their own.  We do feel that the decisions 
made are consistent with current best-practices for statistical analysis.  The 
Documentation panel provides more information about the methods used and decisions
made.  If you have questions please contact 
<a href='mailto:dave.armstrong@uwo.ca'>Dave Armstrong</a> or 
<a href='mailto:laura.stephenson@uwo.ca'>Laura Stephenson</a></p>")
))
