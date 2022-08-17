## Make .Rmd that explains output for each panel and then link to the relevant html.
library(stargazer)
library(DT)
library(htmltools)
library(plotly)
library(ggplot2)
library(colourpicker)

dv.names <- c("Vote Liberal"  = "vote_lib", "Vote Conservative" = "vote_con", 
              "Vote NDP" = "vote_ndp", "Vote BQ" = "vote_bloc")
blocks <- list()
blocks[[1]] <- c("gender", "agegrp", "cathol", "educ", "union", "year_fac", "province")
blocks[[2]] <- c("doquebec", "continent", "market", "moral", "cynicism", "alienreg")
blocks[[3]] <- c("pid", "pidng", "pidnb", "pidnbg")
blocks[[4]] <- c("retroper", "retrocan")
blocks[[5]] <- c("taxpers", "taxcorp", "sp_health", "sp_defence", "sp_envir", "immig")
blocks[[6]] <- c("leader_con", "leader_lib", "leader_ndp", "leader_bloc")

nblocks <- list()
nblocks[[1]] <- c("Gender", "Age Group", "Catholic", "Schooling", "Union Household", "Year", "Region")
nblocks[[2]] <- c("Do More Quebec", "Continentalism", "Market Liberalism", "Moral Tradition", "Political Cynicism", "Regional Alienation")
nblocks[[3]] <- c("Party ID", "Party ID (No Green)", "Party ID (No BQ)", "Party ID (No Green or BQ)")
nblocks[[4]] <- c("Personal Retrospective", "National Retrospective")
nblocks[[5]] <- c("Personal Tax", "Corporate Tax", "Health Spend", "Defense Spend", "Environment Spend", "More Immigration")
nblocks[[6]] <- c("Feelings: Conservative", "Feelings: Liberal", "Feelings: NDP", "Feelings: BQ")

chc <- do.call("c", blocks)
names(chc) <- do.call("c", nblocks)

val_num_vars <- c("none", "market", "moral", "cynicism", "continent", "leader_bloc", "leader_lib", "leader_ndp", "leader_con")
val_num_names <- c(" ", "Market Liberalism", "Moral Traditionalism", "Political Cynicism", "Continentalism", 
                   "Feeling Thermometer: BQ Leader", "Feeling Thermometer: Liberal Leader", 
                   "Feeling Thermometer: NDP Leader", "Feeling Thermometer: Conservative")

val_strat_vars <- c("none", "gender", "agegrp", "cathol", "educ", "union", "year", "province", "doquebec", "alienreg", "pid", "retroper", 
                    "retrocan", "taxpers", "taxcorp", "sp_health", "sp_defence", "sp_envir", "immig", "vote")
val_strat_names <- c("None", "Gender", "Age Group", "Catholic", "Schooling", "Union Household", "Year", "Region", 
                     "Do More Quebec", "Regional Anlienation", "Party ID", "Personal Retrospective Economy", 
                     "National Retrospective Economy", "Personal Tax", "Corporate Tax", "health Spending", 
                     "Defense Spending", "Environment Spending", "More Immigration?", "Vote")
num_chc <- val_num_vars
names(num_chc) <- val_num_names
strat_chc <- val_strat_vars
names(strat_chc) <- val_strat_names



shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$style(HTML("
      .dataTables_filter{
         visibility: hidden;
      }
      .dataTables_length{
         visibility: hidden;
      }              
      "))),
  
  # App title ----
#  titlePanel( 
div(style="display:inline-block; width=300px",
    p(style="font-size: 3rem; font-face: bold; line-height: 3rem; padding: 0; margin: 0", "Voting in Canada"), 
    br(), 
    p(style="font-size: 2.7rem; line-height: 2.7rem; padding: 0; margin: 0", "2004-2019"), 
    br()),
div(style="display:inline-block; width=400px; float:right",
    img(src="ces-logo-wide.png", width="300px")),
#    ),

  # Sidebar layout with input and output definitions ----
  tabsetPanel(
    tabPanel("Descriptives", 
          htmltools::tags$div(style="display:inline-block; width=150px",
            selectInput('desvar1', 'Summarise Variable', num_chc, selectize=TRUE, multiple=FALSE, selected=NULL)),
          htmltools::tags$div(style="display:inline-block; width=150px",
           selectInput('stratdes', 'Group By', strat_chc, selectize=TRUE, multiple=FALSE, selected="none")),
           #Add to your UI:
            # actionButton("browser", "browser"),
           # tags$script("$('#browser').hide();"),
           
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
           div(class = "row", 
               div(class = "column", 
                htmltools::tags$div(style="display:inline-block; width=150px",
                  selectInput('rowvar', 'Row Variable', strat_chc, selectize=TRUE, multiple=FALSE, selected="none")),
                htmltools::tags$div(style="display:inline-block; width=150px",
                  selectInput('colvar', 'Column Variable', strat_chc, selectize=TRUE, multiple=FALSE, selected="none")),
                uiOutput("xtabInst"), 
                dataTableOutput("Xtab", width=100), 
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
      selectInput('dv', 'Dependent Variable', dv.names, selectize=TRUE, selected="vote_lib"),
      radioButtons("impdv", "Imputed DV?", 
                   c("Yes"="yes", "No" = "no"), 
                   selected="yes"),
      selectInput('mod1', 'Model 1', chc, selectize=TRUE, multiple=TRUE, selected=NULL),
      selectInput('mod2', 'Model 2', chc, selectize=TRUE, multiple=TRUE, selected=NULL),
      uiOutput("varinst"), 
      radioButtons("varby", "Vary Effect by ...", 
                   c("None"="none", "Year" = "year", "Region" = "region"), 
                   selected="none"),
      checkboxGroupInput(inputId="provs", "Regions", 
          choices=c("Atlantic", "Quebec", "Ontario", "West"), 
          selected=c("Atlantic", "Quebec", "Ontario", "West")),
      checkboxGroupInput(inputId="years", "Years", 
          choices=c(2004, 2006, 2008, 2011, 2015), 
          selected=c(2004, 2006, 2008, 2011, 2015)), 
      radioButtons("type", "Effect Type", 
                   c("Average Case"="aveCase", "Average Effect" = "aveEff"), 
                   selected="aveCase"), 
      width=2),

    # Main panel for displaying outputs ----
    mainPanel(
  # Output: Tabset w/ plot, summary, and table ----
  # Add to your UI:
  # actionButton("browser", "browser"),
                  tabsetPanel(type = "tabs",
                          tabPanel("Model Output", 
                                   uiOutput("modelInst"), 
                                   uiOutput("mo1"), 
                                   tags$div(
                                     tags$br(),
                                   uiOutput("mfs"), 
                                   uiOutput("mo2"),
                                   uiOutput("modexp")
                                   
                          )),
                          tabPanel("Plot", 
                             uiOutput("plotInst"),
                             div(class="row", 
                              div(class="column", style="width:450px",
                                 plotlyOutput("p1")
                              ),
                              div(class="column", style="width:200px",
                                uiOutput("cp1"),
                                uiOutput("cp2"),
                                uiOutput("lhp"), 
                                uiOutput("lvp")
                                    )
                                  ) 
                                ), 

                         tabPanel("Predicted Probabilities", 
                             uiOutput("predprobInst"), 
                             uiOutput("ppTab1"), 
                             div(align="center", 
                             dataTableOutput("t1a", width="50%")
                             ),
                             uiOutput("ppDiff1"), 
                             div(align="center", 
                                 DTOutput("t1b", width="50%")
                             ),
                             uiOutput("ppTab2"), 
                            div(align="center",
                             DTOutput("t2a", width="50%")
                             ),
                            uiOutput("ppDiff2"), 
                             div(align="center", 
                                 DTOutput("t2b", width="50%")
                             )),
                    tabPanel("Documentation", 
                             tags$div(
                               tags$br(),
                               "The input fieldds on the left give you the ability to specify two different models of vote choice in Canadian
                               federal elections from 2004-2015 using the Canadian Election Study. Below is a description of all of the fields 
                               and how they should be used", 
                               tags$br(),
                               tags$ul(
                                 tags$li("Dependent Variable - This field allows you to choose which party's vote you are modeling.  Each model 
                                         estimates a logistic regression model of the selected party's vote relative to all other parties."), 
                                 tags$li("Imputed DV? - The data were imputed using Multiple Imputation by Chained Equations (MICE).  The results are 
                                          computed using 5 completed datasets.  This radio button allows you to decide whether the dependent variable
                                          imputations will be used.  If not, all other variables will be imputed, but those observations missing on the 
                                          dependent variable will be list-wise deleted."), 
                                 tags$li("Model 1 and Model 2 - This field allows you to specify different models of the vote, though they should contain some common 
                                         overlapping variables.  The goal here is to consider the effect of one of the common variables under different
                                         model specifications."), 
                                 tags$li("Focal Predictor - This field identifies the variable whose effect you want to compare across model specifications.  
                                         For example, if you pick 'Gender', then you will get a plot and numerical output that identifies the predicted probability
                                         of voting for the selected party for both male and female respondents."),
                                 tags$li("Vary Focal Effect By... - This field allows the effect of the focal predictor to vary by year or region.  Technically, 
                                         selecting the radio button adds an interaction term in the model between the focal predictor and either year or region.  " ),
                                 tags$li("Regions - This field allows you to limit the scope of the investigation to particular regions 
                                         within Canada.  This does not include a region variable in the model, it simply subsets the data to exclude 
                                         the un-checked regions" ),
                                 tags$li("Years - This field works like the 'Regions' field by limiting the temporal scope of your investigation to particular
                                         years.  Again, this does not add the year variable to the model, rather it subsets the data to exclude then un-checked years"), 
                                 tags$li("Effect Type - This field allows users to choose whether they want to take an 'average case' or 'marginal 
                                         effect at reasonable values (MER)' approach to calculating predicted probabilities or an 'average effect' or 
                                         'average marginal effect (AME)' approach to calculating predicted probabilities.  In either case, a 
                                         parametric bootstrap is used to calculate the confidence intervals that are presented in both the numerical 
                                         and graphical output.  See", tags$a(href="https://gvpt.umd.edu/sites/gvpt.umd.edu/files/pubs/Hanmer%20and%20Kalkan%20AJPS%20Behind%20the%20Curve.pdf", "Hanmer and Kalkan (AJPS, 2013)"), "for more details.")), 
                               tags$br(), 
                               "There are also three panels in the display.", 
                               tags$ul(
                                 tags$li("Plot - provides a graphical display of the comparison of effects.  Each model produces an effect for the
                                           selected focal predictor.  These effects amount to the predicted probability of voting for the selected party
                                           holding all other modeled factors constant.  These probabilities are indicated with points for categorical variables 
                                           and lines for continuous (quantitative) variables.  The uncertainty around these predicted probabilities are 
                                           95% confidence intervals.  For categorical variables, these are presented as lines around the points and for 
                                           continuous (quantitative) variables these are presented as colored bands around the lines." ), 
                                 tags$li("Predicted Probabilities - provides two pieces of information for each model.  First, for each model you can see
                                           the numerical values that resulted in the plots.  These are the 'Predicted Probabilities' tables.  There are 
                                            also tables that present differenes in predicted probabilities.  This allows you to assess the 
                                            statistical significance of the difference between the levels of the variables.  For categorical variables, 
                                           all pairs of values are evaluated.  For continuous variables, the maximal difference is presented - the difference
                                           between the minimum and maximum values"), 
                                 tags$li("Model Output - provides the logistic regression coefficients for both models as its main entries.  The reference categories for categorical variables 
                                         are as follows: ",includeHTML("rctab.html"), tags$br(), 
                                         "Standard errors computing accorded to Rubin's rules for combining output from multiply imputed data sets are below the coefficients
                                         in parentheses.  For model fit, the proportional reduction in error and expected proportional reduction in error are computed.  For 
                                         a more thorough description of these measures, see ",tags$a(href="https://doi.org/10.1093/oxfordjournals.pan.a029806", "Herron (PA, 1999)")),
                                 tags$li("Documentation - The page you are looking at now is the documentation tab.")
                               )
                             ))
                    ), width=10)))
  
)))
