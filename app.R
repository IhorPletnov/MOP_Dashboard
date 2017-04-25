## app.R ##
library(tidyverse)
library(stringr)
library(readxl)
library(readr)
library(shiny)
library(shinydashboard)
require(ggplot2)
require(shinythemes)
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Mathematics of Poker", titleWidth = 350),
  dashboardSidebar(tags$head(tags$style(HTML('
      .main-header .logo {
                                             font-family: "Jura";
                                              color: blue;
                                             font-weight: bold;
                                             font-size: 24px;
                                             }
                                             '))),
                   width = 350,
                   sliderInput("ChenValue", label = h3("Preflop Range by Bill Chen formula"),
                                                                      post = " points",
                                                                      min = -1,
                                                                      max = 20,
                                                                      value = c(3,10)),
                   fluidRow(column(4,
                   textInput("Flop",label = h3("Flop"),
                                                   value = "4s7dQh",
                                                   width = "180px")),
                   column(4,
  textInput("Turn",label = h3("Turn"),
                                     value = "Kh",
                                     width = "70px")),
  column(4,textInput("River",label = h3("River"),
                                     value = "5s",
                                     width = "70px"))),
  hr(),
                   #radioButtons("radio", label = h3("Choose type of bet sizing"),
                    #                choices = list("Equal" = 1, "Different" = 2),
                     #               selected = 1),
                   numericInput("valuebets", label = h3("Enter # of River value bets"), value = 10),
                   #numericInput("river_pot_size", label = h3("Enter River Pot size (in BBs)"), min = 1.5, max = 200, step = 0.5, value = 7.5),
                   #hr(),
                   sliderInput("bet_sizing", label = h3("Bet size as a % of Pot"), min = 10, max = 200, value = 50, post = "%"),
  hr(),
  actionButton("DeleteRows", "Delete Rows")
  ),
  dashboardBody(fluidRow(
    valueBoxOutput("Calling_percentage"),
    valueBoxOutput("flop_bluff"),
    valueBoxOutput("turn_bluff"),
    infoBoxOutput("river_value_bet"),
      valueBoxOutput("river_bluff")
    ),
    fluidRow(
    box(title = "Hand-to-Hand Distribution", status = "success",solidHeader = TRUE, collapsible = TRUE,
        tabsetPanel(
      tabPanel("Preflop",DT::dataTableOutput("mytable_preflop")),
      tabPanel("Flop", DT::dataTableOutput("mytable_flop")),
      tabPanel("Turn", DT::dataTableOutput("mytable_turn")),
      tabPanel("River", DT::dataTableOutput("mytable_river"))
    )
    ),
    box(title = "Frequency update summary", status = "success", solidHeader = TRUE, collapsible = TRUE,
        tabsetPanel(
          tabPanel("Flop",  DT::dataTableOutput("flop_hand_groupings")),
          tabPanel("Turn",  DT::dataTableOutput("turn_hand_groupings")),
          tabPanel("River",  DT::dataTableOutput("river_hand_groupings"))
        
        ))
    )
  )
)

#library(DT)
# Define "not in" function
'%!in%' <- function(x,y)!('%in%'(x,y))

#Define ident function
#It returns FALSE if any member of the set of inputs is not identical to the others.
ident <- function(...){
  args <- c(...) 
  if( length( args ) > 2L ){
    #  recursively call ident()
    out <- c( identical( args[1] , args[2] ) , ident(args[-1]))
  }else{
    out <- identical( args[1] , args[2] )
  }    
  return( all( out ) )
}

#download datasets
starting_combos <- read.csv("data/combos.csv",stringsAsFactors = FALSE)
# filtering for UI starting_combos <- starting_combos %>% select(Hand, Preflop_value,First_card, Second_card)

primelookup <- c(2,3,5,7,11,13,17,19,23,29,31,37,41)
names(primelookup) <- c(2:9,"T","J","Q","K","A")
evaluator <- read_excel("data/evaluator.xlsx", col_names = FALSE)
names(evaluator) <- c("ranknumber", "un5card", "un6card", "un7card", "un8card", "1card","2card","3card","4card","5card","rankchrabbrev", "rankchr", "boardvalue")


# Define server logic 
server <- function(input, output) {
  
  # use reactive function that filters combos table
  card_removal_combos <- reactive({
    starting_combos %>%
      # updating data table by entered preflop range
      filter(Preflop_value >= min(input$ChenValue) & Preflop_value <= max(input$ChenValue)) %>%
      # removing Community cards
      # Flop
      filter(First_card %!in% strsplit(input$Flop, "(?<=.{2})", perl = TRUE)[[1]] & Second_card %!in% strsplit(input$Flop, "(?<=.{2})", perl = TRUE)[[1]]) %>%
      
      # Turn  
      filter(First_card %!in% strsplit(input$Turn, "(?<=.{2})", perl = TRUE)[[1]] & Second_card %!in% strsplit(input$Turn, "(?<=.{2})", perl = TRUE)[[1]]) %>%
      
      # River
      filter(First_card %!in% strsplit(input$River, "(?<=.{2})", perl = TRUE)[[1]] & Second_card %!in% strsplit(input$River, "(?<=.{2})", perl = TRUE)[[1]]) -> card_removal_combos_dataset
       
    #saving results of filtering
    #write.csv(card_removal_combos_dataset, file = "data/card_removal_combos.csv", row.names = FALSE)
    return(card_removal_combos_dataset)
  })
  
  eventReactive(input$DeleteRows,{
    if (!is.null(input$mytable_preflop_rows_selected)) {
      card_removal_combos_dataset <- values[-as.numeric(input$mytable_preflop_rows_selected),]
    }
  })
  

  
  flop_evaluated_combos <- reactive({
    # card_removal_combos_dataset <- read.csv("data/card_removal_combos.csv",stringsAsFactors = FALSE)    
    
    #concatenation of Pre-flop range and Flop
    flop_board <- strsplit(input$Flop, character(0))
    flop_board <- as.data.frame(matrix(flop_board[[1]],ncol = 6, byrow = FALSE),stringsAsFactors = FALSE)
    names(flop_board) <- c("Third_card_rank","Third_card_suit", "Fourth_card_rank","Fourth_card_suit","Fifth_card_rank","Fifth_card_suit")
    
    flop_evaluated_combos_dataset <- cbind.data.frame(card_removal_combos(),flop_board)
    
    #finding prime number value
    flop_evaluated_combos_dataset$Flop_product <- primelookup[flop_evaluated_combos_dataset$First_card_rank]*primelookup[flop_evaluated_combos_dataset$Second_card_rank]*primelookup[flop_evaluated_combos_dataset$Third_card_rank]*primelookup[flop_evaluated_combos_dataset$Fourth_card_rank]*primelookup[flop_evaluated_combos_dataset$Fifth_card_rank]
    
    #finding flop_value 
    for (i in 1:nrow(flop_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          flop_evaluated_combos_dataset$First_card_suit[i],flop_evaluated_combos_dataset$Second_card_suit[i],flop_evaluated_combos_dataset$Third_card_suit[i],flop_evaluated_combos_dataset$Fourth_card_suit[i],flop_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        flop_evaluated_combos_dataset$Flop_value[i] <- evaluator %>% filter (boardvalue == flop_evaluated_combos_dataset$Flop_product[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        flop_evaluated_combos_dataset$Flop_value[i] <- evaluator %>% filter (boardvalue == flop_evaluated_combos_dataset$Flop_product[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }
    
    #finding flop_rank
    for (i in 1:nrow(flop_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          flop_evaluated_combos_dataset$First_card_suit[i],flop_evaluated_combos_dataset$Second_card_suit[i],flop_evaluated_combos_dataset$Third_card_suit[i],flop_evaluated_combos_dataset$Fourth_card_suit[i],flop_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        flop_evaluated_combos_dataset$Flop_rank[i] <- evaluator %>% filter (boardvalue == flop_evaluated_combos_dataset$Flop_product[i] & rankchrabbrev %in% c("F","SF")) %>% select(rankchr) %>% c(),
        flop_evaluated_combos_dataset$Flop_rank[i] <- evaluator %>% filter (boardvalue == flop_evaluated_combos_dataset$Flop_product[i] & rankchrabbrev %!in% c("F","SF")) %>% select(rankchr) %>% c()
      )
    }    
    # unlisting + adding factors
    flop_evaluated_combos_dataset$Flop_value <- unlist(flop_evaluated_combos_dataset$Flop_value)
    flop_evaluated_combos_dataset$Flop_rank <- as.factor(unlist(flop_evaluated_combos_dataset$Flop_rank))
    flop_evaluated_combos_dataset$Hand <- as.factor(flop_evaluated_combos_dataset$Hand)
    #  write.csv(flop_evaluated_combos_dataset, file = "data/flop_evaluated_combos_dataset.csv", row.names = FALSE)
    return(flop_evaluated_combos_dataset)
  })
  
  # Taking into account TURN card
  turn_evaluated_combos <- reactive({
    #  flop_evaluated_combos_dataset <- read.csv("data/flop_evaluated_combos_dataset.csv",stringsAsFactors = FALSE)    
    
    #Adding Sixth card rank and suit
    turn_board <- strsplit(input$Turn, character(0))
    turn_board <- as.data.frame(matrix(turn_board[[1]],ncol = 2, byrow = FALSE),stringsAsFactors = FALSE)
    names(turn_board) <- c("Sixth_card_rank","Sixth_card_suit")
    #creating dataset
    turn_evaluated_combos_dataset <- cbind.data.frame(flop_evaluated_combos(),turn_board)
    
    #finding prime number product for all 6 5-card combos
    turn_evaluated_combos_dataset$Turn_product1 <- primelookup[turn_evaluated_combos_dataset$First_card_rank]*primelookup[turn_evaluated_combos_dataset$Second_card_rank]*primelookup[turn_evaluated_combos_dataset$Third_card_rank]*primelookup[turn_evaluated_combos_dataset$Fourth_card_rank]*primelookup[turn_evaluated_combos_dataset$Fifth_card_rank]
    turn_evaluated_combos_dataset$Turn_product2 <- primelookup[turn_evaluated_combos_dataset$First_card_rank]*primelookup[turn_evaluated_combos_dataset$Second_card_rank]*primelookup[turn_evaluated_combos_dataset$Third_card_rank]*primelookup[turn_evaluated_combos_dataset$Fourth_card_rank]*primelookup[turn_evaluated_combos_dataset$Sixth_card_rank]
    turn_evaluated_combos_dataset$Turn_product3 <- primelookup[turn_evaluated_combos_dataset$First_card_rank]*primelookup[turn_evaluated_combos_dataset$Second_card_rank]*primelookup[turn_evaluated_combos_dataset$Third_card_rank]*primelookup[turn_evaluated_combos_dataset$Sixth_card_rank]*primelookup[turn_evaluated_combos_dataset$Fifth_card_rank]
    turn_evaluated_combos_dataset$Turn_product4 <- primelookup[turn_evaluated_combos_dataset$First_card_rank]*primelookup[turn_evaluated_combos_dataset$Second_card_rank]*primelookup[turn_evaluated_combos_dataset$Sixth_card_rank]*primelookup[turn_evaluated_combos_dataset$Fourth_card_rank]*primelookup[turn_evaluated_combos_dataset$Fifth_card_rank]
    turn_evaluated_combos_dataset$Turn_product5 <- primelookup[turn_evaluated_combos_dataset$First_card_rank]*primelookup[turn_evaluated_combos_dataset$Sixth_card_rank]*primelookup[turn_evaluated_combos_dataset$Third_card_rank]*primelookup[turn_evaluated_combos_dataset$Fourth_card_rank]*primelookup[turn_evaluated_combos_dataset$Fifth_card_rank]
    turn_evaluated_combos_dataset$Turn_product6 <- primelookup[turn_evaluated_combos_dataset$Sixth_card_rank]*primelookup[turn_evaluated_combos_dataset$Second_card_rank]*primelookup[turn_evaluated_combos_dataset$Third_card_rank]*primelookup[turn_evaluated_combos_dataset$Fourth_card_rank]*primelookup[turn_evaluated_combos_dataset$Fifth_card_rank]
    
    
    # finding the highest TURN rank out of possible 6 5-card combos 
    # First combo
    for (i in 1:nrow(turn_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          turn_evaluated_combos_dataset$First_card_suit[i],turn_evaluated_combos_dataset$Second_card_suit[i],turn_evaluated_combos_dataset$Third_card_suit[i],turn_evaluated_combos_dataset$Fourth_card_suit[i],turn_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        turn_evaluated_combos_dataset$Turn_value1[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product1[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        turn_evaluated_combos_dataset$Turn_value1[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product1[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }
    
    # Second combo
    for (i in 1:nrow(turn_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          turn_evaluated_combos_dataset$First_card_suit[i],turn_evaluated_combos_dataset$Second_card_suit[i],turn_evaluated_combos_dataset$Third_card_suit[i],turn_evaluated_combos_dataset$Fourth_card_suit[i],turn_evaluated_combos_dataset$Sixth_card_suit[i])) == TRUE,
        turn_evaluated_combos_dataset$Turn_value2[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product2[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        turn_evaluated_combos_dataset$Turn_value2[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product2[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }
    
    # Third combo
    for (i in 1:nrow(turn_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          turn_evaluated_combos_dataset$First_card_suit[i],turn_evaluated_combos_dataset$Second_card_suit[i],turn_evaluated_combos_dataset$Third_card_suit[i],turn_evaluated_combos_dataset$Sixth_card_suit[i],turn_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        turn_evaluated_combos_dataset$Turn_value3[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product3[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        turn_evaluated_combos_dataset$Turn_value3[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product3[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }
    
    # Fourth combo
    for (i in 1:nrow(turn_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          turn_evaluated_combos_dataset$First_card_suit[i],turn_evaluated_combos_dataset$Second_card_suit[i],turn_evaluated_combos_dataset$Sixth_card_suit[i],turn_evaluated_combos_dataset$Fourth_card_suit[i],turn_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        turn_evaluated_combos_dataset$Turn_value4[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product4[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        turn_evaluated_combos_dataset$Turn_value4[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product4[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }
    
    
    # Fifth combo
    for (i in 1:nrow(turn_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          turn_evaluated_combos_dataset$First_card_suit[i],turn_evaluated_combos_dataset$Sixth_card_suit[i],turn_evaluated_combos_dataset$Third_card_suit[i],turn_evaluated_combos_dataset$Fourth_card_suit[i],turn_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        turn_evaluated_combos_dataset$Turn_value5[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product5[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        turn_evaluated_combos_dataset$Turn_value5[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product5[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }
    
    
    # Sixth combo
    for (i in 1:nrow(turn_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          turn_evaluated_combos_dataset$Sixth_card_suit[i],turn_evaluated_combos_dataset$Second_card_suit[i],turn_evaluated_combos_dataset$Third_card_suit[i],turn_evaluated_combos_dataset$Fourth_card_suit[i],turn_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        turn_evaluated_combos_dataset$Turn_value6[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product6[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        turn_evaluated_combos_dataset$Turn_value6[i] <- evaluator %>% filter (boardvalue == turn_evaluated_combos_dataset$Turn_product6[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }
    
    # unlisting events
    turn_evaluated_combos_dataset$Turn_value1 <- unlist(turn_evaluated_combos_dataset$Turn_value1)
    turn_evaluated_combos_dataset$Turn_value2 <- unlist(turn_evaluated_combos_dataset$Turn_value2)
    turn_evaluated_combos_dataset$Turn_value3 <- unlist(turn_evaluated_combos_dataset$Turn_value3)
    turn_evaluated_combos_dataset$Turn_value4 <- unlist(turn_evaluated_combos_dataset$Turn_value4)
    turn_evaluated_combos_dataset$Turn_value5 <- unlist(turn_evaluated_combos_dataset$Turn_value5)
    turn_evaluated_combos_dataset$Turn_value6 <- unlist(turn_evaluated_combos_dataset$Turn_value6)
    
    # finding Highest rank
    for (i in 1:nrow(turn_evaluated_combos_dataset)) {
      turn_evaluated_combos_dataset$Turn_value_max[i] <- min(turn_evaluated_combos_dataset$Turn_value1[i],turn_evaluated_combos_dataset$Turn_value2[i],turn_evaluated_combos_dataset$Turn_value3[i],turn_evaluated_combos_dataset$Turn_value4[i],turn_evaluated_combos_dataset$Turn_value5[i],turn_evaluated_combos_dataset$Turn_value6[i])
    }
    # finding name of the hand
    for (i in 1:nrow(turn_evaluated_combos_dataset)) {
      turn_evaluated_combos_dataset$Turn_rank_max[i] <- evaluator %>% filter( ranknumber == turn_evaluated_combos_dataset$Turn_value_max[i]) %>% select (rankchr) %>% c()
    }
    # unlisting events
    turn_evaluated_combos_dataset$Turn_value_max <- unlist(turn_evaluated_combos_dataset$Turn_value_max)
    turn_evaluated_combos_dataset$Turn_rank_max <- as.factor(unlist(turn_evaluated_combos_dataset$Turn_rank_max))

    turn_evaluated_combos_dataset$Hand <- as.factor(turn_evaluated_combos_dataset$Hand)   
     #    write.csv(evaluated_combos_dataset, file = "data/evaluated_combos_dataset.csv", row.names = FALSE)
    return(turn_evaluated_combos_dataset)
  })
  
  
  #----------TAKING INTO ACCOUNT RIVER CARD
  river_evaluated_combos <- reactive({
    #Adding Sevent card rank and suit
    river_board <- strsplit(input$River, character(0))
    river_board <- as.data.frame(matrix(river_board[[1]],ncol = 2, byrow = FALSE),stringsAsFactors = FALSE)
    names(river_board) <- c("Seventh_card_rank","Seventh_card_suit")
    #creating dataset
    river_evaluated_combos_dataset <- cbind.data.frame(turn_evaluated_combos(),river_board)
    
    #finding prime number product for all 21 5-card combos
    river_evaluated_combos_dataset$River_product1 <- primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    river_evaluated_combos_dataset$River_product2 <- primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Sixth_card_rank]
    river_evaluated_combos_dataset$River_product3 <- primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]
    river_evaluated_combos_dataset$River_product4 <- primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]*primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    river_evaluated_combos_dataset$River_product5 <- primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    river_evaluated_combos_dataset$River_product6 <- primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    river_evaluated_combos_dataset$River_product7 <- primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    river_evaluated_combos_dataset$River_product8 <- primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    river_evaluated_combos_dataset$River_product9 <- primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    river_evaluated_combos_dataset$River_product10 <- primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    river_evaluated_combos_dataset$River_product11 <- primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    
    river_evaluated_combos_dataset$River_product12 <- primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]
    river_evaluated_combos_dataset$River_product13 <- primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]
    river_evaluated_combos_dataset$River_product14 <- primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    river_evaluated_combos_dataset$River_product15 <- primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]
    river_evaluated_combos_dataset$River_product16 <- primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]
    river_evaluated_combos_dataset$River_product17 <- primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]
    river_evaluated_combos_dataset$River_product18 <- primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]
    
    river_evaluated_combos_dataset$River_product19 <- primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$First_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    river_evaluated_combos_dataset$River_product20 <- primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$Second_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    river_evaluated_combos_dataset$River_product21 <- primelookup[river_evaluated_combos_dataset$Sixth_card_rank]*primelookup[river_evaluated_combos_dataset$Seventh_card_rank]*primelookup[river_evaluated_combos_dataset$Third_card_rank]*primelookup[river_evaluated_combos_dataset$Fourth_card_rank]*primelookup[river_evaluated_combos_dataset$Fifth_card_rank]
    
    # finding the highest RIVER rank out of possible 21 5-card combos 
    # First combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value1[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product1[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value1[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product1[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    } 
    # Second combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Sixth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value2[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product2[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value2[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product2[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    } 
    
    # Third combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value3[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product3[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value3[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product3[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    } 
    
    # Fourth combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i],river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value4[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product4[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value4[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product4[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }     
    
    # Fifth combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value5[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product5[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value5[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product5[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }
    # Sixth combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value6[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product6[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value6[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product6[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    } 
    # Seventh combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value7[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product7[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value7[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product7[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }  
    # 8th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value8[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product8[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value8[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product8[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    } 
    # 9th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value9[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product9[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value9[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product9[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    } 
    # 10th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value10[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product10[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value10[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product10[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    } 
    # 11th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value11[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product11[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value11[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product11[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    } 
    # 12th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value12[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product12[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value12[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product12[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    } 
    # 13th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value13[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product13[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value13[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product13[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }  
    # 14th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value14[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product14[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value14[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product14[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }    
    # 15th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value15[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product15[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value15[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product15[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    } 
    # 16th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value16[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product16[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value16[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product16[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }   
    # 17th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value17[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product17[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value17[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product17[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }   
    # 18th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value18[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product18[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value18[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product18[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }     
    # 19th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$First_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value19[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product19[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value19[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product19[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }   
    # 20th combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$Second_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value20[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product20[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value20[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product20[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }
    # 21st combo
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      ifelse(ident(
        union_all(
          river_evaluated_combos_dataset$Sixth_card_suit[i],river_evaluated_combos_dataset$Seventh_card_suit[i],river_evaluated_combos_dataset$Third_card_suit[i],river_evaluated_combos_dataset$Fourth_card_suit[i],river_evaluated_combos_dataset$Fifth_card_suit[i])) == TRUE,
        river_evaluated_combos_dataset$River_value21[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product21[i] & rankchrabbrev %in% c("F","SF")) %>% select(ranknumber) %>% c(),
        river_evaluated_combos_dataset$River_value21[i] <- evaluator %>% filter (boardvalue == river_evaluated_combos_dataset$River_product21[i] & rankchrabbrev %!in% c("F","SF")) %>% select(ranknumber) %>% c()
      )
    }
    # unlisting events
    river_evaluated_combos_dataset$River_value1 <- unlist(river_evaluated_combos_dataset$River_value1)
    river_evaluated_combos_dataset$River_value2 <- unlist(river_evaluated_combos_dataset$River_value2)
    river_evaluated_combos_dataset$River_value3 <- unlist(river_evaluated_combos_dataset$River_value3)
    river_evaluated_combos_dataset$River_value4 <- unlist(river_evaluated_combos_dataset$River_value4)
    river_evaluated_combos_dataset$River_value5 <- unlist(river_evaluated_combos_dataset$River_value5)
    river_evaluated_combos_dataset$River_value6 <- unlist(river_evaluated_combos_dataset$River_value6)
    river_evaluated_combos_dataset$River_value7 <- unlist(river_evaluated_combos_dataset$River_value7)
    river_evaluated_combos_dataset$River_value8 <- unlist(river_evaluated_combos_dataset$River_value8)
    river_evaluated_combos_dataset$River_value9 <- unlist(river_evaluated_combos_dataset$River_value9)
    river_evaluated_combos_dataset$River_value10 <- unlist(river_evaluated_combos_dataset$River_value10)
    river_evaluated_combos_dataset$River_value11 <- unlist(river_evaluated_combos_dataset$River_value11)
    river_evaluated_combos_dataset$River_value12 <- unlist(river_evaluated_combos_dataset$River_value12)
    river_evaluated_combos_dataset$River_value13 <- unlist(river_evaluated_combos_dataset$River_value13)
    river_evaluated_combos_dataset$River_value14 <- unlist(river_evaluated_combos_dataset$River_value14)
    river_evaluated_combos_dataset$River_value15 <- unlist(river_evaluated_combos_dataset$River_value15)
    river_evaluated_combos_dataset$River_value16 <- unlist(river_evaluated_combos_dataset$River_value16)
    river_evaluated_combos_dataset$River_value17 <- unlist(river_evaluated_combos_dataset$River_value17)
    river_evaluated_combos_dataset$River_value18 <- unlist(river_evaluated_combos_dataset$River_value18)
    river_evaluated_combos_dataset$River_value19 <- unlist(river_evaluated_combos_dataset$River_value19)
    river_evaluated_combos_dataset$River_value20 <- unlist(river_evaluated_combos_dataset$River_value20)
    river_evaluated_combos_dataset$River_value21 <- unlist(river_evaluated_combos_dataset$River_value21)
    
    # finding Highest rank
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      river_evaluated_combos_dataset$River_value_max[i] <- min(river_evaluated_combos_dataset$River_value1[i],
                                                               river_evaluated_combos_dataset$River_value2[i],
                                                               river_evaluated_combos_dataset$River_value3[i],
                                                               river_evaluated_combos_dataset$River_value4[i],
                                                               river_evaluated_combos_dataset$River_value5[i],
                                                               river_evaluated_combos_dataset$River_value6[i],
                                                               river_evaluated_combos_dataset$River_value7[i],
                                                               river_evaluated_combos_dataset$River_value8[i],
                                                               river_evaluated_combos_dataset$River_value9[i],
                                                               river_evaluated_combos_dataset$River_value10[i],
                                                               river_evaluated_combos_dataset$River_value11[i],
                                                               river_evaluated_combos_dataset$River_value12[i],
                                                               river_evaluated_combos_dataset$River_value13[i],
                                                               river_evaluated_combos_dataset$River_value14[i],
                                                               river_evaluated_combos_dataset$River_value15[i],
                                                               river_evaluated_combos_dataset$River_value16[i],
                                                               river_evaluated_combos_dataset$River_value17[i],
                                                               river_evaluated_combos_dataset$River_value18[i],
                                                               river_evaluated_combos_dataset$River_value19[i],
                                                               river_evaluated_combos_dataset$River_value20[i],
                                                               river_evaluated_combos_dataset$River_value21[i])
    }
    
    # finding name of the hand
    for (i in 1:nrow(river_evaluated_combos_dataset)) {
      river_evaluated_combos_dataset$River_rank_max[i] <- evaluator %>% filter( ranknumber == river_evaluated_combos_dataset$River_value_max[i]) %>% select (rankchr) %>% c()
    }
    # unlisting events + making factor
    river_evaluated_combos_dataset$River_value_max <- unlist(river_evaluated_combos_dataset$River_value_max)
    river_evaluated_combos_dataset$River_rank_max <- as.factor(unlist(river_evaluated_combos_dataset$River_rank_max))
    
    # making Hand column variable as factor
    river_evaluated_combos_dataset$Hand <- as.factor(river_evaluated_combos_dataset$Hand)
    return(river_evaluated_combos_dataset)   
  })
  

  
  # RENDERING DATATABLES---------------------
  #do not forget braces while using reactive functions
  output$mytable_preflop <- DT::renderDataTable(
    data <- select(card_removal_combos(),First_card, Second_card, Hand, Preflop_value),extensions = 'KeyTable',
    options = list(keys = TRUE, order = list(list(4,"desc")))
  )
  output$mytable_flop <- DT::renderDataTable(
    data <- select(flop_evaluated_combos(),First_card, Second_card, Hand, Flop_value,Flop_rank),extensions = 'KeyTable',
    options = list(keys = TRUE,order = list(list(4,"asc")))
  )
  output$mytable_turn <- DT::renderDataTable(
    data <- select(turn_evaluated_combos(),First_card, Second_card, Hand, Turn_value_max, Turn_rank_max),extensions = 'KeyTable',
    options = list(keys = TRUE,order = list(list(4,"asc")))
  )
  output$mytable_river <- DT::renderDataTable(
    data <- select(river_evaluated_combos(),First_card, Second_card, Hand, River_value_max, River_rank_max),extensions = 'KeyTable',
    options = list(keys = TRUE,order = list(list(4,"asc")))
  )
  output$flop_hand_groupings <- DT::renderDataTable(
    data <- flop_evaluated_combos() %>% group_by(Flop_value,Hand,Flop_rank) %>% summarise(Combo_count = n()),extensions = 'KeyTable',
    filter = "top",options = list(keys = TRUE)#,rownames = FALSE
  )
    output$turn_hand_groupings <- DT::renderDataTable(
    data <- turn_evaluated_combos() %>% group_by(Turn_value_max,Hand,Turn_rank_max) %>% summarise(Combo_count = n()),extensions = 'KeyTable',
    filter = "top",options = list(keys = TRUE)#,rownames = FALSE
  )
  
    output$river_hand_groupings <- DT::renderDataTable(
    data <- river_evaluated_combos() %>% group_by(River_value_max,Hand,River_rank_max) %>% summarise(Combo_count = n()), extensions = 'KeyTable',
    filter = "top",options = list(keys = TRUE)#,rownames = FALSE
  )
    
    # VALUE BOXES ------------------
    output$Calling_percentage <- renderValueBox({
      valueBox(subtitle = "of hands that beat a bluff",paste(round(1/(1+input$bet_sizing/100)*100,0), "% CALL" ),icon = icon("thumbs-up"),color = "olive")
    })
    output$river_bluff <- renderValueBox({
      valueBox(subtitle = "",paste(round(input$bet_sizing/(100+input$bet_sizing)*input$valuebets,0), "River Bluffs" ),icon = icon("hand-o-right"),color = "yellow")
      })
    output$turn_bluff <- renderValueBox({
      valueBox(subtitle = "",paste(round((input$bet_sizing*2+100)/(100+input$bet_sizing)*(input$bet_sizing/(100+input$bet_sizing))*input$valuebets,0), "Turn Bluffs" ),icon = icon("hand-o-right"),color = "purple")
    })
    output$flop_bluff <- renderValueBox({
      valueBox(subtitle = "",paste(round(((input$bet_sizing*2+100)/(100+input$bet_sizing))**2*(input$bet_sizing/(100+input$bet_sizing))*input$valuebets,0), "Flop Bluffs" ),icon = icon("hand-o-right"),color = "teal")
    })
    output$river_value_bet <- renderValueBox({
      valueBox(subtitle = "",paste(input$valuebets, "River Value Bets"),icon = icon("hand-o-right"),color = "navy")
    })

  
    }


shinyApp(ui, server)