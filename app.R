library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(dbplyr)
library(bslib)
library(DT)

# Minimum requirement: the SQLite database below must be built using
# the template called "skeleton.sql" and provided alongside with this app: run
# $ cat skeleton.sql | sqlite3 QA.db # on the commandline,
# and this app can run!

sqlitePath <- "QA.db"

table_of_questions <- "Questions"
questionFields <- c("team", "persona", "topic", "question")


################################################
##### TO EDIT: PERSONAS, TEAMS AND TOPICS ######
################################################

personas <- c("smallholder farmer", "extension agent", "policy maker", "business investor", "researcher")

teams <- c("Simba", "Nyama choma", "Mazingira", "Twiga", "Borana", "Livestock bots")

current_topics <- c("access to livestock markets and livelihoods",
                    "estrus detection in dairy cattle",
                    "future foods opportunities",
                    "youth access to livestock business opportunities",
                    "climate change and resilience",
                    "milk yield",

"livestock trade in Horn of Africa",
                    "mastitis prevention",
                    "better breed selection",
                    "use of indigenous knowledge",
                    "vaccine strategies and access",
                    "gender considerations in food systems",

"insurance subsidy",
                    "early warning for droughts and floods",
                    "data privacy",
                    "rangeland restoration",
                    "GHG emission factors",
                    "natural supplements for livestock growth",


"artificial insemination",
                    "digital public infrastructure",
                    "adaptation and resilience to climate change impacts",
                    "access to financial resources",
                    "peace building mechanisms in rangelands",
                    "mobile and extension services",

"animal movement licensing",
                    "climate change forecast",
                    "access to pasture, water and livestock infrastructure",
                    "reproduction - breeding success",
                    "safety of livestock vaccines",
                    "why livestock matters",

"biosecurity and disease prevention",
                    "animal diseases and epidemiology",
                    "farm business plans and records",
                    "improved fodder production for sustainable livestock productivity and environment",
                    "successful and efficient housing for livestock - animal welfare",
                    "marketing and processing (adding value)")



####################################
##### LOADING DATA FROM THE DB #####
####################################

loadData <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table_of_questions)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

loadUnansweredQuestionsAsTibble <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Submit the fetch query and disconnect
  tbl(db, "Questions") |> filter(answered == 0) |> collect() -> data
  dbDisconnect(db)
  data
}

####################################
##### SAVING DATA INTO THE DB ######
####################################

saveQuestion <- function(data) {
  # data here is a named character vector, with
  # items possibly containing quotes
  
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  sql <- "INSERT INTO Questions (team, persona, topic, question) VALUES(?team, ?persona, ?topic, ?question)"
  query <- sqlInterpolate(db, sql, team = data["team"], persona = data["persona"], topic = data["topic"], question = data["question"])
  # Submit the update query and disconnect
  dbExecute(db, query)
  dbDisconnect(db)
}

saveAnswer <- function(data) {
  # data here is named vector
  # with items possibly containing quotes
  
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query
  sql <- "INSERT INTO Answers (question_ID, team_answering, answer, sources) VALUES(?question_ID, ?team, ?answer, ?sources)"
  query <- sqlInterpolate(db, sql, question_ID = data[["qID"]], team = data[["team"]], answer = data[["answer"]], sources = data[["sources"]])
  # Submit the update query and disconnect
  dbExecute(db, query)
  dbDisconnect(db)
}

markAnswered <- function(qID) {
  # marks that specific question as already answered,
  # and returns the dataset as it stands after that
  
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query
  query <- paste("UPDATE Questions SET answered = 1 WHERE ID =", qID)
  # Submit the update query and disconnect
  dbExecute(db, query)
  # and then we reload the data?
  dbDisconnect(db)
}

################
#### LAYOUT ####
################

sidebar_questions <- layout_sidebar(
  sidebar = sidebar(
    selectInput("team", "Your team", teams),
    selectInput("persona", "Which persona asks the question?", personas),
    selectInput("topic", "Which is the question topic?", stringr::str_sort(current_topics)),
    textInput("question", "What is your question?", ""),
    actionButton("submit", "Submit"),
    width = "35%"),
  DT::dataTableOutput("allQuestions")
)


sidebar_answering <- layout_sidebar(
  sidebar = sidebar(
    selectInput("team", "Your team", teams),
    # select whether you are going to answer your own question or others'
    radioButtons("ours_or_others", "Which question(s) do you want to answer?",
                 c("The questions we asked earlier" = "ours",
                   "Questions asked by other teams" = "others")),
    
    # then a subsection of **optional filtering**:
    # (a) on personas asking the question
    selectInput(
      "persona_filter",
      "Filter on personas (if you wish):",
      choices = c("(all)"),
      multiple = T
    ),
    
    # (b) on the topic
    selectInput(
      "topic_filter",
      "Filter on topics (if you wish):",
      choices = c("(all)"),
      multiple = T
    ),
    
    # then the question people have selected:
    h3("The question you are answering:"),
    verbatimTextOutput("selected_question"),
    
    # and their answer:
    textAreaInput("answer", "Write your answer here.", "", height = "5em"),
    
    # and source
    textAreaInput("source", "Please source your info if you can, by providing e.g. literature references (optional).", "", height = "2em"),
    
    actionButton("submitAns", "Submit answer"),
    width = "35%"),
  DT::dataTableOutput("filteredQuestions")
)


cards <- list(
  card(
    full_screen = TRUE,
    card_header("Scoreboard"),
    card_body(DT::dataTableOutput("ScoreBoard")),
    card_body(DT::dataTableOutput("CompleteTable"))
  )
) # end of list of cards


css <- "
table.dataTable tr.selected td, table.dataTable td.selected {
  box-shadow: inset 0 0 0 9999px #FC8995 !important;
}
"

ui <- page_navbar(
  header = tags$style(HTML(css)),
  # Application title
  title = "LLM QA writeshop",
  #sidebar = common_sidebar,
  nav_spacer(),
  nav_panel("Asking questions", sidebar_questions),
  nav_panel("Answering questions", sidebar_answering),
  nav_panel("Scoreboard", cards[[1]])
  
) #end UI



################
#### SERVER ####
################


server <- function(input, output, session) {
  # Attempt at keeping one "master connection" opened on the DB?
  
  # Whenever a field is filled, aggregate all form data
  questionData <- reactive({
    data <- sapply(questionFields, function(x) input[[x]])
    data
  })
  
  unanswered_dataset <- reactive({
    # (update with current response when Submit is clicked)
    input$submit
    loadUnansweredQuestionsAsTibble()
  })
  
  
  # we dynamically recompute the list of personas to enable filtering on:
  observe({
    updateSelectInput(session, "persona_filter",
                      choices = c("(all)", sort(unique(as.character(
                        `[[`(unanswered_dataset(), "persona"))))))
  })
  
  # we dynamically recompute the list of topics to enable filtering on:
  observe({
    updateSelectInput(session, "topic_filter",
                      choices = c("(all)", sort(unique(as.character(
                        `[[`(unanswered_dataset(), "topic"))))))
  })
  
  
  
  filtered_unanswered_questions <- reactive({
    # this is a reactive for questions to pick in the page relative to the
    # "answering questions" task.
    # We filter on:
    # (a) "ours" or "others"
    # (b) personas
    # (c) topics
    
    tabl = unanswered_dataset()
    # (a) filter questions according to our preference ("ours" or "others")
    if(input$ours_or_others == "ours") tabl <- filter(tabl, team == input$team)
    else tabl <- filter(tabl, team != input$team)
    
    # and then some further filtering on personas and/or topics
    
    # (b) personas
    if(is.null(input$persona_filter) | "(all)" %in% input$persona_filter)
      tabl # null-action
    else
      tabl %>% filter(.data[["persona"]] %in% input$persona_filter) -> tabl
    
    # (c) topics
    if(is.null(input$topic_filter) | "(all)" %in% input$topic_filter)
      tabl # null-action
    else
      tabl %>% filter(.data[["topic"]] %in% input$topic_filter)
    
  })
  
  # When the Submit button is clicked (in the page to ask questions),
  # save the question.
  observeEvent(input$submit, {
    saveQuestion(questionData())
  })
  
  # when the submitAns button is clicked (on the page to write answers),
  # (a) the answer is saved to the "Answers"
  # (b) it disappears from the list of answerable questions
  # because we set answered to 1 in the Questions table
  observeEvent(input$submitAns, {
    # we prepare the data to save in the Answers table:
    qID <- filtered_unanswered_questions()[[input$filteredQuestions_rows_selected,"ID"]]
    team_answering <- input$team # careful! not 100% sure whether this refers to the one on the 1st or 2nd page
    answer <- input$answer
    sources <- input$source
    saveAnswer(c(qID = qID, team = team_answering, answer = answer, sources = sources)) # a named vector
    # and then (b), mark the question as answered:
    markAnswered(qID)
  })
  
  
  # Show all the questions
  # (update with current response when Submit is clicked)
  output$allQuestions <- DT::renderDataTable({
    input$submit # trigger
    input$submitAns # trigger
    DT::datatable(loadData(), height = "100%", rownames = FALSE, options = list(
      pageLength = 20,
      lengthMenu = c(10, 20, 50, 100)
    ))
  })     

  # Show the filtered list of unanswered questions in the panel to input answers
  output$filteredQuestions <- DT::renderDataTable({
    input$submit
    input$submitAns
    DT::datatable(filtered_unanswered_questions(), height = "100%",
                  rownames = FALSE, selection = "single",
                  options = list(
                    pageLength = 20,
                    lengthMenu = c(10, 20, 50, 100)
                  ))
  })
  
  # define the verbatim output as the selected question
  output$selected_question = renderPrint({
    s = input$filteredQuestions_rows_selected
    if (length(s)) {
      filtered_unanswered_questions()[[s, "question"]]
    } else "No question selected!"
  })
  
  completeTable <- reactive({
    # Fetch the view from the DB()
    input$submit
    input$submitAns # these are two local triggers, but we should also invalidate
    # this from time to time, irrespective (i.e. for others update the thing)
    db <- dbConnect(SQLite(), sqlitePath)
    # Submit the fetch query and disconnect
    tbl(db, "Complete") |> collect() -> data
    dbDisconnect(db)
    data
  })
  
  
  output$CompleteTable <- DT::renderDataTable({
    input$submit
    input$submitAns

    DT::datatable(completeTable(), height = "100%",
                  rownames = FALSE, selection = "single",
                  options = list(
                    pageLength = 10,
                    lengthMenu = c(10, 20, 50, 100)
                  ))
  })
  
  output$ScoreBoard <- DT::renderDataTable({
    input$submit
    input$submitAns
    
    # we now compute the data in the scoreboard,
    # based on the information in completeTable()
    dat <- completeTable() # a tibble
    
    # (a) number of questions asked by the team
    dat |> group_by(team) |> summarize(n_asked = n_distinct(question)) |> 
      arrange(desc(n_asked)) |> mutate(`Half a point for asking` = 0.5*n_asked) -> to_display
    
    # (b) number of questions answered by self
    dat |> filter(team == team_answering) |> group_by(team) |> 
      summarize(n_self_ans = n_distinct(question)) |> rename(`One point for self answers` = n_self_ans) -> temp
    
    to_display |> full_join(temp, by = "team") -> to_display
    
    # (c) number of "foreign" questions answered
    dat |> filter(!is.na(answer) & team != team_answering) |>
      group_by(team_answering) |> 
      summarize(`Two points for answering a question asked by others` = 2 * n_distinct(question)) -> temp
    
    to_display |> full_join(temp, by = c("team" = "team_answering")) -> to_display
    
    # (d) questions asked and picked by others
    dat |> filter(!is.na(answer) & team != team_answering) |> 
      group_by(team) |>
      summarize(`Two points for asking a question picked by others` = 2 * n_distinct(question)) -> temp
    
    to_display |> full_join(temp, by = "team") -> to_display
    
    # (e) adding a half-point for sourcing
    
    dat |> filter(!is.na(sources)) |> 
      group_by(team) |>
      summarize(`Half a point for sourcing an answer` = 0.5 * n_distinct(question)) -> temp
    
    to_display |> full_join(temp, by = "team") -> to_display
    
    
    # (f) And we calculate the total score
    to_display |> rowwise() |> mutate(total = sum(
      `Half a point for asking`, `One point for self answers`,
      `Two points for answering a question asked by others`,
      `Two points for asking a question picked by others`,
      `Half a point for sourcing an answer`,
      na.rm = T)) |> 
      arrange(desc(total)) -> to_display
    
    
    DT::datatable(to_display, height = "100%",
                  rownames = TRUE, selection = "single",
                  options = list(
                    pageLength = 10,
                    lengthMenu = c(10, 20, 50, 100)
                  ))
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 4591))

