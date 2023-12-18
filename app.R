library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(rsconnect)

# Load data from the predefined file
yourgrade_data <- list(
  thai = read_excel("yourgrade.xlsx", sheet = "ไทย"),
  math = read_excel("yourgrade.xlsx", sheet = "คณิต"),
  science = read_excel("yourgrade.xlsx", sheet = "วิทย์"),
  english = read_excel("yourgrade.xlsx", sheet = "อังกฤษ")
)

# Define UI
ui <- fluidPage(
  titlePanel("แดชบอร์ดการเรียนรู้: พร้อมแผนการปรับปรุง"),
  tags$style(
    HTML("
      body {
        background-color: #f4f4f4; /* Set the background color */
        font-family: 'THSarabunNew', sans-serif; /* Set the font */
      }
      .container-fluid {
        max-width: 1200px; /* Set the maximum width of the container */
      }
      .subject-box {
        border: 1px solid #ddd;
        padding: 10px;
        margin: 10px;
        background-color: #fff; /* Set background color for subject boxes */
        width: 90%; /* Set the width of each subject box to 90% */
      }
      #scoreTable {
        width: 100%; /* Make the table full-width */
        border-collapse: collapse;
        margin-top: 20px;
      }
      #scoreTable th, #scoreTable td {
        border: 1px solid #ddd; /* Add border to table cells */
        padding: 8px; /* Add padding to table cells */
        text-align: center; /* Center-align text */
      }
      #gradeDistributionPlot {
        margin-top: 20px;
      }
      #footer {
        margin-top: 20px;
        text-align: center;
        font-style: italic;
      }
    ")
  ),
  fluidRow(
    column(12,
           selectInput("selectedName", "ชื่อ-นามสกุล :", choices = unique(yourgrade_data$thai$name)),
           actionButton("analyzeBtn", "Analyze")
    )
  ),
  fluidRow(
    column(3, 
           div(class = "subject-box",
               h4("ภาษาไทย"),
               sliderInput("scoreSlider_thai", "คะแนนปลายภาค:", min = 0, max = 20, value = 10),
               actionButton("analyzeBtn_thai", "Analyze"),
               verbatimTextOutput("details_thai")
           )
    ),
    column(3, 
           div(class = "subject-box",
               h4("คณิตศาสตร์"),
               sliderInput("scoreSlider_math", "คะแนนปลายภาค:", min = 0, max = 20, value = 10),
               actionButton("analyzeBtn_math", "Analyze"),
               verbatimTextOutput("details_math")
           )
    ),
    column(3, 
           div(class = "subject-box",
               h4("วิทยาศาสตร์"),
               sliderInput("scoreSlider_science", "คะแนนปลายภาค:", min = 0, max = 20, value = 10),
               actionButton("analyzeBtn_science", "Analyze"),
               verbatimTextOutput("details_science")
           )
    ),
    column(3, 
           div(class = "subject-box",
               h4("ภาษาอังกฤษ"),
               sliderInput("scoreSlider_english", "คะแนนปลายภาค:", min = 0, max = 20, value = 10),
               actionButton("analyzeBtn_english", "Analyze"),
               verbatimTextOutput("details_english")
           )
    )
  ),
  fluidRow(
    column(12,
           selectInput("selectedSubjectForPlot", "เกรดรายห้องจำแนกตามวิชา:", choices = c("thai", "math", "science", "english")),
           actionButton("analyzeBtn_subject", "Analyze")
    )
  ),
  fluidRow(
    column(12,
           plotlyOutput("gradeDistributionPlot")
    )
  ),
  fluidRow(
    column(12,
           tags$div(id = "footer", "แดชบอร์ดนี้เป็นส่วนหนึ่งของรายวิชา Data Visualization : ผู้จัดทำ นางสาวพีรยา ประเสริฐศรี")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  analyzed_data <- reactiveVal(NULL)
  
  observe({
    if (!is.null(input$selectedName)) {
      shinyjs::enable("analyzeBtn_thai")
      shinyjs::enable("analyzeBtn_math")
      shinyjs::enable("analyzeBtn_science")
      shinyjs::enable("analyzeBtn_english")
    }
  })
  
  observeEvent(input$analyzeBtn, {
    selected_name <- input$selectedName
    analyzed_data(list(
      thai = analyze_subject(yourgrade_data$thai, selected_name, input$scoreSlider_thai),
      math = analyze_subject(yourgrade_data$math, selected_name, input$scoreSlider_math),
      science = analyze_subject(yourgrade_data$science, selected_name, input$scoreSlider_science),
      english = analyze_subject(yourgrade_data$english, selected_name, input$scoreSlider_english)
    ))
  })
  
  observeEvent(input$analyzeBtn_thai, {
    analyzed_data(list(
      thai = analyze_subject(yourgrade_data$thai, input$selectedName, input$scoreSlider_thai)
    ))
  })
  
  observeEvent(input$analyzeBtn_math, {
    analyzed_data(list(
      math = analyze_subject(yourgrade_data$math, input$selectedName, input$scoreSlider_math)
    ))
  })
  
  observeEvent(input$analyzeBtn_science, {
    analyzed_data(list(
      science = analyze_subject(yourgrade_data$science, input$selectedName, input$scoreSlider_science)
    ))
  })
  
  observeEvent(input$analyzeBtn_english, {
    analyzed_data(list(
      english = analyze_subject(yourgrade_data$english, input$selectedName, input$scoreSlider_english)
    ))
  })
  
  observeEvent(input$selectedSubjectForPlot, {
    shinyjs::enable("analyzeBtn_subject")
  })
  
  observeEvent(input$analyzeBtn_subject, {
    if (!is.null(input$selectedSubjectForPlot) && !is.null(analyzed_data()) &&
        !is.null(analyzed_data()[[input$selectedSubjectForPlot]])) {
      analyzed_data(list(
        subject = analyze_subject_all(yourgrade_data[[input$selectedSubjectForPlot]], input$selectedName)
      ))
    }
  })
  
  analyze_subject <- function(subject_data, selected_name, score_slider) {
    final_score <- pmin(
      subject_data %>%
        filter(name == selected_name) %>%
        pull(workshop_score) +
        subject_data %>%
        filter(name == selected_name) %>%
        pull(midterm) +
        score_slider,
      100
    )
    
    grade <- cut(
      final_score,
      breaks = c(-Inf, 50, 55, 60, 65, 70, 75, 80, 100),
      labels = c("0", "1", "1.5", "2", "2.5", "3", "3.5", "4")
    )
    
    comment <- subject_data %>%
      filter(name == selected_name) %>%
      pull(comment)
    
    data.frame(
      Grade = as.numeric(as.character(grade)),
      Comment = comment,
      Workshop_Score = subject_data %>%
        filter(name == selected_name) %>%
        pull(workshop_score),
      Midterm = subject_data %>%
        filter(name == selected_name) %>%
        pull(midterm),
      Slider_Score = score_slider,
      Total_Score = final_score
    )
  }
  
  analyze_subject_all <- function(subject_data, selected_name) {
    final_scores <- subject_data %>%
      mutate(
        Final_Score = pmin(
          workshop_score + midterm + 10,  # Use a fixed score_slider value of 10
          100
        )
      ) %>%
      group_by(name) %>%
      summarise(
        Grade = cut(
          Final_Score,
          breaks = c(-Inf, 50, 55, 60, 65, 70, 75, 80, 100),
          labels = c("0", "1", "1.5", "2", "2.5", "3", "3.5", "4")
        ),
        Frequency = n()
      )
    
    final_scores
  }
  
  output$details_thai <- renderPrint({
    if (!is.null(analyzed_data()) && !is.null(analyzed_data()$thai)) {
      details <- analyzed_data()$thai
      cat("คะแนนเก็บ:", details$Workshop_Score, "\n")
      cat("สอบกลางภาค:", details$Midterm, "\n")
      cat("สอบปลายภาค:", details$Slider_Score, "\n")
      cat("รวมคะแนน:", details$Total_Score, "\n")
      cat("Grade:", details$Grade, "\n")
      cat("Comment:", details$Comment)
    }
  })
  
  output$details_math <- renderPrint({
    if (!is.null(analyzed_data()) && !is.null(analyzed_data()$math)) {
      details <- analyzed_data()$math
      cat("คะแนนเก็บ:", details$Workshop_Score, "\n")
      cat("สอบกลางภาค:", details$Midterm, "\n")
      cat("สอบปลายภาค:", details$Slider_Score, "\n")
      cat("รวมคะแนน:", details$Total_Score, "\n")
      cat("Grade:", details$Grade, "\n")
      cat("Comment:", details$Comment)
    }
  })
  
  output$details_science <- renderPrint({
    if (!is.null(analyzed_data()) && !is.null(analyzed_data()$science)) {
      details <- analyzed_data()$science
      cat("คะแนนเก็บ:", details$Workshop_Score, "\n")
      cat("สอบกลางภาค:", details$Midterm, "\n")
      cat("สอบปลายภาค:", details$Slider_Score, "\n")
      cat("รวมคะแนน:", details$Total_Score, "\n")
      cat("Grade:", details$Grade, "\n")
      cat("Comment:", details$Comment)
    }
  })
  
  output$details_english <- renderPrint({
    if (!is.null(analyzed_data()) && !is.null(analyzed_data()$english)) {
      details <- analyzed_data()$english
      cat("คะแนนเก็บ:", details$Workshop_Score, "\n")
      cat("สอบกลางภาค:", details$Midterm, "\n")
      cat("สอบปลายภาค:", details$Slider_Score, "\n")
      cat("รวมคะแนน:", details$Total_Score, "\n")
      cat("Grade:", details$Grade, "\n")
      cat("Comment:", details$Comment)
    }
  })
  
  output$gradeDistributionPlot <- renderPlotly({
    if (!is.null(input$selectedSubjectForPlot) && !is.null(analyzed_data()) &&
        !is.null(analyzed_data()$subject)) {
      subject_data <- analyzed_data()$subject
      
      # Calculate grade distribution
      grade_counts <- table(subject_data$Grade)
      
      # Create a bar plot
      gg <- ggplot(data = as.data.frame(grade_counts), aes(x = Var1, y = Freq)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = paste("Grade Distribution for", input$selectedSubjectForPlot),
             x = "Grade", y = "Frequency") +
        theme_minimal() +
        theme(text = element_text(family = "THSarabunNew")) +
        theme(plot.title = element_text(hjust = 0.5, size = 20)) +
        theme(axis.title = element_text(size = 14)) +
        theme(axis.text = element_text(size = 12)) +
        theme(legend.position="none") +
        annotate("text", x = 1.5, y = -0.5, label = "หมายเหตุ: หากนักเรียนแต่ละคนสอบปลายภาคได้ 10 คะแนน",
                 hjust = 0.5, vjust = 0.5, size = 4)
      
      ggplotly(gg)
    }
  })
  
  output$developerTable <- renderTable({
    data.frame(Developer = "นางสาวพีรยา ประเสริฐศรี")
  })
}

# Run the Shiny app
shinyApp(ui, server)


