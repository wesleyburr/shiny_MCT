library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(knitr)
library(kableExtra)


shinyServer(function(input,output) {
  
  ###################################################### FUNCTION ###############################
  
  function_to_get_list <- function(exam, numchoicesperitem = NULL){
    
    # Check to make sure the file is in the correct format
    correct_format <- function(exam) {
      correct_format <- TRUE # initialize it to T, change to F if need be 
      if (exam[1,1] != 0) {
        correct_format <- FALSE
        warning("Row 1 (answer key) should have student ID listed as '0'")
      }
      # There should be no student answers with lowercase letters
      student_answers <- paste(unique(unname(unlist(exam[,-c(1:3)]))), collapse = "")
      if (student_answers != toupper(student_answers)) {
        correct_format <- FALSE 
        warning("There is a lowercase letter in one of the student's solutions; there should only be uppercase letters")
      }
      
      if (NA %in% unique(unlist(exam[,-c(1:3)]))) {
        correct_format <- FALSE
        warning("There is an empty value somewhere in the dataset; it probably should say 'BLANK' instead, but please check this")
      }
      
      return(correct_format)
    }
    stopifnot(correct_format(exam) == TRUE)
    
    object <- list() # make our empty object of type 'list', name the object itself 'object'
    object$table <- exam
    object$student_data <- object$table[-1, -c(1:3)]
    object$num_students <- nrow(object$table) - 1 # subtract 1 since first line is answer key
    object$num_items <- ncol(object$table) - 3 # number of questions on exam (-3 for first 3 columns)
    object$answer_key <- object$table[1,4:ncol(object$table)] 
    
    # Check for abnormal answer keys
    valid_key <- vector(length = object$num_items)
    for (i in 1:object$num_items) {
      valid_key[i] <- (grepl(",", object$answer_key[i]) == FALSE) & (grepl("BLANK", object$answer_key[i]) == FALSE)
    }
    # Remove the questions with invalid solutions (more than one answer marked as correct, or no answer marked as correct)
    # Tell the user that we're removing the questions that were ambiguous
    if (FALSE %in% valid_key) {
      object$student_data <- object$student_data[, -(which(valid_key == FALSE))]
      object$answer_key <- object$answer_key[-(which(valid_key == FALSE))]
      object$num_items <- ncol(object$student_data)
      if (length(which(valid_key == FALSE)) > 1) {
        warning(paste("Some of your questions had ambiguous solutions; we are removing questions", 
                    paste(which(valid_key == FALSE), collapse = ","), 
                    "from the analysis"), sep = " ")
        object$invalid_key <- paste("Some of your questions had ambiguous solutions; we are removing questions", 
            paste(which(valid_key == FALSE), collapse = ","), 
            "from the analysis. This probably means that these questions were not multiple choice, but were 'select all that apply'.")
      } else {
        warning(paste("One of your questions had an ambiguous solution; we are removing question", 
                      paste(which(valid_key == FALSE), collapse = ","), 
                      "from the analysis"), sep = " ")
        object$invalid_key <- paste("One of your questions had an ambiguous solution; we are removing question", 
                                    paste(which(valid_key == FALSE), collapse = ","), 
                                    "from the analysis. This probably means that this question was not multiple choice, but was 'select all that apply'.")
      }
    }
    
    # Validate that the user's numchoicesperitem makes sense, if they chose to include it 
    if (!is.null(numchoicesperitem) == TRUE) {
      stopifnot(is.vector(numchoicesperitem), is.numeric(numchoicesperitem), length(numchoicesperitem) == object$num_items)
    }
    
    # Student total scores and student answers 
    object$student_scores_item_included <- vector(length = object$num_students) 
    for (i in 1:object$num_students) { 
      object$student_scores_item_included[i] <- sum(object$answer_key == object$student_data[i,]) 
    }
    object$student_answers <- list(length = object$num_items) # creare list to store whether or not each student got each question correct
    for (j in 1:object$num_students) {
      for (i in 1:object$num_items) {
        object$student_answers[[i]] <- object$answer_key[[i]] == object$student_data[1:nrow(object$student_data), i]
      }
    }
    
    # Scores (as proportions)
    object$scores_mean <- mean(object$student_scores_item_included / object$num_items) 
    object$scores_sd <- sd(object$student_scores_item_included / object$num_items) 
    object$scores_max <- max(object$student_scores_item_included / object$num_items) 
    object$scores_min <- min(object$student_scores_item_included / object$num_items) 
    
    # Difficulty Indices
    object$difficulty_indices <- vector(length = object$num_items)
    for (i in 1:object$num_items) {
      object$difficulty_indices[i] <- sum(object$student_answers[[i]]) / object$num_students
    }
    object$p_mean <- mean(object$difficulty_indices, na.rm = TRUE) 
    object$p_sd <- sd(object$difficulty_indices, na.rm = TRUE) 
    object$p_max <- max(object$difficulty_indices, na.rm = TRUE)
    object$p_min <- min(object$difficulty_indices, na.rm = TRUE) 
    
    # Correlation Coefficients (item-excluded)
    # student scores when item is excluded
    object$student_scores_item_excluded <- list(length = object$num_items) 
    for (j in 1:object$num_items) {
      temp <- vector(length = object$num_students)
      for (i in 1:object$num_students) {
        temp[i] <- object$student_scores_item_included[i] - sum(object$answer_key[j] == object$student_data[i, j])
      }
      object$student_scores_item_excluded[[j]] <- temp
    }
    object$r_prime_values <- vector(length = object$num_items) 
    for (i in 1: object$num_items) {
      object$r_prime_values[i] <- cor(object$student_answers[[i]], object$student_scores_item_excluded[[i]] / object$num_items)
    }
    object$r_prime_mean <- mean(object$r_prime_values, na.rm = TRUE)
    object$r_prime_sd <- sd(object$r_prime_values, na.rm = TRUE)
    object$r_prime_max <- max(object$r_prime_values, na.rm = TRUE)
    object$r_prime_min <- min(object$r_prime_values, na.rm = TRUE)
    
    # Number of Choices per Item 
    # It's either what the user tells us, or whatever the largest letter is otherwise.
    letter2num <- function(x) {utf8ToInt(x) - utf8ToInt("A") + 1L} 
    if (!is.null(numchoicesperitem)) {
      object$num_choices_per_item <- numchoicesperitem 
    } else {
      object$num_choices_per_item <- vector(length = object$num_items) 
      for (j in 1:object$num_items) {
        temp <- vector(length = object$num_students)
        for (i in 1:object$num_students) {
          temp[i] <- letter2num(unlist(object$student_data[i,j]))
        }
        object$num_choices_per_item[j] <- max(temp)
      }  
    }
    
    
    my_letters <- LETTERS[1:26]
    object$proportion_choices <- list(length = object$num_items)
    for (j in 1:object$num_items) {
      results <- vector(length = object$num_choices_per_item[j])
      for (i in 1:object$num_choices_per_item[j]) {
        selected <- vector(length = object$num_students)
        for (k in 1:object$num_students) {
          selected[k] <- match(object$student_data[k, j], my_letters) == i
        }
        results[i] <- length(which(selected == TRUE)) / object$num_students
      }
      object$proportion_choices[[j]] <- results
      names(object$proportion_choices[[j]]) <- LETTERS[1:object$num_choices_per_item[j]]
    }
    
    # Distractors
    
    object$num_distractors <- sum(object$num_choices_per_item-1)
    
    object$distractors <- list(length = object$num_items)
    object$distractors_less_3 <- list(length = object$num_items)
    for( i in 1:object$num_items){
      object$distractors[[i]] <-
        object$proportion_choices[[i]][-(letter2num(as.character(object$answer_key[i])))]
      object$distractors_less_3[[i]] <- object$proportion_choices[[i]][object$proportion_choices[[i]] <= 0.03]
    }
    
    p1 <- object$difficulty_indices
    p2 <- 1-p1
    object$cron_alpha <- object$num_items*(1-sum(p1*p2)/var(object$student_scores_item_included))/(object$num_items-1)
    
    
    
    return(object)
  }
  
  exam <- reactive(read.table(file = input$file$datapath, sep = ",", header = TRUE, stringsAsFactors = FALSE))
  my_exam <- reactive(function_to_get_list(exam()))
  
  
  
  #################################### END OF FUNCTION ######################################
  
  #################################### OUTPUT CODE ########################################## 
  
  examinfo <- reactive({
    my_exam <- my_exam()
    coursecode <- my_exam$table[1, 3]
    dept <- my_exam$table[1, 2]
    paste("Information about the exam you've uploaded:", dept, coursecode, sep = " ")
  })
  output$examinfo <- renderText({
    if(is.null(input$file)) {return()}
    examinfo()
  })
  
  infotable <- reactive({
    my_exam <- my_exam()
    coursecode <- my_exam$table[1, 3]
    dept <- my_exam$table[1, 2]
    num_items <- my_exam$num_items
    num_students <- my_exam$num_students
    df <- data.frame("Department" = dept, "Course Code" = coursecode, "Number of Questions" = num_items,
               "Number of Students" = num_students)
    colnames(df) <- c("Department", "Course Code", "Number of Questions", "Number of Students")
    df
  })
  
  invalid_key <- reactive({
    my_exam = my_exam()
    my_exam$invalid_key
  })
  output$invalid_key <- renderText({
    if(is.null(input$file)) {return()}
    invalid_key() 
  })
  
  student_data <- reactive({
    my_exam <- my_exam()
    data.frame(my_exam$student_data)
  })
  output$student_data <- renderDT({ 
    if(is.null(input$file)){return()}
    student_data <- student_data()
    datatable(student_data, class = "cell-border stripe hover", rownames = FALSE, 
              options = list(dom = "tp", ordering = FALSE), 
              selection = "none")
  })
  
  difficulty_indices <- reactive({
    my_exam <- my_exam()
    df <- data.frame("Question" = colnames(my_exam$student_data), "Difficulty Index" = round(my_exam$difficulty_indices, 2))
    names(df) <- c("Question", "Difficulty Index")
    df
  })
  output$difficulty_indices <- renderDT({
    if(is.null(input$file)) {return()}
    difficulty_indices <- difficulty_indices()
    if (nrow(difficulty_indices) <= 10) {
      pagnav <- "tB"
    } else {pagnav <- "ltpB"}
    datatable(difficulty_indices, class = "cell-border stripe hover", extensions = "Buttons", 
              rownames = FALSE, 
              options = list(dom = pagnav, ordering = FALSE, buttons = 
                               list('copy', 'print', 
                                    list(
                                      extend = 'collection',
                                      buttons = c('csv', 'excel', 'pdf'),
                                      text = "Download this table"
                               ))),
              selection = "none")
  })
  
  psumm <- reactive({
    my_exam <- my_exam()
    data.frame("Mean" = round(my_exam$p_mean, 2), "SD" = round(my_exam$p_sd, 2), "Minimum" = round(my_exam$p_min, 2), "Maximum" = round(my_exam$p_max, 2))
  })
  output$psumm <- renderDT({
    if(is.null(input$file)) {return()}
    psumm <- psumm()
    datatable(psumm, class = "cell-border stripe hover",
              rownames = FALSE, 
              options = list(dom = "t", ordering = FALSE),
              selection = "none")
  })

  phist <- reactive({
    my_exam <- my_exam() 
    df <- data.frame("p" = my_exam$difficulty_indices)
    p1 <- ggplot(df, aes(x = p)) +
          geom_histogram(color = "aquamarine4", fill = "aquamarine3", binwidth = 0.1, center = 0.35) +
          xlab("Difficulty Indices")
    p1
  })  
  output$phist <- renderPlot({
    if(is.null(input$file)) {return()}
    phist()
  })
  
  r_prime_values <- reactive({ 
      my_exam <- my_exam()
      df <- data.frame("Question" = colnames(my_exam$student_data), "Discrimination Coefficient" = round(my_exam$r_prime_values, 2))
      names(df) <- c("Question", "Discrimination Coefficient")
      df
    })
  output$r_prime_values <- renderDT({
    if(is.null(input$file)) {return()}
    r_prime_values <- r_prime_values()
    if (nrow(r_prime_values) <= 10) {
      pagnav <- "tB"
    } else {pagnav <- "tpB"}
    datatable(r_prime_values, class = "cell-border stripe hover", extensions = "Buttons", 
              options = list(dom = pagnav, ordering = FALSE, buttons = c("copy", "csv", "excel", "pdf", "print")),
              rownames = FALSE,
              selection = "none")
  })
  
  r_primesumm <- reactive({ 
    my_exam <- my_exam()
    data.frame("Mean" = round(my_exam$r_prime_mean,2), "SD" = round(my_exam$r_prime_sd, 2), "Minimum" = round(my_exam$r_prime_min, 2), "Maximum" = round(my_exam$r_prime_max, 2))
    })
  output$r_primesumm <- renderDT({
    if(is.null(input$file)) {return()}
    r_primesumm <- r_primesumm() 
    datatable(r_primesumm, class = "cell-border stripe hover", rownames = FALSE, 
              options = list(dom = "t", ordering = FALSE), 
              selection = "none")
  })
  
  r_prime_compare_values <- reactive({
    my_exam <- my_exam() 
    if(my_exam$r_prime_mean <= (0.22-0.15/2)) {
      compare_values <- "This is below average, when compared to the average of tests from Trent University."
    } else {
      if (my_exam$r_prime_mean > 0.22-0.15/2 & my_exam$r_prime_mean <= 0.22+0.15/2) {
        compare_values <- "This is average, when compared to the average of tests from Trent University."
      } else {
        if (my_exam$r_prime_mean > 0.22+0.15/2 & my_exam$r_prime_mean <= 0.22+0.15) {
          compare_values <- "This is above average, when compared to the average of tests from Trent University."
        } else {
          if (my_exam$r_prime_mean > 0.22+0.15) {
            comapre_values <- "This is exceptional, when compared to the average of tests from Trent University."
          }
        }
      }
    }
    compare_values
  })
  output$r_prime_compare_values <- renderText({
     r_prime_compare_values()
  })
  
  r_primehist <- reactive({ 
    my_exam <- my_exam() 
    df <- data.frame("rprime" = my_exam$r_prime_values)
    p1 <- ggplot(df, aes(x = rprime)) +
          geom_histogram(fill = "darkseagreen3", color = "darkseagreen4", binwidth = 0.1, center = 0.15) +
          xlab("Discrimination Coefficient")
    p1
  })
  output$r_primehist <- renderPlot({
    if(is.null(input$file)) {return()}
    r_primehist()
  })
  
  r_primeless1 <- reactive({ 
    my_exam <- my_exam() 
    rprimelessthan1 <- data.frame("Question" = colnames(my_exam$student_data), "rprime" = round(my_exam$r_prime_values, 2))
    df <- rprimelessthan1[rprimelessthan1$rprime < 0.1, ]  
    if (nrow(df) != 0) {
      colnames(df) <- c("Question", "Discrimination Coefficient")
      rownames(df) <- NULL
      df
    } else {
      "None of your questions had a discrimination coefficient < +0.10; great work!"
    }
  })
  output$r_primeless1 <- renderUI({
    if(is.null(input$file)) {return()}
    r_primeless1 <- r_primeless1()
    if (is.data.frame(r_primeless1) == TRUE) {
      if (nrow(r_primeless1 <= 10)) {
        pagnav <- "tB"
      } else {pagnav <- "tpB"}
      notemptyrprimeless1 <- datatable(r_primeless1, class = "cell-border stripe hover", extensions = "Buttons", rownames = FALSE, 
                options = list(dom = pagnav, ordering = FALSE, buttons = c("copy", "csv", "excel", "pdf", "print")), 
                selection = "none")
      output$notemptyrprimeless1 <- renderDT(notemptyrprimeless1)
      DTOutput("notemptyrprimeless1")
    } else {
      output$emptyrprimeless1 <- renderText(r_primeless1)
      textOutput("emptyrprimeless1")
    }
  })
  
  output$my_choices <- renderUI({
    if(is.null(input$file)) {return()} 
    my_exam <- my_exam()
    my_choices <- substr(colnames(my_exam$student_data), start = 2, stop = nchar(colnames(my_exam$student_data)))
    selectInput("questionselect", "Select a Question", choices  = my_choices)
  })
  
  output$distractors_table <- renderDT({
    if(is.null(input$file)) {return()}
    my_exam <- my_exam() 
    question_number <- as.numeric(input$questionselect)
    distractors <- my_exam$distractors[[question_number]]
    column1 <- c(names(distractors), "<b>Total % of Students Who Answered Question Incorrectly</b>")
    total_percent <- paste("<b>", round(sum(unname(distractors)*100), 2), "</b>", sep ="")
    column2 <- c(round(unname(distractors)*100, 2), total_percent)
    df <- data.frame("Distractor" = column1, "percent" = column2)
    colnames(df) <- c("Distractor", "Percent of Students Who Chose Distractor (%)")
    rownames(df) <- NULL
    if (nrow(df) <= 10) {
      pagnav <- "t"
    } else {pagnav <- "tp"}
    datatable(df, class = "cell-border stripe hover", rownames = FALSE, 
              options = list(dom = pagnav, ordering = FALSE), 
              selection = "none",
              escape = FALSE)
  })
  
  output$distractors_plot <- renderPlot({
    if(is.null(input$file)) {return()}
    my_exam <- my_exam()
    test_distractors <- barplot(my_exam$distractors[[as.numeric(input$questionselect)]]*100, 
                                xlab = "Distractors", 
                                ylab = "Percent of Students who Chose Distractor",
                                col = "plum2")
  })
  
  pnotbetween39tableforapp <- reactive({ 
    my_exam <- my_exam() 
    pnotbetween39tableforapp <- data.frame("Question" = colnames(my_exam$student_data), "p" = round(my_exam$difficulty_indices, 2), "hard" = NA)
    df <- pnotbetween39tableforapp[pnotbetween39tableforapp$p < 0.3 | pnotbetween39tableforapp$p > 0.9, ]
    if (length(df$Question) > 0) {
      for (i in 1:length(df$Question)) {
        if (df$p[i] > 0.9) {
          df$hard[i] <- "Too Easy"        
        } else {
          df$hard[i] <- "Too Hard"
        }
      }
      colnames(df) <- c("Question", "Difficulty Index", "This question was...")
      rownames(df) <- NULL
      df
    } else {
      empty_pnotbetween39 <- "None of your questions fell outside of the range; great work!" 
      empty_pnotbetween39
    }
  })
  output$pnotbetween39tableforapp <- renderUI({
    if(is.null(input$file)) {return()}
    pnotbetween39tableforapp <- pnotbetween39tableforapp()
    if (is.data.frame(pnotbetween39tableforapp) == TRUE) {
      if (nrow(pnotbetween39tableforapp) <= 10) {
        pagnav <- "tB"
      } else {pagnav <- "tpB"}
      notemptypnotbetween39 <- datatable(pnotbetween39tableforapp, class = "cell-border stripe hover", extensions = "Buttons", rownames = FALSE, 
                options = list(dom = pagnav, ordering = FALSE, buttons = c("copy", "csv", "excel", "pdf", "print")), 
                selection = "none")
      output$notemptypnotbetween39 <- renderDT(notemptypnotbetween39)
      DTOutput("notemptypnotbetween39")
    } else {
      output$emptypnotbetween39 <- renderText(pnotbetween39tableforapp)
      textOutput("emptypnotbetween39")
    }
  })
  
  
  
  pnotbetween39 <- reactive({ 
    my_exam <- my_exam() 
    pnotbetween39 <- data.frame("Question" = colnames(my_exam$student_data), "p" = round(my_exam$difficulty_indices, 2), "hard" = NA)
    df <- pnotbetween39[pnotbetween39$p < 0.3 | pnotbetween39$p > 0.9, ]
    if (length(df$Question) > 0) {
      for (i in 1:length(df$Question)) {
        if (df$p[i] > 0.9) {
          df$hard[i] <- "Too Easy"        
        } else {
          df$hard[i] <- "Too Hard"
        }
      }
      rownames(df) <- NULL
      df
    } else {
      df <- data.frame("q" = logical()) # create a df with 0 rows, just so we can print out the message (it needs to remain a df, because we're using renderTable; so text won't work)
      colnames(df) <- "None of your questions fell outside of the range; great work!"
      rownames(df) <- NULL
      df
    }
  })
  output$pnotbetween39 <- renderDT({
    if(is.null(input$file)) {return()}
    pnotbetween39 <- pnotbetween39()
    datatable(pnotbetween39, class = "cell-border stripe hover", extensions = "Buttons", rownames = FALSE, 
              options = list(dom = "tpB", ordering = FALSE, buttons = c("copy", "csv", "excel", "pdf", "print")),
              selection = "none")
  })
  
  nonfunctionaldistractors <- reactive({
    my_exam <- my_exam() 
    listdistractors <- vector(length = my_exam$num_items, mode = "list")
    for (i in 1:my_exam$num_items) {
      for (j in 1:length(my_exam$distractors_less_3[[i]])) {
        if (length(my_exam$distractors_less_3[[i]]) != 0) {
          listdistractors[[i]][j] <- names(my_exam$distractors_less_3[[i]][j])
        }
      }
    }
    unlistdistractors <- vector(length = my_exam$num_items)
    for (i in 1:my_exam$num_items) {
      unlistdistractors[i] <- paste(listdistractors[[i]], collapse = ", ")
    }
    nonfunctionaldistractors <- data.frame("Question" = colnames(my_exam$student_data), "Non-Functional Distractors" = unlistdistractors)
    df <- data.frame(nonfunctionaldistractors[nonfunctionaldistractors$Non.Functional.Distractors != "", ])
    if (nrow(df) != 0) {
      colnames(df) <- c("Question", "Non-Functional Distractors")
      rownames(df) <- NULL 
      df
    } else {
     "None of your questions had non-functional distractors; great work!"
    }
  })
  output$nonfunctionaldistractors <- renderUI({
    if(is.null(input$file)) {return()}
    nonfunctionaldistractors <- nonfunctionaldistractors()
    if (is.data.frame(nonfunctionaldistractors) == TRUE) {
      if (nrow(nonfunctionaldistractors) <= 10) {
        pagnav <- "tB"
      } else {pagnav <- "tpB"}
      notemptynonfunctionaldistractors <- datatable(nonfunctionaldistractors, class = "cell-border stripe hover", extensions = "Buttons", 
                                                    rownames = FALSE, 
                                                    options = list(dom = "tpB", ordering = FALSE, buttons = c("copy", "csv", "excel", "pdf", "print")), 
                                                    selection = "none")
      output$notemptynonfunctionaldistractors <- renderDT(notemptynonfunctionaldistractors)
      DTOutput("notemptynonfunctionaldistractors")
    } else {
      output$emptynonfunctionaldistractors <- renderText(nonfunctionaldistractors)
      textOutput("emptynonfunctionaldistractors")
    } 
  })
  
  cron_alpha <- reactive({
    my_exam <- my_exam()
    round(my_exam$cron_alpha, 3)
  })
  output$cron_alpha <- renderText({
    if(is.null(input$file)) {return()}
    cron_alpha() 
  })
  
  my_new_exam <- reactive({
    my_exam <- my_exam()
    df <- data.frame("Question" = colnames(my_exam$student_data), "rprime" = round(my_exam$r_prime_values, 2))
    df <- df[order(df$rprime),]
    indices_of_questions_to_remove <- as.numeric(rownames(df[1:5, ]))
    indices_of_questions_to_remove <- indices_of_questions_to_remove + 3 #add 3 since ID, DEPT, COURSECODE columns
    new_exam <- exam()
    new_exam <- new_exam[, -(indices_of_questions_to_remove)]
    function_to_get_list(new_exam)  
  })
  
  new_cron_alpha <- reactive({
    my_new_exam <- my_new_exam()
    round(my_new_exam$cron_alpha, 3)
  })
  output$new_cron_alpha <- renderText({
    if(is.null(input$file)) {return()}
    new_cron_alpha()
  })
  
  worst_5_questions <- reactive({
    my_exam <- my_exam()
    cron_alpha <- cron_alpha()
    new_cron_alpha <- new_cron_alpha()
    worst5 <- NULL 
    if (new_cron_alpha > cron_alpha) {
      df <- data.frame("Question" = colnames(my_exam$student_data), "rprime" = round(my_exam$r_prime_values, 2))
      indices_of_questions_to_remove <- df[order(df$rprime),]
      colnames(indices_of_questions_to_remove) <- c("Question", "Discrimination Coefficients")
      rownames(indices_of_questions_to_remove) <- NULL
      worst5 <- indices_of_questions_to_remove[1:5, ]  
    }
    worst5 # only returns the worst 5 questions if removing the 5 will improve alpha; otherwise returns NULL
  })
  output$worst_5_questions <- renderDT({
    if(is.null(input$file)) {return()}
    worst_5_questions <- worst_5_questions()
    datatable(worst_5_questions, class = "cell-border stripe hover", rownames = FALSE,
              options = list(dom = "t", ordering = FALSE), 
              selection = "none")
  })
  
  output$newreliabilitystring <- renderText({
    newreliabilitystring <- NULL 
    cron_alpha <- cron_alpha() 
    new_cron_alpha <- new_cron_alpha()
    if (new_cron_alpha > cron_alpha) {
      newreliabilitystring <- "Often, removing questions with low discrimination coefficients from your test can greatly improve the reliability of your test. Here are the 5 questions on your test with the lowest discrimination coefficients:" 
    }
    newreliabilitystring
  })
  
  output$newreliabilitystring2 <- renderText({
    newreliabilitystring2 <- NULL
    cron_alpha <- cron_alpha()
    new_cron_alpha <- new_cron_alpha()
    if (new_cron_alpha > cron_alpha) {
      newreliabilitystring2 <- paste("If you were to simply remove these 5 questions from your test altogether, the reliability of your test would have been: ", new_cron_alpha, ".", sep = "")
      if ((new_cron_alpha - cron_alpha) >= 0.025) {
        newreliabilitystring2 <- paste(newreliabilitystring2, "(This is a significant increase.)")
      } else {
        newreliabilitystring2 <- paste(newreliabilitystring2, "(This is not a significant increase.)")
      }
    }
    newreliabilitystring2
  })
  
  exceptional_questions <- reactive({
    my_exam <- my_exam()
    df <- data.frame("Question" = colnames(my_exam$student_data), "rprime" = round(my_exam$r_prime_values, 2))
    df <- df[df$rprime >= 0.40,]
    colnames(df) <- c("Question", "Discrimination Coefficient")
    rownames(df) <- NULL
    df
  })
  output$exceptional_questions <- renderDT({
    if(is.null(input$file)) {return()}
    exceptional_questions <- exceptional_questions()
    if (nrow(exceptional_questions) <= 10) {
      pagnav <- "tB"
    } else {pagnav <- "tpB"}
    datatable(exceptional_questions, class = "cell-border stripe hover", extensions = "Buttons", rownames = FALSE, 
              options = list(dom = pagnav, ordering = FALSE, buttons = c("copy", "csv", "excel", "pdf", "print")), 
              selection = "none")
  })
  
  ############################ END OF OUTPUT CODE ###################################
  
  ############################ DOWNLOAD BUTTONS #####################################    
  
  output$OutputDifficultyIndices <- downloadHandler(
    filename = function() {
      paste("DifficultyIndices", Sys.Date(), "csv", sep =".")
    },
    content = function(file) {
      write.table(difficulty_indices(), file, sep = ",", row.names = FALSE)
    }
  )
  
  output$OutputItemExcludedCorrelations <- downloadHandler(
    filename = function() {
      paste("DiscriminationCoefficients", Sys.Date(), "csv", sep =".")
    },
    content = function(file) {
      write.table(r_prime_values(), file, sep = ",", row.names = FALSE)
    }
  )
  
  ########################### END OF DOWNLOAD BUTTONS ############################
  
  ########################### CREATE RMARKDOWN FILE ##############################
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(examinfo = examinfo(), infotable = infotable(), invalid_key = invalid_key(), 
                     difficulty_indices = difficulty_indices(), 
                     psumm = psumm(), phist = phist(), r_prime_values = r_prime_values(),
                     r_primesumm = r_primesumm(), r_primehist = r_primehist(), 
                     r_prime_compare_values = r_prime_compare_values(),
                     r_primeless1 = r_primeless1(), pnotbetween39 = pnotbetween39(), 
                     nonfunctionaldistractors = nonfunctionaldistractors(), cron_alpha = cron_alpha(),
                     worst_5_questions = worst_5_questions(), new_cron_alpha = new_cron_alpha(),
                     exceptional_questions = exceptional_questions())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$report2 <- downloadHandler(
    filename = "partialreport.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "partialreport.Rmd")
      file.copy("partialreport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(examinfo = examinfo(), infotable = infotable(), invalid_key = invalid_key(), 
                     difficulty_indices = difficulty_indices(), 
                     psumm = psumm(), phist = phist(), r_prime_values = r_prime_values(),
                     r_primesumm = r_primesumm(), r_primehist = r_primehist(),
                     r_prime_compare_values = r_prime_compare_values(),
                     r_primeless1 = r_primeless1(), pnotbetween39 = pnotbetween39(), 
                     nonfunctionaldistractors = nonfunctionaldistractors(), cron_alpha = cron_alpha(),
                     worst_5_questions = worst_5_questions(), new_cron_alpha = new_cron_alpha(),
                     exceptional_questions = exceptional_questions())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ########################### END OF RMARKDOWN CREATION ##############################
  
  ########################### MainPanel tabset renderUI code #########################
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. 
  # Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(
        tabPanel("Recommendations",
                 helpText("This tab will provide recommendations to you to help improve the quality of your test. 
                          To see more detailed explanations and breakdowns of various diagnostics, 
                          click on the other tabs. If you'd prefer to see all of the information summarized
                          in one place, click the 'Comprehensive Report' tab, where you'll be able to 
                          download a PDF report."),
                 helpText("We suggest taking a closer look at the following questions on your test.
                          You should consider rewording or removing these questions from future iterations of this test,
                          if you are planning on using it again. These recommendations are based off of 
                          current psychometric literature."),
                 br(),
                 helpText("Ideally, multiple choice questions should have discrimination coeffiicents higher than 0.1,
                          should have item difficulties ranging from 0.3-0.9, and should have functional 
                          distractors. Below, we point out any questions that do not meet these recommendations."),
                 h3("Questions with Discrimination Coefficient < 0.1"),
                 uiOutput("r_primeless1"),
                 h3("Questions with Difficulty Index < 0.3 or > 0.9"),
                 uiOutput("pnotbetween39tableforapp"), 
                 h3("Questions with Non-Functional Distractors"),
                 uiOutput("nonfunctionaldistractors"),
                 br()),
        tabPanel("Comprehensive Report",
                 helpText("Click the download button to download a comprehensive pdf report
                          of your test. This report will combine the results from each of the
                          tabs in this app, all in one convenient file."),
                 br(),
                 helpText("There are two comprehensive reports which you can download. The first
                          report (full report) contains all of the tables from the tabs in this app,
                          along with detailed descriptions and explanations of the information 
                          contained in the tables. If this is your first time using this app, we recommend
                          downloading the full report. If you've used this app before, you can download
                          the partial report to have a copy with just the tables and no explanations."),
                 br(), 
                 helpText("Note: The download should begin within 10 seconds of clicking the button -- please be
                          patient!"),
                 downloadButton("report", "Download Full Comprehensive Report"),
                 br(),
                 br(),
                 downloadButton("report2", "Download Partial Comprehensive Report")),
        tabPanel("Item Difficulty", 
                 helpText("The 'Difficulty Index' of a question refers to the proportion of students
                          who answered the question correctly. Typically, we represent this value
                          with the letter p. For example, if p = 0.4 for question 12, then it means
                          that 40% of students answered question 12 correctly, and 60% of students
                          answered question 12 incorrectly. You can use this as a way of gauging
                          how easy or difficult each of your questions were. Note: if you scroll
                          down, you can see the difficulty index for each question individually."),
                 br(),
                 DTOutput("psumm"),
                 br(),
                 plotOutput("phist"),
                 downloadButton("OutputDifficultyIndices", "Download this table"),
                 br(),
                 DTOutput("difficulty_indices")),
        tabPanel("Discrimination Coefficients",
                 helpText("The discrimination coefficient measures how well a question discriminates 
                          the stronger students from the weaker students. The value of a discrimination 
                          coefficient can range from -1 to +1, where the best possible value is +1 
                          (stronger students are more likely to get the question correct than weaker students are), 
                          and the worst possible value is -1 (weaker students are more likely to get the question 
                          correct than stronger students are)."),
                 br(),
                 DTOutput("r_primesumm"),
                 br(),
                 textOutput("r_prime_compare_values"),
                 br(),
                 plotOutput("r_primehist"),
                 downloadButton("OutputItemExcludedCorrelations", "Download this table"),
                 br(),
                 DTOutput("r_prime_values"),
                 br()),
        tabPanel("Distractors",
                 helpText("On a multiple choice question, the correct answer is referred to as the 'keyed
                          response'. Any of the remaining options are known as 'distractors', since they
                          are intended to 'distract' students from selecting the correct response. As a 
                          general rule of thumb, a distractor is considered to be 'functional' (good) if
                          at least 3% of the students chose that distractor. Use the dropdown menu below
                          to choose one of the questions on the exam, and see how the distractors for that
                          question performed."),
                 br(),
                 htmlOutput("my_choices"),
                 br(),
                 plotOutput("distractors_plot"),
                 br(),
                 DTOutput("distractors_table"),
                 br()),
        tabPanel("Reliability",
                 helpText("The reliability of a test can range from 0 to 1, where 1 means most reliable. 
                          It should be noted, though, that increasing the number of questions on a 
                          test will always increase the reliability of the test; this means that the value
                          of reliability should not necessarily be interpreted directly, but should 
                          perhaps be interpreted by comparing different iterations of the same test."),
                 h3("Recommended Ranges for Reliability"),
                 helpText("0.55-0.65: Adequate for midterm exam or term test/quiz"),
                 helpText("0.65-0.8: Adequate for a final exam"),
                 helpText("0.8-0.9: Excellent for a final exam or term test/quiz"),
                 helpText("0.9-1.0: Standardized test level exam"),
                 br(),
                 helpText("The reliability of your test, currently, is:"),
                 textOutput("cron_alpha"),
                 br(),
                 textOutput("newreliabilitystring"),
                 br(),
                 DTOutput("worst_5_questions"),
                 br(),
                 span(textOutput("newreliabilitystring2"), style = "color:red"),
                 br()),
        tabPanel("Exceptional Questions",
                 helpText("Any questions which have a discrimination coefficient of +0.40 or 
                          higher are considered to be exceptional questions. Here 
                          are the questions on your test which met or surpassed this threshold: "),
                 br(),
                 DTOutput("exceptional_questions")),
        tabPanel("Original Data",
                 helpText("Below you'll find the original data. This is placed here for your 
                          convenience; if you find that the diagnostics on one of the questions
                          is particularly troublesome, you can use this page to take a closer
                          look at student responses for that question (each row represents a different student).
                          Note that student numbers have been removed for you, in case you forgot to anonymize
                          your data."),
                 br(),
                 DTOutput("student_data"))
        )
  })
  
  
})