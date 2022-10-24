library("caret")

IS_FIRST_TIME <- TRUE

preprocessing_tab <- 
    fluidRow(
        class = "custom-tab",
        column(12, 
            h3("Preprocessing"),
        ),

        #  select target column
        column(12, 
            h4("Select target column"),
            selectInput(
                "target_column",
                label = "Target column",
                choices = NULL,
                selected = 1
            )
        ),

        column(12, 
            h4("Cleaning"),
            h6("Droping columns containing 60% missing values and high dimensional categorical variables")
        ),


        column(12, 
            h4("Imputing"),
            h6("Replace missing values with median for numeric columns and mode for categorical columns"),
        ),

        column(12, 
            h4("Normalization"),
            h6("Scale numeric columns to have mean 0 and standard deviation 1"),
        ),

        column(12, 
            h4("Encoding"),
            h6("Encode categorical columns to numeric columns"),
        ),

        column(12, 
            h4("Final dataset"),
            h6("Final dataset after preprocessing"),
            verbatimTextOutput("final_dataset"),

        ),
    )



# separate numeric and categorical columns
separate_num_cat <- function(data){

    cat_cols <- sapply(data, is.character)

    cat_cols_names <- names(cat_cols[cat_cols == TRUE])

    cat_data <- as.data.frame(data[, cat_cols_names])
    numeric_cols_names <- names(cat_cols[cat_cols == FALSE])

    num_data <- as.data.frame(data[, numeric_cols_names])


    return(list(num_cols=num_data, cat_cols=cat_data))
}

# one hot encoding
dummify <- function(data){
    
    # separate numeric and categorical columns
    numeric_cat <- separate_num_cat(data)

    numeric_data <- numeric_cat$num_cols
    cat_data <- numeric_cat$cat_cols

    if (ncol(cat_data) == 0) return(data)

    # get the dummy variables
    dummy_data <- dummyVars(" ~ .", data=cat_data)
    # get the dummy data frame
    dummy_data <- data.frame(predict(dummy_data, newdata = cat_data))
    # merge the dummy data frame with the numeric data frame
    data <- cbind.data.frame(numeric_data, dummy_data)
    return(data)
}

# remove categorical columns with more than 6 unique values
remove_high_columns <- function(data){
    # separate numeric and categorical columns
    numeric_cat <- separate_num_cat(data)
    
    numeric_data <- numeric_cat$num_cols

    
    cat_data <- numeric_cat$cat_cols


    unique_values <- sapply(cat_data, function(x) length(unique(x)))

    # get the names of the columns with more than 6 unique values
    high_unique_values <- names(unique_values[unique_values > 8])
    # remove the columns with more than 6 unique values
    data <- data[, !(names(data) %in% high_unique_values)]
    return(data)
}

zscore_numeric <- function(data){
    # separate numeric and categorical columns
    numeric_cat <- separate_num_cat(data)

    
    numeric_data <- numeric_cat$num_cols
    cat_data <- numeric_cat$cat_cols

    # scale only the numeric columns with more than 2 unique values
    unique_values <- sapply(numeric_data, function(x) length(unique(x)))
    numeric_data <- numeric_data[, names(unique_values[unique_values > 2])]

    numeric_data <- scale(numeric_data)

    # cast to data frame
    numeric_data <- as.data.frame(numeric_data)

    # if (cat_data==NULL || ncol(cat_data) == 0) return(numeric_data)

    data <- cbind.data.frame(cat_data, numeric_data)

    return(data)
}

impute <- function(data){

    # separate numeric and categorical columns
    numeric_cat <- separate_num_cat(data)
    
    numeric_data <- numeric_cat$num_cols
    cat_data <- numeric_cat$cat_cols

    # impute numeric columns with median
    numeric_data <- sapply(numeric_data, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

    # impute categorical columns with mode

    # cat_data <- sapply(cat_data, function(x) ifelse(is.na(x), names(which.max(table(x))), x))

    data <- cbind.data.frame(numeric_data, cat_data)

    return(data)
}


clean <- function(data){

    max_missing <- nrow(data) * 0.6

    # # drop columns with more than max missing values
    dropped_columns <- names(data[, colSums(is.na(data)) >= max_missing])

    data <- data[, !(names(data) %in% dropped_columns)]

    # remove categorical columns with more than 6 unique values
    data <- remove_high_columns(data)

    numeric_cat <- separate_num_cat(data)

    numeric_data <- numeric_cat$num_cols
    cat_data <- numeric_cat$cat_cols

    # remove highly correlated features
    cor_mat <- cor(numeric_data)
    cor_mat[upper.tri(cor_mat)] <- 0
    diag(cor_mat) <- 0

    numeric_data <- numeric_data[, !apply(cor_mat, 2, function(x) any(abs(x) > 0.99, na.rm = TRUE))]

    data <- cbind(numeric_data, cat_data)

    return(data)
}

encode_target <- function(data, target){

    # get unique values of target column
    unique_values <- unique(data[, target])

    # encode values with 0 and 1
    data[, target] <- sapply(data[, target], function(x) ifelse(x == unique_values[1], 0, 1))

    return(data)
}


preprocess <- function(data, target){

    # bug with imputing categorical columns

    # store target column 
    target_col <- as.data.frame(data[, target])

    # give a name to the target column
    names(target_col) <- target

    # remove target column
    data <- data[, !(names(data) %in% target)]

    # clean the data
    data <- clean(data)

    # impute missing values
    data <- impute(data)

    # scale numeric columns
    data <- zscore_numeric(data)
    
    # add target column
    data <- cbind.data.frame(data, target_col)

    # encode target column
    data <- encode_target(data, target)

    # one hot encoding  
    data <- dummify(data)
    
    result <- list(data = data, dropped_columns = c("hi"))

    return(result)
}

potential_targets <- function(data){
    
    # get the name of the columns with less than 2 unique values
    unique_values <- sapply(data, function(x) length(unique(x)))
    potential_targets <- names(unique_values[unique_values == 2])

    return(potential_targets)
}



action <- function(input, output, session, selected_target){
    # read dataset on click next
    data <- read.csv(input$file$datapath)

    if(is.null(selected_target)){
        possible_targets <- potential_targets(data)
        selected_target <- possible_targets[1]
        updateSelectInput(session, "target_column", choices = possible_targets, selected = selected_target)
    }

    preprocess_result <- preprocess(data, selected_target)

    data <- preprocess_result$data
    
    output$final_dataset <- renderPrint(head(data))

    session$userData$info$df <- data
    session$userData$info$target <- input$target_column
}


preprocess_action <- function(input, output, session){

    updateTabsetPanel(session, "step_tabs", selected = "Preprocessing")
    
    action(input, output, session, NULL)

    # observe on change target
    observeEvent(input$target_column, {
        # update the possible targets
        if(IS_FIRST_TIME){
            IS_FIRST_TIME <<- FALSE
        }
        else{
            action(input, output, session, input$target_column)
        }
    })
}


      