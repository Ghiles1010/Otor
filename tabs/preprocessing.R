library("caret")


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

    cat_data <- data[, cat_cols_names]
    numeric_cols_names <- names(cat_cols[cat_cols == FALSE])

    num_data <- data[, numeric_cols_names]


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
    data <- cbind(numeric_data, dummy_data)
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
    high_unique_values <- names(unique_values[unique_values > 6])
    # remove the columns with more than 6 unique values
    data <- data[, !(names(data) %in% high_unique_values)]
    return(data)
}

zscore_numeric <- function(data){
    # separate numeric and categorical columns
    numeric_cat <- separate_num_cat(data)

    
    numeric_data <- numeric_cat$num_cols
    cat_data <- numeric_cat$cat_cols

    numeric_data <- scale(numeric_data)
    
    

    if (ncol(cat_data) == 0) return(numeric_data)

    data <- cbind(cat_data, numeric_data)

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

    data <- cbind(numeric_data, cat_data)

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


preprocess <- function(data){

    # bug with imputing categorical columns

    # clean the data
    data <- clean(data)

    # impute missing values
    data <- impute(data)

    # scale numeric columns
    data <- zscore_numeric(data)

    # one hot encoding  
    data <- dummify(data)

    result <- list(data = data, dropped_columns = c("hi"))

    return(result)
}


preprocess_action <- function(input, output){
    session <- shiny::getDefaultReactiveDomain()

    # read dataset on click next
    data <- read.csv(input$file$datapath)

    preprocess_result <- preprocess(data)

    data <- preprocess_result$data
    output$dropped_columns <- renderText(paste(preprocess_result$dropped_columns, collapse = ", "))

    print(data)

    # populate selectInput with column names
    updateSelectInput(session, "target_column", choices = names(data))

    output$final_dataset <- renderPrint(head(data))

    # select next tab
    updateTabsetPanel(session, "step_tabs", selected = "Preprocessing")

    return(list(data = data))#, target_column=input$target_column))
    
}


      