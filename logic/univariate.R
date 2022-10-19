plotOccurences <- function(column,data)
{
    if (is.numeric(data[, column]))
    {
        fig <- plot_ly(y = data[, column], type = 'box')
        fig <- fig %>% layout(title = paste('Boxplot of', column, sep = ' '))
        return(fig)
    }
    else
    {
        numbers <- as.data.frame(table(data[, column]))

        fig <- plot_ly(x = numbers[, 'Var1'], y = numbers[, 'Freq'], type = 'bar')
        fig <- fig %>% layout(title = paste('Barplot of', column, sep = ' '))

        return(fig)
    }
}

plotFrequencies <- function(column,data)
{
    numbers <- as.data.frame(table(data[, column]))

    if (is.numeric(data[, column]))
    {
        fig <- plot_ly(y = numbers[, 'Freq'], x = numbers[, 'Var1'], type = 'bar')
        fig <- fig %>% layout(title = paste('Histogram of', column, sep = ' '))

        return(fig)
    }
    else
    {
        fig <- plot_ly(labels = numbers[, 'Var1'], values = numbers[, 'Freq'], type = 'pie')
        fig <- fig %>% layout(title = paste('Frequency pie chart distribution of', column, sep = ' '))

        return(fig)
    }
}

plotCumulativeOccurences <- function(column,data)
{
    if(! is.numeric(data[, column])) return(NULL)

    # Getting data from histogram
    tmp.hist <- hist( data[, column], plot = FALSE,
                      right = FALSE)

    fig <- plot_ly(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts), mode = 'lines+markers')
    fig <- fig %>% layout(title = paste('Population cumulative curve of', column, sep = ' '))

    return(fig)
}