plotBivariateCloudPoints <- function(first_feature, second_feature,data)
{
    if (! is.numeric(data[, first_feature]) & ! is.numeric(data[, second_feature]))
    {
        p <- ggplot(data, aes_string(x = first_feature , fill=second_feature)) +
            geom_bar() +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    }
    else if( is.numeric(data[, first_feature]) &  is.numeric(data[, second_feature]))
    {
        p <- plot_ly(
            x = data[, first_feature],
            y = data[, second_feature],
            name = paste(second_feature, ' according to ', first_feature),
            type = 'scatter',
            mode = 'markers'
        )

        p <- p %>% add_trace(
            x = data[, first_feature],
            y = fitted(lm(data[, second_feature]~data[, first_feature])),
            mode = 'lines',
            name = 'Linear model'
        )

        return(p)
    }
    else if(
        (is.numeric(data[, first_feature]) & !is.numeric(data[, second_feature]))
        |
        (is.numeric(data[, second_feature]) & !is.numeric(data[, first_feature]))
    )
    {
        xlabel <- if(is.numeric(data[, first_feature])) first_feature else second_feature
        ylabel <- if(is.numeric(data[, first_feature])) second_feature else first_feature

        p <- ggplot(data=data)+
            geom_histogram(mapping = aes_string(xlabel, fill=ylabel), bins = 10)+
            xlab(label = xlabel)+
            ylab(label="Frequency")+
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    }
}

computeCorrelation <- function(first_feature, second_feature,data) {
    if(
        ! is.numeric(data[, first_feature])
            |
            ! is.numeric(data[, second_feature])
    ) return(NULL)

    covariance <- cov(
        data[, first_feature],
        data[, second_feature]
    )

    first_variance <- var(data[, first_feature])
    second_variance <- var(data[, second_feature])

    coeff_correlation.tmp <- covariance / (sqrt(first_variance * second_variance))

    paste('Coeff de corrélation linéaire = ', round(coeff_correlation.tmp, digits=2))
}

bivariateBoxPlot <- function(first_feature, second_feature,data) {
    if(
        is.numeric(data[, first_feature])
            &
            is.numeric(data[, second_feature])
    )
    {
        columns <- c(first_feature, second_feature)

        # Reshape data()
        data.stack <- melt(data[, columns], measure.vars = columns)

        p <- qplot(
            x = data.stack[,1],
            y = data.stack[,2],
            xlab = "Modalités",
            ylab = "Mesures",
            geom = "boxplot",
            fill = data.stack[,1]
        ) + theme(legend.title = element_blank())

        return(ggplotly(p))
    }
    else if(
        is.numeric(data[, first_feature])
            &
            ! is.numeric(data[, second_feature])
    )
    {
        p <- qplot(
            x = data[, second_feature],
            y = data[, first_feature],
            xlab = "Modalités",
            ylab = "Mesures",
            geom= "boxplot",
            fill=data[, second_feature]
        ) + theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    }
    else if (
        ! is.numeric(data[, first_feature])
            &
            is.numeric(data[, second_feature])
    )
    {
        p <- ggplot(data=data) +
            geom_boxplot(mapping = aes_string(first_feature, second_feature)) +
            xlab(label = first_feature) +
            ylab(label = second_feature) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    }
}