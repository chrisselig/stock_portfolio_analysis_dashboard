# Plotting Functions ----

# 1.0 Line Charts ----
line_chart_function <- function(data,
                                y_axis,
                                title,
                                y_axis_label){
    g <- data %>% 
        ggplot(aes(x = date,y_axis, group = asset, label = label_text)) +
        # Geoms
        geom_line(aes(color = asset)) +
        theme_tufte() +
        labs(
            x = '',
            y = y_axis_label,
            title = title
        ) +
        scale_y_continuous(
            labels = scales::number_format(accuracy = 0.01),
            breaks = scales::pretty_breaks(12)
        ) +
        scale_color_jama() +
        scale_x_date(breaks = scales::pretty_breaks(12)) +
        theme(
            #plot.title = element_text(hjust = 0.5),
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
    
    ggplotly(g, tooltip = c("label"))
    
}

# 2.0 Bar Charts ----
bar_chart_function <- function(data,title){
    
    g <- data %>% 
        ggplot(aes(reorder(asset,contribution),contribution_formatted,label = label_text)) +
        geom_col(aes(fill = contribution)) +
        geom_text(aes(label=contribution_formatted, vjust= -8)) +
        labs(
            x = '',
            y = '',
            title = title
        ) +
        scale_fill_gradient(low="#EBECED",high="#A2AAB0") +
        theme_tufte() +
        coord_flip() +
        theme(
            axis.text.x = element_blank(),
            legend.position = "none"
        )
    
    ggplotly(g, tooltip = c("label"))
        
}

# 3.0 Heatmaps ----

heat_map_function <- function(data, title = "title") {
    
    g <- data %>% 
        ggplot(aes(x=var1, y=var2, fill=value,label= value)) +
        geom_tile() +
        geom_text(color = "black") +
        theme_tufte() +
        labs(
            title = title,
            x = '',
            y = ''
        ) +
        scale_fill_gradient(low="#A2AAB0",high="#EBECED") +
        theme(
            legend.title = element_blank()
        )
    
    ggplotly(g)
}
