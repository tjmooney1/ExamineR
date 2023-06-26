#' dual_axis_app
#' @description Loads an app for the user to select a csv file from storage, select two variables as well as any colours that they wish to plot over time. The app has the same functionality as plot_dual_axis_plot, but provides the user with some control over the output
#' @param input ui
#' @param output server
#' @return An interactive web application hosted in the users R console
#' @export
#'
#' @examples runApp('R/dual_axis_app.R')

ui <- shiny::bootstrapPage(theme = shinythemes::shinytheme("simplex"),
                shiny::titlePanel("Basic Plotting App"),
                shiny::sidebarLayout(
                  shiny::sidebarPanel(
                    shiny::fileInput("import_data", "Import Data File", placeholder = "No File Selected...", accept = c(".csv")),
                    shiny::selectInput("variable_one", label = "First Variable", choices = NULL),
                    colourpicker::colourInput("colour_one", label = "First Variable Colour", allowTransparent = TRUE),
                    shiny::selectInput("variable_two", label = "Second Variable", choices = NULL),
                    colourpicker::colourInput("colour_two", label = "Second Variable Colour", allowTransparent = TRUE),
                    shiny::selectInput("date_breaks", label = "Choose Date Break Format", choices = c("1 year", "1 month", "2 weeks", "1 week", "1 day")),
                    shiny::actionButton("render_plot", "Render Plot", width = "135px"),
                    tags$img(src = "https://images.ctfassets.net/k49d63tr8kcn/5lTAuBdak8qsamGwC0kEMO/61052086f3eca17dc27bd79b4b5737aa/Shiny.png", width = "100px", height = "35px"),
                    p("A", a("Shiny", href = "http://shiny.rstudio.com"), "Web Application by Timbo")
                  ),
                  shiny::mainPanel(
                    shiny::plotOutput("dualAxisPlot", height = "600px", width = "950px")
                  )
                )
)

server <- function(input, output, session) {
  data <- shiny::reactive({
    shiny::req(input$import_data)
    read.csv(input$import_data$datapath)
  })

  shiny::observeEvent(input$import_data, {
    shiny::updateSelectInput(session, "variable_one", choices = sort(colnames(data()), decreasing = FALSE))
    shiny::updateSelectInput(session, "variable_two", choices = sort(colnames(data()), decreasing = FALSE))
  })

  shiny::observeEvent(input$render_plot, {
    selected_var_one <- input$variable_one
    selected_var_two <- input$variable_two
    custom_colours <- c(input$colour_one, input$colour_two)
    date_break_choices <- input$date_breaks

    summarised_data <- data() %>%
      dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(mean_var1 = mean(!!dplyr::sym(selected_var_one), na.rm = TRUE),
                mean_var2 = mean(!!dplyr::sym(selected_var_two), na.rm = TRUE), .groups = "drop")

    max_mean_var1 <- max(summarised_data$mean_var1, na.rm = TRUE)
    max_mean_var2 <- max(summarised_data$mean_var2, na.rm = TRUE)

    coeff <- ifelse(max_mean_var1 > max_mean_var2, max_mean_var1 / max_mean_var2, max_mean_var2 / max_mean_var1)

    options(scipen = 999)
  output$dualAxisPlot <- shiny::renderPlot({
    if (max_mean_var2 > max_mean_var1) {
      ggplot2::ggplot(summarised_data, ggplot2::aes(x = date)) +
        ggplot2::geom_line(ggplot2::aes(y = mean_var1, color = selected_var_one)) +
        ggplot2::geom_line(ggplot2::aes(y = mean_var2 / coeff, color = selected_var_two)) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Dual Axes Time Series Plot", x = "Date", color = "") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, vjust = 1),
              legend.title = ggplot2::element_blank(),
              panel.grid = ggplot2::element_blank(),
              text = ggplot2::element_text(color = "#000000", family = "Helvetica", size = 12),
              plot.title = ggplot2::element_text(face = "bold", size = 16, margin = margin(b = 10)),
              plot.subtitle = ggplot2::element_text(size = 14, margin = margin(b = 10))) +
        ggplot2::scale_y_continuous(name = selected_var_one, sec.axis = ggplot2::sec_axis(~ . * coeff, name = selected_var_two)) +
        ggplot2::scale_x_date(date_breaks = input$date_breaks) +
        ggplot2::scale_color_manual(values = custom_colours, breaks = c(selected_var_one, selected_var_two))
    } else { # adding this means the variables can be in either selection var1/var2
      ggplot2::ggplot(summarised_data, aes(x = date)) +
        ggplot2::geom_line(ggplot2::aes(y = mean_var1, color = selected_var_one)) +
        ggplot2::geom_line(ggplot2::aes(y = mean_var2 * coeff, color = selected_var_two)) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Dual Axes Time Series Plot", x = "Date", color = "") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, vjust = 1),
              legend.title = ggplot2::element_blank(),
              panel.grid = ggplot2::element_blank(),
              text = ggplot2::element_text(color = "#000000", family = "Helvetica", size = 12),
              plot.title = ggplot2::element_text(face = "bold", size = 16, margin = margin(b = 10)),
              plot.subtitle = ggplot2::element_text(size = 14, margin = margin(b = 10))) +
        ggplot2::scale_y_continuous(name = selected_var_one, sec.axis = sec_axis(~ . / coeff, name = selected_var_two)) +
        ggplot2::scale_x_date(date_breaks = input$date_breaks) +
        ggplot2::scale_color_manual(values = custom_colours, breaks = c(selected_var_one, selected_var_two))
    }
    })
  })
}

shiny::shinyApp(ui = ui, server = server)
