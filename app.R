library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Scientific Calculator"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("calculation_type", "Select calculation type:",
                  choices = list("Addition" = "addition",
                                 "Subtraction" = "subtraction",
                                 "Multiplication" = "multiplication",
                                 "Division" = "division",
                                 "Factorial" = "factorial",
                                 "Exponentiation" = "exponentiation",
                                 "Root" = "root",
                                 "Logarithm" = "logarithm",
                                 "Derivative" = "derivative",
                                 "Integral" = "integral")),
      numericInput("value1", "Enter value 1:", 0),
      numericInput("value2", "Enter value 2:", 0)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Calculation function
  calculate <- function(operation, x, y) {
    switch(operation,
           addition = x + y,
           subtraction = x - y,
           multiplication = x * y,
           division = ifelse(y != 0, x / y, "Error: Division by zero"),
           factorial = ifelse(x >= 0, factorial(x), "Error: Factorial not defined for negative numbers"),
           exponentiation = x ^ y,
           root = ifelse(y > 0, x ^ (1 / y), "Error: Root must be greater than zero"),
           logarithm = ifelse(x > 0, log(x, y), "Error: Logarithm base must be greater than zero"),
           derivative = {
             sym_x <- sym("x")
             sym_y <- substitute(parse(text = x), list(x = sym_x))
             simplify(diff(sym_y, sym_x))
           },
           integral = {
             sym_x <- sym("x")
             sym_y <- substitute(parse(text = x), list(x = sym_x))
             simplify(integrate(sym_y, sym_x))
           }
    )
  }
  
  # Output table with the result
  output$result <- renderTable({
    operation <- input$calculation_type
    value1 <- input$value1
    value2 <- input$value2
    
    if (operation %in% c("addition", "subtraction", "multiplication", "division",
                         "factorial", "exponentiation", "root", "logarithm")) {
      result <- calculate(operation, value1, value2)
    } else if (operation %in% c("derivative", "integral")) {
      result <- calculate(operation, value1, "")
    } else {
      result <- "Error: Invalid operation"
    }
    
    data.frame(Result = result)
  })
}

# Run the application
shinyApp(ui = ui, server = server)