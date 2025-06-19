library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(here)

# Load data
apisguru_apis <- read.csv(here("data", "apisguru_apis.csv")) |>
  mutate(added = as_datetime(added),
         updated = as_datetime(updated))
api_categories <- read.csv(here("data","api_categories.csv"))

# Get unique categories
categories <- sort(unique(api_categories$apisguru_category))


ui <- page_sidebar(
  title = "API Search",
  sidebar = sidebar(
    
    textInput("searchText", "Search:", placeholder = "Enter search term..."),
    
    # Category selection 
    selectizeInput(
      "categoryFilter",
      "Categories",
      choices = categories,
      selected = categories[1],
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Select categories")
    ),
    
    
    # Sort options
    radioButtons(
      "sortOption",
      "Sort By",
      choices = c(
        "Name (A-Z)" = "name_asc",
        "Name (Z-A)" = "name_desc",
        "Latest First" = "date_desc",
        "Oldest First" = "date_asc"
      ),
      selected = "name_asc"
    ),
    
    # Apply filters button
    actionButton("apply_filter", "Apply Filters", class = "btn-primary")
    
  ),
  
  # Main content
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        h4("APIs"),
        textOutput("itemCount")
      )
    ),
    uiOutput("itemsUI"),
    div(
      style = "text-align: center; margin: 20px 0;",
      actionButton("load_more", "Load More", class = "btn-primary"),
      textOutput("items_shown_text")
    )
  )
)

server <- function(input, output, session) {
  
  # Track how many items to display
  items_to_show <- reactiveVal(50)
  
  # Filtered and sorted items
  filtered_items <- eventReactive(input$apply_filter, {
    
    # Start with base items
    result <- apisguru_apis
  
    
    # Filter by category
    if (!is.null(input$categoryFilter) && length(input$categoryFilter) > 0) {
      # ALL match: Item contains all selected categories
      matching_apis <- api_categories %>%
        group_by(name) %>%
        summarize(
          matches = sum(apisguru_category %in% input$categoryFilter),
          .groups = "drop"
        ) %>%
        filter(matches == length(input$categoryFilter)) %>%
        pull(name)
      
      # Filter the result to only include items with matching IDs
      result <- result %>%
        filter(name %in% matching_apis)
    }
    
    # Filter by search text (if not empty)
    if (!is.null(input$searchText) && input$searchText != "") {
      search_term <- tolower(input$searchText)
      result <- result %>% 
        filter(
          grepl(search_term, tolower(name)) | grepl(search_term, tolower(description))
        )
    }
    
    # Sort items
    if (input$sortOption == "name_asc") {
      result <- result %>% arrange(name)
    } else if (input$sortOption == "name_desc") {
      result <- result %>% arrange(desc(name))
    } else if (input$sortOption == "date_desc") {
      result <- result %>% arrange(desc(updated))
    } else if (input$sortOption == "date_asc") {
      result <- result %>% arrange(updated)
    }
    
    return(result)
  }, ignoreNULL = FALSE)
  
  # Reset number of items when filter is applied
  observeEvent(input$apply_filter, {
    items_to_show(50)
  })
  
  # Display item count
  output$itemCount <- renderText({
    paste(nrow(filtered_items()), "items found")
  })
  
  # Generate UI for filtered items
  output$itemsUI <- renderUI({
    all_items <- filtered_items()
    
    if (nrow(all_items) == 0) {
      return(card(
        "No items match your current filters. Please adjust your filter criteria."
      ))
    }
    
    n <- items_to_show()
    
    subset_data <- all_items[1:min(n, nrow(all_items)), ]
    
    # Create a layout with cards in a grid
    layout_column_wrap(
      width = 1/3,
      heights_equal = "row",
      !!!lapply(1:nrow(subset_data), function(i) {
        item <- subset_data[i, ]
        item_cats <- api_categories %>% filter(name == item$name) %>% pull(apisguru_category)
        
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              h5(item$name)
            )
          ),
          card_body(
            div(
            p(ifelse(nchar(item$description) > 40, 
                     paste0(str_sub(item$description, 1, 40), "..."), 
                     item$description)),
            title = item$description),
            p(class = "text-muted", paste("Version:", item$version, "|" ,"Updated:", format(item$updated, "%Y-%m-%d"))),
            div(
              class = "d-flex flex-wrap gap-1 mt-2",
              lapply(item_cats, function(cat) {
                span(cat, class = "badge bg-primary me-1")
              })
            )
          )
        )
      })
    )
  })
  
  # Display text showing how many items are currently displayed
  output$items_shown_text <- renderText({
    n <- items_to_show()
    total <- nrow(filtered_items())
    paste("Showing", min(n, total), "of", total, "items")
  })
  
  # Load more items when button is clicked
  observeEvent(input$load_more, {
    current <- items_to_show()
    # Add 50 more items, but don't exceed the total
    items_to_show(min(current + 50, nrow(filtered_items())))
  })
}

shinyApp(ui, server)
