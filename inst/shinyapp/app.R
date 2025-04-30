# inst/shinyapp/app.R
print("--- Sourcing app.R ---")
# Load required packages (shiny + the package being built)
# Dependencies like jpeg, png are handled via the package's DESCRIPTION
library(shiny)
print("Library shiny loaded.")
library(img2ASCII) # Load functions from the package itself
print("Library img2ASCII loaded.")

# === UI Definition (Copied from original script) ===
print("Defining UI...")
ui <- fluidPage(
  titlePanel("ASCII Image Converter"),
  sidebarLayout(
    sidebarPanel(
      fileInput("image", "Choose a .jpg or .png Image", accept = c(".jpg", ".jpeg", ".png")),
      numericInput("max_width", "Maximum Width (characters)",
                   min = 10, max = 300, value = 80),
      sliderInput("fontsize", "Preview Font Size (px)",
                  min = 4, max = 20, value = 10),
      selectInput("save_format", "Save as:",
                  choices = c("Text (.txt)", "Image (.png)", "Image (.jpeg)")),
      downloadButton("downloadFile", "Download")
    ),
    mainPanel(
      htmlOutput("ascii_display")
    )
  )
)
print("UI object defined.")

# === Server Logic (Modified to use package functions) ===
server <- function(input, output, session) { # Added session for potential future use
  print("Defining Server function...")
  # --- Reactive Values ---
  img_data_rv <- reactiveVal()
  orig_dims_rv <- reactiveVal()
  error_message_rv <- reactiveVal(NULL) # To store potential errors

  # --- Observer: Load and Process Uploaded Image ---
  observeEvent(input$image, {
    print("observeEvent for input$image triggered")
    error_message_rv(NULL) # Clear previous errors
    req(input$image)
    img <- NULL # Initialize img

    # Use tryCatch for robust file reading
    tryCatch({
      ext <- tolower(tools::file_ext(input$image$datapath))
      img <- switch(ext,
                    "jpg"  = jpeg::readJPEG(input$image$datapath),
                    "jpeg" = jpeg::readJPEG(input$image$datapath),
                    "png"  = png::readPNG(input$image$datapath),
                    stop(paste("Unsupported image format:", ext))
      )
      img_data_rv(img)
      orig_dims_rv(dim(img)[1:2])
    },
    error = function(e) {
      error_message_rv(paste("Error loading image:", e$message))
      img_data_rv(NULL) # Clear image data on error
      orig_dims_rv(NULL)
    }
    )
  })

  # --- Reactive Expression: Generate ASCII Art ---
  ascii_lines_reactive <- reactive({
    # Require input data AND check that no error occurred during loading
    print("ascii_lines_reactive START")
    req(img_data_rv(), is.null(error_message_rv()))

    img <- img_data_rv()
    od <- orig_dims_rv()

    # Call package functions for processing steps
    print("Calling img2ASCII:::convert_to_grayscale...")
    gray_matrix <- img2ASCII:::convert_to_grayscale(img)
    req(gray_matrix) # Require successful grayscale conversion

    print("Calling img2ASCII:::calculate_target_dims...")
    target_dims <- img2ASCII:::calculate_target_dims(
      original_dims = od,
      target_width = input$max_width,
      original_width = ncol(gray_matrix) # Pass original width from gray matrix
      # char_aspect_ratio default is 2.2
    )
    req(target_dims)

    print("Calling img2ASCII:::downsample_image...")
    gray_small <- img2ASCII:::downsample_image(gray_matrix, target_dims)
    req(gray_small)

    print("Calling img2ASCII:::map_to_ascii...")
    ascii_result <- img2ASCII:::map_to_ascii(gray_small) # Use default character set
    req(ascii_result)

    print("ascii_lines_reactive END")
    return(ascii_result)
  })

  # --- Render UI: Display ASCII Art (or Error Message) ---
  output$ascii_display <- renderUI({
    print("renderUI for ascii_display START")
    # Display error message if it exists
    err_msg <- error_message_rv()
    if (!is.null(err_msg)) {
      return(tags$p(style = "color: red;", err_msg))
    }

    # Otherwise, get ASCII lines and display them
    ascii <- ascii_lines_reactive()
    req(ascii) # Ensure ASCII was generated

    ascii_text <- paste(ascii, collapse = "\n")
    style <- paste0(
      "font-family: monospace; font-size: ", input$fontsize, "px; ",
      "white-space: pre; overflow: auto; max-height: 600px; line-height: 1;"
    )
    print("renderUI for ascii_display END")
    div(style = style, ascii_text)
  })

  # --- Download Handler: Save Output ---
  output$downloadFile <- downloadHandler(
    filename = function() {
      print("downloadHandler filename START")
      paste0("ascii_output",
             switch(input$save_format,
                    "Text (.txt)" = ".txt",
                    "Image (.png)" = ".png",
                    "Image (.jpeg)" = ".jpeg",
                    ".txt") # Default
      )
    },
    content = function(file) {
      print("downloadHandler content START")
      ascii <- ascii_lines_reactive()
      req(ascii)

      if (input$save_format == "Text (.txt)") {
        writeLines(ascii, file)
      } else {
        # Determine format based on selection, default to png if needed
        img_format <- switch(input$save_format,
                             "Image (.png)" = "png",
                             "Image (.jpeg)" = "jpeg",
                             "png") # Default format

        # Call the package function to save the image
        tryCatch({
          img2ASCII:::save_ascii_image(ascii_lines = ascii, file_path = file, format = img_format)
        },
        error = function(e){
          # Optionally notify user of save error via Shiny notification
          showNotification(paste("Error saving image:", e$message), type = "error")
          # We might want to prevent the download browser dialog from closing
          # but that's more advanced. For now, error is logged internally.
        }
        )
      }
    }
  ) # End downloadHandler
  print("Server function definition complete (outputs assigned)")
} # End server
print("Server function defined.")
print("--- app.R finished ---")
shiny::shinyApp(ui = ui, server = server)
# NOTE: No shinyApp() call here! It's handled by runApp() or deployment tools.
