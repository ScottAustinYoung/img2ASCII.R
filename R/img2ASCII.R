# --- img2ASCII.R ---
# Author: Scott Young
# Description: A Shiny web application that converts uploaded JPG or PNG images into ASCII art.
# Users can adjust the width, preview the font size,
# and download the result as a text file or an image.

# === 1. Package Management ===
# This section ensures that all required packages are installed and loaded.
## --- Define Required Packages ---
required_packages <- c(
  "shiny",
  "jpeg",
  "png"
)
## --- Install and Load Packages ---
# Utilizes a loop through the list of packages.
for (pkg in required_packages) {
  # Check whether the packages are already installed.
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # If not installed, prints a message and installs it.
    message(paste("Installing package:", pkg))
    install.packages(pkg)
  }
  # Loads the package into the current session.
  library(pkg, character.only = TRUE)
  message(paste("Package loaded:", pkg))
}

# === 2. User Interface Definition ===
# This section defines the layout and appearance of the ShinyApp UI.
ui <- fluidPage(
  # Set the title that appears in the browser tab/window.
  titlePanel("ASCII Image Converter"),
  # Define the main layout structure: Sidebar & main panel.
  sidebarLayout(
    # --- Sidebar Panel: Input Controls ---
    sidebarPanel(
      # Input control for uploading images.
      fileInput("image", "Choose a .jpg or .png Image", accept = c(".jpg", ".jpeg", ".png")),
      numericInput("max_width", "Maximum Width (characters)",
                   min = 10, max = 300, value = 80),
      # Numeric input for setting the maximum width of the ASCII array.
      sliderInput("fontsize", "Preview Font Size (px)",
                  min = 4, max = 20, value = 10),
      # Dropdown menu for choosing the download format.
      selectInput("save_format", "Save as:",
                  choices = c("Text (.txt)", "Image (.png)", "Image (.jpeg)")),
      # Download button initialization.
      downloadButton("downloadFile", "Download")
    ),
    # --- Main Panel: Output Display ---
    # Generates ASCII array within HTML as a preview. The character array can be copied from this as well.
    mainPanel(
      htmlOutput("ascii_display")
    )
  )
)

# === 3. Server Logic ===
# This section defines the backend logic for the application. It handles inputs
# from the UI, processes the image, and generates the ASCII art.
server <- function(input, output) {
  ## --- Reactive Values ---
  # Reactive values are used to store data that changes over time.
  img_data <- reactiveVal()
  orig_dims <- reactiveVal()
  ## --- Observer: Load and Process the Uploaded Image ---
  # This 'observeEvent' triggers whenever the 'input$image' changes as a file is
  # uploaded.
  observeEvent(input$image, {
    # 'req()' ensures that 'input$image' is not NULL.
    req(input$image)
    # Saves the file extension from the uploaded file path.
    ext <- tolower(tools::file_ext(input$image$datapath))
    # Read the image based on its file extension.
    img <- switch(ext,
                  "jpg" = readJPEG(input$image$datapath),
                  "jpeg" = readJPEG(input$image$datapath),
                  "png" = readPNG(input$image$datapath),
                  stop("Unsupported image format"))
    # Store image data and original dimensions in the reactive values.
    img_data(img)
    orig_dims(dim(img)[1:2])
  })
  ## --- Reactive Expression: Generate ASCII Art ---
  # This section handles the 'reactive' expression to calculate the ASCII art.
  # It will rerun automatically if 'img_data()' or 'input$max_width' change.
  ascii_lines <- reactive({
    # Ensures that image data is available before proceeding.
    req(img_data())
    img <- img_data()

    # --- Convert to Grayscale ---
    # Check if the image has 3 dimensions (height, width, color channels).
    if (length(dim(img)) == 3) {
      # Applies a standard luminosity formula for grayscal conversion.
      # Formula: Gray = 0.3*R + 0.59*G + 0.11*B
      gray <- 0.3 * img[,,1] + 0.59 * img[,,2] + 0.11 * img[,,3]
    } else {
      # If the image is already grayscale (2 dimensions), use it directly.
      gray <- img
    }
    # --- Calculate Target Dimensions ---
    # Estimate character aspect ratio for typical monospace fonts.
    char_aspect_ratio <- 2.2
    # Determine target width based on user input.
    target_width <- min(input$max_width, ncol(gray))
    # Calculate target height while adjusting aspect ratio.
    target_height <- round(target_width * (orig_dims()[1]/orig_dims()[2]) / char_aspect_ratio)

    # Ensure minimum dimensions to avoid errors.
    target_width <- max(10, target_width)
    target_height <- max(10, target_height)

    # --- Downsample the Image ---
    # Select rows and columns from the grayscale image to match the target dimensions
    # 'seq' generates evenly spaced indices across original dimensions.
    row_indices <- round(seq(1, nrow(gray), length.out = target_height))
    col_indices <- round(seq(1, ncol(gray), length.out = target_width))
    # Create the downsampled grayscale image using the selected indicies.
    gray <- gray[row_indices, col_indices]

    # --- Map Grayscale to ASCII Character ---
    # Define the character set, ordered from darkets to lightest.
    # More characters can provide finer detail.
    chars <- c("@", "#", "S", "%", "?", "*", "+", ";", ":", ",", ".")

    # Apply a function to each row of the downsampled grayscale image.
    ascii <- apply(gray, 1, function(row) {
      # Map grayscale values (0-1) to indicies in the 'chars' vector
      # 1. Multiply by # of chars - 1 to scale the range.
      # 2. Add 1 to shift the index range from 0-based to 1-based.
      # 3. Round to the nearest integer index.
      # 4. Use pmax and pmin to clamp indicies within valid range.
      char_indices <- pmin(pmax(round(row * (length(chars)-1)) + 1, 1), length(chars))
      paste(chars[char_indices], collapse = "")
    })
    # Return the vector of the ASCII character strings.
    ascii
  })

  ## --- Render UI: Display ASCII Art ---
  # This 'renderUI' function generates the HTML content for 'ascii_display' output.
  output$ascii_display <- renderUI({
    # Get the generated ASCII lines from the reactive expression above.
    ascii <- ascii_lines()
    # Ensure the ASCII data exists before rendering.
    req(ascii)
    # Combine the vector of ASCII lines into a single string with newline characters.
    ascii_text <- paste(ascii, collapse = "\n")
    # Define the CSS style for the output container.
    style <- paste0(
      "font-family: monospace; font-size: ",
      input$fontsize, "px; ",
      "white-space: pre; ",
      "overflow: auto; ",
      "max-height: 600px; ",
      "line-height: 1;"
    )
    # Create an HTML 'div' element containing the styled ASCII text.
    div(style = style, ascii_text)
  })
  ## --- Download Handler: Save Output ---
  # This section defines the logic executed when the download button is activated.
  output$downloadFile <- downloadHandler(
    # --- Define Filename ---
    # This function determines the name of the file to be downloaded.
    filename = function() {
      # Choose the file extension based on the user's selection in 'input$save_format'.
      paste0("ascii_output",
             ifelse(input$save_format == "Text (.txt)", ".txt",
                    ifelse(input$save_format == "Image (.png)", ".png", ".jpeg")))
    },
    # --- Generate File Content ---
    # This function generates the content to be saved.
    content = function(file) {
      ascii <- ascii_lines()
      req(ascii)

      # --- Save as Text ---
      if (input$save_format == "Text (.txt)") {
        writeLines(ascii, file)
      }
      # --- Save as Image ---
      else {
        # Determine image format.
        ext <- ifelse(input$save_format == "Image (.png)", "png", "jpeg")

        # --- Calculate Image Dimensions ---
        # Estimate character dimensions for rendering the image.
        # These might need adjustment based on the chosen font and char array.
        char_aspect_ratio <- 2.2
        n_lines <- length(ascii)
        max_chars <- max(nchar(ascii))

        # Character dimensions
        char_width <- 10
        char_height <- char_width * char_aspect_ratio
        padding <- 20

        # Calculate total image width and height with a padding border.
        width_px <- max_chars * char_width + padding
        height_px <- n_lines * char_height + padding

        # --- Create Image File ---
        if (ext == "png") {
          png(file, width = width_px, height = height_px)
        } else {
          jpeg(file, width = width_px, height = height_px)
        }

        # --- Plotting ---
        # Set graphics parameters: remove margins, set white background.
        par(mar = c(0, 0, 0, 0), bg = "white")
        # Create a new, blank plot.
        plot.new()
        # Set up the plotting coords (0 to 1 in both directions).
        plot.window(xlim = c(0, 1), ylim = c(0, 1))
        # 'adj' = c(0.5, 0.5)' centers the text block.
        # 'cex' might need adjustment depending on char_width/char_height for size.
        # 'family' = "mono" is for using a monospace font.
        text(0.5, 0.5, paste(ascii, collapse = "\n"),
             family = "mono", col = "black", adj = c(0.5, 0.5), cex = 1)
        # Close the graphics device, which writes the data to the file.
        dev.off()
      } # End of image saving block.
    } # End of content function.
  ) # End of downloadHandler.
} # End of server function.

# === 4. Run the Application ===
# This command launches the ShinyApp by combining the UI and Server Logic.
shinyApp(ui = ui, server = server)
