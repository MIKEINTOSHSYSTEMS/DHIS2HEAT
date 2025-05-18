library(shinyjs)
library(shinyWidgets)
library(shinydashboard)

ui <- dashboardPage(
    useShinyjs(),
    header = dashboardHeader(disable = T),
    sidebar = dashboardSidebar(disable = T),
    body = dashboardBody(
        tags$head(tags$script(
            #src = "https://cdn.tiny.cloud/1/rui9r56yqtagda6h8pwym7yiy9lagf0l814k1g44hv5548uj/tinymce/6/tinymce.min.js",
            src = "./www/js/tinymce.js",
            referrerpolicy = "origin"
        )),

        # Register a custom message handler that gets the content of the editor
        # and forces update of the textarea
        tags$head(tags$script("Shiny.addCustomMessageHandler('getTxt',
        function(message) {
          var content = tinymce.activeEditor.getContent();;
          Shiny.onInputChange('tinyTxt', content);
          })")),
        flowLayout(
            actionButton("open", "Open"),
            htmlOutput("content")
        )
    )
)

myModal <- function() {
    modalDialog(
        size = "l",
        title = "A modal dialog",
        textAreaInput("tinyTxt", "the content"),
        actionButton("ok", "OK"),
        easyClose = T
    )
}

server <- function(input, output, session) {
    observeEvent(
        input$open,
        {
            showModal(myModal())

            # Create the tinyMCE editor
            runjs("var ed = new tinymce.Editor('tinyTxt', {
        menubar: false,
branding: false,
plugins: 'lists, table, link',
contextmenu: 'lists, link, table',
toolbar1: 'bold italic forecolor backcolor | formatselect fontselect fontsizeselect | alignleft aligncenter alignright alignjustify',
toolbar2: 'undo redo removeformat bullist numlist table blockquote code superscript  subscript strikethrough link'},
        tinymce.EditorManager);
        ed.render();")
        }
    )

    observeEvent(
        input$ok,
        {
            # Retrieve the content of the editor
            session$sendCustomMessage("getTxt", "")

            output$content <- renderText(
                input$tinyTxt
            )

            delay(500, print(input$tinyTxt))

            removeModal()
        }
    )
}

shinyApp(ui, server = server)