
# INTRO and TOUR ----------------------------------------------------

# #run intro modal
observeEvent(c("", input$runIntro), {
  showModal(modalDialog(
    includeHTML("intro_text.html"),
    easyClose = TRUE,
    footer = tagList(
      actionButton(inputId = "intro", label = "Introduction Tour", icon = icon("info-circle")),
      actionButton(inputId = "closeIntro", label = "Close")
    )
  ))
})

observeEvent(c(input$intro,
               input$closeIntro),{

                 if(input$intro > 0) return(removeModal())
                 if(input$closeIntro == 0) return()
                 removeModal()
               })

# run intro tour
observeEvent(input$intro,
             introjs(session, options = list("nextLabel" = "Continue",
                                             "prevLabel" = "Previous",
                                             "doneLabel" = "Explore on Your Own!"))
)

# DYNAMIC RENDER RULES ----------------------------------------------------

observeEvent("", {
  #deafining default tab to show at start-up
  shinyjs::show("introPanel")
  shinyjs::hide("loadDataPanel")
  shinyjs::hide("runInterPanel")
  shinyjs::hide("viewResPanel")
  shinyjs::hide("aboutUsPanel")
  cat(file = stderr(),"Done startup panels\n")
}, once = TRUE,
label="OEpanelInitialLoading")

observeEvent(input$introBut, {

  uiReactiveValues$tab = "intro"
  shinyjs::show("introPanel")
  shinyjs::hide("loadDataPanel")
  shinyjs::hide("runInterPanel")
  shinyjs::hide("viewResPanel")
  shinyjs::hide("aboutUsPanel")
  cat(file = stderr(),"Done introBut\n")
},
label="OEpanelIntroPanel")

observeEvent(input$loadDataBut, {
  uiReactiveValues$tab = "loadData"
  shinyjs::hide("introPanel")
  shinyjs::show("loadDataPanel")
  shinyjs::hide("runInterPanel")
  shinyjs::hide("viewResPanel")
  shinyjs::hide("aboutUsPanel")
  cat(file = stderr(),"Done loadDataBut\n")
},
label="OEpanelLoadData")

observeEvent(input$runInterBut, {
  uiReactiveValues$tab = "runInter"
  shinyjs::hide("introPanel")
  shinyjs::hide("loadDataPanel")
  shinyjs::show("runInterPanel")
  shinyjs::hide("viewResPanel")
  shinyjs::hide("aboutUsPanel")
},
label="OEpanelRunInter")

observeEvent(input$viewResBut, {
  uiReactiveValues$tab = "viewRes"
  shinyjs::hide("introPanel")
  shinyjs::hide("loadDataPanel")
  shinyjs::hide("runInterPanel")
  shinyjs::show("viewResPanel")
  shinyjs::hide("aboutUsPanel")
},
label="OEpanelViewRes")

observeEvent(input$aboutUsBut, {
  uiReactiveValues$tab = "aboutUs"
  shinyjs::hide("introPanel")
  shinyjs::hide("loadDataPanel")
  shinyjs::hide("runInterPanel")
  shinyjs::hide("viewResPanel")
  shinyjs::show("aboutUsPanel")
},
label="OEpanelAboutUs")

uiReactiveValues = reactiveValues(tab = "intro")

observeEvent(uiReactiveValues$tab, {

  x <- uiReactiveValues$tab
  updateButton(session, "introBut", style = {
    if (x == "intro") {
      paste("warning")
    } else {
      paste("success")
    }
  })
  updateButton(session, "loadDataBut", style = {

    if (x == "loadData") {
      paste("warning")
    } else {
      paste("success")
    }
  })
  updateButton(session, "runInterBut", style = {
    if (x == "runInter") {
      paste("warning")
    } else {
      paste("success")
    }
  })
  updateButton(session, "viewResBut", style = {
    if (x == "viewRes") {
      paste("warning")
    } else {
      paste("success")
    }
  })
  updateButton(session, "aboutUsBut", style = {
    if (x == "aboutUs") {
      paste("warning")
    } else {
      paste("success")
    }
  })
},label="OEalterUIButtons")

output$logoPlot = renderPlotly(logoPlot)
