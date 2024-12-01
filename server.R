#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringi)

# Define server logic required to draw a histogram
function(input, output, session) {
    
    #Load helping functions------
    ListOfHelperFunctions <- list.files(
        path = file.path("HelpingFunctions"),
        pattern = ".R",
        recursive = T,
        full.names = T
    )
    sapply(
        X = ListOfHelperFunctions,
        FUN = source
    )
    

    #CheckedValuesLatWGS84---------------
    CheckedValuesLatWGS84 <- reactive({
        req(input$Panel1_EPSG4326_Lat)
        In <- input$Panel1_EPSG4326_Lat
        In <- unlist(stri_split_lines(In))
        if ( is.na(In[length(In)]) ) {
            In <- In[1:(length(In) - 1)]
        }
        if ( In[length(In)] == "" ) {
            In <- In[1:(length(In) - 1)]
        }
        if ( any(is.na(In)) ) {
            showNotification(
                ui = paste("Latitude contains NA"),
                type = "error"
            )
            return(NULL)
        }
        Out <- rep(NA, length(In))
        for ( i in 1:length(In) ) {
            Current <- In[i]
            CurrentNum <- suppressWarnings(as.numeric(Current))
            if ( is.na(CurrentNum) ) {
                showNotification(
                    ui = paste("Latitude", Current, "is not numeric."),
                    type = "error"
                )
            }
            Out[i] <- CurrentNum
        }
        
        return(Out)
    })
    
    
    #CheckedValuesLonWGS84--------
    CheckedValuesLonWGS84 <- reactive({
        req(input$Panel1_EPSG4326_Lon)
        In <- input$Panel1_EPSG4326_Lon
        In <- unlist(stri_split_lines(In))
        if ( is.na(In[length(In)]) ) {
            In <- In[1:(length(In) - 1)]
        }
        if ( In[length(In)] == "" ) {
            In <- In[1:(length(In) - 1)]
        }
        if ( any(is.na(In)) ) {
            showNotification(
                ui = paste("Longitude contains NA"),
                type = "error"
            )
            return(NULL)
        }
        Out <- rep(NA, length(In))
        for ( i in 1:length(In) ) {
            Current <- In[i]
            CurrentNum <- suppressWarnings(as.numeric(Current))
            if ( is.na(CurrentNum) ) {
                showNotification(
                    ui = paste("Longitude", Current, "is not numeric."),
                    type = "error"
                )
            }
            Out[i] <- CurrentNum
        }
        return(Out)
    })
    
    #ConvertedCoordsFromWGS84-------
    ConvertedCoordsFromWGS84 <- reactive({
        req(CheckedValuesLatWGS84())
        req(CheckedValuesLonWGS84())
        
        Lat <- CheckedValuesLatWGS84()
        Lon <- CheckedValuesLonWGS84()
        
        if ( is.null(Lat) ) {
            return(NULL)
        }
        if ( is.null(Lon) ) {
            return(NULL)
        }
    
        
        ConversionSuccess <- TRUE
        Coords_ICPF <- tryCatch(
            expr = {
                ConvertCoordsToICPF(
                    LatitudeWGS84Decimal = Lat,
                    LongitudeWGS84Decimal = Lon
                )
            },
            error = function(ErrorMessage) {
                ConversionSuccess <<- FALSE
                showNotification(
                    ui = ErrorMessage$message,
                    type = "error"
                )
            }
        ) #end of try catch
        
        if ( !ConversionSuccess ) {
            return(NULL)
        }
        
        #Coords_ICPF$LatICPFFormat)
        return(Coords_ICPF)
        
    })
    
    
    # #Update ICPFPanel1---------
    # observe({
    #     
    #     # input$Panel_EPSG4326_Lon
    #     
    #     # showNotification(
    #     #     ui = "Updated",
    #     #     type = "default"
    #     # )
    #     
    #     updateTextAreaInput(
    #         session,
    #         inputId = "Panel1_ICPF_Lat",
    #         value = paste(ConvertedCoordsFromWGS84()$LatICPFFormat, collapse = "\n")
    #     )
    #     
    #     updateTextAreaInput(
    #         session,
    #         inputId = "Panel1_ICPF_Lon",
    #         value = paste(ConvertedCoordsFromWGS84()$LonICPFFormat, collapse = "\n")
    #     )        
    #     
    #     
    # })

    
    #Update ICPFPanel1---------
    observe({
        
        req(input$Panel1_EPSG4326_Lat)
        req(input$Panel1_EPSG4326_Lon)
        
        LatsIn <- unlist(stri_split_lines(input$Panel1_EPSG4326_Lat))
        LonsIn <- unlist(stri_split_lines(input$Panel1_EPSG4326_Lon))
        
        ConversionSuccess <- TRUE
        Converted <- tryCatch(
            expr = {
                ConvertCoordsToICPF(
                    LatitudeWGS84Decimal = LatsIn,
                    LongitudeWGS84Decimal = LonsIn
                )
            },
            error = function(ErrorMessage) {
                ConversionSuccess <<- FALSE
                showNotification(
                    ui = ErrorMessage$message,
                    type = "error"
                )
            }
        ) #end of try catch
        
        if ( !ConversionSuccess ) {
            output$Panel1_ICPF_Lat <- renderText({
                ""
            })
            output$Panel1_ICPF_Lon <- renderText({
                ""
            })
            return()
        }
        
        output$Panel1_ICPF_Lat <- renderText({
            paste(Converted$Lat_ICFP, collapse = "\n")
        })
        
        output$Panel1_ICPF_Lon <- renderText({
            paste(Converted$Lon_ICFP, collapse = "\n")
        })  
        
    }) 
    
    #Update WGSPanel2--------
    observe({
        
        req(input$Panel2_ICPF_Lat)
        req(input$Panel2_ICPF_Lon)

        LatsIn <- unlist(stri_split_lines(input$Panel2_ICPF_Lat))
        LonsIn <- unlist(stri_split_lines(input$Panel2_ICPF_Lon))
        
        ConversionSuccess <- TRUE
        Converted <- tryCatch(
            expr = {
                ConvertCoordsFromICPF(
                    lats = LatsIn,
                    lons = LonsIn
                )
            },
            error = function(ErrorMessage) {
                ConversionSuccess <<- FALSE
                showNotification(
                    ui = ErrorMessage$message,
                    type = "error"
                )
            }
        ) #end of try catch
            
        if ( !ConversionSuccess ) {
            output$Panel2_EPSG4326_Lat <- renderText({
                ""
            })
            output$Panel2_EPSG4326_Lon <- renderText({
                ""
            })
            return()
        }

        output$Panel2_EPSG4326_Lat <- renderText({
            paste(Converted$Lat_WGS84, collapse = "\n")
        })
        
        output$Panel2_EPSG4326_Lon <- renderText({
            paste(Converted$Lon_WGS84, collapse = "\n")
        })  
        
    })
}
