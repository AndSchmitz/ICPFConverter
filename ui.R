#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
TextAreaInputHeight_pixel <- 200

# Define UI for application that draws a histogram
fluidPage(

    titlePanel(
        title = "ICP Forests coordinate converter"
    ),
    
    tabsetPanel(
        
        id = "TabSetPanel",
    
        tabPanel(
        
            #Panel 1: WGS84 to ICPF------
            title = "WGS84 to ICPF",
        
            #_Header row-----
            fluidRow(
                column(
                    width = 6,
                    h3("WGS84 / EPSG4326"),
                    align = "center"
                ),
                
                column(
                    width = 6,
                    align = "center",
                    h3("ICP Forests format")
                )
            ),
            
            #_Column header row----
            fluidRow(
                column(
                    width = 3,
                    "Latitude",
                    align = "center"
                ),
                column(
                    width = 3,
                    "Longitude",
                    align = "center"
                ),
                column(
                    width = 3,
                    "Latitude",
                    align = "center"
                ),
                column(
                    width = 3,
                    "Longitude",
                    align = "center"
                )
            ),

            #_I/O row----
            fluidRow(
                
                column(
                    width = 3,
                    textAreaInput(
                        inputId = "Panel1_EPSG4326_Lat",
                        label = "",
                        height = TextAreaInputHeight_pixel,
                        value = "52.915000\n52.576111"
                    )
                ),
                
                column(
                    width = 3,
                    textAreaInput(
                        inputId = "Panel1_EPSG4326_Lon",
                        label = "",
                        height = TextAreaInputHeight_pixel,
                        value = "10.237222\n7.881111"
                    )
                ),
                
                column(
                    width = 3,
                    br(),
                    verbatimTextOutput(
                        outputId = "Panel1_ICPF_Lat"
                    )
                ),
                
                column(
                    width = 3,
                    br(),
                    verbatimTextOutput(
                        outputId = "Panel1_ICPF_Lon"
                    )
                )
                
            )# End of fluid row
                
        ), # End of first tab
        
        #Panel 2: ICPF to WGS84-----
        tabPanel(
            
            title = "ICPF to WGS84",
            
            #_Header row----
            fluidRow(
                column(
                    width = 6,
                    h3("ICP Forests format"),
                    align = "center"
                ),
                
                column(
                    width = 6,
                    align = "center",
                    h3("WGS84 / EPSG4326")
                )
            ),
            
            #_Column header row----
            fluidRow(
                column(
                    width = 3,
                    "Latitude",
                    align = "center"
                ),
                column(
                    width = 3,
                    "Longitude",
                    align = "center"
                ),
                column(
                    width = 3,
                    "Latitude",
                    align = "center"
                ),
                column(
                    width = 3,
                    "Longitude",
                    align = "center"
                )
            ),
            
            #_I/O row----
            fluidRow(
                
                column(
                    width = 3,
                    textAreaInput(
                        inputId = "Panel2_ICPF_Lat",
                        label = "",
                        value = c("+525401\n+523432"),
                        height = TextAreaInputHeight_pixel
                    )
                ),
                
                column(
                    width = 3,
                    textAreaInput(
                        inputId = "Panel2_ICPF_Lon",
                        label = "",
                        height = TextAreaInputHeight_pixel,
                        value = c("+101431\n+075242")
                    )
                ),
                
                column(
                    width = 3,
                    br(),
                    verbatimTextOutput(
                        outputId = "Panel2_EPSG4326_Lat"
                    )
                ),
                
                column(
                    width = 3,
                    br(),
                    verbatimTextOutput(
                        outputId = "Panel2_EPSG4326_Lon"
                    )
                )
                
            )# End of fluid row
            
        ) #end of second tab
        
    ) #end of tabset panel
    
) #end of fluidpage

