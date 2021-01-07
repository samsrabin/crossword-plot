#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("setup_import.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # # Application title
    # titlePanel("My NYT Crossword Times"),
    
    title = "My NYT Crossword Times",
    
    fluidRow(
        
        # Input: Specification of range within an interval
        column(6, offset = 0, style='padding-left:40px',
               sliderInput("daterange", "Date range:",
                           min = as.Date("2019-01-07"), max = as.Date(Sys.Date()),
                           value = c(as.Date("2019-01-07"), as.Date(Sys.Date())),
                           timeFormat="%Y-%m-%d", 
                           width = "95%")
        ),
        
        # Input: Days to include
        column(4, offset = 1, style='padding-left:40px',
               checkboxGroupInput("incldays", "Days to include",
                                  daylist,
                                  selected = daylist,
                                  width = "95%",
                                  inline = TRUE)
        )
    ),
    
    hr(),
    
    # Show a plot of the generated distribution
    tabsetPanel(
        tabPanel("Plot", plotOutput("xwplot", width="100%")),
        tabPanel("Table", tableOutput("table"))
    )

)

# Define server logic ----
server <- function(input, output, session) {
    
    # autoWidth=TRUE and scrollX=T required for fluidRow columns to work properly
    # https://stackoverflow.com/questions/34850382/setting-column-width-in-r-shiny-datatable-does-not-work-in-case-of-lots-of-colum
    options = list(autoWidth = TRUE,
                   scrollX = T)
    
    # Reactive expression to create data frame of all input values
    sliderValues <- reactive({
        data.frame(
            Name = c("DateRange"),
            Value = input$daterange,
            stringsAsFactors = FALSE)
    })
    
    output$table <- renderTable({
        # Get the records in the date range we've specified
        DateRange <- input$daterange
        thisrange_df = xwords_df[xwords_df$PuzzleDate>=as.Date(DateRange[1]) & xwords_df$PuzzleDate<=as.Date(DateRange[2]),]
        thisrange_df <- thisrange_df[order(thisrange_df$PuzzleDate),]
        
        out <- thisrange_df %>%
            group_by(Day) %>%
            summarise(Fastest = as.duration(min(SolveTime)),
                      Median = median(SolveTime),
                      Mean = mean(SolveTime),
                      Slowest = max(SolveTime))
        out$Day <- factor(out$Day, levels=c(daylist))
        out <- out[order(out$Day), ]
        fastest <- format_duration(out$Fastest)
        slowest <- format_duration(out$Slowest)
        for (d in 1:7) {
            fastest[d] <- paste(fastest[d], " (", thisrange_df$PuzzleDate[as.numeric(thisrange_df$SolveTime)==as.numeric(out$Fastest[d])][1], ")", sep="")
            slowest[d] <- paste(slowest[d], " (", thisrange_df$PuzzleDate[as.numeric(thisrange_df$SolveTime)==as.numeric(out$Slowest[d])][1], ")", sep="")
        }
        out$Fastest <- fastest
        out$Median <- format_duration(ceiling(out$Median))
        out$Mean <- format_duration(ceiling(out$Mean))
        out$Slowest <- slowest
        out
        })
    
    output$xwplot <- renderPlot({
        
        # Get the records in the date range we've specified
        DateRange <- input$daterange
        thisrange_df = xwords_df[xwords_df$PuzzleDate>=as.Date(DateRange[1]) & xwords_df$PuzzleDate<=as.Date(DateRange[2]),]
        thisrange_df = thisrange_df[order(thisrange_df$PuzzleDate),]

        for (d in daylist) {
            if (!any(input$incldays==d)) {
                thisrange_df = thisrange_df[thisrange_df$Day != d,]
            }
        }
        
        # Ancillaries
        give.n <- function(x){
            return(c(y = median(x)*1.05, label = length(x))) 
            # experiment with the multiplier to find the perfect position
        }
        give.n_zero <- function(x){
            return(c(y = median(x)*0, label = length(x))) 
        }
        give.MS_text <- function(x) {
            strptime(x, format="%M:%S")
        }
        give.DOW_max_ypos <- function(x,y){
            max_time <- max(x$SolveTime[x$Day2==y])
            #return(as.numeric(max_time, units="mins")+1.25) 
            return(max_time/60+1.25) 
        }
        give.DOW_min_ypos <- function(x,y){
            min_time <- min(x$SolveTime[x$Day2==y])
            #return(as.numeric(min_time, units="mins")-1.25) 
            return(min_time/60-1.25) 
        }
        give.DOW_max_label <- function(x,y){
            max_time <- max(x$SolveTime[x$Day2==y])
            max_time_min <- floor(as.numeric(max_time, units="mins"))
            max_time_sec <- as.numeric(max_time, units="secs") %% 60
            max_time_string <- paste(max_time_min, max_time_sec, sep=":")
            return(max_time_string)
        }
        give.DOW_max_date <- function(x,y){
            x2 <- x[x$Day2==y,]
            x3 <- x2[order(-x2$SolveTime),]
            return(x3$PuzzleDate[1])
        }
        give.DOW_min_date <- function(x,y){
            x2 <- x[x$Day2==y,]
            x3 <- x2[order(x2$SolveTime),]
            return(x3$PuzzleDate[1])
        }
        
        # ggplot elements
        theme_just_ymajors <- theme(panel.grid.minor = element_blank(),
                                    panel.grid.major.x = element_blank(),
                                    panel.grid.major = element_line(color = "#E0E0E0"),
                                    panel.background = element_blank())
        outlier_label_size = 4.5
        
        # Set up color palette, removing unchecked days but keeping the colors the same
        mypalette = brewer.pal(n=7, name="Paired")
        mypalette = mypalette[is.element(daylist,input$incldays)]
        
        # Scatter plot with smoothed line: Current streak ----
        # http://www.sthda.com/sthda/RDoc/images/rcolorbrewer.png
        pmain <- ggplot(thisrange_df, aes(x=PuzzleDate, y=as.integer(SolveTime)/60, color = Day2)) +
            geom_point(shape=1, stroke = 0.8) +
            geom_smooth(method = "loess", se=F) +
            theme_just_ymajors +
            ggtitle(sprintf("Current streak: %d days (showing %d days)", Nstreak, length(thisrange_df$SolveTime))) +
            ylab("Time (minutes)") +
            theme(legend.key=element_blank(), 
                  plot.title = element_text(size=14, face="bold"),
                  axis.title.x = element_blank(),
                  axis.text.x = element_text(size=14, angle=45, hjust=1), 
                  axis.text.y = element_text(size=14),
                  axis.title.y = element_text(size=14, margin=margin(r=10)), 
                  axis.ticks.y = element_blank(), 
                  axis.ticks.length = unit(0.25, "cm"), 
                  axis.ticks.x = element_line(color="#E0E0E0"), 
                  legend.text=element_text(size=14)) +
            scale_color_manual(values=mypalette) +
            labs(color = element_blank()) +
            scale_y_continuous(limits = c(-2, max(thisrange_df$SolveTime)/60+1.5),
                               expand = c(0,0)) +
            scale_x_date(expand=c(0,0))
        
        
        annot1 <- function(x, this_day){
            min_time <- min(x$SolveTime[x$Day2==this_day])
            this_label <- sprintf("%d:%02d", floor(min_time/60), min_time %% 60)
            out <- annotate("text", 
                            x=this_day, 
                            y=give.DOW_min_ypos(thisrange_df, this_day), 
                            label=this_label, 
                            size=outlier_label_size,
                            angle = 90,
                            hjust=0.8)
            return(out) 
        }
        
        
        ybox <- axis_canvas(pmain, axis = "y") +
            theme_just_ymajors +
            geom_boxplot(data = thisrange_df, aes(x=Day2, y=as.integer(SolveTime)/60, color = Day2)) +
            scale_x_discrete() +
            scale_color_manual(values=mypalette) +
            expand_limits(x = -0.1)
        for (d in daylist) {
            if (any(input$incldays==d)) {
                ybox <- ybox + annot1(thisrange_df, d)
            }
        }
        p1 <- insert_yaxis_grob(pmain, ybox, grid::unit(1.5, "in"), position = "right")
        ggdraw(p1)
        
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
