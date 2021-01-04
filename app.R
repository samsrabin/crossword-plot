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
    
    # Application title
    titlePanel("My NYT Crossword Times"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input: Specification of range within an interval
            sliderInput("daterange", "Date range:",
                        min = as.Date("2019-01-07"), max = as.Date(Sys.Date()),
                        value = c(as.Date("2019-01-07"), as.Date(Sys.Date())),
                        timeFormat="%Y-%m-%d")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("xwplot")
            # tableOutput("values")
        )
    )
    
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    
    # Reactive expression to create data frame of all input values
    sliderValues <- reactive({
        
        data.frame(
            Name = c("DateRange"),
            Value = input$daterange,
            stringsAsFactors = FALSE)
        
    })
    
    # Show the values in an HTML table ----
    output$values <- renderTable({
        sliderValues()
    })
    
    
    
    output$xwplot <- renderPlot({
        
        # Get the records in the date range we've specified 
        DateRange <- input$daterange
        thisrange_df = xwords_df[xwords_df$PuzzleDate>=as.Date(DateRange[1]) & xwords_df$PuzzleDate<=as.Date(DateRange[2]),]
        thisrange_df = thisrange_df[order(thisrange_df$PuzzleDate),]
        
        # # Get week number (in streak)
        # w <- 0
        # for (d in seq(1, length(thisrange_df$PuzzleDate))) {
        #     thisDOW <- as.character(thisrange_df$Day2[d])
        #     if (thisDOW=="Mon") {
        #         print(thisDOW)
        #         w <- w + 1
        #     }
        #     thisrange_df$WeekNum[d] <- w
        # }
        
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
        outlier_label_size = 3.25
        
        
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
                  axis.text.x = element_text(size=12, angle=45, hjust=1), 
                  axis.text.y = element_text(size=12),
                  axis.title.y = element_text(size=13, margin=margin(r=10)), 
                  axis.ticks.y = element_blank(), 
                  axis.ticks.length = unit(0.25, "cm"), 
                  axis.ticks.x = element_line(color="#E0E0E0"), 
                  legend.text=element_text(size=12)) +
            scale_color_brewer(palette="Paired") +
            labs(color = element_blank()) +
            scale_y_continuous(limits = c(-2, max(thisrange_df$SolveTime)/60+1.5),
                               expand = c(0,0)) +
            annotate(geom="label", x=as_date(give.DOW_max_date(thisrange_df, "Sun")), 
                     y=give.DOW_max_ypos(thisrange_df, "Sun"), 
                     label=give.DOW_max_date(thisrange_df, "Sun"), 
                     size=outlier_label_size, 
                     hjust=-0.1, vjust=1, fill="#FCBE75") +
            annotate(geom="label", x=as_date(give.DOW_max_date(thisrange_df, "Wed")), 
                     y=give.DOW_max_ypos(thisrange_df, "Wed"), 
                     label=give.DOW_max_date(thisrange_df, "Wed"), 
                     size=outlier_label_size, 
                     hjust=-0.1, vjust=1, fill="#B3DE8E") +
            annotate(geom="label", x=as_date(give.DOW_max_date(thisrange_df, "Sat")), 
                     y=give.DOW_max_ypos(thisrange_df, "Sat"), 
                     label=give.DOW_max_date(thisrange_df, "Sat"), 
                     size=outlier_label_size, 
                     hjust=1, vjust=0.2, fill="#E01F27", color="white") +
            annotate(geom="label", x=as_date(give.DOW_max_date(thisrange_df, "Fri")), 
                     y=give.DOW_max_ypos(thisrange_df, "Fri"), 
                     label=give.DOW_max_date(thisrange_df, "Fri"), 
                     size=outlier_label_size, 
                     hjust=-0.1, vjust=1, fill="#F99B9B", color="white") #+
        #scale_x_date(breaks = seq(as_date("2019-01-01"), as_date("2020-01-31"), by="month"), labels = c("Jan. '19", rep("", 2), "Apr. '19", rep("", 2), "Jul. '19", rep("", 2), "Oct. '19", rep("", 2), "Jan. '20"))
        
        #ggMarginal(plot_scatter, type = "boxplot", margins = "y", fill="transparent", groupColour=TRUE)
        
        
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
            scale_color_brewer(palette="Paired") +
            ylab("test") +
            annot1(thisrange_df, "Mon") +
            annot1(thisrange_df, "Tue") +
            annot1(thisrange_df, "Wed") +
            annot1(thisrange_df, "Thu") +
            annot1(thisrange_df, "Fri") +
            annot1(thisrange_df, "Sat") +
            annot1(thisrange_df, "Sun")
        p1 <- insert_yaxis_grob(pmain, ybox, grid::unit(1.25, "in"), position = "right")
        ggdraw(p1)
        
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
