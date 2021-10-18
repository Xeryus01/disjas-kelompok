library(methods)
library(base)
library(stats)
library(datasets)

library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(broom)
library(gganimate)
library(transformr)
library(gifski)
library(shinybusy)

myfile <- "https://raw.githubusercontent.com/Xeryus01/data/main/UNVR_fix.csv"

data1 <- read_csv(myfile)
data1$tanggal <- as.Date(data1$tanggal, "%m/%d/%Y")

#ini cuma buat background graph
theme_chris <- function (base_size = 12, base_family = "serif", ticks = TRUE) 
{
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(legend.background = element_blank(), legend.key = element_blank(), 
          panel.border = element_blank(), 
          strip.background = element_blank(), 
          panel.background = element_rect(fill = "#94B1C533", colour = NA),
          plot.background = element_rect(fill = "#ffffff"),
          axis.line = element_blank(), 
          panel.grid = element_blank(),
          axis.text.x = element_text(colour = "#2a3132"),
          axis.title.x = element_text(colour = "#2a3132"),
          axis.title.y = element_text(colour="#2a3132"),
          axis.text.y = element_text(colour="#2a3132"),
          axis.title = element_text(colour = "#2a3132"),
          plot.title = element_text(colour = "#2a3132", 
                                    margin = margin(0,0,10,0)),
          plot.subtitle = element_text(colour = "#2a3132"),
          plot.caption = element_text(colour = "#2a3132"),
          legend.title = element_text(colour = "#2a3132"),
          legend.text = element_text(colour = "#2a3132"))
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}

function(input, output){
  
  output$plot_stat <- renderPlot({
      label <- paste(format(as.Date(input$plotFrom), "%d %B %Y"),
                     'sampai',
                     format(as.Date(input$plotTo), "%d %B %Y"))
      
      data1 %>%
        filter(tanggal > input$plotFrom & tanggal < input$plotTo) %>%
        ggplot(aes(tanggal, harga)) + 
        geom_line(aes(y = harga, color = "Harga UNVR"), size = 1.2) + 
        geom_smooth(aes(y = harga, color = "Trend Linear"), size = 1, linetype = "dashed", method = "lm", formula = y ~ x, size = 1, se = FALSE) +
        geom_smooth(aes(y = harga, color = "Trend Kuadratik"), size = 1, linetype = "dashed",method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE)+
  
        labs(title = "Harga Saham Harian UNVR",
             subtitle = label,
             x = "Periode",
             y = 'Harga',
             color = "Keterangan")
  })
    
  observeEvent(input$race, {
    output$plot <- renderImage({
      show_modal_spinner()
      
      outfile <- tempfile(fileext='.gif')
      
      datat <- data1 %>%
        filter(tanggal > input$dateFrom & tanggal < input$dateTo)
      
      lin_mod <- lm(harga ~ tanggal, datat)
      quad_mod <- lm(harga ~ poly(tanggal, 2), datat)
      
      #augment trendline linear dan kuadratik biar bisa diambil titik"nya
      aug_mod <- augment(lin_mod)
      new_mod <- augment(quad_mod)
      
      #titik" di kuadratik jadiin satu variabel sama yang linear
      aug_mod$.quad <- new_mod$.fitted
      
      #colors <- c("Harga UNVR" = "red", "Trend Linear" = "darkgreen", "Trend Kuadratik" = "green")
      
      p <- aug_mod %>% 
        ggplot(aes(x=tanggal, y = harga)) + 
        #scale_color_manual(values = colors) +
        geom_line(aes(x=tanggal, y = harga, color="Harga UNVR"), size=1) +
        geom_point(size = 3) + 
        geom_text(aes(label=paste("Rp", harga,sep=" ")),color="darkblue",fontface="bold", vjust=-2, size=5)+
        geom_line(aes(y = .fitted, color="Trend Linear"), size = 0.8, linetype = "dashed") +
        geom_text(aes(y=.fitted, x = max(tanggal)+.1, label = sprintf("Rp %5.0f", .fitted)), hjust=0, vjust=0, size=5) +
        geom_segment(aes(y=.fitted, xend=max(tanggal), yend = .fitted), linetype=2, colour='blue') +
        geom_line(aes(y = .quad, color="Trend Kuadrati"), size = 0.8, linetype = "dashed") +
        geom_text(aes(y = .quad, x = max(tanggal)+.1, label = sprintf("Rp %5.0f", .quad)), hjust=0, vjust=0, size=5) +
        geom_segment(aes(y = .quad, xend=max(tanggal), yend = .quad), linetype=2, colour='blue') +
        view_follow(fixed_y = TRUE)+
        coord_cartesian(clip = 'off') + 
        labs(title = "Harga Saham Harian UNVR : {format(as.Date(input$dateFrom), '%d %B %Y')} sampai {format(as.Date(input$dateTo), '%d %B %Y')}",
             subtitle = "Tanggal saat ini : {format(as.Date(frame_along), '%d %B %Y')}",
             x = "Periode",
             y = 'Harga',
             color = "Keterangan") +
        enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
        theme_bw() +
        theme(axis.line = element_line(colour = "black")) +
        transition_reveal(tanggal)
      
      # use animate() to control nframes and pass shiny progress update function
      anim_save("outfile.gif", animate(p, fps=3, nframes = 100, height = 720, width = 1600))
      
      remove_modal_spinner()
      
      return(list(src = "outfile.gif",
                  contentType = 'image/gif',
                  height = 720, 
                  width = 1080,
                  alt = "This is alternate text"
      ))
    }, deleteFile = TRUE)
  })
}
