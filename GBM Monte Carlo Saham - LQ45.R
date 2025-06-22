library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(dplyr)
library(DT)
library(quantmod)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # ===== Tambahkan CSS responsif global =====
  tags$head(
    tags$style(HTML("
      .shiny-plot-output {
        max-width: 100%;
        height: auto;
      }

      .responsive-dt-container {
        max-width: 100%;
        overflow-x: auto;
      }

      .responsive-textbox {
        max-width: 100%;
        white-space: pre-wrap;
        word-wrap: break-word;
      }

      .container-fluid, .row, .col {
        padding-left: 10px;
        padding-right: 10px;
      }

      .warning-text {
        color: red;
        font-weight: bold;
        text-align: center;
      }
    "))
  ),
  
  navbarPage("-- GUI R --",
             
             #### Inventor Tab ####
             tabPanel("Inventor",
                      h4("Prediksi Harga Saham (LQ45) Menggunakan Metode Geometric Brownian Motion (GBM) dengan Simulasi Monte Carlo", style = "text-align:center"),
                      br(), br(), br(),
                      h5("Inventor:", style = "text-align:center"), 
                      h5("Zahra Ulaya Sifa", style = "text-align:center"), 
                      h5("Bachelor Degree of Statistics", style = "text-align:center"), 
                      h5("Diponegoro University", style = "text-align:center"),
                      br(),
                      h5("Contact:", style = "text-align:center"),
                      h5(tags$a("LinkedIn: Zahra Ulaya Sifa", href = "https://www.linkedin.com/in/zahraulayasifa/", target = "_blank", style = "color: #66ccff; text-align:center; display:block;")),
                      h5(tags$a("Email: zahraulayasifa.work@gmail.com", href = "mailto:zahraulayasifa.work@gmail.com", target = "_blank", style = "color: #66ccff; text-align:center; display:block;")),
                      h5(tags$a("Instagram: @zhrauysf", href = "https://www.instagram.com/zhrauysf/", target = "_blank", style = "color: #66ccff; text-align:center; display:block;"))
             ),
             
             #### Input Data & Uji Normalitas ####
             tabPanel("Input Data & Uji Normalitas",
                      tabPanel("Penel", 
                               fluidRow(
                                 column(12, h3("Input Data")),
                                 column(4,
                                        selectInput("selected_stock", "Pilih Saham (LQ45):",
                                                    choices = c("Pilih Saham" = "", "ACES.JK", "ADMR.JK", "ADRO.JK", "AKRA.JK", "AMMN.JK", "AMRT.JK", "ANTM.JK", "ARTO.JK", 
                                                                "ASII.JK", "BBCA.JK", "BBNI.JK", "BBRI.JK", "BBTN.JK", "BMRI.JK", "BRIS.JK", "BRPT.JK", "BUKA.JK", "CPIN.JK", 
                                                                "ESSA.JK", "EXCL.JK", "GOTO.JK", "ICBP.JK", "INCO.JK", "INDF.JK", "INKP.JK", "INTP.JK", "ISAT.JK", "ITMG.JK", 
                                                                "JSMR.JK", "KLBF.JK", "MAPI.JK", "MBMA.JK", "MDKA.JK", "MEDC.JK", "PGAS.JK", "PGEO.JK", "PTBA.JK", "SIDO.JK", 
                                                                "SMGR.JK", "SMRA.JK", "TLKM.JK", "TOWR.JK", "UNTR.JK", "UNVR.JK", "CTRA.JK"),
                                                    selected = ""
                                        ),
                                        actionButton("reset_stock", "Reset Pilihan Saham"),
                                        dateRangeInput("date_range", "Pilih Rentang Tanggal:",
                                                       start = "2025-01-01", end = Sys.Date())
                                 ),
                                 column(4, sliderInput("train_percentage", "Persentase Data Training:", min = 10, max = 100, value = 70, step = 1))
                               ),
                               fluidRow(
                                 column(4, h4(textOutput("data_uploaded_text")), tableOutput("data_table")),
                                 column(4, h4(textOutput("train_data_text")), tableOutput("train_table")),
                                 column(4, h4(textOutput("test_data_text")), tableOutput("test_table"))
                               )
                      ),
                      tabPanel("Perhitungan Return & Uji Normalitas",
                               fluidRow(
                                 column(12, h3("Uji Normalitas K-S Return Data Training")),
                                 column(12, div(class = "responsive-textbox", verbatimTextOutput("ks_test_result"))),
                                 column(12, tableOutput("return_table"))
                               )
                      )
             ),
             
             #### Monte Carlo Simulation Tab ####
             tabPanel("Hasil Simulasi Monte Carlo",
                      fluidRow(column(12, h3("Hasil Simulasi Monte Carlo dengan GBM"))),
                      fluidRow(
                        column(3, numericInput("seed_value", "Nilai set.seed:", value = 123, min = 1, max = 100000, step = 1)),
                        column(3, numericInput("num_simulations", "Jumlah Simulasi:", value = 10, min = 1, max = 10000, step = 1)),
                        column(3, numericInput("suku_bunga", "Suku Bunga (r)", value = 0.06, min = 0, max = 1, step = 0.001)),
                        column(3, numericInput("n_prediksi", "Langkah Prediksi (hari)", value = 10, min = 1, max = 100000, step = 1))
                      ),
                      fluidRow(column(12, plotOutput("monte_carlo_plot"))),
                      fluidRow(column(12, div(class = "responsive-dt-container", DTOutput("simulations_table"))))
             ),
             
             #### Hasil Prediksi Tab ####
             tabPanel("Hasil Prediksi Harga Saham & Evaluasi Model",
                      fluidRow(
                        column(12, h3("Tabel Perbandingan Data Testing (Harga Aktual) vs Hasil Prediksi")),
                        column(12, div(class = "responsive-dt-container", DTOutput("comparison_table"))),
                        
                        column(12, h3("Evaluasi Model Prediksi")),
                        column(12, div(style = "max-width: 600px; width: 100%; white-space: pre-wrap; word-wrap: break-word;",verbatimTextOutput("model_evaluation"))),
                        
                        column(12, h3("Hasil Prediksi Sesuai Input Langkah")),
                        column(12, div(style = "max-width: 600px; width: 100%; overflow-x: auto;",DTOutput("hasil_prediksi"))),
                        
                        column(12, h3("Grafik Perbandingan Harga Aktual vs Harga Prediksi")),
                        column(12, plotOutput("comparison_plot"))
                      )
             )
  )
)


library(shiny)
library(quantmod)
library(tidyr)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$selected_stock, input$date_range)
    stock_symbol <- input$selected_stock
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    # Ambil data dari Yahoo Finance
    stock_data <- suppressWarnings(
      getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    )
    
    df <- data.frame(Date = format(as.Date(index(stock_data)), "%Y-%m-%d"),
                     Close = as.numeric(Cl(stock_data)))
    df <- na.omit(df)
    df
  })
  
  train_data <- reactive({
    req(data())
    temp_data <- data()
    train_size <- floor(input$train_percentage / 100 * nrow(temp_data))
    train_data <- temp_data[1:train_size, ]
    
    train_data$Return <- c(NA, diff(log(train_data$Close)))
    train_data$Number <- seq_len(nrow(train_data))
    train_data <- train_data[!is.na(train_data$Return), ]
    train_data
  })
  
  test_data <- reactive({
    req(data())
    temp_data <- data()
    train_size <- floor(input$train_percentage / 100 * nrow(temp_data))
    test_data <- temp_data[(train_size + 1):nrow(temp_data), ]
    test_data$Number <- seq_len(nrow(test_data)) + max(train_data()$Number)
    test_data
  })
  
  output$data_table <- renderTable({
    req(data())
    temp_data <- data()
    temp_data$Number <- seq_len(nrow(temp_data))
    temp_data
  })
  
  output$data_uploaded_text <- renderText({
    req(data())
    paste("Data yang Diunggah ( Total", nrow(data()), "Data ):")
  })
  
  output$train_table <- renderTable({
    train <- train_data()
    train$Return <- formatC(train$Return, format = "f", digits = 4)  # Alternatif: tampil sebagai karakter tapi rapi
    train
  })
  
  observeEvent(input$reset_stock, {
    updateSelectInput(session, "selected_stock", selected = "")
  })
  
  
  output$test_table <- renderTable({
    test_data()
  })
  
  output$train_data_text <- renderText({
    paste("Data Training (", nrow(train_data()), "Data ):")
  })
  
  output$test_data_text <- renderText({
    paste("Data Testing (", nrow(test_data()), "Data ):")
  })
  
  output$ks_test_result <- renderPrint({
    req(train_data())
    ks_test_result <- ks.test(train_data()$Return, "pnorm", mean = mean(train_data()$Return), sd = sd(train_data()$Return))
    alpha <- 0.05
    ks_statistic <- ks_test_result$statistic
    ks_p_value <- ks_test_result$p.value
    
    if (ks_p_value < alpha) {
      showModal(
        modalDialog(
          title = "Peringatan",
          "Data return tidak terdistribusi secara normal. Silakan unggah data yang berbeda karena hasil analisis yang tertampil tidak akan menjadi akurat.",
          easyClose = TRUE
        )
      )
    }
    
    cat(
      "Uji Normalitas (Kolmogorov-Smirnov Test) pada Return Harian Data Training:\n",
      "~ Hipotesis:\n",
      "H0: Data return mengikuti distribusi normal\n",
      "H1: Data return tidak mengikuti distribusi normal\n",
      "~ Taraf Signifikansi: alpha =", alpha, "\n",
      "~ Daerah Kritis: Tolak H0 jika p-value < alpha\n",
      "~ Statistik Uji: D =", round(ks_statistic,4), "dan p-value =", signif(ks_p_value,4), "\n",
      if (ks_p_value < alpha) {
        paste0("~ Kesimpulan: Tolak H0, data return tidak mengikuti distribusi normal karena nilai p-value = ", signif(ks_p_value,4), " < alpha = ", alpha, "\n")
      } else {
        paste0("~ Kesimpulan: Gagal menolak H0, data return mengikuti distribusi normal karena nilai p-value = ", signif(ks_p_value,4), " >= alpha = ", alpha, "\n")
      }
    )
  })
  
  # Fungsi simulasi GBM Monte Carlo (step per hari)
  simulate_gbm <- function(seed_value, num_simulations, initial_price, r, sigma, n_testing, n_prediksi, dt = 1/252) {
    n_langkah <- n_testing + n_prediksi
    
    simulations <- matrix(0, nrow = num_simulations, ncol = n_langkah)
    
    set.seed(seed_value + 1)
    Z <- rnorm(num_simulations)
    simulations[, 1] <- initial_price * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
    
    for (langkah in 2:n_langkah) {
      set.seed(seed_value + langkah)
      Z <- rnorm(num_simulations)
      simulations[, langkah] <- simulations[, langkah - 1] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
    }
    
    sim_df <- data.frame(simulations)
    sim_df$Simulation <- 1:num_simulations
    sim_df <- pivot_longer(sim_df, cols = -Simulation, names_to = "Day", values_to = "Price")
    sim_df$Day <- as.numeric(gsub("X", "", sim_df$Day))
    
    list(simulations = simulations, simulations_df = sim_df)
  }
  
  observe({
    req(train_data(), test_data())
    
    Vt <- train_data()$Close[nrow(train_data())]
    sigma <- sd(train_data()$Return) * sqrt(252)  # volatilitas tahunan
    r <- input$suku_bunga
    n_testing <- nrow(test_data())
    n_prediksi <- input$n_prediksi
    
    results <- simulate_gbm(
      seed_value = input$seed_value,
      num_simulations = input$num_simulations,
      initial_price = Vt,
      r = r,
      sigma = sigma,
      n_testing = n_testing,
      n_prediksi = n_prediksi,
      dt = 1/252
    )
    
    final_prices <- colMeans(results$simulations)
    
    # Plot Simulasi Monte Carlo
    output$monte_carlo_plot <- renderPlot({
      ggplot(results$simulations_df, aes(x = Day, y = Price, group = Simulation)) +
        geom_line(alpha = 0.7, color = "blue", size = 0.5) +
        labs(title = "Simulasi Monte Carlo GBM", x = "Hari", y = "Harga Aset") +
        theme_minimal()
    })
    
    # Tabel simulasi
    output$simulations_table <- DT::renderDT({
      results$simulations_df
    })
    
    # Tabel Perbandingan Harga Aktual vs Hasil Prediksi
    output$comparison_table <- renderDT({
      actual_prices <- test_data()$Close
      dates <- test_data()$Date
      
      # Sesuaikan panjang final_prices jika lebih panjang
      if(length(final_prices) > length(actual_prices)) {
        final_prices_use <- final_prices[1:length(actual_prices)]
      } else {
        final_prices_use <- final_prices
      }
      
      comparison_df <- data.frame(
        `Tanggal` = dates,
        `Harga_Aktual` = actual_prices,
        `Harga_Prediksi_Testing` = final_prices_use
      )
      
      datatable(comparison_df, options = list(pageLength = 10))
    })
    
    # Evaluasi Model Prediksi
    output$model_evaluation <- renderPrint({
      actual_prices <- test_data()$Close
      
      if(length(final_prices) > length(actual_prices)) {
        final_prices_use <- final_prices[1:length(actual_prices)]
      } else {
        final_prices_use <- final_prices
      }
      
      mse <- mean((final_prices_use - actual_prices)^2)
      rmse <- sqrt(mse)
      mape <- mean(abs((final_prices_use - actual_prices) / actual_prices)) * 100
      smape <- mean(2 * abs(final_prices_use - actual_prices) / (abs(final_prices_use) + abs(actual_prices))) * 100
      
      cat(
        "Evaluasi Model Prediksi:\n",
        "Mean Squared Error (MSE):", round(mse, 6), "\n",
        "Root Mean Squared Error (RMSE):", round(rmse, 6), "\n",
        "Symmetric Mean Absolute Percentage Error (SMAPE):", round(smape, 2), "%\n",
        "Mean Absolute Percentage Error (MAPE):", round(mape, 2), "%\n"
      )
    })
    
    # Tabel hasil prediksi sesuai input langkah prediksi
    output$hasil_prediksi <- renderDT({
      req(test_data(), input$n_prediksi, final_prices)
      
      n_testing <- nrow(test_data())
      n_prediksi <- input$n_prediksi
      n_langkah <- n_testing + n_prediksi
      
      langkah_prediksi <- 1:n_prediksi
      
      comparison_hasil <- data.frame(
        `Langkah_Prediksi` = langkah_prediksi,
        `Harga_Prediksi_Langkah` = final_prices[(n_testing + 1):n_langkah]
      )
      
      datatable(
        comparison_hasil,
        options = list(
          pageLength = 10,
          autoWidth = FALSE
        ),
        rownames = FALSE
      )
    })
    
    # Grafik Perbandingan Data Training, Testing, dan Prediksi
    output$comparison_plot <- renderPlot({
      actual_prices <- test_data()$Close
      train_prices <- train_data()$Close
      
      train_days <- seq_len(length(train_prices))
      test_days <- seq_len(length(actual_prices)) + max(train_days)
      
      # Panjang prediksi langkah (input$n_prediksi harus angka)
      n_prediksi <- input$n_prediksi
      pred_days <- seq_len(n_prediksi) + max(test_days)
      
      # Pastikan variabel ini ada dan benar
      n_testing <- length(actual_prices)
      n_langkah <- length(final_prices)
      
      # Prediksi yang matching dengan test data
      if(length(final_prices) > n_testing) {
        final_prices_use <- final_prices[1:n_testing]
      } else {
        final_prices_use <- final_prices
      }
      
      # Prediksi langkah ke depan (beyond test data)
      pred_prices <- if(n_langkah > n_testing) {
        final_prices[(n_testing + 1):n_langkah]
      } else {
        numeric(0)  # kosong kalau gak ada prediksi langkah lanjut
      }
      
      df_train <- data.frame(Day = train_days, Price = train_prices, Type = "Harga Aktual (Training)")
      df_test <- data.frame(Day = test_days, Price = actual_prices, Type = "Harga Aktual (Testing)")
      df_pred1 <- data.frame(Day = test_days, Price = final_prices_use, Type = "Harga Prediksi Testing")
      df_pred2 <- data.frame(Day = pred_days, Price = pred_prices, Type = "Harga Prediksi Langkah")
      
      combined_df <- rbind(df_train, df_test, df_pred1)
      if(nrow(df_pred2) > 0) {
        combined_df <- rbind(combined_df, df_pred2)
      }
      
      ggplot(combined_df, aes(x = Day, y = Price, color = Type)) +
        geom_line(size = 1) +
        labs(x = "Hari", y = "Harga") +
        scale_color_manual(values = c(
          "Harga Aktual (Training)" = "grey",
          "Harga Aktual (Testing)" = "orange",
          "Harga Prediksi Testing" = "red",
          "Harga Prediksi Langkah" = "blue"
        )) +
        theme_minimal() +
        theme(legend.title = element_blank())
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)