#growth as a function of temperature for zoops/phytos
#adapted from python source code (aed_util.F90) using chatgpt

library(ggplot2)

aed_bio_temp_function <- function(numg, theta, T_std, T_opt, T_max, name) {
  tol <- 0.05
  t20 <- 20.0
  curvef <- TRUE
  
  # Pre-allocate kTn, aTn, bTn
  kTn <- numeric(numg)
  aTn <- numeric(numg)
  bTn <- numeric(numg)
  
  # Prepare a data frame for storing growth rates for plotting
  plot_data <- data.frame(Temperature = numeric(0), GrowthRate = numeric(0), Group = character(0))
  
  for (group in 1:numg) {
    v <- theta[group]
    
    if (v < 1.01) {
      warning(paste("theta_growth for group", group, "< 1.01"))
    }
    
    Tm <- T_max[group]
    Ts <- T_std[group]
    To <- T_opt[group]
    
    if (Ts < 0.0 && To < 0.0 && Tm < 0.0) {
      # The user inputs the values of kTn, aTn and bTn directly
      kTn[group] <- -Ts
      bTn[group] <- -Tm
      aTn[group] <- -To
      
      value <- numeric(401)
      
      for (i in 0:400) {
        b <- i / 10.0
        value[i + 1] <- v^(b - 20) - v^(kTn[group] * (b - aTn[group])) + bTn[group]
        plot_data <- rbind(plot_data, data.frame(Temperature = b, GrowthRate = value[i + 1], Group = name[group]))
      }
      
      a <- 0.0
      for (i in 1:length(value)) {
        b <- (i - 1) / 10.0
        if (value[i] > 0.0) {
          T_max[group] <- b
        }
        if (value[i] > a) {
          T_opt[group] <- b
          a <- value[i]
        }
        if (value[i] > v^(b - 20) - tol && value[i] < v^(b - 20) + tol) {
          T_std[group] <- b
        }
      }
      
    } else {
      input_var <- 1.0
      a0 <- v^(Ts - t20)
      a1 <- v^(To - t20)
      a2 <- v^(Tm - t20)
      
      k <- 6.0
      i <- 0
      G <- tol + 1.0
      
      while ((G <= -tol) || (G >= tol)) {
        i <- i + 1
        if (i == 100) {
          i <- 0
          tol <- tol + 0.01
        }
        if (curvef) {
          G <- k * v^(k * To) * a2 - a1 * (v^(k * Tm) - v^(k * Ts))
          devG <- v^(k * To) * a2 * (1 + k * To * log(v)) - a1 * log(v) * (Tm * v^(k * Tm) - Ts * v^(k * Ts))
        } else {
          G <- k * v^(k * To) * (a0 - a2 - input_var) - a1 * (v^(k * Ts) - v^(k * Tm))
          devG <- (a0 - a2 - input_var) * v^(k * To) * (1 + k * To * log(v)) - a1 * log(v) * (Ts * v^(k * Ts) - Tm * v^(k * Tm))
        }
        k <- k - G / devG
      }
      
      if (k != 0.0) {
        a <- -log(a1 / (k * v^(k * To))) / (k * log(v))
        if (curvef) {
          b <- v^(k * (Ts - a))
        } else {
          b <- input_var + v^(k * (Ts - a)) - a0
        }
      } else {
        a <- 0.0
        b <- 0.0
      }
      
      kTn[group] <- k
      aTn[group] <- a
      bTn[group] <- b
      
      for (i in 0:400) {
        temp <- i / 10.0
        growth_rate <- v^(temp - 20) - v^(k * (temp - a)) + b
        plot_data <- rbind(plot_data, data.frame(Temperature = temp, GrowthRate = growth_rate, Group = name[group]))
      }
    }
    
    if (kTn[group] < 0.1 && bTn[group] > 100.0) {
      stop(paste("Cannot solve for fT for:", name[group]))
    }
  }
  
  ggplot(plot_data, aes(x = Temperature, y = GrowthRate, color = Group)) +
    geom_line() + ylim(0,1.5) + xlim(0,35) +
    labs(#title = "Growth Rate as a Function of Temperature",
         x = "Temperature (°C)",
         y = "f(T)") + 
    annotate("text", x=c(2,5,8), y=1.5, label = topt_lab) +
    #scale_color_manual("", values = c("#006d77","#83c5be","#e29578")) +
    scale_color_manual("", values = c("cyan","brown","darkgreen")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
}

# 3 phyto groups:
numg <- 3
theta <- c(1.05, 1.08, 1.02)
T_std <- c(10, 10, 10)
T_opt <- c(28, 28, 12)
T_max <- c(35, 35, 30)
name <- c("Cyano", "Green", "Diatom")
topt_lab <- T_opt[c(2,3,1)]

aed_bio_temp_function(numg, theta, T_std, T_opt, T_max, name)

# 3 phyto groups based on rqt nml file
numg <- 3
theta <- c(1.05, 1.08, 1.05)
T_std <- c(20, 18, 8)
T_opt <- c(28, 25, 18)
T_max <- c(35, 30, 25)
name <- c("Cyano", "Green", "Diatom")
topt_lab <- T_opt[c(1,3,2)]

aed_bio_temp_function(numg, theta, T_std, T_opt, T_max, name)

# 3 zoop groups - par set 1 
numg <- 3
theta <- c(1.08, 1.06, 1.09)
T_std <- c(10, 20, 10)
T_opt <- c(30, 25, 25)
T_max <- c(35, 35, 35)
name <- c("Rotifer", "Cladoceran", "Copepod")

aed_bio_temp_function(numg, theta, T_std, T_opt, T_max, name)

# 3 zoop groups - par set 2
numg <- 3
theta <- c(1.04, 1.04, 1.03)
T_std <- c(20, 10, 10)
T_opt <- c(20, 18, 15)
T_max <- c(35, 35, 35)
name <- c("Rotifer", "Cladoceran", "Copepod")

aed_bio_temp_function(numg, theta, T_std, T_opt, T_max, name)


# 3 zoop groups - another par set with clads and copes the same, but rots lower
numg <- 3
theta <- c(1.04, 1.06, 1.09)
T_std <- c(20, 20, 10)
T_opt <- c(20, 25, 25)
T_max <- c(35, 35, 35)
name <- c("Rotifer", "Cladoceran", "Copepod")

aed_bio_temp_function(numg, theta, T_std, T_opt, T_max, name)

# 3 zoop groups - default glm aed pars
numg <- 3
theta <- c(1.08, 1.08, 1.08)
T_std <- c(20, 20, 20)
T_opt <- c(22, 22, 22)
T_max <- c(30, 30, 30)
name <- c("Rotifer", "Cladoceran", "Copepod")

aed_bio_temp_function(numg, theta, T_std, T_opt, T_max, name)

#-----------------------------------------------------------------#
# Another option is to compare two zoop par sets with differences in T_opt only

# 3 zoop groups - high ish t_opt
numg <- 3
theta <- c(1.08, 1.06, 1.09)
T_std <- c(10, 10, 10)
T_opt <- c(20, 20, 18)
T_max <- c(35, 35, 35)
name <- c("Rotifer", "Cladoceran", "Copepod")
topt_lab <- T_opt[c(2,3,1)]

ps1 <- aed_bio_temp_function(numg, theta, T_std, T_opt, T_max, name)

# 3 zoop groups - low t_opt
numg <- 3
theta <- c(1.08, 1.06, 1.09)
T_std <- c(10, 10, 10)
T_opt <- c(25, 25, 23)
T_max <- c(35, 35, 35)
name <- c("Rotifer", "Cladoceran", "Copepod")
topt_lab <- T_opt[c(2,3,1)]

ps2 <- aed_bio_temp_function(numg, theta, T_std, T_opt, T_max, name)

ggpubr::ggarrange(ps1,ps2, common.legend = T)
ggsave("figures/topt_option3.jpg")
