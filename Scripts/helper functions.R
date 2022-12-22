#Load functions----
SBP_shift <- function(df, shift, n){
  
  if(shift == "None"){
    return(df)
  }
  
  if(shift == "lag"){
    temp <- df %>%
      mutate(SBP_Time = lag(df$SBP_Time,n),
             SBP = lag(df$SBP,n))
    
    return(temp)
  }
  
  if(shift == "lead"){
    temp <- df %>%
      mutate(SBP_Time = lead(df$SBP_Time,n),
             SBP = lead(df$SBP,n))
    
    return(temp)
  }
}

#this function flags sequences of values that go up or down
cBRS <- function(df, RR_diff, SBP_diff) {
  
  df <- df %>%
    drop_na() %>%
    add_column(SBP_UP = NA,
               RR_UP = NA,
               SEQ_UP = NA,
               SBP_DOWN = NA,
               RR_DOWN = NA,
               SEQ_DOWN = NA)
  
  #Sequence UP ----
  ##Creating SBP_UP ----
  
  j = 1  #setting j at 1
  i = 1  #setting i at 1
  
  while(i < length(df$SBP_UP) - 1) {  #creating a while loop to run for the length of SBP_UP
    if(df$SBP[i+1] - df$SBP[i] > SBP_diff ) { #Checking if the value ahead is greater than the current value
      if(df$SBP[i+2] - df$SBP[i+1] > SBP_diff) { #Checking if the value 2 spots ahead is greater than the value 1 spot ahead
        df$SBP_UP[i] = j           #Marking the current value
        df$SBP_UP[i+1] = j         #Marking the next value
        if (is.na(df$SBP_UP[i+2])) {  #Marking the value 2 spots ahead if it hasn't been already
          df$SBP_UP[i+2] = j
        }
      } else { #if the value 2 spots ahead is not greater than the value 1 spot ahead value will not be marked
        df$SBP_UP[i+2] = ""
      }
    } else {
      df$SBP_UP[i+1] = ""  #if the next value is not greater than the current value, value will not be marked
      j = j+1 #increasing j by 1
    }
    i = i+1  #increasing i by 1
  }
  
  ##Creating RR_UP ----
  
  j = 1
  i = 1
  
  while(i < length(df$RR_UP) - 2) {
    if(df$RR[i+1] - df$RR[i] > RR_diff) {
      if(df$RR[i+2] - df$RR[i+1] > RR_diff) {
        df$RR_UP[i] = j
        df$RR_UP[i+1] = j
        if (is.na(df$RR_UP[i+2])) {
          df$RR_UP[i+2] = j
        }
      } else {
        df$RR_UP[i+2] = ""
      }
    } else {
      df$RR_UP[i+1] = ""
      j = j+1
    }
    i = i+1
  }
  
  ##Creating SEQ_UP ----
  
  j = 1
  i = 1
  
  while(i < length(df$RR_UP) - 2) {
    if(df$RR[i+1] - df$RR[i] > RR_diff & df$SBP[i+1] - df$SBP[i] > SBP_diff ) {
      if(df$RR[i+2] - df$RR[i+1] > RR_diff & df$SBP[i+2] - df$SBP[i+1] > SBP_diff){
        df$SEQ_UP[i] = paste0(j, sep = "-", "U")
        df$SEQ_UP[i+1] = paste0(j, sep = "-", "U")
        if (is.na(df$SEQ_UP[i+2])) {
          df$SEQ_UP[i+2] = paste0(j, sep = "-", "U")
        }
      } else {
        df$SEQ_UP[i+2] = ""
      }
    } else {
      df$SEQ_UP[i+1] = ""
      j = j+1
    }
    i = i+1
  }
  
  ##Getting rid of extra NA values ----
  
  df$SBP_UP[is.na(df$SBP_UP)] <- ""
  df$RR_UP[is.na(df$RR_UP)] <- ""
  df$SEQ_UP[is.na(df$SEQ_UP)] <- ""
  
  #Sequence DOWN ----
  ##Creating SBP_DOWN ----
  
  j = 1
  i = 1
  
  while(i < length(df$SBP_DOWN) - 2) {
    if(df$SBP[i] - df$SBP[i+1] > SBP_diff) {
      if(df$SBP[i+1]- df$SBP[i+2] > SBP_diff) {
        df$SBP_DOWN[i] = j
        df$SBP_DOWN[i+1] = j
        if (is.na(df$SBP_DOWN[i+2])) {
          df$SBP_DOWN[i+2] = j
        }
      } else {
        df$SBP_DOWN[i+2] = ""
      }
    } else {
      df$SBP_DOWN[i+1] = ""
      j = j+1
    }
    i = i+1
  }
  
  ##Creating RR_DOWN ----
  
  j = 1
  i = 1
  
  while(i < length(df$RR_DOWN) - 2) {
    if(df$RR[i] - df$RR[i+1] > RR_diff) {
      if(df$RR[i+1] - df$RR[i+2] > RR_diff) {
        df$RR_DOWN[i] = j
        df$RR_DOWN[i+1] = j
        if (is.na(df$RR_DOWN[i+2])) {
          df$RR_DOWN[i+2] = j
        }
      } else {
        df$RR_DOWN[i+2] = ""
      }
    } else {
      df$RR_DOWN[i+1] = ""
      j = j+1
    }
    i = i+1
  }
  
  ##Creating SEQ_DOWN ----
  
  j = 1
  i = 1
  
  while(i < length(df$RR_DOWN) - 2) {
    if(df$RR[i] - df$RR[i+1] > RR_diff & df$SBP[i] - df$SBP[i+1] > SBP_diff) {
      if(df$RR[i+1] - df$RR[i+2] > RR_diff & df$SBP[i+1] - df$SBP[i+2] > SBP_diff) {
        df$SEQ_DOWN[i] = paste0(j, sep = "-", "D")
        df$SEQ_DOWN[i+1] = paste0(j, sep = "-", "D")
        if (is.na(df$SEQ_DOWN[i+2])) {
          df$SEQ_DOWN[i+2] = paste0(j, sep = "-", "D")
        }
      } else {
        df$SEQ_DOWN[i+2] = ""
      }
    } else {
      df$SEQ_DOWN[i+1] = ""
      j = j+1
    }
    i = i+1
  }
  
  ##Getting rid of extra NA values ----
  
  df$SBP_DOWN[is.na(df$SBP_DOWN)] <- ""
  df$RR_DOWN[is.na(df$RR_DOWN)] <- ""
  df$SEQ_DOWN[is.na(df$SEQ_DOWN)] <- ""
  
  return(df)
  
}

model_UP <- function(df) {
  
  model_UP <- df[df$SEQ_UP != "", ] %>% 
    dplyr::select("SBP","RR","SEQ_UP") %>%   #Option 2L dplyr::select("SBP" , "RR" , "SP_Time")
    rename(Sequence = SEQ_UP) %>% 
    group_by(Sequence) %>% nest() %>% 
    mutate(model = map(data, ~lm(.$RR ~ .$SBP, data = .)),
           tidy = map(model, tidy))
  
  return(model_UP)
}

model_DOWN <- function(df) {
  
  model_DOWN <- df[df$SEQ_DOWN != "", ] %>% 
    dplyr::select("SBP","RR","SEQ_DOWN") %>% 
    rename(Sequence = SEQ_DOWN) %>%
    group_by(Sequence) %>% nest() %>% 
    mutate(model = map(data, ~lm(.$RR ~ .$SBP, data = .)),
           tidy = map(model, tidy))
  
}

Summary_UP <- function(model_UP) {
  
  Summary_UP <- tibble(Sequence = NA, 
                       Slope = NA, 
                       .rows = nrow(model_UP))
  
  for(i in seq_along(model_UP$Sequence)){
    
    Summary_UP$Sequence[i] <- pluck(model_UP, 1, i)
    Summary_UP$Slope[i] <- pluck(model_UP, 4, i, 2, 2)
  }
  
  return(Summary_UP)
  
}

Summary_DOWN <- function(model_DOWN) {
  
  Summary_DOWN <- tibble(Sequence = NA, 
                         Slope = NA, 
                         .rows = nrow(model_DOWN))
  
  for(i in seq_along(model_DOWN$Sequence)){
    
    Summary_DOWN$Sequence[i] <- pluck(model_DOWN, 1, i)
    Summary_DOWN$Slope[i] <- pluck(model_DOWN, 4, i, 2, 2)
  }
  
  return(Summary_DOWN)
  
}

Summary_Combined <- function(model_combined) {
  
  Summary <- tibble(Sequence = NA, 
                    Slope = NA, 
                    .rows = nrow(model_combined))
  
  for(i in seq_along(model_combined$Sequence)){
    
    Summary$Sequence[i] <- pluck(model_combined, 1, i)
    Summary$Slope[i] <- pluck(model_combined, 4, i, 2, 2)
  }
  
  return(Summary)
  
}

means <- function(slope_UP, slope_DOWN, slope_COMBINED) {
  
  mean_table <- tibble(Sequence = c("Slope Up", "Slope Down", "Slope Combined"),
                       Slope_Means = c(slope_UP, slope_DOWN, slope_COMBINED))
  
  return(mean_table)
  
}

lengths <- function(df_up, df_down, df_combined) {
  
  lengths_table <- tibble(Sequences_Amount = c(length(df_up), length(df_down), 
                                     length(df_combined)))
  
  return(lengths_table)
  
}

slope_plot <- function(df) {
  
  plot <- ggplotly(df[df$SEQ_UP != "" | df$SEQ_DOWN != "", ] %>% 
                   dplyr::select("SBP", "RR", "SEQ_UP", "SEQ_DOWN") %>% 
                   ggplot(aes(SBP, RR, group = interaction(SEQ_UP, SEQ_DOWN), colour = interaction(SEQ_UP, SEQ_DOWN)))+
                   geom_line(size = 0.5) +
                   xlab("Sytolic BP (mmHg)")+
                   ylab("R-R interval (ms)") + 
                   theme_bw())
  
  return(plot)
  
}

seq_plot <- function(df) {
  
  plot <- subplot(
            ggplotly(ggplot() +
                       geom_line(data = df, 
                                 mapping = aes(SBP_Time, SBP), colour = "Black", size = 0.4) +
                       geom_line(data = df[df$SEQ_UP != "" | df$SEQ_DOWN != "", ], 
                                 mapping = aes(SBP_Time, SBP, group = interaction(SEQ_UP, SEQ_DOWN), colour = interaction(SEQ_UP, SEQ_DOWN)), size = 1.2) +
                       theme_classic()),
            
            ggplotly(ggplot() +
                       geom_line(data = df, 
                                 mapping = aes(SBP_Time, RR), colour = "Black", size = 0.4) +
                       geom_line(data = df[df$SEQ_UP != "" | df$SEQ_DOWN != "", ], 
                                 mapping = aes(SBP_Time, RR, group = interaction(SEQ_UP, SEQ_DOWN), colour = interaction(SEQ_UP, SEQ_DOWN)), size = 1.2) +
                       theme_classic()),
            nrows = 2,
            shareX = TRUE, 
            titleY = TRUE)
  
  return(plot)
  
}

interpolate <- function(df) {
  
  #Load data and zero times
  df <- df %>% 
    drop_na() %>% 
    mutate(R_Time = R_Time - SBP_Time,
           SBP_Time = SBP_Time - (min(SBP_Time)),
           R_Time = SBP_Time + R_Time) 
  
  #Interpolate data
  xi = seq(0,max(df[[1]]), by = 0.2)
  
  df_interp <- tibble(
    Time = xi,
    SBP = interp1(df$SBP_Time, df$SBP, xi, method = "spline"),
    RR = interp1(df$R_Time, df$RR, xi, method = "spline")) %>%
    drop_na()
  
  return(df_interp)
  
}


transfer_fn <- function(df_interp, HF) {

  #Run transfer function on interpolated data
  #Create matrix with both variables
  input <- df_interp$SBP
  output <- df_interp$RR
  df <- cbind(input, output)
  
  #pwelch
  temp1 <- pwelch(x = df,
                  window = 512,
                  fs = 5,
                  detrend = "short-linear")
  
  #Allows separation of input and output
  Spec = as_tibble(temp1$spec)
  
  #Create dataframe with all variables of interest
  TF <- tibble(Frequency = as.numeric(temp1$freq),
               Input_Power = as.numeric(Spec[[1]]),
               Output_Power = as.numeric(Spec[[2]]),
               Gain = as.numeric(Mod(temp1$trans)),
               Coherence = as.numeric(temp1$coh),
               Phase = as.numeric((Arg(temp1$trans))*(180/pi))*-1) %>%
    dplyr::filter(Frequency < HF)
  
  return(TF)
  
}

vline <- function(x = 0, color = "green") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}

TF_Summary <- function(df, VLF, LF, HF){
  
  #Nest Input and Output power and quantify AUC
  temp1 <- df %>%
    select(., Frequency, Input_Power, Output_Power) %>%
    gather(., key = "Variable", value = "Value", -Frequency) %>%
    group_by(Variable) %>% nest() %>%
    mutate(Bands = map(data, ~tibble(
      Frequency = c("VLF" , "LF" , "HF", "Total"),
      Spectral_Power = c(
        AUC(.$Frequency, .$Value, from= 0, to= VLF, method = "spline"),
        AUC(.$Frequency, .$Value, from= VLF, to= LF, method = "spline"),
        AUC(.$Frequency, .$Value, from= LF, to= HF, method = "spline"),
        AUC(.$Frequency, .$Value, from= 0, to= HF, method = "spline")))))
  
  #Nest Gain, Phase, and Coherence and quantify averages
  temp2 <- df %>%
    select(., Frequency, Gain, Phase, Coherence) %>%
    gather(., key = "Variable", value = "Value", -Frequency) %>%
    group_by(Variable) %>% nest() %>%
    mutate(Bands = map(data, ~tibble(
      Frequency = c("VLF" , "LF" , "HF", "Total"),
      Spectral_Power = c(
        mean(.$Value[.$Frequency >= 0 & .$Frequency <= VLF]),
        mean(.$Value[.$Frequency >= VLF & .$Frequency <= LF]),
        mean(.$Value[.$Frequency >= LF & .$Frequency <= HF]),
        mean(.$Value[.$Frequency >= 0 & .$Frequency <= HF])))))
  
  #Create combined df
  TF_Summary <- tibble(Frequency = c("VLF", "LF", "HF", "Total"),
                       Input_Power = temp1[[3]][[1]][[2]],
                       Output_Power = temp1[[3]][[2]][[2]],
                       Gain = temp2[[3]][[1]][[2]],
                       Phase = temp2[[3]][[2]][[2]],
                       Coherence = temp2[[3]][[3]][[2]])
  
  
  
  return(TF_Summary)
  
}

