
normal_data <- c(67,70,63,65,68,60,70,64,69,61,66,65,71,62,66,68,64,67,62,66,65,63,66,65,63,
                 69,62,67,59,66,65,63,65,60,67,64,68,61,69,65,62,67,70,64,63,68,64,65,61,66)

skewed_right_data <- c(31,43,30,30,38,26,29,55,46,26,29,57,34,34,36,40,28,26,66,63,30,33,24,35,34,
                       40,24,29,24,27,35,33,75,38,34,85,29,40,41,35,26,34,19,23,28,26,31,25,22,28)

skewed_left_data <- c(102,55,70,95,73,79,60,73,89,85,72,92,76,93,76,97,10,70,85,25,83,58,10,92,82,
                      87,104,75,80,66,93,90,84,73,98,79,35,71,90,71,63,58,82,72,93,44,65,77,81,77)

uniform_data <- c(12.1,12.1,12.4,12.1,12.1,12.2,12.2,12.2,11.9,12.2,12.3,12.3,11.7,12.3,12.3,12.4,12.4,12.1,12.4,12.4,12.5,11.8,12.5,12.5,12.5,
                  11.6,11.6,12.0,11.6,11.6,11.7,12.3,11.7,11.7,11.7,11.8,12.5,11.8,11.8,11.8,11.9,11.9,11.9,12.2,11.9,12.0,11.9,12.0,12.0,12.0)

#1
#Normal Data
moment1_mean <- mean(normal_data)  #(mean)
moment2_var <- var(normal_data)   #(variance)
moment3_skew <- skewness(normal_data)  #Skewness
moment4_kurt <- kurtosis(normal_data)  #Kurtosis

moment1_mean
moment2_var
moment3_skew
moment4_kurt

#Skewed-Right Data
moment1_skewed_right_mean <- mean(skewed_right_data)  
moment2_skewed_right_var <- var(skewed_right_data)   
moment3_skewed_right_skew <- skewness(skewed_right_data)  
moment4_skewed_right_kurt <- kurtosis(skewed_right_data)  

moment1_skewed_right_mean
moment2_skewed_right_var
moment3_skewed_right_skew
moment4_skewed_right_kurt

#Skewed-Left Data
moment1_skewed_left_mean <- mean(skewed_left_data)  
moment2_skewed_left_var <- var(skewed_left_data)   
moment3_skewed_left_skew <- skewness(skewed_left_data)  
moment4_skewed_left_kurt <- kurtosis(skewed_left_data)  

moment1_skewed_left_mean
moment2_skewed_left_var
moment3_skewed_left_skew
moment4_skewed_left_kurt

#Uniform Data
moment1_uniform_mean <- mean(uniform_data)  
moment2_uniform_var <- var(uniform_data)   
moment3_uniform_skew <- skewness(uniform_data)  
moment4_uniform_kurt <- kurtosis(uniform_data)  

moment1_uniform_mean
moment2_uniform_var
moment3_uniform_skew
moment4_uniform_kurt

#2
#Normal Mean
mean_normal <- mean(normal_data)

moment1_about_mean_normal <- mean (normal_data - mean_normal)
moment2_about_mean_normal <- var(normal_data - mean_normal)  
moment3_about_mean_normal <- skewness(normal_data - mean_normal)  
moment4_about_mean_normal <- kurtosis(normal_data - mean_normal)  

moment1_about_mean_normal
moment2_about_mean_normal
moment3_about_mean_normal
moment4_about_mean_normal

#Skewed-Right Mean
mean_skewed_right <- mean(skewed_right_data)

moment1_about_mean_skewed_right <- mean(skewed_right_data - mean_skewed_right)  
moment2_about_mean_skewed_right <- var(skewed_right_data - mean_skewed_right)  
moment3_about_mean_skewed_right <- moment(skewed_right_data - mean_skewed_right)  
moment4_about_mean_skewed_right <- moment(skewed_right_data - mean_skewed_right)  

moment1_about_mean_skewed_right
moment2_about_mean_skewed_right
moment3_about_mean_skewed_right
moment4_about_mean_skewed_right

#Skewed-Left Mean
mean_skewed_left <- mean(skewed_left_data)

moment1_about_mean_skewed_left <- mean(skewed_left_data - mean_skewed_left)  
moment2_about_mean_skewed_left <- var(skewed_left_data - mean_skewed_left)  
moment3_about_mean_skewed_left <- moment(skewed_left_data - mean_skewed_left)  
moment4_about_mean_skewed_left <- moment(skewed_left_data - mean_skewed_left)  

moment1_about_mean_skewed_left
moment2_about_mean_skewed_left
moment3_about_mean_skewed_left
moment4_about_mean_skewed_left

#Uniform Mean
mean_uniform <- mean(uniform_data)

moment1_about_mean_uniform <- mean(uniform_data - mean_uniform) 
moment2_about_mean_uniform <- var(uniform_data - mean_uniform)  
moment3_about_mean_uniform <- skewness(uniform_data - mean_uniform)  
moment4_about_mean_uniform <- kurtosis(uniform_data - mean_uniform)  

moment1_about_mean_uniform
moment2_about_mean_uniform
moment3_about_mean_uniform
moment4_about_mean_uniform

#3
fem_measure <- 75

# Calculate moments about the number 75
moment1_75 <- mean(abs(normal_data - fem_measure))  
moment2_75 <- var(abs(normal_data - fem_measure))   
moment3_75 <- skewness(abs(normal_data - fem_measure))  
moment4_75 <- kurtosis(abs(normal_data - fem_measure))  

moment1_75
moment2_75
moment3_75
moment4_75

#4
# Female height measurements
moment1_75 <- mean(abs(normal_data - fem_measure))
moment2_about_mean_normal <- var(abs(normal_data - fem_measure))

# Calculate m_2' - m_1'^2
m1  <- moment2_about_mean_normal - (moment1_about_mean_normal^2)
m1


# Moments calculated from item 2 for female height measurements
moment1_about_mean_normal <- mean(abs(normal_data - fem_measure))
moment2_about_mean_normal <- var(abs(normal_data - fem_measure))
moment3_about_mean_normal <- moment(abs(normal_data - fem_measure))

# Calculate m_3' - 3m_1' m_2' + 2m_1'^3
m2 <- moment3_about_mean_normal - 3 * moment1_about_mean_normal * moment2_about_mean_normal + 2 * (moment1_about_mean_normal^3)
m2

# Moments calculated from item 2 for female height measurements
moment1_about_mean_normal <- mean(abs(normal_data - fem_measure))
moment2_about_mean_normal <- var(abs(normal_data - fem_measure))
moment3_about_mean_normal <- moment(abs(normal_data - fem_measure))
moment4_about_mean_normal <- moment(abs(normal_data - fem_measure))

# Calculate m_4' - 4m_1' m_3' + 6m_1'^2 m_2' - 3m_1'^4
m3 <- moment4_about_mean_normal - 4 * moment1_about_mean_normal * moment3_about_mean_normal +
  6 * (moment1_about_mean_normal^2) * moment2_about_mean_normal - 3 * (moment1_about_mean_normal^4)
m3

