library(neuralnet)

x <- seq(1, 100, by = 1)  # argumenty
y <- x ^ 3 - 2 * x # wartosci 

df <- data.frame(  
  val = x,  
  res = y 
) 

# Skalowanie danych
maxs <- apply(df, 2, max) 
mins <- apply(df, 2, min)

scaled_df <- as.data.frame(scale(df, center = mins, scale = maxs - mins))

# Trenowanie sieci
nn <- neuralnet(res ~ val, scaled_df, hidden=c(9,3),linear.output=FALSE, threshold = 0.001)

plot(nn)

# Testowanie sieci
test_results <- compute(nn, scaled_df[1])

# Odwrócenie skalowania dla wyników
unscaled_results <- (test_results$net.result * (max(df$res) - min(df$res))) + min(df$res)

print(unscaled_results)

# Wyswietl wyniki
#plot(df$val, unscaled_results, col = 'green', type = 'l')
#points(df$val, df$res, col = 'red')

plot(df$val, df$res, col = 'red', type = 'l')
points(df$val,  unscaled_results, col = 'blue')