library(neuralnet)

wczytana_ramka <- read.csv("tablety.csv", sep = ";",  dec = ",")

# Stwórz druga ramke danych  
df <- data.frame(
  Nazwa = wczytana_ramka$nazwa, 
  Cena = wczytana_ramka$cena, 
  RAM = wczytana_ramka$ram, 
  HDD = wczytana_ramka$hdd, 
  Ekran = wczytana_ramka$wyswietlacz)  



# Skalowanie danych

maxs <- apply(df[,2:5], 2, max) 
mins <- apply(df[,2:5], 2, min)

scaled_df <- as.data.frame(scale(df[2:5], center = mins, scale = maxs - mins))

# Trenowanie sieci
nn <- neuralnet(Cena ~ RAM + HDD + Ekran, scaled_df, hidden=c(9,3),linear.output=FALSE, threshold = 0.001, lifesign = "full")

plot(nn)

# Testowanie sieci
test_results <- compute(nn, scaled_df[2:4])

# Odwrócenie skalowania dla wyników
unscaled_results <- (test_results$net.result * (max(df$Cena) - min(df$Cena))) + min(df$Cena)

print(unscaled_results)

df_res = cbind(df$Cena, unscaled_results)
print(df_res)
