#testing lizzie's issue

body_size <- c(50, 100, 150, 200, 250, 300, 350)
fecun <- c(0, 100, 200, 500, 600, 1000, 1500)
desired <- as.data.frame(cbind(body_size, fecun))


ggplot(data=desired, aes(x=body_size, y=fecun)) +
  geom_line() +
  stat_function(fun=function(x)  126.07 * exp (0.0061
 * x), color = 'red' ) +
  theme_bw()

Egg_Mean_par1 * exp (Egg_Mean_par2 * Size)