# Day 3 - Data visualisation in R
# Run one expression at a time in RStudio to see each change in the Plots pane.
# Blocks 1-3 cover base graphics. Only blocks 4-6 require ggplot2.
# If needed, install ggplot2 before class with install.packages("ggplot2").

# 1. Look at the data and choose a plot ------------------------------------
# One row per guinea pig. Three doses x two delivery methods, 10 per group.
growth <- ToothGrowth
str(growth)
with(growth, table(dose, supp))

# Ask which plot describes the distribution of length, then vary the bins.
hist(growth$len, breaks = seq(0, 35, by = 5),
     xlab = "Odontoblast length", main = "",
     col = "#56B4E9", border = "white")
hist(growth$len, breaks = seq(0, 35, by = 2.5),
     xlab = "Odontoblast length", main = "",
     col = "#56B4E9", border = "white")

# The bars count animals in discrete study groups.
barplot(table(growth$dose),
        xlab = "Vitamin C dose (mg/day)",
        ylab = "Animals", col = "#56B4E9")

# 2. Start a base plot and add to it ---------------------------------------
# Run plot(), the two points() calls and legend() separately.
plot(len ~ dose, data = growth, type = "n",
     xlab = "Vitamin C dose (mg/day)",
     ylab = "Odontoblast length")
with(subset(growth, supp == "OJ"),
     points(dose, len, pch = 16, col = "#0072B2"))
with(subset(growth, supp == "VC"),
     points(dose, len, pch = 17, col = "#D55E00"))
legend("bottomright",
       c("Orange juice", "Ascorbic acid"),
       pch = c(16, 17),
       col = c("#0072B2", "#D55E00"),
       bty = "n")

# Optional variation. A reference line can be added to the current plot.
# This is an illustration of syntax, with no clinical threshold implied.
abline(h = median(growth$len), lty = 2, lwd = 2, col = "grey40")

# Ask which dose shows the most similar distributions between methods.
boxplot(len ~ supp + dose, data = growth,
        xlab = "Method and dose (mg/day)",
        ylab = "Odontoblast length",
        col = c("#A6D7ED", "#F4BA9B"))

# 3. Share scales across panels and restore device settings ---------------
par("mar")
old <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), las = 1)
for (method in c("OJ", "VC")) {
  group <- subset(growth, supp == method)
  boxplot(len ~ dose, data = group,
          main = method, ylim = range(growth$len),
          xlab = "Dose (mg/day)", ylab = "Length")
}
par(old)

# Indometh has 11 blood samples per person for six people after an IV dose.
# Confirm the layout is restored with a time plot for one person.
one <- subset(Indometh, Subject == "1")
one <- one[order(one$time), ]
plot(conc ~ time, data = one, type = "b",
     xlab = "Time since dose (hours)",
     ylab = "Plasma concentration (mcg/mL)",
     pch = 16, lwd = 2, col = "#0072B2")

# 4. Build the same scatterplot with a grammar ----------------------------
library(ggplot2)

axes <- ggplot(growth, aes(x = dose, y = len))
print(axes)

# Ask which expression creates a legend.
print(axes + geom_point(colour = "steelblue"))
print(axes + geom_point(aes(colour = supp)))

# Show the final mapping. Colour and shape both represent the same column.
p <- axes +
  geom_point(aes(colour = supp, shape = supp),
             size = 2.5) +
  labs(x = "Vitamin C dose (mg/day)",
       y = "Odontoblast length",
       colour = "Method", shape = "Method")
print(p)

# 5. Compare methods and save a figure ------------------------------------
panels <- ggplot(growth, aes(x = factor(dose), y = len)) +
  geom_boxplot(fill = "#A6D7ED") +
  facet_wrap(~ supp) +
  labs(x = "Vitamin C dose (mg/day)",
       y = "Odontoblast length")
print(panels)

method_labels <- c("Orange juice", "Ascorbic acid")
p_scaled <- p +
  scale_x_continuous(breaks = c(0.5, 1, 2)) +
  scale_colour_manual(
    values = c(OJ = "#0072B2", VC = "#D55E00"),
    labels = method_labels
  ) +
  scale_shape_discrete(labels = method_labels)
print(p_scaled)
p_final <- p_scaled +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom") +
  labs(title = "Vitamin C and odontoblast length")
print(p_final)

# Write to a fresh temporary filename so the live demo can be repeated.
output <- tempfile(pattern = "dose-response-", fileext = ".png")
ggsave(output, plot = p_final, width = 7, height = 4, units = "in", dpi = 300)
message("Saved figure to ", output)

# 6. Optional practice - connect repeated samples within each person -----
# Ask why the line zigzags when it connects samples from different people.
concentration <- ggplot(Indometh, aes(x = time, y = conc))
print(concentration + geom_line() + geom_point())

# Use the subject ID to give each person a separate line.
profiles <- concentration +
  geom_line(aes(group = Subject), colour = "#0072B2", alpha = 0.6) +
  geom_point(colour = "#0072B2", alpha = 0.6) +
  labs(x = "Time since dose (hours)",
       y = "Plasma concentration (mcg/mL)")
print(profiles)
