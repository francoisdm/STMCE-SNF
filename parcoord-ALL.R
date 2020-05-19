# ====== Packages ======
required_packages <- c("hrbrthemes",
                       "GGally",
                       "data.table",
                       "viridis")

uninstalled_packages <- required_packages[!(required_packages %in% installed.packages()[,'Package'])]
if (length(uninstalled_packages) > 0) {
  install.packages(uninstalled_packages, dependencies = TRUE, repos = 'http://cran.us.r-project.org')
}

lapply(required_packages, require, character.only = TRUE)

rm(required_packages)
rm(uninstalled_packages)

# Data set
data <- read.csv("rankings-ALL.csv", colClasses=c(rep('numeric', 8), 'factor'))

# Plot
ggparcoord(data,
    columns = 1:8, groupColumn = 9,
    scale="globalminmax",
    showPoints = TRUE, 
    title = "Mean rankings from multi-criteria evaluations and social impact analysis",
    alphaLines = 1
    ) + 
  scale_color_viridis(discrete=TRUE) +
  scale_y_continuous(breaks = seq(1, 8, 1), limits = c(1, 8)) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10),
    axis.title.x= element_blank(),
    axis.title.y= element_blank(),
    panel.grid.minor = element_blank()
  )