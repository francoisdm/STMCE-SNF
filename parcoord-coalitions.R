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
data <- read.csv("coalitions.csv", colClasses=c(rep('numeric', 5), 'factor'))

# Plot
ggparcoord(data,
    columns = 1:5, groupColumn = 6, order = "allClass",
    scale="globalminmax",
    showPoints = TRUE, 
    title = "Ranking by coalitions",
    alphaLines = 1
    ) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10),
    axis.title.x= element_blank(),
    axis.title.y= element_blank()
  )