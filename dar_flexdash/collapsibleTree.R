library(collapsibleTree)

dar <-data.frame(
    Tier1 = c(
    NA, "Days in AR", "Days in AR", "Days in AR",
    ),
    Tier2 =c("Days in AR", "Number of Days in Period", "Gross Charges", "Ending AR Balance")
    )




# Create a simple org chart
org <- data.frame(
    Manager = c(
        NA, "Ana", "Ana", "Bill", "Bill", "Bill", "Claudette", "Claudette", "Danny",
        "Fred", "Fred", "Grace", "Larry", "Larry", "Nicholas", "Nicholas"
    ),
    Employee = c(
        "Ana", "Bill", "Larry", "Claudette", "Danny", "Erika", "Fred", "Grace",
        "Henri", "Ida", "Joaquin", "Kate", "Mindy", "Nicholas", "Odette", "Peter"
    ),
    Title = c(
        "President", "VP Operations", "VP Finance", "Director", "Director", "Scientist",
        "Manager", "Manager", "Jr Scientist", "Operator", "Operator", "Associate",
        "Analyst", "Director", "Accountant", "Accountant"
    )
)

# Add in colors and sizes
org$Color <- org$Title
levels(org$Color) <- colorspace::rainbow_hcl(11)

# Use unsplash api to add in random photos to tooltip
org$tooltip <- paste0(
    org$Employee,
    "<br>Title: ",
    org$Title,
    "<br><img src='https://source.unsplash.com/collection/385548/150x100'>"
)

collapsibleTreeNetwork(
    org,
    attribute = "Title",
    fill = "Color",
    nodeSize = "leafCount",
    tooltipHtml = "tooltip"
)
