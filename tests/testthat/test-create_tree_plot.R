context("create_tree_plot")

# TODO: solve treeplot problems with multiple candidate splits for ggplot

test_that("create_tree_plot: only continuous variables", {
  skip("treeplot: plots are random objects, structure tested by hand")
  data("iris")
  iris$Sepal.Length[sample(1:nrow(iris), 30, replace = F)] <- NA
  tree <- rpart::rpart(Sepal.Length ~., data = iris, method = "anova",
                       control = rpart::rpart.control(minbucket = 5, cp = 0.0001))
  treeplot <- create_tree_plot(tree, "Sepal.Length", sapply(iris[, -1], is.factor))

})

test_that("create_tree_plot: factor in predictors", {
  skip("treeplot: plots are random objects, structure tested by hand")
  data("iris")
  iris$Species[sample(1:nrow(iris), 30, replace = F)] <- NA
  tree <- rpart::rpart(Species ~., data = iris, method = "class",
                       control = rpart::rpart.control(minbucket = 5, cp = 0.0001))
  treeplot <- create_tree_plot(tree, "Species", sapply(iris[, -5], is.factor))
})
