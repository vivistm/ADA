# Installiere und lade notwendige Pakete
library("pacman")
p_load("usethis")
p_load("devtools")
p_load("testthat")
p_load("checkmate")
p_load("Metrics")
p_load("purrr")

# Initialisiere das Paket
usethis::create_package("C:/Pfad/anpassen/package")

# Wechsle in das Paketverzeichnis
setwd("~/Studium/Master/4. Semester/ADA")

# License hinzufügen
usethis::use_gpl3_license()

# testthat Infrastruktur hinzufügen
usethis::use_testthat()

# Automatische Erstellung und Befüllung der R-Skripte für Funktionen


# mit use_r() wird neue R-Skriptdatei im R-Verzeichnis des Pakets erstellt und
# geöffnet

usethis::use_r("evaluate_transformations")
cat(
  '
#\' Evaluate Transformations
#\'
#\' This function evaluates different transformations using bootstrap samples.
#\' @param data data.frame with p covariates
#\' @param y numeric, target variable
#\' @param B integer, number of bootstrap samples
#\' @return A data.frame with formulas and their OOB errors
#\' @importFrom purrr map map_dbl
#\' @importFrom Metrics mse
#\' @export
evaluate_transformations <- function(data, y, B) {
  check_inputs(data, y, B)
  data$y <- y  # Füge y zu den Daten hinzu
  combinations <- generate_combinations(data)
  bootstrap_stichproben <- ziehe_stichproben(data, B)
  models <- fit_model(combinations, bootstrap_stichproben)
  oob_error <- berechne_oob_error(models, data, bootstrap_stichproben)
  combine_results(combinations, oob_error)
}
', file = "R/evaluate_transformations.R", append = TRUE)

usethis::use_r("check_inputs")
cat(
  '
#\' Check Inputs
#\'
#\' This function checks the inputs for validity.
#\' @param data data.frame
#\' @param y numeric vector
#\' @param B integer
#\' @importFrom checkmate assertDataFrame assertNumeric assert_count
#\' @export
check_inputs <- function(data, y, B) {
  assertDataFrame(data, min.rows = 2, min.cols = 1, max.cols = 5, any.missing = FALSE)
  assertNumeric(y, any.missing = FALSE, len = nrow(data))
  assert_count(B)
  if (nrow(data) < 2) {
    stop("data must have at least 2 rows")
  }
  if (ncol(data) < 2) {
    stop("data must have at least 2 columns")
  }
}
', file = "R/check_inputs.R", append = TRUE)

usethis::use_r("generate_combinations")
cat(
  '
#\' Generate Combinations
#\'
#\' This function generates all possible formulas from the given covariates.
#\' @param data data.frame
#\' @return A list of formulas
#\' @importFrom purrr map
#\' @export
generate_combinations <- function(data) {
  vars <- names(data)[names(data) != "y"]
  combinations <- unlist(purrr::map(1:length(vars), function(n) {
    combn(vars, n, simplify = FALSE)
  }), recursive = FALSE)
  purrr::map(combinations, function(kombis) {
    as.formula(paste("y ~", paste(kombis, collapse = "+")))
  })
}
', file = "R/generate_combinations.R", append = TRUE)

usethis::use_r("ziehe_stichproben")
cat(
  '
#\' Ziehe Stichproben
#\'
#\' This function draws bootstrap samples from the data.
#\' @param data data.frame
#\' @param B integer
#\' @importFrom purrr map
#\' @export
ziehe_stichproben <- function(data, B) {
  n <- nrow(data)
  purrr::map(1:B, function(sample_index) {
    indices <- sample(n, replace = TRUE)
    list(data = data[indices, , drop = FALSE], indices = indices)
  })
}
', file = "R/ziehe_stichproben.R", append = TRUE)

usethis::use_r("fit_model")
cat(
  '
#\' Fit Model
#\'
#\' This function fits linear models to each bootstrap sample and formula.
#\' @param combinations list of formulas
#\' @param bootstrap_stichproben list of bootstrap samples
#\' @importFrom purrr map
#\' @export
fit_model <- function(combinations, bootstrap_stichproben) {
  purrr::map(combinations, function(formeln) {
    purrr::map(bootstrap_stichproben, function(sample) {
      lm(formeln, data = sample$data)
    })
  })
}
', file = "R/fit_model.R", append = TRUE)

usethis::use_r("berechne_oob_error")
cat(
  '
#\' Berechne OOB Error
#\'
#\' This function calculates the out-of-bag error for each model.
#\' @param models list of fitted models
#\' @param data data.frame
#\' @param bootstrap_stichproben list of bootstrap samples
#\' @importFrom purrr map_dbl
#\' @importFrom Metrics mse
#\' @export
berechne_oob_error <- function(models, data, bootstrap_stichproben) {
  purrr::map_dbl(seq_along(models), function(i) {
    model_list <- models[[i]]
    purrr::map_dbl(seq_along(model_list), function(j) {
      model <- model_list[[j]]
      oob_indices <- setdiff(1:nrow(data), bootstrap_stichproben[[j]]$indices)
      oob_data <- data[oob_indices, , drop = FALSE]
      pred_y <- predict(model, newdata = oob_data)
      Metrics::mse(data$y[oob_indices], pred_y)
    }) |> mean()
  })
}
', file = "R/berechne_oob_error.R", append = TRUE)

usethis::use_r("combine_results")
cat(
  '
#\' Combine Results
#\'
#\' This function combines the results into a data.frame.
#\' @param combinations list of formulas
#\' @param oob_error numeric vector of OOB errors
#\' @export
combine_results <- function(combinations, oob_error) {
  data.frame(Formula = as.character(combinations), OOB_Error = oob_error)
}
', file = "R/combine_results.R", append = TRUE)

usethis::use_description(
  # fügt description-datei neu hinzu oder aktualisiert diese
  fields = list(
    Title = "package",
    Description = "Eine kurze Beschreibung des Pakets.",
    `Authors@R` = 'person("Paul", "Messer", 
                          email = "paul.messer@uni-bamberg.de", 
                          role = c("aut", "cre"))',
    License = "GPL-3",
    Imports = "Metrics, checkmate, purrr",
    Suggests = "testthat"
  )
)

# Dokumentation generieren
devtools::document()

# Testdatei erstellen
usethis::use_test("evaluate_transformations")
cat(
  '
# In der Datei tests/testthat/test-evaluate_transformations.R:

library(testthat)
library(package)

set.seed(39)
n <- 2000
data <- data.frame(
  x1 = rnorm(n, mean = 5, sd = 2),
  x2 = runif(n, min = -5, max = 10),
  x3 = rpois(n, lambda = 3)
)
y <- data$x1 - data$x2 + data$x3 * data$x1

test_that("evaluate_transformations works correctly", {
  result <- evaluate_transformations(data, y, 10)
  expect_s3_class(result, "data.frame")
  expect_true("Formula" %in% names(result))
  expect_true("OOB_Error" %in% names(result))
  expect_equal(nrow(result), choose(3, 1) + choose(3, 2) + choose(3, 3))
})

test_that("evaluate_transformations handles empty data", {
  data <- data.frame(x1 = numeric(), x2 = numeric(), x3 = numeric())
  y <- numeric()
  expect_error(evaluate_transformations(data, y, 10))
})

test_that("evaluate_transformations handles insufficient columns", {
  data <- data.frame(x1 = rnorm(2))
  y <- data$x1
  expect_error(evaluate_transformations(data, y, 10), 
  "data must have at least 2 columns")
})
', file = "tests/testthat/test-evaluate_transformations.R", append = TRUE)

# Tests ausführen
devtools::test()

# Schritte zur Veröffentlichung des Pakets

# Überprüfen des Pakets
devtools::check()

# Paket bauen und installieren
devtools::build()
devtools::install()

# Einreichen bei CRAN
# devtools::release()