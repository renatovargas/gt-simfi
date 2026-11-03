test_that("ningún archivo versionado supera 25 MB", {
  skip_if_not(dir.exists(file.path(raiz, ".git")), "no es un repo git")
  archivos <- system2(
    "git", c("-C", shQuote(raiz), "ls-files", "--cached", "--others", "--exclude-standard"),
    stdout = TRUE
  )
  archivos <- file.path(raiz, archivos)
  archivos <- archivos[file.exists(archivos)]
  tam <- file.size(archivos)
  grandes <- basename(archivos)[tam > 25 * 1024^2]
  expect_length(grandes, 0)
})
