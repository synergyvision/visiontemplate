library(rmarkdown)
find_file <- function(template, file) {
  template <- system.file("rmarkdown", "templates", template, file,
                          package = "visiontemplate")
  if (template == "") {
    stop("Couldn't find template file ", template, "/", file, call. = FALSE)
  }

  template
}

find_resource <- function(template, file) {
  find_file(template, file.path("resources", file))
}

knitr_fun <- function(name) utils::getFromNamespace(name, 'knitr')

output_asis <- knitr_fun('output_asis')

#' Render a pandoc template.
#'
#' This is a hacky way to access the pandoc templating engine.
#'
#' @param metadata A named list containing metadata to pass to template.
#' @param template Path to a pandoc template.
#' @param output Path to save output.
#' @return (Invisibly) The path of the generate file.
#' @examples
#' x <- rticles:::template_pandoc(
#'   list(preamble = "%abc", filename = "wickham"),
#'   rticles:::find_resource("rjournal_article", "RJwrapper.tex"),
#'   tempfile()
#' )
#' if (interactive()) file.show(x)
#' @noRd
template_pandoc <- function(metadata, template, output, verbose = FALSE) {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp))

  cat("---\n", file = tmp)
  cat(yaml::as.yaml(metadata), file = tmp, append = TRUE)
  cat("---\n", file = tmp, append = TRUE)
  cat("\n", file = tmp, append = TRUE)

  rmarkdown::pandoc_convert(tmp, "markdown", output = output,
                            options = paste0("--template=", template), verbose = verbose)

  invisible(output)
}


# Call rmarkdown::pdf_document and mark the return value as inheriting pdf_document
inherit_pdf_document <- function(...) {
  fmt <- rmarkdown::pdf_document(...)
  fmt$inherits <- "pdf_document"
  fmt
}

# Helper function to create a custom format derived from pdf_document
# that includes a custom LaTeX template and custom CSL definition
pdf_document_format <- function(..., format, template, keep_tex=TRUE) {

  # base format
  fmt <- inherit_pdf_document(..., template = find_resource(format, template), keep_tex = keep_tex)

  # return format
  fmt
}

# default rendering and chunk options
knitr::render_markdown()
knitr::opts_chunk$set(tidy = FALSE, error = FALSE)

# store info about the final output format in opts_knit
# knitr::opts_knit$set(
#   rmarkdown.pandoc.from = output_format$pandoc$from,
#   rmarkdown.pandoc.to = pandoc_to,
#   rmarkdown.pandoc.id_prefix = id_prefix,
#   rmarkdown.keep_md = output_format$keep_md,
#   rmarkdown.df_print = output_format$df_print,
#   rmarkdown.version = 2,
#   rmarkdown.runtime = runtime
# )

