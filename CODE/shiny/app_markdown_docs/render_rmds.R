library(rmarkdown)

#render rmarkdown files to html into app folder

output_path <- "mod_geo_mockup/"

#about
render("app_markdown_docs/about.Rmd", output_dir = output_path,
       output_format = "md_document")

# -- social epi ---
render("app_markdown_docs/socepi_pretermbirth.Rmd", output_dir = output_path,
       output_format = "md_document")

#aim 1
render("app_markdown_docs/socepi_aim1.Rmd", output_dir = output_path,
       output_format = "md_document")

#aim 2
render("app_markdown_docs/socepi_aim2.Rmd", output_dir = output_path,
       output_format = "md_document")

#aim 3
render("app_markdown_docs/socepi_aim3.Rmd", output_dir = output_path,
       output_format = "md_document")

#references
#aim 3
render("app_markdown_docs/socepi_ref.Rmd", output_dir = output_path,
       output_format = "md_document")
