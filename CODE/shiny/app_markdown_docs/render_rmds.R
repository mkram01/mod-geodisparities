library(rmarkdown)

#render rmarkdown files to html into app folder

output_path <- "/home/e/Documents/git_repos/mod-geodisparities/CODE/shiny/mod_geo_mockup/"

#about
render("/home/e/Documents/git_repos/mod-geodisparities/CODE/shiny/app_markdown_docs/about.Rmd", output_dir = output_path,
       output_format = "html_document")




# -- social epi ---
render("/home/e/Documents/git_repos/mod-geodisparities/CODE/shiny/app_markdown_docs/socepi_pretermbirth.Rmd", output_dir = output_path,
       output_format = "html_document")

#aim 1
render("/home/e/Documents/git_repos/mod-geodisparities/CODE/shiny/app_markdown_docs/socepi_aim1.Rmd", output_dir = output_path,
       output_format = "html_document")

#aim 2
render("/home/e/Documents/git_repos/mod-geodisparities/CODE/shiny/app_markdown_docs/socepi_aim2.Rmd", output_dir = output_path,
       output_format = "html_document")

#aim 3
render("/home/e/Documents/git_repos/mod-geodisparities/CODE/shiny/app_markdown_docs/socepi_aim3.Rmd", output_dir = output_path,
       output_format = "html_document")

#references
#aim 3
render("/home/e/Documents/git_repos/mod-geodisparities/CODE/shiny/app_markdown_docs/socepi_ref.Rmd", output_dir = output_path,
       output_format = "html_document")
