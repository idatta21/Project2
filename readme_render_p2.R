## The render code to output the README.md file

rmarkdown::render("./README.Rmd", 
                  output_format = "github_document",
                  output_dir = "./", 
                  output_options = list(
                    toc = FALSE, 
                    html_preview = FALSE, 
                    keep_html = FALSE
                  )
)

