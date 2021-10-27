## The render code to output the .md files
## Automation
source("./README.RMD")

out <- paste0(channel, ".md")
params = lapply(channel, FUN = function(x){list(channel = x)})

docs <- tibble(out, params)

apply(
  docs, 
  MARGIN = 1, 
  FUN = function(x){
    rmarkdown::render(input = "./README.Rmd", 
                      output_file = x[[1]], 
                      output_format = "github_document",
                      output_dir = "./", 
                      output_options = list(
                        toc = FALSE, 
                        html_preview = FALSE, 
                        keep_html = FALSE
                      ),
                      params = x[[2]]
    )
  }
)

