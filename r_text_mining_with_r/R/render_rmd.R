# find all the .Rmd file
files <- list.files(pattern = "[.]Rmd$")

# render all the .Rmd file
for (f in files) {rmarkdown::render(f, "prettydoc::html_pretty", output_dir = "_doc/")}
