# Read in the file as raw bytes
raw_data <- readBin("Term Paper Code.qmd", "raw", file.info("Term Paper Code.qmd")$size)

# Convert raw bytes to characters, omitting null characters
char_data <- rawToChar(raw_data[raw_data != as.raw(0)])

# Write the cleaned data back to a new file
writeLines(char_data, "cleaned_file.qmd")

content <- readLines("Term Paper Code.qmd")
content <- sapply(content, function(x) iconv(x, "latin1", "ASCII", sub=""))
writeLines(content, "cleaned_file.qmd")
# Read the cleaned file
cleaned_content <- readLines("cleaned_file.qmd")
