## code to prepare `lee` dataset goes here

lee <- featuretable:::ft_from.phyloseq(DivNet::Lee)

lee$sample_data$color <- factor(lee$sample_data$color)
lee$sample_data$temp <- as.numeric(as.character(lee$sample_data$temp))

colnames(lee$sample_data) <- c("Temp", "Type", "Char", "Color")

usethis::use_data(lee, overwrite = TRUE)
