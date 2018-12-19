library(tidyverse)
load("~/Desktop/ird-5acc-paper/data/all-sep-deseq.Rdata")
load("~/Desktop/ird-5acc-paper/data/rlog-pca.Rdata")


pc_spc <- pc_spc$x %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  dplyr::rename(locus_id = "rowname") %>%
  select(locus_id, PC1)

to_plot <- pcro %>%
  select(PC5, locus_id) %>%
  inner_join(pc_spc)

p <- ggplot(to_plot,
            aes(x = PC5,
                y = PC1)) + 
  geom_hex(bins = 80) +
  theme_void() +
  # xlab("Var 1") +
  # ylab("Var 2") +
  # scale_fill_gradient(high = "#001a33",
  #                     low = "#cce6ff",
  #                     guide = FALSE)
  scale_fill_viridis_c(
    # begin = 1, end = 0,
    option = "D",
    guide = FALSE,
    trans = "log")

p

# pdf(file = "../fig/fig-PC5-rlog-PC1-stat.pdf")
png(filename = "cards/galaxy.png",
     height = 1200,
     width = 1200,
     res = 300)
p
dev.off()

pdf("cards/galaxy.pdf",
    height = 12,
    width = 12)
p
dev.off()
