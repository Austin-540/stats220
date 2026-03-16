library(magick)

white_box <- image_blank(width=60, height=50, color="#EEEEEE")

meme <- image_read("inspo_meme.png") %>%
  image_composite(white_box, offset="+210+25") %>% #Cover up the old text from the original comic by putting a white box over it
  image_annotate(text="I use GNU\n/Linux, or \nas I've \nrecently \ntaken to \ncalling it, \nGNU plus \nLinux---",
                 #Then add my text to it
                 size=15,
                 location="+210+25") #This bit lines it up with the box

image_write(meme, "my_meme.png")
