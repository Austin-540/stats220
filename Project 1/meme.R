library(magick)

white_box <- image_blank(width=60, height=50, color="#EEEEEE")

meme <- image_read("inspo_meme.png") %>%
  image_composite(white_box, offset="+210+25") %>% #Cover up the old text from the original comic by putting a white box over it
  image_annotate(text="I use GNU\n/Linux, or \nas I've \nrecently \ntaken to \ncalling it, \nGNU plus \nLinux---",
                 #Then add my text to it
                 size=15,
                 location="+210+25") #This bit lines it up with the box

image_write(meme, "my_meme.png")

#Original comic frames
frame1 <- image_crop(meme, "195x180+0+0")
frame2 <- image_crop(meme, "87x180+195+0") #This one has my text in it
frame3 <- image_crop(meme, "219x180+420+0")
frame4 <- image_crop(meme, "224x140+0+190")
frame5 <- image_crop(meme, "238x140+233+190")
frame6 <- image_crop(meme, "148x140+483+190")

explosion_frames <- NULL
for (num in 3:17) {  #Honestly I should have just not bothered with a loop - it took a lot of debugging
  explosion_img <- image_read(paste(paste("explosiongif_", num, sep=""), ".png",  sep=""))
  img <- image_composite(frame6, explosion_img, offset = "+0+15")
  
  if (is.null(explosion_frames)) { # If this is the first explosion frame, create a list of type magick-image
    explosion_frames <- c(img)
  }
  explosion_frames <- c(explosion_frames, img) # Otherwise just add the frame to the list of explosion frames
}


frames <- c(frame1, frame1, frame1, frame1, frame2, frame2, frame2, frame2, frame2, frame2, frame3, frame3, frame4, frame4, frame5, frame5, frame6, explosion_frames)
image_animate(frames, fps=6)
