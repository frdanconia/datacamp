#%%
# Import the modules from skimage
from skimage import data, color
from matplotlib import pyplot as plt
import numpy as np

def show_image(image, title='Image', cmap_type='gray'):
    plt.imshow(image, cmap=cmap_type)
    plt.title(title)
    plt.axis('off')
    plt.show()

#%%
# Load the rocket image
rocket = data.rocket()

# Convert the image to grayscale
gray_scaled_rocket = color.rgb2gray(rocket)

# Show the original image
show_image(rocket, 'Original RGB image')

# Show the grayscale image
show_image(gray_scaled_rocket, 'Grayscale image')
#%%
# Flip the image vertically
seville_vertical_flip = np.flipud(rocket)

# Flip the image horizontally
seville_horizontal_flip = np.fliplr(seville_vertical_flip)

# The original, flipped image
show_image(rocket, 'Flipped Seville')

# Show the resulting image
show_image(seville_horizontal_flip, 'Seville')


#%%



