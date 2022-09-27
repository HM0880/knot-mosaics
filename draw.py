"""Draw each knot as an array and save as a PNG image.

To run:
    python3 draw.py <m> <n> <logfile> <keep_about_this_many>

  where
    m is the number of rows in the board,
    n is the number of columns in the board,
    logfile is the path to the log file written by solve.py, and
    keep_about_this_many is the approximate number of randomly chosen
    knots to draw.

For example, for a 5x3 board to draw ~20 knots, the command is
    python3 draw.py 5 3 5x3.log 20

Hannah Miller
https://hm0880.github.io/
September 2022

"""

import matplotlib.image as mpl
import matplotlib.pyplot as plt
import numpy as np
import os
import random
import sys

import encode as utils  # custom Python file to encode basic constraints


def read_tile_image(k):
    """Return a 2D array of the image of tile k."""
    return mpl.imread(f"./tiles/{k}.png")


def read_tiles():
    """Return a dictionary of the tile images to access later."""
    return {
        0: read_tile_image(0),
        1: read_tile_image(1),
        2: read_tile_image(2),
        3: read_tile_image(3),
        4: read_tile_image(4),
        5: read_tile_image(5),
        6: read_tile_image(6),
        7: read_tile_image(7),
        8: read_tile_image(8),
        9: read_tile_image(9),
        10: read_tile_image(10),
    }


def draw_one_solution(board, line, tiles, save_root, make_plot=True):
    # Initialize an m x n array to hold the image
    m = board.m
    n = board.n
    td = 80  # tile dimension td; all tiles are 80 x 80
    img = np.ones((m * td, n * td)) * 0.9  # make grayscale

    # Loop
    filename = ""
    for v in line.split(" "):  # variable v
        if int(v) > 0:  # if the variable is True, draw the tile
            i, j, k = utils.var2tup(board, int(v))
            r1 = i * td  # the image covers rows r1 to r2
            c1 = j * td  # the image covers columns c1 to c2
            r2 = (i + 1) * td
            c2 = (j + 1) * td
            img[r1:r2, c1:c2] = tiles[k]  # add to image
            if k == 10:  # convert 10 to A to save space in the file name
                k = "A"
            filename = filename + "{}".format(k)

    # Insert a dash to mark each row of the image.  Stride length is n
    # (i.e., skip by each row so need to move by the number of columns n).
    filename = "-".join(filename[q : q + n] for q in range(0, m * n, n))
    filename = os.path.join(save_root, filename + ".png")

    # Draw the image
    if make_plot:
        fig, ax = plt.subplots(nrows=1, ncols=1)
        plt.imshow(img, cmap="binary_r", interpolation="none")
        ax.set_title("{}".format(filename, fontsize=50, family="serif"))
        ax.set_xticks([])
        ax.set_yticks([])
        plt.tight_layout(pad=0.3, w_pad=0.0, h_pad=1.0)
        plt.savefig(filename)
        plt.close()


def process_logfile(m, n, logfile, keep_about_this_many, save_root):
    tiles = read_tiles()
    board = utils.MakeBoard(m, n)

    num_lines = 0
    with open(logfile) as f:
        for line in f:
            num_lines += 1
    prob = keep_about_this_many / num_lines

    with open(logfile) as f:
        for line in f:
            if random.random() < prob:
                # remove leading spaces and trailing newline
                line = line.lstrip().strip("\n")
                draw_one_solution(board, line, tiles, save_root, make_plot=True)


################################################################

if __name__ == "__main__":
    # Get command line arguments A
    A = sys.argv
    m = int(sys.argv[1])
    n = int(sys.argv[2])
    logfile = sys.argv[3]
    keep_about_this_many = int(sys.argv[4])

    process_logfile(
        m, n, logfile, keep_about_this_many, save_root="./knot-mosaic-images/"
    )
