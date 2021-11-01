
import pygame
import random
import sys

if len(sys.argv) == 1:
    print("Please provide the project folder name.")
    quit()

pygame.init()

size = width, height = 1280, 720
black = 0, 0, 0
white = 255, 255, 255
red = 255, 100, 100

pygame.font.init() 
myfont = pygame.font.SysFont('Courier', 14)

SQUARE_SIZE = pygame.display.Info().current_w // 24

def color(name):
    global mods
    name = name + name + name
    n = [ord(n) % 16 for n in name]
    color = (
        (sum(n[::3]) * mods[0]) % 255,
        (sum(n[1::3]) * mods[1]) % 255,
        (sum(n[2::3]) * mods[2]) % 255
    )
    return color


primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
mods = []

def randomize_colors():
    global mods
    mods = [random.choice(primes), random.choice(primes), random.choice(primes)]

def cell_name_to_json_path(cell_name):
    expart_name = cell_name.split('.')

    if len(expart_name) == 0:
        return "NAME ERROR"


    subnames = map(lambda s: s.split('-')[-1], expart_name)
    json_path = ".".join(subnames)

    return json_path