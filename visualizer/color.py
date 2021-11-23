
import random
import pygame

def color(name):
    global mods
    name = name + name + name
    n = [ord(n) % 16 for n in name]
    color = (
        (sum(n[::3]) * mods[0]) % 255,
        (sum(n[1::3]) * mods[1]) % 255,
        (sum(n[2::3]) * mods[2]) % 255
    )
    return pygame.Color(color)

def mute(color):
    mute_one = lambda c : min(c + 100, 255)
    r = mute_one(color.r)
    g = mute_one(color.g)
    b = mute_one(color.b)
    return pygame.Color((r, g, b))

primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
mods = [59, 31, 97]

def randomize_colors():
    global mods
    mods = [random.choice(primes), random.choice(primes), random.choice(primes)]


def is_bright(color_any):
    color = pygame.Color(color_any)
    return (color.r * 0.299 + color.g * 0.587 + color.b * 0.114) > 150