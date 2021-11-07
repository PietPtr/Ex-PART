
import pygame
import init
from init import *

def draw_system_rects(screen, system_list):
    for system in system_list:
        key = next(iter(system))
        if 'br' in system[key] and 'tl' in system[key]:
            tl = system[key]['tl']
            br = system[key]['br']
            left = tl['x'] * init.SQUARE_SIZE
            top = tl['y'] * init.SQUARE_SIZE 
            w = abs(tl['x'] - (br['x'] + 1)) * init.SQUARE_SIZE 
            h = abs(tl['y'] - (br['y'] + 1)) * init.SQUARE_SIZE 
            
            rect_color = mute(color(key))
            pygame.draw.rect(screen, rect_color, pygame.Rect(left, top, w, h))
        
        else:
            for key in system:
                draw_system_rects(screen, system[key])

def draw_system_labels(screen, system_list):
    for system in system_list:
        key = next(iter(system))
        if 'br' in system[key] and 'tl' in system[key]:
            tl = system[key]['tl']
            br = system[key]['br']
            left = tl['x'] * init.SQUARE_SIZE
            top = tl['y'] * init.SQUARE_SIZE 
            w = abs(tl['x'] - (br['x'] + 1)) * init.SQUARE_SIZE 
            h = abs(tl['y'] - (br['y'] + 1)) * init.SQUARE_SIZE 
            
            rect_color = mute(color(key))

            text_color = (0, 0, 0) if is_bright(rect_color) else (255, 255, 255)
            textsurface = myfont.render(key, True, text_color)
            if h > w:
                textsurface = pygame.transform.rotate(textsurface, -90)

            text_offset = init.SQUARE_SIZE/10
            if init.SQUARE_SIZE > 18:
                screen.blit(
                    textsurface,
                    (tl['x'] * init.SQUARE_SIZE + text_offset, tl['y'] * init.SQUARE_SIZE + text_offset),
                    area=pygame.Rect(0, 0, w - 2*text_offset, h - 2*text_offset))
        
        else:
            for key in system:
                draw_system_labels(screen, system[key])