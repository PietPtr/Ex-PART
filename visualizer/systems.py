
import pygame
import init
from init import *
import color

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
            
            rect_color = color.mute(color.color(key))
            bw = max(init.SQUARE_SIZE // 12, 2)
            pygame.draw.rect(screen, rect_color, 
                pygame.Rect(init.view(left, top), (w, h)), 
                width=bw, border_radius=1)
        
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
            
            rect_color = color.mute(color.color(key))

            text_color = (0, 0, 0) if color.is_bright(rect_color) else (255, 255, 255)
            textsurface = myfont.render(key, True, text_color)
            if h > w:
                textsurface = pygame.transform.rotate(textsurface, -90)

            text_offset = init.SQUARE_SIZE // 12


            if init.SQUARE_SIZE > 18:

                text_x = tl['x'] * init.SQUARE_SIZE + text_offset
                text_y = tl['y'] * init.SQUARE_SIZE + text_offset
                text_w = min(textsurface.get_width(), w - 2*text_offset) + 1
                text_h = min(textsurface.get_height(), h - 2*text_offset) + 1

                pygame.draw.rect(screen, rect_color, pygame.Rect(init.view(text_x, text_y), (text_w, text_h)))
        

                screen.blit(
                    textsurface,
                    init.view(text_x, text_y),
                    area=pygame.Rect(0, 0, w - 2*text_offset, h - 2*text_offset))
        
        else:
            for key in system:
                draw_system_labels(screen, system[key])