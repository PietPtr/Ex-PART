import slice
import init
import pygame

class LegendEntry:
    def __init__(self, name):
        self.name = name


def draw_legend(screen, component_names):
    width = 0
    height = 2
    v_space = 6
    h_space = 42
    surfaces = []
    for cmp_name in component_names:
        text = init.myfont.render(init.cell_name_to_json_path(cmp_name), True, (80, 80, 80))
        surfaces.append((text, cmp_name))
        if text.get_width() > width:
            width = text.get_width()
        height += text.get_height() + v_space

    width += h_space + 2

    leg_rect = pygame.Rect(init.width - width, 0, width, height)
    
    pygame.draw.rect(screen, (201, 211, 221), leg_rect)
    pygame.draw.rect(screen, 0xaaaaaa, leg_rect, width=1)

    half = (h_space * 0.9) // 2

    y = 2
    for (surf, name) in surfaces:
        screen.blit(surf, (init.width - surf.get_width() - h_space, y))

        pygame.draw.rect(screen, init.mute(init.color(name)),
            pygame.Rect(init.width - half * 2, y, h_space // 2, surf.get_height()))
            
        pygame.draw.rect(screen, init.color(name),
            pygame.Rect(init.width - half, y, h_space // 2, surf.get_height()))
        
        y += surf.get_height() + v_space