import slice
import init
import pygame
import color

class LegendEntry:
    def __init__(self, name):
        self.name = name


def draw_legend(screen, component_names):
    if init.PAPER:
        return
        
    width = 0
    height = 2
    v_space = 6
    h_space = 42
    surfaces = []
    for cmp_type in component_names:
        cmp_name = init.cell_name_to_json_path(cmp_type)
        init.myfont.underline = False

        if init.OVER_COMPONENT == cmp_type:
            init.myfont.underline = True

        text = init.myfont.render(cmp_name, True, (80, 80, 80))
        surfaces.append((text, cmp_type))
        if text.get_width() > width:
            width = text.get_width()
        height += text.get_height() + v_space
    init.myfont.underline = False

    width += h_space + 2

    leg_rect = pygame.Rect(init.width - width, 0, width, height)

    
    
    pygame.draw.rect(screen, (201, 211, 221), leg_rect)
    pygame.draw.rect(screen, 0xaaaaaa, leg_rect, width=1)

    half = (h_space * 0.9) // 2

    y = 2
    for (surf, name) in surfaces:
        screen.blit(surf, (init.width - surf.get_width() - h_space, y))

        pygame.draw.rect(screen, color.mute(color.color(name)),
            pygame.Rect(init.width - half * 2, y, h_space // 2, surf.get_height()))
            
        pygame.draw.rect(screen, color.color(name),
            pygame.Rect(init.width - half, y, h_space // 2, surf.get_height()))
        
        y += surf.get_height() + v_space