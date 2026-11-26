"""Draws docs/banner.png and docs/feature-graphic.png from the game's tiles.

    python3 docs/make_art.py assets/sprites/tiles.png NotoSans-Bold.ttf docs

It needs Pillow. The font is the one of the game's titles, Noto Sans Bold.
"""

import random, sys
from PIL import Image, ImageDraw, ImageFilter, ImageFont

SHEET = Image.open(sys.argv[1]).convert("RGBA")
FONT = sys.argv[2]
OUT = sys.argv[3]

def hexc(v, a=255): return ((v >> 16) & 255, (v >> 8) & 255, v & 255, a)
NIGHT, TEXT, DIM, GOLD, WATER = 0x0b0d17, 0xe8e4d4, 0x8f96ad, 0xf2c14e, 0x2e6f95

def tile(col, row, color, scale, alpha=255):
    cell = SHEET.crop((col * 16, row * 16, col * 16 + 16, row * 16 + 16))
    mask = cell.split()[3]
    out = Image.new("RGBA", (16, 16), (0, 0, 0, 0))
    out.paste(Image.new("RGBA", (16, 16), hexc(color, alpha)), (0, 0), mask)
    return out.resize((16 * scale, 16 * scale), Image.NEAREST)

def glow(img, at, size, color, radius, strength):
    layer = Image.new("RGBA", img.size, (0, 0, 0, 0))
    d = ImageDraw.Draw(layer)
    x, y = at
    d.ellipse((x - size, y - size, x + size, y + size), fill=hexc(color, strength))
    img.alpha_composite(layer.filter(ImageFilter.GaussianBlur(radius)))

def lerp(a, b, t): return tuple(int(a[i] + (b[i] - a[i]) * t) for i in range(4))

def make(w, h, title_size, sub_size, tagline, columns=3, tall=4):
    img = Image.new("RGBA", (w, h))
    top, mid, low = hexc(NIGHT), hexc(0x141128), hexc(0x0f2a33)
    d = ImageDraw.Draw(img)
    for y in range(h):
        t = y / (h - 1)
        d.line([(0, y), (w, y)], fill=lerp(top, mid, t / 0.6) if t < 0.6 else lerp(mid, low, (t - 0.6) / 0.4))
    rng = random.Random(7)
    for _ in range(int(w * h / 2600)):
        x, y, s = rng.randrange(w), rng.randrange(int(h * 0.75)), rng.choice([2, 2, 3, 4])
        d.rectangle((x, y, x + s - 1, y + s - 1), fill=hexc(TEXT, rng.randrange(40, 170)))

    scale = max(3, h // 100)
    cell = 16 * scale
    water_top = h - int(cell * 1.15)
    # The tower: walls of the Star-Well on both sides, down into the water.
    for side in (0, 1):
        for i in range(columns):
            x = i * cell if side == 0 else w - (i + 1) * cell
            d.rectangle((x, water_top - cell * (tall - i), x + cell - 1, water_top), fill=hexc(0x110e22))
            for y in range(water_top - cell * (tall - i), water_top, cell):
                img.alpha_composite(tile(10, 17, 0x2a2350 if i else 0x3a3270, scale), (x, y))
            img.alpha_composite(tile(18, 6, 0x8f7fe0, scale, 140), (x, water_top - cell * (tall + 1 - i)))
    # Water and its waves.
    d.rectangle((0, water_top + cell // 2, w, h), fill=hexc(0x0d2a3d))
    for x in range(-cell // 2, w, cell):
        img.alpha_composite(tile(18, 5, WATER, scale), (x, water_top))
        img.alpha_composite(tile(18, 5, 0x1f4f6e, scale), (x + cell // 2, water_top + cell // 2))
    # Who goes down, and what waits.
    ledge = water_top - cell
    figures = [((28, 0), TEXT, 3.6), ((32, 10), 0xfff3b0, 4.7)]
    left = [((28, 0), TEXT)]
    right = [((26, 6), 0x79c7f0), ((25, 5), 0xe0a34a), ((27, 8), 0x9a7bd1)]
    for i, (s, c) in enumerate(left):
        img.alpha_composite(tile(*s, c, scale), (3 * cell + i * cell + cell // 3, ledge))
    glow(img, (3 * cell + cell // 3 + cell // 2, ledge + cell // 2), cell // 2, 0xf2c14e, cell // 2, 70)
    img.alpha_composite(tile(28, 0, TEXT, scale), (3 * cell + cell // 3, ledge))
    for i, (s, c) in enumerate(right):
        img.alpha_composite(tile(*s, c, scale), (w - 4 * cell - i * int(cell * 1.15) - cell // 3, ledge))

    # The Lodestar, and the title.
    title = ImageFont.truetype(FONT, title_size)
    sub = ImageFont.truetype(FONT, sub_size)
    emblem_scale = max(4, scale + 1)
    emblem = tile(32, 10, GOLD, emblem_scale)
    title_w = d.textlength("Sunken Observatory", font=title)
    block = emblem.height + int(title_size * 0.35) + title_size + int(sub_size * 1.2) + sub_size
    y0 = max(int(h * 0.07), (ledge - block) // 2)
    glow(img, (w // 2, y0 + emblem.height // 2), emblem.height, GOLD, emblem.height // 2, 75)
    glow(img, (w // 2, y0 + emblem.height // 2), emblem.height // 2, 0xfff3b0, emblem.height // 4, 90)
    img.alpha_composite(emblem, ((w - emblem.width) // 2, y0))
    ty = y0 + emblem.height + int(title_size * 0.2)
    d = ImageDraw.Draw(img)
    shadow = Image.new("RGBA", img.size, (0, 0, 0, 0))
    ImageDraw.Draw(shadow).text(((w - title_w) / 2, ty + 4), "Sunken Observatory", font=title, fill=(0, 0, 0, 200))
    img.alpha_composite(shadow.filter(ImageFilter.GaussianBlur(4)))
    d.text(((w - title_w) / 2, ty), "Sunken Observatory", font=title, fill=hexc(TEXT))
    sy = ty + int(title_size * 1.25)
    d.text(((w - d.textlength(tagline, font=sub)) / 2, sy), tagline, font=sub, fill=hexc(GOLD))
    return img.convert("RGB")

make(1280, 400, 72, 24, "A turn-based descent into a flooded tower").save(OUT + "/banner.png", optimize=True)
make(1024, 500, 76, 27, "The Lodestar lies eight floors down. Bring it back.", columns=2, tall=2).save(OUT + "/feature-graphic.png", optimize=True)
