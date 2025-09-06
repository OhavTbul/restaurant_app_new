import pygame
import sys
import random
import socket
import select
import time
import os
import math

# --- Enhanced Settings ---
SCREEN_WIDTH = 1400
SCREEN_HEIGHT = 900
GRID_SIZE = 25  # Slightly smaller grid for the smaller window
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
GRAY = (150, 150, 150)
GREEN = (0, 200, 0)
RED = (255, 0, 0)
BLUE = (0, 0, 200)
YELLOW = (255, 255, 0)
BROWN = (101, 67, 33)
LIGHT_GRAY = (200, 200, 200)
DARK_BROWN = (139, 69, 19)
LIGHT_BROWN = (205, 133, 63)
KITCHEN_GRAY = (64, 64, 64)
FLOOR_COLOR = (245, 245, 220)
WALL_COLOR = (255, 248, 220)

# --- Beautiful Restaurant-specific colors ---
TABLE_COLOR = (160, 82, 45)  # Saddle brown
CHAIR_COLOR = (139, 69, 19)  # Saddle brown
MACHINE_COLOR = (105, 105, 105)  # Dim gray
WAITER_COLOR = (25, 25, 112)  # Midnight blue
CUSTOMER_COLOR = (255, 182, 193)  # Light pink
KITCHEN_COLOR = (47, 79, 79)  # Dark slate gray
DINING_AREA_COLOR = (255, 250, 240)  # Cream
BAR_COLOR = (139, 69, 19)  # Saddle brown
ENTRANCE_COLOR = (255, 215, 0)  # Gold
DECORATION_COLOR = (220, 20, 60)  # Crimson

# --- Game Constants ---
MAX_TABLES = 120
MAX_MACHINES = 25
MAX_WAITERS = 50
MAX_QUEUE_DISPLAY = 5
KITCHEN_POS = (2, 25)  # Moved kitchen much lower and wider
QUEUE_START_POS = (2, 2)  # Better queue position
BAR_POS = (45, 14)  # Moved bar to the right side
ENTRANCE_POS = (12, 2)  # Adjusted entrance

# --- Enhanced Entity Classes ---
class Entity:
    def __init__(self, entity_id, entity_type, pos, color):
        self.id = entity_id
        self.type = entity_type
        self.pos = pos
        self.color = color
        self.rect = pygame.Rect(pos[0] * GRID_SIZE, pos[1] * GRID_SIZE, GRID_SIZE, GRID_SIZE)

    def draw(self, screen):
        pygame.draw.rect(screen, self.color, self.rect)

class Table(Entity):
    def __init__(self, table_id, pos):
        super().__init__(table_id, 'table', pos, TABLE_COLOR)
        self.is_dirty = False
        self.has_customers = False
        # Load table image - bigger size
        try:
            self.table_image = pygame.image.load('gui/table.png')
            # Scale to 1.5x the original grid size for bigger tables
            table_size = int(GRID_SIZE * 1.5)
            self.table_image = pygame.transform.scale(self.table_image, (table_size, table_size))
        except pygame.error:
            self.table_image = None

    def draw(self, screen):
        # Draw table shadow
        shadow_rect = pygame.Rect(self.rect.x + 3, self.rect.y + 3, self.rect.width, self.rect.height)
        pygame.draw.rect(screen, (100, 100, 100), shadow_rect, border_radius=5)
        
        if self.table_image:
            # Calculate bigger table size and center it
            table_size = int(GRID_SIZE * 1.5)
            table_x = self.rect.x + (self.rect.width - table_size) // 2
            table_y = self.rect.y + (self.rect.height - table_size) // 2
            table_rect = pygame.Rect(table_x, table_y, table_size, table_size)
            
            # Draw the table image
            screen.blit(self.table_image, table_rect)
            
            # Add dirty overlay if table is dirty
            if self.is_dirty:
                overlay = pygame.Surface((table_size, table_size), pygame.SRCALPHA)
                overlay.fill((255, 0, 0, 60))  # Semi-transparent red
                screen.blit(overlay, table_rect)
        else:
            # Fallback to original drawing if image not available
            table_rect = pygame.Rect(self.rect.x + 2, self.rect.y + 2, self.rect.width - 4, self.rect.height - 4)
            color = RED if self.is_dirty else TABLE_COLOR
            pygame.draw.rect(screen, color, table_rect, border_radius=5)
            
            # Draw table border with highlight
            pygame.draw.rect(screen, DARK_BROWN, table_rect, 3, border_radius=5)
            pygame.draw.rect(screen, LIGHT_BROWN, table_rect, 1, border_radius=5)
            
            # Draw beautiful chairs around the table
            chair_size = 8
            chair_color = (139, 69, 19)
            chair_highlight = (160, 82, 45)
            
            # Top chair with backrest
            top_chair = pygame.Rect(self.rect.x + 6, self.rect.y - 6, chair_size, chair_size)
            pygame.draw.rect(screen, chair_color, top_chair, border_radius=3)
            pygame.draw.rect(screen, chair_highlight, top_chair, 1, border_radius=3)
            # Chair backrest
            pygame.draw.rect(screen, chair_color, (self.rect.x + 6, self.rect.y - 8, chair_size, 2))
            
            # Bottom chair
            bottom_chair = pygame.Rect(self.rect.x + 6, self.rect.y + self.rect.height - 2, chair_size, chair_size)
            pygame.draw.rect(screen, chair_color, bottom_chair, border_radius=3)
            pygame.draw.rect(screen, chair_highlight, bottom_chair, 1, border_radius=3)
            pygame.draw.rect(screen, chair_color, (self.rect.x + 6, self.rect.y + self.rect.height + 6, chair_size, 2))
            
            # Left chair
            left_chair = pygame.Rect(self.rect.x - 6, self.rect.y + 6, chair_size, chair_size)
            pygame.draw.rect(screen, chair_color, left_chair, border_radius=3)
            pygame.draw.rect(screen, chair_highlight, left_chair, 1, border_radius=3)
            pygame.draw.rect(screen, chair_color, (self.rect.x - 8, self.rect.y + 6, 2, chair_size))
            
            # Right chair
            right_chair = pygame.Rect(self.rect.x + self.rect.width - 2, self.rect.y + 6, chair_size, chair_size)
            pygame.draw.rect(screen, chair_color, right_chair, border_radius=3)
            pygame.draw.rect(screen, chair_highlight, right_chair, 1, border_radius=3)
            pygame.draw.rect(screen, chair_color, (self.rect.x + self.rect.width + 6, self.rect.y + 6, 2, chair_size))
        
        # Draw table number above the table object
        font = pygame.font.Font(None, 24)
        text = font.render(str(self.id), True, WHITE)
        # Position above the bigger table
        if self.table_image:
            table_size = int(GRID_SIZE * 1.5)
            table_y = self.rect.y + (self.rect.height - table_size) // 2
            text_rect = text.get_rect(center=(self.rect.centerx, table_y - 15))
        else:
            text_rect = text.get_rect(center=(self.rect.centerx, self.rect.y - 15))
        
        # Text background
        text_bg = text_rect.inflate(8, 4)
        pygame.draw.rect(screen, BLACK, text_bg, border_radius=3)
        pygame.draw.rect(screen, WHITE, text_bg, 1, border_radius=3)
        
        screen.blit(text, text_rect)
        
        # Draw "CLEAN" message if table is dirty
        if self.is_dirty:
            clean_font = pygame.font.Font(None, 20)
            clean_text = clean_font.render("CLEAN", True, WHITE)
            clean_rect = clean_text.get_rect(center=(self.rect.centerx, self.rect.bottom + 15))
            
            # Background for clean message
            clean_bg = clean_rect.inflate(10, 4)
            pygame.draw.rect(screen, RED, clean_bg, border_radius=3)
            pygame.draw.rect(screen, WHITE, clean_bg, 1, border_radius=3)
            
            screen.blit(clean_text, clean_rect)

class Machine(Entity):
    def __init__(self, machine_id, pos, state='idle'):
        super().__init__(machine_id, 'machine', pos, MACHINE_COLOR)
        self.is_busy = False
        self.level = 0
        self.state = state
        # Load cook machine image - bigger size
        try:
            self.machine_image = pygame.image.load('gui/cook_machine.png')
            # Scale to 1.5x the original grid size for bigger machines
            machine_size = int(GRID_SIZE * 1.5)
            self.machine_image = pygame.transform.scale(self.machine_image, (machine_size, machine_size))
        except pygame.error:
            self.machine_image = None

    def draw(self, screen):
        # Draw machine shadow
        shadow_rect = pygame.Rect(self.rect.x + 3, self.rect.y + 3, self.rect.width, self.rect.height)
        pygame.draw.rect(screen, (60, 60, 60), shadow_rect, border_radius=4)
        
        if self.machine_image:
            # Draw the cook machine image
            screen.blit(self.machine_image, self.rect)
            
            # Add cooking effect overlay if machine is busy
            if self.state in ['cooking', 'busy']:
                # Create a red overlay for cooking effect
                overlay = pygame.Surface((self.rect.width, self.rect.height), pygame.SRCALPHA)
                overlay.fill((255, 0, 0, 80))  # Semi-transparent red
                screen.blit(overlay, self.rect)
                
                # Add glow effect around the machine
                for i in range(3):
                    glow_radius = 8 + i * 2
                    glow_alpha = 60 - i * 20
                    glow_surface = pygame.Surface((glow_radius * 2, glow_radius * 2), pygame.SRCALPHA)
                    pygame.draw.circle(glow_surface, (255, 0, 0, glow_alpha), (glow_radius, glow_radius), glow_radius)
                    screen.blit(glow_surface, (self.rect.centerx - glow_radius, self.rect.centery - glow_radius))
        else:
            # Fallback to original drawing if image not available
            machine_rect = pygame.Rect(self.rect.x + 1, self.rect.y + 1, self.rect.width - 2, self.rect.height - 2)
            color = RED if self.state in ['cooking', 'busy'] else MACHINE_COLOR
            pygame.draw.rect(screen, color, machine_rect, border_radius=4)
            
            # Draw metallic highlights
            highlight_rect = pygame.Rect(self.rect.x + 2, self.rect.y + 2, self.rect.width - 4, 4)
            pygame.draw.rect(screen, (180, 180, 180), highlight_rect, border_radius=2)
            
            # Draw control panel with buttons
            panel_rect = pygame.Rect(self.rect.x + 4, self.rect.y + 6, 10, 6)
            pygame.draw.rect(screen, BLACK, panel_rect, border_radius=2)
            
            # Control buttons
            pygame.draw.circle(screen, RED, (self.rect.x + 6, self.rect.y + 8), 1)
            pygame.draw.circle(screen, GREEN, (self.rect.x + 9, self.rect.y + 8), 1)
            pygame.draw.circle(screen, YELLOW, (self.rect.x + 12, self.rect.y + 8), 1)
            
            # Heat indicator with glow effect
            if self.state in ['cooking', 'busy']:
                # Glow effect
                for i in range(3):
                    glow_radius = 5 + i
                    glow_alpha = 100 - i * 30
                    glow_surface = pygame.Surface((glow_radius * 2, glow_radius * 2), pygame.SRCALPHA)
                    pygame.draw.circle(glow_surface, (255, 0, 0, glow_alpha), (glow_radius, glow_radius), glow_radius)
                    screen.blit(glow_surface, (self.rect.x + self.rect.width - 8 - glow_radius, self.rect.y + 6 - glow_radius))
                
                pygame.draw.circle(screen, RED, (self.rect.x + self.rect.width - 6, self.rect.y + 6), 3)
        
        # Draw machine number above the machine object
        font = pygame.font.Font(None, 20)
        text = font.render(str(self.id), True, WHITE)
        text_rect = text.get_rect(center=(self.rect.centerx, self.rect.y - 15))
        
        # Text background
        text_bg = text_rect.inflate(6, 2)
        pygame.draw.rect(screen, BLACK, text_bg, border_radius=2)
        pygame.draw.rect(screen, WHITE, text_bg, 1, border_radius=2)
        
        screen.blit(text, text_rect)

class Waiter(Entity):
    def __init__(self, waiter_id, pos, state='idle'):
        super().__init__(waiter_id, 'waiter', pos, WAITER_COLOR)
        self.level = 0
        self.state = state
        # Load waiter image - bigger size
        try:
            self.waiter_image = pygame.image.load('gui/waiter.png')
            # Scale to 1.5x the original grid size for bigger waiters
            waiter_size = int(GRID_SIZE * 1.5)
            self.waiter_image = pygame.transform.scale(self.waiter_image, (waiter_size, waiter_size))
        except pygame.error:
            self.waiter_image = None

    def draw(self, screen):
        # Draw waiter shadow
        shadow_rect = pygame.Rect(self.rect.x + 2, self.rect.y + 2, self.rect.width, self.rect.height)
        pygame.draw.rect(screen, (100, 100, 100), shadow_rect, border_radius=10)
        
        if self.waiter_image:
            # Calculate bigger waiter size and center it
            waiter_size = int(GRID_SIZE * 1.5)
            waiter_x = self.rect.x + (self.rect.width - waiter_size) // 2
            waiter_y = self.rect.y + (self.rect.height - waiter_size) // 2
            waiter_rect = pygame.Rect(waiter_x, waiter_y, waiter_size, waiter_size)
            
            # Draw the waiter image
            screen.blit(self.waiter_image, waiter_rect)
            
            # Add serving effect overlay if waiter is busy
            if self.state in ['serving', 'busy']:
                overlay = pygame.Surface((waiter_size, waiter_size), pygame.SRCALPHA)
                overlay.fill((0, 255, 0, 60))  # Semi-transparent green
                screen.blit(overlay, waiter_rect)
        else:
            # Fallback to original drawing if image not available
            waiter_rect = pygame.Rect(self.rect.x + 3, self.rect.y + 3, self.rect.width - 6, self.rect.height - 6)
            color = GREEN if self.state in ['serving', 'busy'] else WAITER_COLOR
            pygame.draw.rect(screen, color, waiter_rect, border_radius=10)
            
            # Draw uniform details
            pygame.draw.rect(screen, WHITE, (self.rect.x + 6, self.rect.y + 8, self.rect.width - 12, 4))
            pygame.draw.rect(screen, WHITE, (self.rect.x + 6, self.rect.y + 16, self.rect.width - 12, 2))
            
            # Draw waiter head with hair
            head_radius = 7
            head_pos = (self.rect.centerx, self.rect.y + 7)
            pygame.draw.circle(screen, (255, 218, 185), head_pos, head_radius)  # Skin color
            
            # Draw hair
            hair_rect = pygame.Rect(self.rect.centerx - 4, self.rect.y + 2, 8, 4)
            pygame.draw.rect(screen, (139, 69, 19), hair_rect, border_radius=2)
            
            # Draw eyes
            pygame.draw.circle(screen, BLACK, (self.rect.centerx - 2, self.rect.y + 6), 1)
            pygame.draw.circle(screen, BLACK, (self.rect.centerx + 2, self.rect.y + 6), 1)
            
            # Draw waiter arms with hands
            arm_color = (255, 218, 185)
            # Left arm
            pygame.draw.rect(screen, arm_color, (self.rect.x + 2, self.rect.y + 10, 4, 10))
            # Left hand
            pygame.draw.circle(screen, arm_color, (self.rect.x + 4, self.rect.y + 20), 2)
            
            # Right arm
            pygame.draw.rect(screen, arm_color, (self.rect.x + self.rect.width - 6, self.rect.y + 10, 4, 10))
            # Right hand
            pygame.draw.circle(screen, arm_color, (self.rect.x + self.rect.width - 4, self.rect.y + 20), 2)
        
        # Draw waiter number above the waiter object
        font = pygame.font.Font(None, 18)
        text = font.render(str(self.id), True, WHITE)
        # Position above the bigger waiter
        if self.waiter_image:
            waiter_size = int(GRID_SIZE * 1.5)
            waiter_y = self.rect.y + (self.rect.height - waiter_size) // 2
            text_rect = text.get_rect(center=(self.rect.centerx, waiter_y - 15))
        else:
            text_rect = text.get_rect(center=(self.rect.centerx, self.rect.y - 15))
        
        # Badge background
        badge_rect = text_rect.inflate(8, 4)
        pygame.draw.rect(screen, BLACK, badge_rect, border_radius=3)
        pygame.draw.rect(screen, WHITE, badge_rect, 1, border_radius=3)
        
        screen.blit(text, text_rect)

class Customer(Entity):
    def __init__(self, customer_id, pos, state='idle'):
        super().__init__(customer_id, 'customer', pos, CUSTOMER_COLOR)
        self.state = state

    def draw(self, screen):
        if self.state == 'seated':
            # Draw seated customer shadow
            shadow_rect = pygame.Rect(self.rect.x + 2, self.rect.y + 2, self.rect.width, self.rect.height)
            pygame.draw.rect(screen, (100, 100, 100), shadow_rect, border_radius=8)
            
            # Draw seated customer body
            customer_rect = pygame.Rect(self.rect.x + 3, self.rect.y + 3, self.rect.width - 6, self.rect.height - 6)
            pygame.draw.rect(screen, CUSTOMER_COLOR, customer_rect, border_radius=8)
            
            # Draw customer head with hair
            head_radius = 6
            head_pos = (self.rect.centerx, self.rect.y + 6)
            pygame.draw.circle(screen, (255, 218, 185), head_pos, head_radius)
            
            # Draw hair
            hair_rect = pygame.Rect(self.rect.centerx - 3, self.rect.y + 2, 6, 3)
            pygame.draw.rect(screen, (139, 69, 19), hair_rect, border_radius=1)
            
            # Draw eyes
            pygame.draw.circle(screen, BLACK, (self.rect.centerx - 1, self.rect.y + 5), 1)
            pygame.draw.circle(screen, BLACK, (self.rect.centerx + 1, self.rect.y + 5), 1)
            
            # Draw customer arms with hands
            arm_color = (255, 218, 185)
            pygame.draw.rect(screen, arm_color, (self.rect.x + 2, self.rect.y + 10, 3, 8))
            pygame.draw.rect(screen, arm_color, (self.rect.x + self.rect.width - 5, self.rect.y + 10, 3, 8))
            
            # Draw hands
            pygame.draw.circle(screen, arm_color, (self.rect.x + 3, self.rect.y + 18), 2)
            pygame.draw.circle(screen, arm_color, (self.rect.x + self.rect.width - 3, self.rect.y + 18), 2)
            
        else:
            # Draw standing customer (in queue) with shadow
            shadow_rect = pygame.Rect(self.rect.x + 1, self.rect.y + 1, self.rect.width, self.rect.height)
            pygame.draw.rect(screen, (100, 100, 100), shadow_rect, border_radius=6)
            
            customer_rect = pygame.Rect(self.rect.x + 2, self.rect.y + 2, self.rect.width - 4, self.rect.height - 4)
            pygame.draw.rect(screen, CUSTOMER_COLOR, customer_rect, border_radius=6)
            
            # Draw customer head with hair
            head_radius = 5
            head_pos = (self.rect.centerx, self.rect.y + 5)
            pygame.draw.circle(screen, (255, 218, 185), head_pos, head_radius)
            
            # Draw hair
            hair_rect = pygame.Rect(self.rect.centerx - 2, self.rect.y + 2, 4, 2)
            pygame.draw.rect(screen, (139, 69, 19), hair_rect, border_radius=1)
            
            # Draw eyes
            pygame.draw.circle(screen, BLACK, (self.rect.centerx - 1, self.rect.y + 4), 1)
            pygame.draw.circle(screen, BLACK, (self.rect.centerx + 1, self.rect.y + 4), 1)
        
        # Draw customer number with beautiful styling
        font = pygame.font.Font(None, 18)
        text = font.render(str(self.id), True, BLACK)
        text_rect = text.get_rect(center=self.rect.center)
        
        # Text background for better visibility
        text_bg = text_rect.inflate(6, 2)
        pygame.draw.rect(screen, WHITE, text_bg, border_radius=2)
        pygame.draw.rect(screen, BLACK, text_bg, 1, border_radius=2)
        
        screen.blit(text, text_rect)

class Button:
    def __init__(self, text, x, y, width, height, color, text_color):
        self.rect = pygame.Rect(x, y, width, height)
        self.text = text
        self.color = color
        self.text_color = text_color
        
        # Ensure text is a string and not empty
        if text is None:
            self.text = "Button"
        elif not isinstance(text, str):
            self.text = str(text)
        
        # Calculate appropriate font size based on button dimensions and text length
        text_length = len(str(self.text))
        if text_length > 15:
            font_size = 24
        elif text_length > 10:
            font_size = 28
        else:
            font_size = 32
        
        # Try to use a system font first, fall back to default if not available
        try:
            # Try to use a common system font that supports more characters
            self.font = pygame.font.SysFont('arial', font_size)
        except:
            # Fall back to default pygame font
            self.font = pygame.font.Font(None, font_size)
            
        # Create text surface with anti-aliasing
        self.text_surf = self.font.render(str(self.text), True, self.text_color)
        self.text_rect = self.text_surf.get_rect(center=self.rect.center)
        self.hover = False

    def update_position(self, x, y):
        """Update button position and recalculate text position"""
        self.rect.centerx = x
        self.rect.centery = y
        self.text_rect = self.text_surf.get_rect(center=self.rect.center)

    def draw(self, screen):
        # Draw button shadow with better depth
        shadow_rect = pygame.Rect(self.rect.x + 4, self.rect.y + 4, self.rect.width, self.rect.height)
        pygame.draw.rect(screen, (40, 40, 40), shadow_rect, border_radius=12)
        
        # Draw main button with gradient effect
        pygame.draw.rect(screen, self.color, self.rect, border_radius=12)
        
        # Add beautiful border with highlight
        pygame.draw.rect(screen, (80, 80, 80), self.rect, 3, border_radius=12)
        pygame.draw.rect(screen, (200, 200, 200), self.rect, 1, border_radius=12)
        
        # Add inner highlight for 3D effect
        highlight_rect = pygame.Rect(self.rect.x + 2, self.rect.y + 2, self.rect.width - 4, self.rect.height // 2)
        highlight_color = tuple(min(255, c + 30) for c in self.color)
        pygame.draw.rect(screen, highlight_color, highlight_rect, border_radius=10)
        
        # Create a fresh text surface each time to avoid any caching issues
        text_surface = self.font.render(str(self.text), True, self.text_color)
        text_rect = text_surface.get_rect(center=self.rect.center)
        
        # Draw the text with a subtle shadow for better readability
        shadow_text = self.font.render(str(self.text), True, (50, 50, 50))
        shadow_rect = text_rect.copy()
        shadow_rect.x += 1
        shadow_rect.y += 1
        screen.blit(shadow_text, shadow_rect)
        
        # Draw the main text
        screen.blit(text_surface, text_rect)

    def is_clicked(self, pos):
        return self.rect.collidepoint(pos)

# --- Enhanced Game GUI Class ---
class GameGUI:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
        pygame.display.set_caption("Restaurant Management Simulation")
        self.clock = pygame.time.Clock()

        self.tables = {}
        self.machines = {}
        self.waiters = {}
        self.customers = {}
        self.waiting_customers = {}

        # Enhanced UI buttons with beautiful styling
        self.start_button = Button(">> Start Restaurant", SCREEN_WIDTH // 2 - 120, SCREEN_HEIGHT // 2, 240, 60, GREEN, WHITE)
        self.add_table_button = Button("+ Add Table", SCREEN_WIDTH - 200, 50, 160, 50, TABLE_COLOR, WHITE)
        self.add_waiter_button = Button("+ Add Waiter", SCREEN_WIDTH - 200, 120, 160, 50, WAITER_COLOR, WHITE)
        self.add_machine_button = Button("+ Add Machine", SCREEN_WIDTH - 200, 190, 160, 50, MACHINE_COLOR, WHITE)

        # State management
        self.adding_entity = False
        self.selected_add_type = None
        self.add_price = 0
        self.add_price_message = ""
        self.confirm_add_button = None
        self.cancel_add_button = None
        self.upgrading_entity = False
        self.upgrade_price_message = ""
        self.confirm_upgrade_button = None
        self.cancel_upgrade_button = None
        self.clean_button = None
        self.upgrade_button = None
        self.selected_entity = None
        self.upgrade_level = None
        self.game_started = False

        # Network connection
        self.erlang_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            self.erlang_socket.connect(('localhost', 8080))
            print("GUI: Successfully connected to server.")
        except socket.error as e:
            print(f"GUI: Failed to connect to server: {e}")
        self.erlang_socket.setblocking(False)
        time.sleep(0.5)

        self.status_message = None
        self.status_message_timer = 0
        self.font = pygame.font.Font(None, 24)
        self.balance = 0
        self.add_popup_message = None
        self.popup_start_time = 0

    def draw_restaurant_background(self):
        # Load and display the restaurant background image
        try:
            # Load the background image
            background_image = pygame.image.load('gui/restaurant_bg.png')
            # Scale the image to fit the screen
            background_image = pygame.transform.scale(background_image, (SCREEN_WIDTH, SCREEN_HEIGHT))
            # Draw the background image
            self.screen.blit(background_image, (0, 0))
        except pygame.error as e:
            print(f"Could not load restaurant background image: {e}")
            # Fallback to the original gradient background if image loading fails
            for y in range(0, SCREEN_HEIGHT, 2):
                color_ratio = y / SCREEN_HEIGHT
                floor_color = (
                    int(245 - color_ratio * 20),
                    int(245 - color_ratio * 20),
                    int(220 - color_ratio * 20)
                )
                pygame.draw.rect(self.screen, floor_color, (0, y, SCREEN_WIDTH, 2))
        
        # Draw elegant wall borders
        wall_thickness = 8
        pygame.draw.rect(self.screen, WALL_COLOR, (0, 0, SCREEN_WIDTH, wall_thickness))
        pygame.draw.rect(self.screen, WALL_COLOR, (0, 0, wall_thickness, SCREEN_HEIGHT))
        pygame.draw.rect(self.screen, WALL_COLOR, (0, SCREEN_HEIGHT - wall_thickness, SCREEN_WIDTH, wall_thickness))
        pygame.draw.rect(self.screen, WALL_COLOR, (SCREEN_WIDTH - wall_thickness, 0, wall_thickness, SCREEN_HEIGHT))
        
        # Draw kitchen area with professional styling - bigger and wider
        kitchen_rect = pygame.Rect(KITCHEN_POS[0] * GRID_SIZE, KITCHEN_POS[1] * GRID_SIZE, 
                                  50 * GRID_SIZE, 12 * GRID_SIZE)
        
        # Kitchen shadow
        shadow_rect = pygame.Rect(kitchen_rect.x + 4, kitchen_rect.y + 4, kitchen_rect.width, kitchen_rect.height)
        pygame.draw.rect(self.screen, (40, 40, 40), shadow_rect, border_radius=8)
        
        # Kitchen base
        pygame.draw.rect(self.screen, KITCHEN_COLOR, kitchen_rect, border_radius=8)
        
        # Kitchen border with highlight
        pygame.draw.rect(self.screen, BLACK, kitchen_rect, 4, border_radius=8)
        pygame.draw.rect(self.screen, (100, 100, 100), kitchen_rect, 2, border_radius=8)
        
        # Kitchen label with professional styling
        font = pygame.font.Font(None, 36)
        kitchen_text = font.render("KITCHEN", True, WHITE)
        text_rect = kitchen_text.get_rect(center=kitchen_rect.center)
        
        # Text background
        text_bg = text_rect.inflate(20, 10)
        pygame.draw.rect(self.screen, BLACK, text_bg, border_radius=5)
        pygame.draw.rect(self.screen, WHITE, text_bg, 2, border_radius=5)
        
        self.screen.blit(kitchen_text, text_rect)
        
        # Draw bar area
        bar_rect = pygame.Rect(BAR_POS[0] * GRID_SIZE, BAR_POS[1] * GRID_SIZE, 
                              8 * GRID_SIZE, 4 * GRID_SIZE)
        
        # Bar shadow
        bar_shadow = pygame.Rect(bar_rect.x + 2, bar_rect.y + 2, bar_rect.width, bar_rect.height)
        pygame.draw.rect(self.screen, (80, 40, 20), bar_shadow, border_radius=4)
        
        # Bar base
        pygame.draw.rect(self.screen, BAR_COLOR, bar_rect, border_radius=4)
        pygame.draw.rect(self.screen, BLACK, bar_rect, 2, border_radius=4)
        
        # Bar label
        bar_text = font.render("BAR", True, WHITE)
        bar_text_rect = bar_text.get_rect(center=bar_rect.center)
        self.screen.blit(bar_text, bar_text_rect)
        
        # Draw entrance area
        entrance_rect = pygame.Rect(ENTRANCE_POS[0] * GRID_SIZE, ENTRANCE_POS[1] * GRID_SIZE, 
                                   6 * GRID_SIZE, 3 * GRID_SIZE)
        
        # Entrance shadow
        entrance_shadow = pygame.Rect(entrance_rect.x + 2, entrance_rect.y + 2, entrance_rect.width, entrance_rect.height)
        pygame.draw.rect(self.screen, (200, 150, 0), entrance_shadow, border_radius=3)
        
        # Entrance base
        pygame.draw.rect(self.screen, ENTRANCE_COLOR, entrance_rect, border_radius=3)
        pygame.draw.rect(self.screen, BLACK, entrance_rect, 2, border_radius=3)
        
        # Entrance label
        entrance_text = font.render("ENTRANCE", True, BLACK)
        entrance_text_rect = entrance_text.get_rect(center=entrance_rect.center)
        self.screen.blit(entrance_text, entrance_text_rect)
        
        # Draw dining area label with elegant styling
        dining_text = font.render("DINING AREA", True, BLACK)
        dining_rect = dining_text.get_rect(topleft=(50, 50))
        
        # Dining area background
        dining_bg = dining_rect.inflate(20, 10)
        pygame.draw.rect(self.screen, WHITE, dining_bg, border_radius=5)
        pygame.draw.rect(self.screen, BLACK, dining_bg, 2, border_radius=5)
        
        self.screen.blit(dining_text, dining_rect)
        
        # Draw decorative elements
        self.draw_decorations()

    def draw_grid(self):
        # Draw beautiful subtle grid lines
        for x in range(0, SCREEN_WIDTH, GRID_SIZE):
            pygame.draw.line(self.screen, (230, 230, 230), (x, 0), (x, SCREEN_HEIGHT), 1)
        for y in range(0, SCREEN_HEIGHT, GRID_SIZE):
            pygame.draw.line(self.screen, (230, 230, 230), (0, y), (SCREEN_WIDTH, y), 1)
        
        # Draw accent lines every 4 grid cells
        for x in range(0, SCREEN_WIDTH, GRID_SIZE * 4):
            pygame.draw.line(self.screen, (200, 200, 200), (x, 0), (x, SCREEN_HEIGHT), 2)
        for y in range(0, SCREEN_HEIGHT, GRID_SIZE * 4):
            pygame.draw.line(self.screen, (200, 200, 200), (0, y), (SCREEN_WIDTH, y), 2)

    def draw_decorations(self):
        # Draw elegant wall decorations
        decoration_color = DECORATION_COLOR
        
        # Wall sconces
        for i in range(5):
            x = 100 + i * 300
            y = 20
            
            # Sconce base
            pygame.draw.circle(self.screen, decoration_color, (x, y), 8)
            pygame.draw.circle(self.screen, BLACK, (x, y), 8, 2)
            
            # Light glow
            for j in range(3):
                glow_radius = 12 + j * 2
                glow_alpha = 60 - j * 20
                glow_surface = pygame.Surface((glow_radius * 2, glow_radius * 2), pygame.SRCALPHA)
                pygame.draw.circle(glow_surface, (255, 255, 200, glow_alpha), (glow_radius, glow_radius), glow_radius)
                self.screen.blit(glow_surface, (x - glow_radius, y - glow_radius))
        
        # Draw elegant corner decorations
        corner_size = 20
        for corner in [(0, 0), (SCREEN_WIDTH - corner_size, 0), (0, SCREEN_HEIGHT - corner_size), (SCREEN_WIDTH - corner_size, SCREEN_HEIGHT - corner_size)]:
            pygame.draw.rect(self.screen, decoration_color, (corner[0], corner[1], corner_size, corner_size))
            pygame.draw.rect(self.screen, BLACK, (corner[0], corner[1], corner_size, corner_size), 2)
        
        # Draw elegant centerpiece decorations in dining area
        center_x = SCREEN_WIDTH // 2
        center_y = SCREEN_HEIGHT // 2
        
        # Center chandelier effect
        for i in range(8):
            angle = i * 45
            radius = 40
            x = center_x + int(radius * math.cos(math.radians(angle)))
            y = center_y + int(radius * math.sin(math.radians(angle)))
            
            pygame.draw.circle(self.screen, (255, 255, 200), (x, y), 3)
            pygame.draw.circle(self.screen, BLACK, (x, y), 3, 1)

    def draw_kitchen_machines(self):
        """Draw cooking machines in the kitchen area with their states"""
        # Calculate kitchen area for machine placement - bigger kitchen
        kitchen_start_x = KITCHEN_POS[0] * GRID_SIZE
        kitchen_start_y = KITCHEN_POS[1] * GRID_SIZE
        kitchen_width = 50 * GRID_SIZE
        kitchen_height = 12 * GRID_SIZE
        
        # Draw kitchen workstations with distinct separation - bigger machines
        workstation_width = 50
        workstation_height = 55
        spacing = 25
        
        # Create 3 rows of machines with distinct separation
        machine_positions = []
        
        # Row 1 - Top row
        for i in range(8):
            x = kitchen_start_x + 50 + (i * (workstation_width + spacing))
            y = kitchen_start_y + 50
            machine_positions.append((x, y))
        
        # Row 2 - Middle row  
        for i in range(8):
            x = kitchen_start_x + 50 + (i * (workstation_width + spacing))
            y = kitchen_start_y + 50 + workstation_height + spacing
            machine_positions.append((x, y))
        
        # Row 3 - Bottom row
        for i in range(8):
            x = kitchen_start_x + 50 + (i * (workstation_width + spacing))
            y = kitchen_start_y + 50 + (workstation_height + spacing) * 2
            machine_positions.append((x, y))
        
        # Draw machines without gray backgrounds
        for i, pos in enumerate(machine_positions):
            machine_id = i + 1
            
            # Check if this machine exists in our machines dict (try both integer and string IDs)
            if machine_id in self.machines:
                # Draw the actual machine with its state - bigger size
                machine = self.machines[machine_id]
                machine_size = int(GRID_SIZE * 1.5)
                # Center the machine in the workstation
                machine_x = pos[0] + (workstation_width - machine_size) // 2
                machine_y = pos[1] + (workstation_height - machine_size) // 2
                machine.rect = pygame.Rect(machine_x, machine_y, machine_size, machine_size)
                machine.draw(self.screen)
            elif f"machine_{machine_id}" in self.machines:
                # Draw the actual machine with string ID - bigger size
                machine = self.machines[f"machine_{machine_id}"]
                machine_size = int(GRID_SIZE * 1.5)
                # Center the machine in the workstation
                machine_x = pos[0] + (workstation_width - machine_size) // 2
                machine_y = pos[1] + (workstation_height - machine_size) // 2
                machine.rect = pygame.Rect(machine_x, machine_y, machine_size, machine_size)
                machine.draw(self.screen)
            else:
                # Draw placeholder for empty machine slot - bigger size
                self.draw_machine_placeholder(pos, machine_id)
    
    def draw_machine_placeholder(self, pos, machine_id):
        """Draw a placeholder for an empty machine slot - bigger size"""
        # Calculate bigger machine size
        machine_size = int(GRID_SIZE * 1.5)
        # Center the placeholder in the workstation
        machine_x = pos[0] + (50 - machine_size) // 2  # 50 is workstation_width
        machine_y = pos[1] + (55 - machine_size) // 2   # 55 is workstation_height
        
        # Machine shadow
        shadow_rect = pygame.Rect(machine_x + 3, machine_y + 3, machine_size, machine_size)
        pygame.draw.rect(self.screen, (60, 60, 60), shadow_rect, border_radius=4)
        
        # Machine base (grayed out)
        machine_rect = pygame.Rect(machine_x + 1, machine_y + 1, machine_size - 2, machine_size - 2)
        pygame.draw.rect(self.screen, (100, 100, 100), machine_rect, border_radius=4)
        
        # Draw "Empty" indicator above the machine object
        font = pygame.font.Font(None, 18)
        text = font.render(f"Empty", True, WHITE)
        text_rect = text.get_rect(center=(machine_x + machine_size//2, machine_y - 15))
        
        # Text background with better styling
        text_bg = text_rect.inflate(8, 4)
        pygame.draw.rect(self.screen, BLACK, text_bg, border_radius=3)
        pygame.draw.rect(self.screen, WHITE, text_bg, 1, border_radius=3)
        
        self.screen.blit(text, text_rect)

    def draw_add_popup(self):
        if self.adding_entity:
            # Semi-transparent overlay
            overlay = pygame.Surface((SCREEN_WIDTH, SCREEN_HEIGHT), pygame.SRCALPHA)
            overlay.fill((0, 0, 0, 150))
            self.screen.blit(overlay, (0, 0))

            # Better positioned popup window - centered and not overlapping buttons
            popup_width = 400
            popup_height = 250
            popup_x = (SCREEN_WIDTH - popup_width) // 2
            popup_y = (SCREEN_HEIGHT - popup_height) // 2
            
            popup_rect = pygame.Rect(popup_x, popup_y, popup_width, popup_height)
            
            # Enhanced popup with better styling
            # Shadow effect
            shadow_rect = pygame.Rect(popup_x + 5, popup_y + 5, popup_width, popup_height)
            pygame.draw.rect(self.screen, (50, 50, 50), shadow_rect, border_radius=20)
            
            # Main popup background with gradient
            pygame.draw.rect(self.screen, (255, 255, 255), popup_rect, border_radius=20)
            
            # Border with highlight
            pygame.draw.rect(self.screen, (100, 100, 100), popup_rect, 3, border_radius=20)
            pygame.draw.rect(self.screen, (200, 200, 200), popup_rect, 1, border_radius=20)

            # Popup title with better styling
            font = pygame.font.Font(None, 40)
            title_text = font.render(f"Add New {self.selected_add_type.title()}", True, (50, 50, 50))
            title_rect = title_text.get_rect(center=(popup_x + popup_width//2, popup_y + 50))
            
            # Title background
            title_bg = title_rect.inflate(20, 10)
            pygame.draw.rect(self.screen, (240, 240, 240), title_bg, border_radius=10)
            pygame.draw.rect(self.screen, (100, 100, 100), title_bg, 2, border_radius=10)
            
            self.screen.blit(title_text, title_rect)

            # Price message with better styling
            if self.add_price_message:
                price_font = pygame.font.Font(None, 32)
                price_surface = price_font.render(self.add_price_message, True, (200, 50, 50))
                price_rect = price_surface.get_rect(center=(popup_x + popup_width//2, popup_y + 120))
                
                # Price background
                price_bg = price_rect.inflate(30, 15)
                pygame.draw.rect(self.screen, (255, 240, 240), price_bg, border_radius=8)
                pygame.draw.rect(self.screen, (200, 50, 50), price_bg, 2, border_radius=8)
                
                self.screen.blit(price_surface, price_rect)

            # Enhanced buttons with better positioning
            if self.confirm_add_button:
                # Update button position to be centered in popup
                self.confirm_add_button.update_position(popup_x + popup_width//2 - 80, popup_y + popup_height - 60)
                self.confirm_add_button.draw(self.screen)
                
            if self.cancel_add_button:
                # Update button position to be centered in popup
                self.cancel_add_button.update_position(popup_x + popup_width//2 + 80, popup_y + popup_height - 60)
                self.cancel_add_button.draw(self.screen)

    def draw_upgrade_popup(self):
        if self.upgrading_entity:
            overlay = pygame.Surface((SCREEN_WIDTH, SCREEN_HEIGHT), pygame.SRCALPHA)
            overlay.fill((0, 0, 0, 150))
            self.screen.blit(overlay, (0, 0))

            # Better positioned upgrade popup - centered
            popup_width = 400
            popup_height = 250
            popup_x = (SCREEN_WIDTH - popup_width) // 2
            popup_y = (SCREEN_HEIGHT - popup_height) // 2
            
            popup_rect = pygame.Rect(popup_x, popup_y, popup_width, popup_height)
            
            # Enhanced popup with better styling
            # Shadow effect
            shadow_rect = pygame.Rect(popup_x + 5, popup_y + 5, popup_width, popup_height)
            pygame.draw.rect(self.screen, (50, 50, 50), shadow_rect, border_radius=20)
            
            # Main popup background
            pygame.draw.rect(self.screen, (255, 255, 255), popup_rect, border_radius=20)
            pygame.draw.rect(self.screen, (100, 100, 100), popup_rect, 3, border_radius=20)
            pygame.draw.rect(self.screen, (200, 200, 200), popup_rect, 1, border_radius=20)

            font = pygame.font.Font(None, 40)
            title_text = font.render("Upgrade Entity", True, (50, 50, 50))
            title_rect = title_text.get_rect(center=(popup_x + popup_width//2, popup_y + 50))
            
            # Title background
            title_bg = title_rect.inflate(20, 10)
            pygame.draw.rect(self.screen, (240, 240, 240), title_bg, border_radius=10)
            pygame.draw.rect(self.screen, (100, 100, 100), title_bg, 2, border_radius=10)
            
            self.screen.blit(title_text, title_rect)

            if self.upgrade_price_message:
                price_font = pygame.font.Font(None, 32)
                price_surface = price_font.render(self.upgrade_price_message, True, (200, 50, 50))
                price_rect = price_surface.get_rect(center=(popup_x + popup_width//2, popup_y + 120))
                
                # Price background
                price_bg = price_rect.inflate(30, 15)
                pygame.draw.rect(self.screen, (255, 240, 240), price_bg, border_radius=8)
                pygame.draw.rect(self.screen, (200, 50, 50), price_bg, 2, border_radius=8)
                
                self.screen.blit(price_surface, price_rect)

            # Enhanced buttons with better positioning
            if self.confirm_upgrade_button:
                self.confirm_upgrade_button.update_position(popup_x + popup_width//2 - 80, popup_y + popup_height - 60)
                self.confirm_upgrade_button.draw(self.screen)
                
            if self.cancel_upgrade_button:
                self.cancel_upgrade_button.update_position(popup_x + popup_width//2 + 80, popup_y + popup_height - 60)
                self.cancel_upgrade_button.draw(self.screen)

    def draw_waiting_customers(self):
        i = 0
        for customer in list(self.waiting_customers.values())[:MAX_QUEUE_DISPLAY]:
            queue_pos = (QUEUE_START_POS[0], QUEUE_START_POS[1] + i)
            customer.rect = pygame.Rect(queue_pos[0] * GRID_SIZE, queue_pos[1] * GRID_SIZE, GRID_SIZE, GRID_SIZE)
            customer.draw(self.screen)
            i += 1
        
        # Enhanced queue counter with beautiful styling
        queue_count = len(self.waiting_customers)
        if queue_count > 0:
            font = pygame.font.Font(None, 32)
            counter_text = f"👥 Waiting Queue: {queue_count}"
            text_surf = font.render(counter_text, True, YELLOW)
            
            # Beautiful background for counter
            counter_bg = pygame.Rect(QUEUE_START_POS[0] * GRID_SIZE - 15, (QUEUE_START_POS[1] - 1) * GRID_SIZE - 8, 
                                    text_surf.get_width() + 30, text_surf.get_height() + 15)
            
            # Shadow effect
            shadow_bg = pygame.Rect(counter_bg.x + 3, counter_bg.y + 3, counter_bg.width, counter_bg.height)
            pygame.draw.rect(self.screen, (100, 100, 0), shadow_bg, border_radius=6)
            
            # Main background with gradient
            pygame.draw.rect(self.screen, BLACK, counter_bg, border_radius=6)
            pygame.draw.rect(self.screen, YELLOW, counter_bg, 3, border_radius=6)
            
            # Gradient effect
            for i in range(2):
                gradient_rect = pygame.Rect(counter_bg.x + i, counter_bg.y + i, 
                                          counter_bg.width - i*2, counter_bg.height - i*2)
                gradient_color = (255, 200 + i*20, 0)
                pygame.draw.rect(self.screen, gradient_color, gradient_rect, border_radius=6)
            
            counter_pos = (QUEUE_START_POS[0] * GRID_SIZE, (QUEUE_START_POS[1] - 1) * GRID_SIZE)
            self.screen.blit(text_surf, counter_pos)

    def draw(self):
        self.draw_restaurant_background()
        self.draw_grid()
        self.draw_add_popup()
        self.draw_upgrade_popup()

        if not self.game_started:
            self.start_button.draw(self.screen)
        else:
            # Draw all entities
            for table in self.tables.values():
                table.draw(self.screen)
            for machine in self.machines.values():
                machine.draw(self.screen)
            for waiter in self.waiters.values():
                waiter.draw(self.screen)
            for customer in self.customers.values():
                customer.draw(self.screen)
            self.draw_waiting_customers()
            
            # Draw cooking machines in kitchen area (on top of background)
            self.draw_kitchen_machines()
            
            # Draw UI buttons
            self.add_table_button.draw(self.screen)
            self.add_waiter_button.draw(self.screen)
            self.add_machine_button.draw(self.screen)

            if self.clean_button:
                self.clean_button.draw(self.screen)
            
            if self.upgrade_button:
                self.upgrade_button.draw(self.screen)
                font = pygame.font.Font(None, 30)
                if self.selected_entity and hasattr(self.selected_entity, 'level'):
                    level_text = font.render(f"Speed Level: {self.selected_entity.level}", True, WHITE)
                    # Position level text below the upgrade button with better styling
                    level_x = self.upgrade_button.rect.centerx
                    level_y = self.upgrade_button.rect.bottom + 15
                    
                    # Create a beautiful background for the level text
                    level_bg = pygame.Rect(level_x - 80, level_y - 8, 160, 25)
                    pygame.draw.rect(self.screen, (50, 50, 50), level_bg, border_radius=6)
                    pygame.draw.rect(self.screen, (100, 100, 100), level_bg, 2, border_radius=6)
                    
                    # Center the text in the background
                    text_rect = level_text.get_rect(center=(level_x, level_y + 4))
                    self.screen.blit(level_text, text_rect)


        
            # Enhanced balance display with beautiful styling in lower left corner
            font = pygame.font.Font(None, 36)
            balance_text = font.render(f"Balance: ${self.balance:,}", True, WHITE)

            # Beautiful background for balance in lower left
            balance_bg = pygame.Rect(15, SCREEN_HEIGHT - 60, balance_text.get_width() + 30, balance_text.get_height() + 15)
            pygame.draw.rect(self.screen, BLACK, balance_bg, border_radius=8)
            pygame.draw.rect(self.screen, GREEN, balance_bg, 3, border_radius=8)

            # Add gradient effect to balance background
            for i in range(3):
                gradient_rect = pygame.Rect(15 + i, SCREEN_HEIGHT - 60 + i, balance_text.get_width() + 30 - i*2, balance_text.get_height() + 15 - i*2)
                gradient_color = (0, 150 + i*20, 0)
                pygame.draw.rect(self.screen, gradient_color, gradient_rect, border_radius=8)

            self.screen.blit(balance_text, (30, SCREEN_HEIGHT - 55))

        if self.confirm_add_button:
            self.confirm_add_button.draw(self.screen)

        if self.cancel_add_button:
            self.cancel_add_button.draw(self.screen)

        # Status messages
        if self.add_popup_message:
            current_time = pygame.time.get_ticks()
            if current_time - self.popup_start_time < 5000:
                # Enhanced status message display
                if self.add_popup_message == "add_approved":
                    msg_text = "Entity added successfully!"
                    msg_color = GREEN
                elif self.add_popup_message == "not_enough_money":
                    msg_text = "Not enough money!"
                    msg_color = RED
                elif self.add_popup_message == "already_exists":
                    msg_text = "⚠️ Entity already exists!"
                    msg_color = YELLOW
                elif self.add_popup_message == "upgrade_approved":
                    msg_text = "✅ Upgrade successful!"
                    msg_color = GREEN
                elif self.add_popup_message == "error:unknown":
                    msg_text = "❌ Unknown error occurred!"
                    msg_color = RED
                else:
                    msg_text = self.add_popup_message
                    msg_color = RED
                
                # Create enhanced message display
                msg_surface = self.font.render(msg_text, True, msg_color)
                msg_rect = msg_surface.get_rect(center=(SCREEN_WIDTH // 2, SCREEN_HEIGHT // 2 + 100))
                
                # Message background with shadow
                msg_bg = msg_rect.inflate(40, 20)
                shadow_bg = pygame.Rect(msg_bg.x + 3, msg_bg.y + 3, msg_bg.width, msg_bg.height)
                pygame.draw.rect(self.screen, (50, 50, 50), shadow_bg, border_radius=10)
                
                # Main background
                pygame.draw.rect(self.screen, WHITE, msg_bg, border_radius=10)
                pygame.draw.rect(self.screen, msg_color, msg_bg, 3, border_radius=10)
                
                self.screen.blit(msg_surface, msg_rect)
            else:
                self.add_popup_message = None

        if self.status_message and self.status_message_timer > pygame.time.get_ticks():
            text_surf = self.font.render(self.status_message, True, WHITE)
            text_rect = text_surf.get_rect(center=(SCREEN_WIDTH // 2, 50))
            
            bg_rect = text_rect.inflate(20, 10)
            pygame.draw.rect(self.screen, RED, bg_rect, border_radius=5)
            pygame.draw.rect(self.screen, WHITE, bg_rect, 2, border_radius=5)
            
            self.screen.blit(text_surf, text_rect)

        pygame.display.flip()

    def send_packet(self, data):
        length = len(data)
        header = length.to_bytes(4, 'big')
        self.erlang_socket.sendall(header + data)

    def send_start_game_message(self):
        print("Sending 'player:start_game' to Erlang...")
        message = "player:start_game".encode('utf-8')
        try:
            self.send_packet(message)
        except socket.error as e:
            print(f"Failed to send start game message: {e}")
            self.display_status_message("Communication error with Erlang server.")

    def send_clean_table_message(self, table_id):
        print(f"Sending clean table request for {table_id}")
        message = f'player:clean_dirty_table:{table_id}'.encode('utf-8')
        try:
            self.send_packet(message)
        except socket.error as e:
            self.display_status_message("Communication error with server.")

    def send_upgrade_message(self, entity_type, entity_id):
        message = f'player:upgrade:{entity_type}:{entity_id}'.encode('utf-8')
        try:
            self.send_packet(message)
        except socket.error as e:
            self.display_status_message(f"Communication error: {e}")

    def send_get_level_and_display(self, entity_type, entity_id):
        try:
            if entity_type == 'waiter':
                message = f'waiter_mng:get_level:{entity_id}'.encode('utf-8')
            elif entity_type == 'machine':
                message = f'machine_mng:get_level:{entity_id}'.encode('utf-8')
            else:
                return
            
            self.send_packet(message)
        except (socket.error, ValueError) as e:
            print(f"Failed to get level: {e}")
            self.upgrade_level = "Error"
    
    def send_get_price_message(self, entity_type, action="add"):
        try:
            message = f'player:get_price:{action}:{entity_type}'.encode('utf-8')
            self.send_packet(message)
        except (socket.error, ValueError) as e:
            self.display_status_message(f"Failed to get price: {e}")
            
    def send_add_entity_message(self, entity_type):
        try:
            message = f'player:add_{entity_type}'.encode('utf-8')
            self.send_packet(message)
        except socket.error as e:
            self.display_status_message(f"Communication error: {e}")
    
    def process_erlang_message(self, message):
        try:
            parts = message.split(':')
            target = parts[0]
            command = parts[1]
            
            if target == 'gui':
                if command == 'add_entity':
                    print("[GUI] Received message add_entity")
                    entity_type = parts[2]
                    entity_id = parts[3]
                    pos_x = int(parts[4])
                    pos_y = int(parts[5])
                    state = parts[6]
                    
                    self.add_entity(entity_type, entity_id, (pos_x, pos_y), {'state': state})

                elif command == 'update_state':
                    print("[GUI] Updating state for", parts)
                    entity_type = parts[2]
                    entity_id = parts[3]
                    state = parts[4]
                    pos_x = int(parts[5])
                    pos_y = int(parts[6])
                    self.update_entity_state(entity_type, entity_id, state, (pos_x, pos_y))

                elif command == 'update_balance':
                    self.balance = int(parts[2])
                    print(f"Balance updated: {self.balance}")
                    
                elif command == 'upgrade_level_response':
                    entity_id = parts[2]
                    level = int(parts[3])
                    if self.selected_entity and self.selected_entity.id == entity_id:
                        self.selected_entity.level = level
                        self.display_status_message(f"{entity_id} upgraded to level {level}")

                elif command == 'show_add_button':
                    entity_type = parts[2]
                    price = int(parts[3])
                    self.selected_add_type = entity_type
                    self.add_price = price
                    self.add_price_message = f"Add {entity_type} will cost {price}$"
                    
                    # Create better positioned buttons with improved colors and text
                    button_text = f"Add {entity_type}"
                    # Use a darker green for better contrast with white text
                    button_color = (0, 150, 0)  # Darker green
                    self.confirm_add_button = Button(button_text, 0, 0, 150, 50, button_color, WHITE)
                    self.cancel_add_button = Button("Cancel", 0, 0, 150, 50, RED, WHITE)

                elif command == "add_approved":
                    self.popup_start_time = pygame.time.get_ticks() 
                    self.add_popup_message = "add_approved"
                    self.confirm_add_button = None
                    self.cancel_add_button = None
                    self.selected_add_type = None
                    self.add_price = 0
                    print("[GUI] Entity added successfully.")

                elif command == "not_enough_money":
                    self.popup_start_time = pygame.time.get_ticks() 
                    self.add_popup_message = "not_enough_money"
                    self.upgrading_entity = False
                    self.confirm_upgrade_button = None
                    self.cancel_upgrade_button = None

                elif command == "already_exists":
                    self.popup_start_time = pygame.time.get_ticks() 
                    self.add_popup_message = "already_exists"

                elif command == "unknown_error":
                    self.popup_start_time = pygame.time.get_ticks() 
                    self.add_popup_message = "error:unknown"
                    self.upgrading_entity = False
                    self.confirm_upgrade_button = None
                    self.cancel_upgrade_button = None

                elif command == 'show_upgrade_button':
                    entity_type = parts[2]
                    price = int(parts[3])
                    self.upgrading_entity = True
                    self.upgrade_price_message = f"Upgrade {entity_type} will cost {price}$"
                    
                    # Create better positioned buttons with improved colors and text
                    # Use a darker blue for better contrast with white text
                    button_color = (0, 100, 200)  # Darker blue
                    self.confirm_upgrade_button = Button("Confirm Upgrade", 0, 0, 150, 50, button_color, WHITE)
                    self.cancel_upgrade_button = Button("Cancel", 0, 0, 150, 50, RED, WHITE)

                elif command == "upgrade_approved":
                    self.popup_start_time = pygame.time.get_ticks()
                    self.add_popup_message = "upgrade_approved"
                    self.upgrading_entity = False
                    self.confirm_upgrade_button = None
                    self.cancel_upgrade_button = None
                    if self.selected_entity:
                        self.selected_entity.level += 1
                    print("[GUI] Upgrade approved.")

                elif command == "customer_cancelled":
                    customer_id = parts[2]
                    if customer_id in self.waiting_customers:
                        del self.waiting_customers[customer_id]
                        print(f"[GUI] Customer {customer_id} removed from waiting queue")
                    if customer_id in self.customers:
                        del self.customers[customer_id]
                        print(f"[GUI] Customer {customer_id} removed from customers list")
                    self.display_status_message(f"Customer {customer_id} cancelled and left")

                else:
                    print(f"[GUI] Unknown message received: {message}")
                    
        except (IndexError, ValueError) as e:
            print(f"Error parsing message '{message}': {e}")

    def clear_add_buttons(self):
        self.confirm_add_button = None
        self.cancel_add_button = None
            
    def update_entity_state(self, entity_type, entity_id, state, pos=None):
        print(f"State received: [{state}] for {entity_id}")
        if entity_type == 'table':
            if entity_id in self.tables:
                table = self.tables[entity_id]
                if state == 'idle' or state == 'taken':
                    table.is_dirty = False
                elif state == 'dirty':
                    table.is_dirty = True

        elif entity_type == 'waiter':
            if entity_id in self.waiters:
                waiter = self.waiters[entity_id]
                if pos:
                    waiter.pos = pos
                    waiter.rect = pygame.Rect(pos[0] * GRID_SIZE, pos[1] * GRID_SIZE, GRID_SIZE, GRID_SIZE)
                waiter.state = state
                if state == 'serving' or state == 'busy':
                    waiter.color = GREEN
                else:
                    waiter.color = WAITER_COLOR

        elif entity_type == 'machine':
            if entity_id in self.machines:
                machine = self.machines[entity_id]
                machine.state = state
                if state == 'idle':
                    machine.color = MACHINE_COLOR
                if state == 'cooking' or state == 'busy':
                    machine.color = RED

        elif entity_type == 'customer':
            customer = self.customers.get(entity_id)
            if customer:
                if pos:
                    customer.pos = pos
                    customer.rect = pygame.Rect(pos[0] * GRID_SIZE, pos[1] * GRID_SIZE, GRID_SIZE, GRID_SIZE)
                customer.state = state
                if state == 'seated':
                    self.waiting_customers.pop(entity_id, None)

    def add_entity(self, entity_type, entity_id, pos, state_info={}):
        if entity_type == 'table':
            new_table = Table(entity_id, pos)
            new_table.is_dirty = state_info.get('is_dirty', False)
            self.tables[new_table.id] = new_table
        elif entity_type == 'waiter':
            new_waiter = Waiter(entity_id, pos)
            self.waiters[new_waiter.id] = new_waiter
        elif entity_type == 'machine':
            new_machine = Machine(entity_id, pos)
            new_machine.is_busy = state_info.get('is_busy', False)
            self.machines[new_machine.id] = new_machine
        elif entity_type == 'customer':
            state = state_info.get('state', 'idle')
            new_customer = Customer(entity_id, pos, state)
            self.waiting_customers[entity_id] = new_customer
            self.customers[new_customer.id] = new_customer

    def run(self):
        while True:
            self.handle_events()
            self.check_for_updates()
            self.draw()
            self.clock.tick(60)

    def check_for_updates(self):
        try:
            header = self.erlang_socket.recv(4)
            if not header or len(header) < 4:
                return

            message_length = int.from_bytes(header, byteorder='big')

            data = b""
            while len(data) < message_length:
                chunk = self.erlang_socket.recv(message_length - len(data))
                if not chunk:
                    break
                data += chunk

            if len(data) == message_length:
                message = data.decode('utf-8')
                print(f"[GUI] Received message from Erlang: {message}")
                self.process_erlang_message(message)
        except socket.error:
            pass
            
    def display_status_message(self, message):
        self.status_message = message
        self.status_message_timer = pygame.time.get_ticks() + 3000

    def quit_game(self):
        pygame.quit()
        sys.exit()

    def handle_events(self):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                self.quit_game()

            if event.type == pygame.MOUSEBUTTONDOWN:
                pos = event.pos

                if not self.game_started:
                    if self.start_button.is_clicked(pos):
                        print("Start restaurant button clicked!")
                        self.game_started = True
                        self.send_start_game_message()

                else:
                    # Handle popup windows first
                    if self.adding_entity:
                        if self.confirm_add_button and self.confirm_add_button.is_clicked(pos):
                            print(f"Confirm clicked: Adding {self.selected_add_type}")
                            message = f"player:add:{self.selected_add_type}"
                            self.send_packet(message.encode('utf-8'))

                            self.adding_entity = False
                            self.selected_add_type = None
                            self.add_price = 0
                            self.add_price_message = ""
                            self.confirm_add_button = None
                            self.cancel_add_button = None

                        elif self.cancel_add_button and self.cancel_add_button.is_clicked(pos):
                            print("Cancel clicked")
                            self.adding_entity = False
                            self.selected_add_type = None
                            self.add_price = 0
                            self.add_price_message = ""
                            self.confirm_add_button = None
                            self.cancel_add_button = None

                    elif self.upgrading_entity:
                        if self.confirm_upgrade_button and self.confirm_upgrade_button.is_clicked(pos):
                            print(f"Confirm upgrade clicked for {self.selected_entity.type}:{self.selected_entity.id}")
                            self.send_upgrade_message(self.selected_entity.type, self.selected_entity.id)
                            self.upgrading_entity = False
                            self.confirm_upgrade_button = None
                            self.cancel_upgrade_button = None
                            self.upgrade_price_message = ""

                        elif self.cancel_upgrade_button and self.cancel_upgrade_button.is_clicked(pos):
                            print("Upgrade cancelled")
                            self.upgrading_entity = False
                            self.confirm_upgrade_button = None
                            self.cancel_upgrade_button = None
                            self.upgrade_price_message = ""

                    else:
                        # Handle game interactions
                        self.handle_game_click(pos)

                        # Handle UI buttons
                        if self.add_table_button.is_clicked(pos):
                            print("Add Table button clicked")
                            self.selected_add_type = 'table'
                            self.adding_entity = True
                            self.send_get_price_message('table')

                        elif self.add_waiter_button.is_clicked(pos):
                            print("Add Waiter button clicked")
                            self.selected_add_type = 'waiter'
                            self.adding_entity = True
                            self.send_get_price_message('waiter')

                        elif self.add_machine_button.is_clicked(pos):
                            print("Add Machine button clicked")
                            self.selected_add_type = 'machine'
                            self.adding_entity = True
                            self.send_get_price_message('machine')




    def handle_game_click(self, pos):
        # Check if clicked on clean button
        if self.clean_button and self.clean_button.is_clicked(pos):
            print(f"Player cleaning table {self.selected_entity.id}")
            self.send_clean_table_message(self.selected_entity.id)
            self.clean_button = None
            return

        # Check if clicked on upgrade button
        if self.upgrade_button and self.upgrade_button.is_clicked(pos):
            print(f"Requesting upgrade price for {self.selected_entity.type}")
            self.send_get_price_message(self.selected_entity.type, "upgrade")
            self.selected_add_type = self.selected_entity.type
            self.upgrade_button = None
            return

        # Check if clicked on entity
        for entity in list(self.tables.values()) + list(self.machines.values()) + list(self.waiters.values()):
            if entity.rect.collidepoint(pos):
                self.selected_entity = entity

                # Create clean button only if table is dirty
                if entity.type == 'table' and entity.is_dirty:
                    # Create a better clean button with improved styling
                    clean_text = "Clean Table"
                    # Use a beautiful green color for clean buttons
                    button_color = (34, 139, 34)  # Forest green
                    # Position the button in a better location - left side of screen
                    button_x = 50
                    button_y = 200
                    self.clean_button = Button(clean_text, button_x, button_y, 180, 60, button_color, WHITE)

                # Create upgrade button only for waiter/machine
                elif entity.type in ('waiter', 'machine'):
                    # Create a better upgrade button with improved text and colors
                    upgrade_text = f"Upgrade {entity.type.title()}"
                    # Use a beautiful gold color for upgrade buttons
                    button_color = (218, 165, 32)  # Golden rod
                    # Position the button in a better location - right side of screen
                    button_x = SCREEN_WIDTH - 200
                    button_y = 260
                    self.upgrade_button = Button(upgrade_text, button_x, button_y, 180, 60, button_color, BLACK)
                    self.send_get_level_and_display(entity.type, entity.id)

                return

        # If nothing clicked - reset
        self.selected_entity = None
        self.clean_button = None
        self.upgrade_button = None
        self.upgrade_level = None



    def handle_add_buttons(self, pos):
        if not self.adding_entity:
            return

        if self.confirm_add_button and self.confirm_add_button.is_clicked(pos):
            print(f"Confirm clicked: Adding {self.selected_add_type}")
            response = self.send_packet(f"player:add:{self.selected_add_type}")

            if response == "add_approved":
                print(f"{self.selected_add_type.capitalize()} added successfully!")
                self.adding_entity = False
                self.selected_add_type = None
                self.confirm_add_button = None
                self.cancel_add_button = None
                self.add_price_message = ""
            elif response == "no_funds":
                print("Not enough money to add.")
                self.add_price_message = "Not enough money"
            elif response == "already_exists":
                print("Entity already exists.")
                self.add_price_message = "Already exists"
            else:
                print("Unknown error occurred.")
                self.add_price_message = "Unknown error"

        elif self.cancel_add_button and self.cancel_add_button.is_clicked(pos):
            print("Cancel clicked")
            self.adding_entity = False
            self.selected_add_type = None
            self.confirm_add_button = None
            self.cancel_add_button = None
            self.add_price_message = ""


            
    def send_packet(self, data):
        length = len(data)
        header = length.to_bytes(4, 'big')
        self.erlang_socket.sendall(header + data)

    def send_start_game_message(self):
        print("Sending 'player:start_game' to Erlang...")
        message = "player:start_game".encode('utf-8')
        try:
            self.send_packet(message)
        except socket.error as e:
            print(f"Failed to send start game message: {e}")
            self.display_status_message("Communication error with Erlang server.")

    def send_clean_table_message(self, table_id):
        print(f"Sending clean table request for {table_id}")
        message = f'player:clean_dirty_table:{table_id}'.encode('utf-8')
        try:
            self.send_packet(message)
        except socket.error as e:
            self.display_status_message("Communication error with server.")

    def send_upgrade_message(self, entity_type, entity_id):
        message = f'player:upgrade:{entity_type}:{entity_id}'.encode('utf-8')
        try:
            self.send_packet(message)
        except socket.error as e:
            self.display_status_message(f"Communication error: {e}")

    def send_get_level_and_display(self, entity_type, entity_id):
        try:
            if entity_type == 'waiter':
                message = f'waiter_mng:get_level:{entity_id}'.encode('utf-8')
            elif entity_type == 'machine':
                message = f'machine_mng:get_level:{entity_id}'.encode('utf-8')
            else:
                return
            
            self.send_packet(message)
        except (socket.error, ValueError) as e:
            print(f"Failed to get level: {e}")
            self.upgrade_level = "Error"
    
    def send_get_price_message(self, entity_type, action="add"):
        try:
            message = f'player:get_price:{action}:{entity_type}'.encode('utf-8')
            self.send_packet(message)
        except (socket.error, ValueError) as e:
            self.display_status_message(f"Failed to get price: {e}")

            
    def send_add_entity_message(self, entity_type):
        try:
            message = f'player:add_{entity_type}'.encode('utf-8')
            self.send_packet(message)
        except socket.error as e:
            self.display_status_message(f"Communication error: {e}")
    
    def process_erlang_message(self, message):
        try:
            parts = message.split(':')
            target = parts[0]
            command = parts[1]
            
            if target == 'gui':
                if command == 'add_entity':
                    print("[GUI] Received message add_entity")
                    entity_type = parts[2]
                    entity_id = parts[3]
                    pos_x = int(parts[4])
                    pos_y = int(parts[5])
                    state = parts[6]
                    
                    self.add_entity(entity_type, entity_id, (pos_x, pos_y), {'state': state})

                elif command == 'update_state':
                    print("[GUI] Updating state for", parts)
                    entity_type = parts[2]
                    entity_id = parts[3]
                    state = parts[4]
                    pos_x = int(parts[5])
                    pos_y = int(parts[6])
                    self.update_entity_state(entity_type, entity_id, state, (pos_x, pos_y))

                elif command == 'update_balance':
                    self.balance = int(parts[2])
                    print(f"Balance updated: {self.balance}")
                    
                elif command == 'upgrade_level_response':
                    entity_id = parts[2]
                    level = int(parts[3])
                    # Update the selected entity's level
                    if self.selected_entity and self.selected_entity.id == entity_id:
                        self.selected_entity.level = level
                        self.display_status_message(f"{entity_id} upgraded to level {level}")

                elif command == 'show_add_button':
                    entity_type = parts[2]
                    price = int(parts[3])
                    self.selected_add_type = entity_type
                    self.add_price = price
                    self.add_price_message = f"Add {entity_type} will cost {price}$"
                    
                    # Create better positioned buttons with improved colors and text
                    button_text = f"Add {entity_type}"
                    # Use a darker green for better contrast with white text
                    button_color = (0, 150, 0)  # Darker green
                    self.confirm_add_button = Button(button_text, 0, 0, 150, 50, button_color, WHITE)
                    self.cancel_add_button = Button("Cancel", 0, 0, 150, 50, RED, WHITE)

                elif command == 'show_upgrade_button':
                    entity_type = parts[2]
                    price = int(parts[3])
                    self.upgrading_entity = True
                    self.upgrade_price_message = f"Upgrade {entity_type} will cost {price}$"
                    
                    # Create better positioned buttons with improved colors and text
                    # Use a darker blue for better contrast with white text
                    button_color = (0, 100, 200)  # Darker blue
                    self.confirm_upgrade_button = Button("Confirm Upgrade", 0, 0, 150, 50, button_color, WHITE)
                    self.cancel_upgrade_button = Button("Cancel", 0, 0, 150, 50, RED, WHITE)

                elif command == "add_approved":
                    self.popup_start_time = pygame.time.get_ticks() 
                    self.add_popup_message = "add_approved"
                    self.confirm_add_button = None
                    self.cancel_add_button = None
                    self.selected_add_type = None
                    self.add_price = 0
                    print("[GUI] Entity added successfully.")

                elif command == "not_enough_money":
                    self.popup_start_time = pygame.time.get_ticks() 
                    self.add_popup_message = "not_enough_money"
                    self.upgrading_entity = False
                    self.confirm_upgrade_button = None
                    self.cancel_upgrade_button = None

                elif command == "already_exists":
                    self.popup_start_time = pygame.time.get_ticks() 
                    self.add_popup_message = "already_exists"

                elif command == "unknown_error":
                    self.popup_start_time = pygame.time.get_ticks() 
                    self.add_popup_message = "error:unknown"
                    self.upgrading_entity = False
                    self.confirm_upgrade_button = None
                    self.cancel_upgrade_button = None

                elif command == "upgrade_approved":
                    self.popup_start_time = pygame.time.get_ticks()
                    self.add_popup_message = "upgrade_approved"
                    self.upgrading_entity = False
                    self.confirm_upgrade_button = None
                    self.cancel_upgrade_button = None
                    if self.selected_entity:
                        self.selected_entity.level += 1
                    print("[GUI] Upgrade approved.")

                elif command == "customer_cancelled":
                    customer_id = parts[2]
                    # Remove customer from waiting queue and customers list
                    if customer_id in self.waiting_customers:
                        del self.waiting_customers[customer_id]
                        print(f"[GUI] Customer {customer_id} removed from waiting queue")
                    if customer_id in self.customers:
                        del self.customers[customer_id]
                        print(f"[GUI] Customer {customer_id} removed from customers list")
                    # Display status message
                    self.display_status_message(f"Customer {customer_id} cancelled and left")

                # --- Additional messages can be added here as needed ---
                else:
                    print(f"[GUI] Unknown message received: {message}")
                    
        except (IndexError, ValueError) as e:
            print(f"Error parsing message '{message}': {e}")


    def clear_add_buttons(self):
        self.confirm_add_button = None
        self.cancel_add_button = None

            
    def update_entity_state(self, entity_type, entity_id, state, pos=None):
        print(f"State received: [{state}] for {entity_id}")
        if entity_type == 'table':
            if entity_id in self.tables:
                table = self.tables[entity_id]
                if state == 'idle' or state == 'taken':
                    table.is_dirty = False
                elif state == 'dirty':
                    table.is_dirty = True

        elif entity_type == 'waiter':
            if entity_id in self.waiters:
                waiter = self.waiters[entity_id]
                if pos:
                    waiter.pos = pos
                    waiter.rect = pygame.Rect(pos[0] * GRID_SIZE, pos[1] * GRID_SIZE, GRID_SIZE, GRID_SIZE)
                waiter.state = state
                if state == 'serving' or state == 'busy':
                    waiter.color = GREEN
                else:
                    waiter.color = WAITER_COLOR

        elif entity_type == 'machine':
            if entity_id in self.machines:
                
                machine = self.machines[entity_id]
                machine.state = state
                if state == 'idle':
                    machine.color = MACHINE_COLOR
                if state == 'cooking' or state == 'busy':
                    machine.color = RED

        elif entity_type == 'customer':
            customer = self.customers.get(entity_id)
            if customer:
                if pos:
                    customer.pos = pos
                    customer.rect = pygame.Rect(pos[0] * GRID_SIZE, pos[1] * GRID_SIZE, GRID_SIZE, GRID_SIZE)
                customer.state = state
                if state == 'seated':
                    self.waiting_customers.pop(entity_id, None)


    def add_entity(self, entity_type, entity_id, pos, state_info={}):
        if entity_type == 'table':
            new_table = Table(entity_id, pos)
            new_table.is_dirty = state_info.get('is_dirty', False)
            self.tables[new_table.id] = new_table
        elif entity_type == 'waiter':
            new_waiter = Waiter(entity_id, pos)
            self.waiters[new_waiter.id] = new_waiter
        elif entity_type == 'machine':
            new_machine = Machine(entity_id, pos)
            new_machine.is_busy = state_info.get('is_busy', False)
            self.machines[new_machine.id] = new_machine
        elif entity_type == 'customer':
            state = state_info.get('state', 'idle')
            new_customer = Customer(entity_id, pos, state)  # ⬅ Pass state as needed
            self.waiting_customers[entity_id] = new_customer
            self.customers[new_customer.id] = new_customer


    def run(self):
        while True:
            self.handle_events()
            self.check_for_updates() # New function to check for updates
            self.draw()
            self.clock.tick(60)

    def check_for_updates(self):
        try:
            # First read the header (4 bytes) to know the message length
            header = self.erlang_socket.recv(4)
            if not header or len(header) < 4:
                return  # No complete message yet

            message_length = int.from_bytes(header, byteorder='big')

            # Now read the message itself (body)
            data = b""
            while len(data) < message_length:
                chunk = self.erlang_socket.recv(message_length - len(data))
                if not chunk:
                    break
                data += chunk

            if len(data) == message_length:
                message = data.decode('utf-8')
                print(f"[GUI] Received message from Erlang: {message}")
                self.process_erlang_message(message)
        except socket.error:
            # No information currently - completely fine
            pass

            
    def display_status_message(self, message):
        self.status_message = message
        self.status_message_timer = pygame.time.get_ticks() + 3000

    def quit_game(self):
        pygame.quit()
        sys.exit()


if __name__ == "__main__":
    game = GameGUI()
    game.run()