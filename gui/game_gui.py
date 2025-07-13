import pygame
import sys
import random
import socket
import select
import time

# --- הגדרות בסיסיות ---
SCREEN_WIDTH = 1400
SCREEN_HEIGHT = 900
GRID_SIZE = 20
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
GRAY = (150, 150, 150)
GREEN = (0, 200, 0)
RED = (255, 0, 0)
BLUE = (0, 0, 200)
YELLOW = (255, 255, 0)
BROWN = (101, 67, 33)
LIGHT_GRAY = (200, 200, 200)


# --- קבועים של המשחק ---
MAX_TABLES = 120
MAX_MACHINES = 25
MAX_WAITERS = 50
MAX_QUEUE_DISPLAY = 5
KITCHEN_POS = (4, 8) 
QUEUE_START_POS = (1, 0)

# --- קלאסים לייצוג הישויות ---
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
        super().__init__(table_id, 'table', pos, BROWN)
        self.is_dirty = False

    def draw(self, screen):
        color = RED if self.is_dirty else BROWN
        pygame.draw.rect(screen, color, self.rect)
        font = pygame.font.Font(None, 24)
        text = font.render(str(self.id), True, WHITE)
        screen.blit(text, (self.rect.x, self.rect.y))

class Machine(Entity):
    def __init__(self, machine_id, pos,state='idle'):
        super().__init__(machine_id, 'machine', pos, GRAY)
        self.is_busy = False
        self.level = 0
        self.state = state

    def draw(self, screen):
        pygame.draw.rect(screen, self.color, self.rect)
        font = pygame.font.Font(None, 24)
        text = font.render(str(self.id), True, BLACK)
        screen.blit(text, (self.rect.x, self.rect.y))

class Waiter(Entity):
    def __init__(self, waiter_id, pos,state='idle'):
        super().__init__(waiter_id, 'waiter', pos, BLUE)
        self.level = 0
        self.state = state

    def draw(self, screen):
        pygame.draw.rect(screen, self.color, self.rect)
        font = pygame.font.Font(None, 24)
        text = font.render(str(self.id), True, WHITE)
        screen.blit(text, (self.rect.x, self.rect.y))

class Customer(Entity):
    def __init__(self, customer_id, pos, state='idle'):
        super().__init__(customer_id, 'customer', pos, GREEN)
        self.state = state

    def draw(self, screen):
        if self.state == 'seated':
            color = BLUE
            pygame.draw.rect(screen, color, self.rect)
        else:
            color = GREEN
            pygame.draw.circle(screen, color, self.rect.center, GRID_SIZE // 2)
        font = pygame.font.Font(None, 24)
        text = font.render(str(self.id), True, WHITE)
        text_rect = text.get_rect(center=self.rect.center)
        screen.blit(text, text_rect)


class Button:
    def __init__(self, text, x, y, width, height, color, text_color):
        self.rect = pygame.Rect(x, y, width, height)
        self.text = text
        self.color = color
        self.text_color = text_color
        self.font = pygame.font.Font(None, 36)
        self.text_surf = self.font.render(self.text, True, self.text_color)
        self.text_rect = self.text_surf.get_rect(center=self.rect.center)

    def draw(self, screen):
        pygame.draw.rect(screen, self.color, self.rect, border_radius=10)
        screen.blit(self.text_surf, self.text_rect)

    def is_clicked(self, pos):
        return self.rect.collidepoint(pos)

# --- הקלאס הראשי של המשחק ---
class GameGUI:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
        pygame.display.set_caption("SAFE NODE Simulation")
        self.clock = pygame.time.Clock()

        self.tables = {}
        self.machines = {}
        self.waiters = {}
        self.customers = {}
        self.waiting_customers = {}  # לקוחות שממתינים בתור


        self.start_button = Button("Start Game", SCREEN_WIDTH // 2 - 100, SCREEN_HEIGHT // 2, 200, 50, BLUE, WHITE)
        self.add_table_button = Button("Add Table", SCREEN_WIDTH - 200, 50, 150, 50, BLUE, WHITE)
        self.add_waiter_button = Button("Add Waiter", SCREEN_WIDTH - 200, 120, 150, 50, BLUE, WHITE)
        self.add_machine_button = Button("Add Machine", SCREEN_WIDTH - 200, 190, 150, 50, BLUE, WHITE)

        # מצב חלון הוספה
        self.adding_entity = False
        self.selected_add_type = None        # סוג הישות שנבחרה להוספה (למשל "table" או "waiter")
        self.add_price = 0                   # מחיר שהתקבל מהשרת
        self.add_price_message = ""          # הודעת המחיר לתצוגה

        # כפתורי אישור וביטול
        self.confirm_add_button = None       # כפתור "הוסף"
        self.cancel_add_button = None        # כפתור "בטל"



        
        self.clean_button = None
        self.upgrade_button = None
        self.add_button = None
        
        self.selected_entity = None
        self.upgrade_level = None
        self.selected_add_type = None
        self.add_price = None

        self.game_started = False
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



    def draw_add_popup(self):
        if self.adding_entity:
            # רקע שקוף מאחורי הפופאפ
            overlay = pygame.Surface((SCREEN_WIDTH, SCREEN_HEIGHT), pygame.SRCALPHA)
            overlay.fill((0, 0, 0, 150))  # שחור שקוף
            self.screen.blit(overlay, (0, 0))

            # חלון הפופאפ
            pygame.draw.rect(self.screen, LIGHT_GRAY, (SCREEN_WIDTH - 400, 200, 350, 150), border_radius=10)

            # כפתור אישור
            if self.confirm_add_button:
                self.confirm_add_button.draw(self.screen)

            # כפתור ביטול
            if self.cancel_add_button:
                self.cancel_add_button.draw(self.screen)

            # הודעת מחיר
            if self.add_price_message:
                font = pygame.font.Font(None, 28)
                text_surface = font.render(self.add_price_message, True, RED)
                text_rect = text_surface.get_rect(center=(SCREEN_WIDTH - 225, 330))
                self.screen.blit(text_surface, text_rect)




    def draw_waiting_customers(self):
        i = 0
        for customer in list(self.waiting_customers.values())[:MAX_QUEUE_DISPLAY]:
            queue_pos = (QUEUE_START_POS[0], QUEUE_START_POS[1] + i)
            customer.rect = pygame.Rect(queue_pos[0] * GRID_SIZE, queue_pos[1] * GRID_SIZE, GRID_SIZE, GRID_SIZE)
            customer.draw(self.screen)
            i += 1


    def draw_grid(self):
        for x in range(0, SCREEN_WIDTH, GRID_SIZE):
            pygame.draw.line(self.screen, GRAY, (x, 0), (x, SCREEN_HEIGHT))
        for y in range(0, SCREEN_HEIGHT, GRID_SIZE):
            pygame.draw.line(self.screen, GRAY, (0, y), (SCREEN_WIDTH, y))

    def draw(self):
        self.screen.fill(BLACK)
        self.draw_grid()
        self.draw_add_popup()


        if not self.game_started:
            self.start_button.draw(self.screen)
        else:
            for table in self.tables.values():
                table.draw(self.screen)
            for machine in self.machines.values():
                machine.draw(self.screen)
            for waiter in self.waiters.values():
                waiter.draw(self.screen)
            for customer in self.customers.values():
                customer.draw(self.screen)
            self.draw_waiting_customers()
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
                    self.screen.blit(level_text, (self.upgrade_button.rect.x, self.upgrade_button.rect.y + 60))


            if self.add_button:
                self.add_button.draw(self.screen)
        
            font = pygame.font.Font(None, 36)
            balance_text = font.render(f"Balance: {self.balance}$", True, WHITE)
            self.screen.blit(balance_text, (10, 10))

        if self.status_message and self.status_message_timer > pygame.time.get_ticks():
            text_surf = self.font.render(self.status_message, True, WHITE)
            text_rect = text_surf.get_rect(center=(SCREEN_WIDTH // 2, 50))
            self.screen.blit(text_surf, text_rect)


        if self.confirm_add_button:
            self.confirm_add_button.draw(self.screen)

        if self.cancel_add_button:
            self.cancel_add_button.draw(self.screen)

        # בדיקה אם יש הודעה, והאם עברו פחות מ־5 שניות מאז הופיעה
        if self.add_popup_message:
            current_time = pygame.time.get_ticks()
            if current_time - self.popup_start_time < 5000:
                msg_surface = self.font.render(self.add_popup_message, True, RED)
                self.screen.blit(msg_surface, (SCREEN_WIDTH - 380, 330))
            else:
                self.add_popup_message = None  # מחיקת ההודעה אחרי 5 שניות


        
        pygame.display.flip()

    def handle_events(self):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                self.quit_game()

            if event.type == pygame.MOUSEBUTTONDOWN:
                pos = event.pos

                if not self.game_started:
                    if self.start_button.is_clicked(pos):
                        print("Start game button clicked!")
                        self.game_started = True
                        self.send_start_game_message()

                else:
                    # ==========================
                    # 1. חלון פתוח - אישור/ביטול
                    # ==========================
                    if self.adding_entity:
                        if self.confirm_add_button and self.confirm_add_button.is_clicked(pos):
                            print(f"Confirm clicked: Adding {self.selected_add_type}")
                            message = f"player:add:{self.selected_add_type}"
                            self.send_packet(message.encode('utf-8'))

                            # איפוס מצב הפופאפ
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

                    # ==========================
                    # 2. כפתורי שדרוג/ניקוי וישויות
                    # ==========================
                    else:
                        self.handle_game_click(pos)

                        # ==========================
                        # 3. לחיצה על כפתורי ההוספה
                        # ==========================
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
        # 1. בדיקה אם לחצו על כפתור ניקוי
        if self.clean_button and self.clean_button.is_clicked(pos):
            print(f"Player cleaning table {self.selected_entity.id}")
            self.send_clean_table_message(self.selected_entity.id)
            self.clean_button = None
            return

        # 2. בדיקה אם לחצו על כפתור שדרוג
        if self.upgrade_button and self.upgrade_button.is_clicked(pos):
            print(f"Requesting upgrade price for {self.selected_entity.type}")
            self.send_get_price_message(self.selected_entity.type, "upgrade")
            # נחכה לתשובה מהשרת ואז נחליט אם לבצע שדרוג
            self.selected_add_type = self.selected_entity.type  # נשמור סוג לרגע ההחלטה
            self.add_button = Button("Confirm Upgrade", 200, 100, 180, 50, YELLOW, BLACK)
            self.upgrade_button = None  # נסתיר את כפתור השדרוג הרגיל
            return


        # 3. בדיקה אם לחצו על ישות
        for entity in list(self.tables.values()) + list(self.machines.values()) + list(self.waiters.values()):
            if entity.rect.collidepoint(pos):
                self.selected_entity = entity

                # יוצרים כפתור ניקוי רק אם השולחן מלוכלך
                if entity.type == 'table' and entity.is_dirty:
                    self.clean_button = Button("Clean Table", 200, 260, 150, 50, GREEN, WHITE)

                # יוצרים כפתור שדרוג רק אם זו מכונה/מלצר
                elif entity.type in ('waiter', 'machine'):
                    self.upgrade_button = Button(f"Upgrade {entity.type}", 10, 10, 150, 50, YELLOW, BLACK)
                    self.send_get_level_and_display(entity.type, entity.id)

                return  # סיימנו את הטיפול בלחיצה על ישות

        # 4. אם לא לחצו על כלום – איפוס
        self.selected_entity = None
        self.clean_button = None
        self.upgrade_button = None
        self.upgrade_level = None
        self.add_button = None



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
                self.add_price_message = "לא מספיק כסף"
            elif response == "already_exists":
                print("Entity already exists.")
                self.add_price_message = "כבר קיים"
            else:
                print("Unknown error occurred.")
                self.add_price_message = "שגיאה לא צפויה"

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
                    self.confirm_add_button = Button(f"Add {entity_type}", SCREEN_WIDTH - 200, 260, 150, 50, GREEN, BLACK)
                    self.cancel_add_button = Button("Cancel", SCREEN_WIDTH - 370, 260, 150, 50, RED, BLACK)

                # --- תשובה על ניסיון הוספה ---
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

                elif command == "already_exists":
                    self.popup_start_time = pygame.time.get_ticks() 
                    self.add_popup_message = "already_exists"

                elif command == "unknown_error":
                    self.popup_start_time = pygame.time.get_ticks() 
                    self.add_popup_message = "error:unknown"

                # --- אפשר להוסיף כאן הודעות אחרות לפי צורך ---
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
                    waiter.color = BLUE

        elif entity_type == 'machine':
            if entity_id in self.machines:
                
                machine = self.machines[entity_id]
                machine.state = state
                if state == 'idle':
                    machine.color = GRAY
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
            new_customer = Customer(entity_id, pos, state)  # ⬅ מעבירים את state כמו שצריך
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
            # קודם כל נקרא את ה-header (4 בתים) כדי לדעת את אורך ההודעה
            header = self.erlang_socket.recv(4)
            if not header or len(header) < 4:
                return  # אין הודעה מלאה עדיין

            message_length = int.from_bytes(header, byteorder='big')

            # עכשיו נקרא את ההודעה עצמה (body)
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
            # אין מידע כרגע - סבבה לגמרי
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