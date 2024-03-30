# import keyboard
# import time

# class keyboardInput:
#     def __init__(self):
#         self.stop = False

#     def collectInput(self):

#         # Hotkey method seems like it accepts input slightly slower,
#         # So if the below version accepts too fast, we could use hotkeys
#         # keyboard.add_hotkey('a', lambda: print('a'))
#         # keyboard.add_hotkey('w', lambda: print('w'))
#         # keyboard.add_hotkey('s', lambda: print('s'))
#         # keyboard.add_hotkey('d', lambda: print('d'))

#         # keyboard.wait('esc')




#         while not self.stop:
#             try:
#                 event = keyboard.read_event()
#                 if event.name in ['a', 'w', 's', 'd']:
#                     print(f"Input: {event.name}")
#                 elif event.name in ['up', 'down', 'left', 'right']:
#                     print(f"Input: {event.name}")
#                 elif event.name in ['t']:
#                     print("Entering a Tag State")
#                     self.stop = True
#                 elif event.name == 'q':
#                     break
#             except KeyboardInterrupt:
#                 break

#     # def stopInput(self):
#     #     self.stop = True




# def main():
#     print("Press a, w, s, d or press q to quit.")
#     input = keyboardInput()
#     input.collectInput()

# if __name__ == "__main__":
#     main()


import curses
from time import sleep

def main(stdscr):
    curses.curs_set(0)  # Hide the cursor
    stdscr.addstr(0, 0, "Press a key (Enter, arrow keys, 'a', 'w', 's', 'd', or 'q'): ")
    stdscr.refresh()

    while True:
        char = stdscr.getch()
        if char == curses.KEY_ENTER or char == 10:
            stdscr.addstr(1, 0, "'Enter' key pressed")
        elif char == curses.KEY_DOWN:
            stdscr.addstr(1, 0, "Down arrow key pressed")
        elif char == curses.KEY_UP:
            stdscr.addstr(1, 0, "Up arrow key pressed")
        elif char == curses.KEY_LEFT:
            stdscr.addstr(1, 0, "Left arrow key pressed")
        elif char == curses.KEY_RIGHT:
            stdscr.addstr(1, 0, "Right arrow key pressed")
        elif char == ord('a'):
            stdscr.addstr(1, 0, "'a' key pressed")
        elif char == ord('w'):
            stdscr.addstr(1, 0, "'w' key pressed")
        elif char == ord('s'):
            stdscr.addstr(1, 0, "'s' key pressed")
        elif char == ord('d'):
            stdscr.addstr(1, 0, "'d' key pressed")
        elif char == ord('q'):
            break  # Exit on 'q' key

        stdscr.refresh()

curses.wrapper(main)
