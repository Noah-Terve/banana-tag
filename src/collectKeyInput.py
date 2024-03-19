import keyboard
import time

class keyboardInput:
    def __init__(self):
        self.stop = False

    def collectInput(self):
        while not self.stop:
            try:
                event = keyboard.read_event()
                if event.name in ['a', 'w', 's', 'd']:
                    print(f"Input: {event.name}")
                elif event.name in ['up', 'down', 'left', 'right']:
                    print(f"Input: {event.name}")
                elif event.name in ['t']:
                    print("Entering a Tag State")
                    self.stop = True
                elif event.name == 'q':
                    break
            except KeyboardInterrupt:
                break

    # def stopInput(self):
    #     self.stop = True




def main():
    print("Press a, w, s, d or press q to quit.")
    input = keyboardInput()
    input.collectInput()

if __name__ == "__main__":
    main()