#include "Display.h"


int main(int argc, char *argv[]) {
  Display display;
  display.initialiseScreen();

  if (argc >= 2) {
    display.getEditor()->openFile(argv[1]);
  }

  while(1) {
    display.draw();
    display.processKey();
  }

  return 0;
}
