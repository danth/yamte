#include "Editor.h"
#include "Display.h"


int main(int argc, char *argv[]) {
  Editor editor;
  Display display(&editor);
  display.initialiseScreen();

  if (argc >= 2) {
    editor.openFile(argv[1]);
  }

  while(1) {
    display.refreshScreen();
    editor.processKey(display.getKey());
  }

  return 0;
}
