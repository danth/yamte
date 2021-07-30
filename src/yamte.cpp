#include "Editor.h"


int main(int argc, char *argv[]) {
  Editor editor;
  editor.initialiseScreen();

  if (argc >= 2) {
    editor.openFile(argv[1]);
  }

  while(1) {
    editor.processKey();
  }

  return 0;
}
