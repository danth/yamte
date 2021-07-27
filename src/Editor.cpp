#include "Editor.h"
#include "Buffer.h"
#include "Cursor.h"
#include <ncurses.h>
#include <string>
#include <iostream>
#include <fstream>

#define CTRL_KEY(k) ((k) & 0x1f)


void Editor::insertCharacter(char character) {
  buffer.insertCharacter(cursor.getRow(), cursor.getColumn(), character);
  cursor.adjustColumn(1);
}

void Editor::insertNewline() {
  buffer.insertNewline(cursor.getRow(), cursor.getColumn());
  cursor.adjustRow(1);
  cursor.setColumn(0);
}

void Editor::deleteCharacter() {
  int row = cursor.getRow();
  int column = cursor.getColumn();

  if (column > 0) {
    cursor.adjustColumn(-1);
  } else {
    cursor.setColumn(buffer.getRow(row - 1)->size());
    cursor.adjustRow(-1);
  }

  buffer.deleteCharacter(row, column);
}

Editor::Editor() : cursor(&buffer) {
  filename = "";
}

Buffer* Editor::getBuffer() {
  return &buffer;
}

Cursor* Editor::getCursor() {
  return &cursor;
}

bool Editor::isFileOpen() {
  return filename.size();
}

std::string Editor::getFilename() {
  return filename;
}

void Editor::openFile(std::string f) {
  filename = f;

  std::fstream file;
  file.open(filename, std::ios::in);
  if (file.is_open()) {
    std::string line;
    while(getline(file, line)) {
      buffer.insertRow(buffer.countRows(), line);
    }
    file.close();
  }
}

void Editor::saveFile() {
  std::fstream file;
  file.open(filename, std::ios::out);
  if (file.is_open()) {
    int row;
    for (row = 0; row < buffer.countRows(); row++) {
      file << buffer.getRow(row)->getText() << '\n';
    }
  }
  file.close();
}

void Editor::processKey(int key) {
  switch (key) {
    case CTRL_KEY('q'):
      endwin();
      exit(0);
      break;

    case CTRL_KEY('s'):
      saveFile();
      break;

    case KEY_UP:
    case KEY_DOWN:
    case KEY_LEFT:
    case KEY_RIGHT:
    case KEY_HOME:
    case KEY_END:
      cursor.move(key);
      break;

    case '\r':
      insertNewline();
      break;

    case KEY_BACKSPACE:
    case KEY_DC:
    case CTRL_KEY('h'):
      if (key == KEY_DC) cursor.move(KEY_RIGHT);
      deleteCharacter();
      break;

    default:
      insertCharacter(key);
      break;
  }
}
