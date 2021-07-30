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
  insert_mode = FALSE;
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

std::string Editor::getStatusMessage() {
  return status_message;
}

std::string Editor::getModeName() {
  return insert_mode ? "Edit" : "Move";
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

    status_message = "Opened " + filename;
  } else {
    status_message = "Failed to open " + filename;
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
    file.close();

    status_message = "Saved " + filename;
  } else {
    status_message = "Failed to save " + filename;
  }
}

void Editor::processKeyNormal(int key) {
  switch (key) {
    case CTRL_KEY('q'):
      endwin();
      exit(0);
      break;

    case CTRL_KEY('o'):
      saveFile();
      break;

    case 'e':
      insert_mode = TRUE;
      break;

    case 'w':
    case KEY_UP:
      cursor.moveUp();
      break;

    case 's':
    case KEY_DOWN:
      cursor.moveDown();
      break;

    case 'a':
    case KEY_LEFT:
      cursor.moveLeft();
      break;

    case 'd':
    case KEY_RIGHT:
      cursor.moveRight();
      break;

    case KEY_HOME:
    case CTRL_KEY('a'):
      cursor.moveHome();
      break;

    case KEY_END:
    case CTRL_KEY('d'):
      cursor.moveEnd();
      break;

    case CTRL_KEY('w'):
      cursor.moveTop();
      break;

    case CTRL_KEY('s'):
      cursor.moveBottom();
      break;
  }
}

void Editor::processKeyInsert(int key) {
  switch (key) {
    case CTRL_KEY('q'):
      insert_mode = FALSE;
      break;

    case '\r':
      insertNewline();
      break;

    case KEY_BACKSPACE:
    case KEY_DC:
    case CTRL_KEY('h'):
      if (key == KEY_DC) cursor.moveRight();
      deleteCharacter();
      break;

    default:
      insertCharacter(key);
      break;
  }
}

void Editor::processKey(int key) {
  if (insert_mode)
    processKeyInsert(key);
  else
    processKeyNormal(key);
}
