#include "Editor.h"
#include "Buffer.h"
#include "Cursor.h"
#include <ncurses.h>
#include <string>
#include <iostream>
#include <fstream>

#define CTRL_KEY(k) ((k) & 0x1f)


Editor::Editor() : cursor(&buffer) {
  filename = "";
  message = "Welcome to Yamte!";
  input_mode = FALSE;
}

std::string Editor::getFilename() {
  return filename;
}

std::string Editor::getMessage() {
  return message;
}

Buffer* Editor::getBuffer() {
  return &buffer;
}

Cursor* Editor::getCursor() {
  return &cursor;
}

std::string Editor::getModeName() {
  return input_mode ? "Input" : "Action";
}

void Editor::insertCharacter(wchar_t character) {
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

  if (row == buffer.countRows()) return;
  if (row == 0 && column == 0) return;

  if (column > 0) {
    cursor.adjustColumn(-1);
  } else {
    cursor.setColumn(buffer.getRow(row - 1)->size());
    cursor.adjustRow(-1);
  }

  buffer.deleteCharacter(row, column);
}

void Editor::openFile(std::string f) {
  filename = f;

  std::fstream file;
  file.open(filename, std::ios::in);
  if (file.is_open()) {
    buffer.fromFile(&file);
    file.close();

    message = "Opened " + filename;
  } else {
    message = "Failed to open " + filename;
  }
}

void Editor::saveFile() {
  std::fstream file;
  file.open(filename, std::ios::out);
  if (file.is_open()) {
    buffer.toFile(&file);
    file.close();

    message = "Saved " + filename;
  } else {
    message = "Failed to save " + filename;
  }
}

void Editor::processKeyNormal(wchar_t key) {
  switch (key) {
    case CTRL_KEY('q'):
      endwin();
      exit(0);
      break;

    case CTRL_KEY('o'):
      saveFile();
      break;

    case 'e':
      input_mode = TRUE;
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

bool isKeyPrintable(wchar_t key) {
  return (
    key > 31 && key < 255
    && key != 127
    && key != 129
    && key != 141
    && key != 143
    && key != 144
    && key != 157
  );
}

void Editor::processKeyInsert(wchar_t key) {
  switch (key) {
    case CTRL_KEY('q'):
      input_mode = FALSE;
      break;

    case '\r':
      insertNewline();
      break;

    case KEY_BACKSPACE:
    case KEY_DC:
    case CTRL_KEY('h'):
    case 127:
      if (key == KEY_DC) cursor.moveRight();
      deleteCharacter();
      break;

    case KEY_UP:
      cursor.moveUp();
      break;

    case KEY_DOWN:
      cursor.moveDown();
      break;

    case KEY_LEFT:
      cursor.moveLeft();
      break;

    case KEY_RIGHT:
      cursor.moveRight();
      break;

    case KEY_HOME:
      cursor.moveHome();
      break;

    case KEY_END:
      cursor.moveEnd();
      break;

    default:
      if (isKeyPrintable(key)) insertCharacter(key);
      break;
  }
}

void Editor::processKey(wchar_t key) {
  if (input_mode)
    processKeyInsert(key);
  else
    processKeyNormal(key);
}
