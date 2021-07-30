#include "Editor.h"
#include "Buffer.h"
#include "Cursor.h"
#include "Display.h"
#include <ncurses.h>
#include <string>
#include <iostream>
#include <fstream>

#define CTRL_KEY(k) ((k) & 0x1f)


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

Editor::Editor() : cursor(&buffer) {
  filename = "";
  input_mode = FALSE;
}

void Editor::drawStatus() {
  display.drawStatus(&buffer, filename, input_mode ? "Input" : "Action");
}

void Editor::drawBuffer() {
  display.drawBuffer(&cursor, &buffer);
}

void Editor::drawCursor() {
  display.drawCursor(&cursor, &buffer);
}

void Editor::initialiseScreen() {
  display.initialiseScreen();
  drawStatus();
  drawBuffer();
  display.drawMessage("Welcome to Yamte!");
  drawCursor();
}

void Editor::openFile(std::string f) {
  filename = f;

  std::fstream file;
  file.open(filename, std::ios::in);
  if (file.is_open()) {
    buffer.fromFile(&file);
    file.close();

    drawBuffer();
    display.drawMessage("Opened " + filename);
  } else {
    display.drawMessage("Failed to open " + filename);
  }

  drawStatus();
  drawCursor();
}

void Editor::saveFile() {
  std::fstream file;
  file.open(filename, std::ios::out);
  if (file.is_open()) {
    buffer.toFile(&file);
    file.close();

    display.drawMessage("Saved " + filename);
  } else {
    display.drawMessage("Failed to save " + filename);
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
      drawStatus();
      break;

    case 'e':
      input_mode = TRUE;
      drawStatus();
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

  drawBuffer();
  drawCursor();
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

  drawStatus();
  drawBuffer();
  drawCursor();
}

void Editor::processKey() {
  wchar_t key = display.getKey();

  if (input_mode)
    processKeyInsert(key);
  else
    processKeyNormal(key);
}
