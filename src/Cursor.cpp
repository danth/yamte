#include "Cursor.h"
#include "Buffer.h"
#include <ncurses.h>


Cursor::Cursor(Buffer* b) {
  buffer = b;
  cursor_row = 0;
  cursor_column = 0;
}

int Cursor::getRow() {
  return cursor_row;
}

void Cursor::setRow(int row) {
  cursor_row = row;
}

void Cursor::adjustRow(int by) {
  cursor_row += by;
}

int Cursor::getColumn() {
  return cursor_column;
}

void Cursor::setColumn(int column) {
  cursor_column = column;
}

void Cursor::adjustColumn(int by) {
  cursor_column += by;
}

void Cursor::move(int key) {
  switch (key) {
    case KEY_UP:
      if (cursor_row > 0) cursor_row--;
      break;
    case KEY_DOWN:
      // Move down unless we are on the last line
      if (cursor_row < buffer->countRows()) cursor_row++;
      break;
    case KEY_LEFT:
      // Move left unless we are at the first column
      if (cursor_column > 0) cursor_column--;
      // Go to the last column of the previous line
      else if (cursor_row > 0) {
        cursor_row--;
        cursor_column = buffer->getRow(cursor_row)->size();
      }
      break;
    case KEY_RIGHT:
      if (cursor_row < buffer->countRows()) {
        Row* row = buffer->getRow(cursor_row);
        // Move right unless we are at the last column
        if (cursor_column < row->size()) cursor_column++;
        // Go to the first column of the next line
        else if (cursor_column == row->size()) {
          cursor_row++;
          cursor_column = 0;
        }
      }
      break;
    case KEY_HOME:
      // Move to the first column
      cursor_column = 0;
      break;
    case KEY_END:
      // Move to the right of the screen
      if (cursor_row < buffer->countRows()) {
        cursor_column = buffer->getRow(cursor_row)->size();
      }
      break;
  }

  if (cursor_row > buffer->countRows()) {
    cursor_column = 0;
  } else {
    int row_length = buffer->getRow(cursor_row)->size();
    if (cursor_column > row_length) cursor_column = row_length;
  }
}
