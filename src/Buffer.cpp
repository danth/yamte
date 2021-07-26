#include "Buffer.h"
#include "Row.h"
#include <vector>
#include <string>

int Buffer::countRows() {
  return rows.size();
}

Row* Buffer::getRow(int at) {
  return &rows[at];
}

void Buffer::insertRow(int at, std::string text) {
  if (at < 0 || at > rows.size()) return;

  Row row;
  row.setText(text);
  rows.insert(rows.begin() + at, row);
}

void Buffer::deleteRow(int at) {
  if (at < 0 || at > rows.size()) return;

  rows.erase(rows.begin() + at);
}

void Buffer::insertCharacter(int row, int column, char character) {
  if (row == rows.size()) insertRow(rows.size(), "");
  getRow(row)->insertCharacter(column, character);
}

void Buffer::insertNewline(int row, int column) {
  if (column == 0) {
    insertRow(row, "");
  } else {
    insertRow(row + 1, getRow(row)->getText().substr(column));
    getRow(row)->resizeText(column);
  }
}

void Buffer::deleteCharacter(int row, int column) {
  if (row == rows.size()) return;
  if (row == 0 && column == 0) return;

  if (column > 0) {
    getRow(row)->deleteCharacter(column - 1);
  } else {
    getRow(row - 1)->appendText(getRow(row)->getText());
    deleteRow(row);
  }
}
