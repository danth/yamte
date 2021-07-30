#include "Buffer.h"
#include "Row.h"
#include <vector>
#include <string>
#include <locale>
#include <codecvt>
#include <fstream>

int Buffer::countRows() {
  return rows.size();
}

Row* Buffer::getRow(int at) {
  return &rows[at];
}

void Buffer::insertRow(int at, std::wstring text) {
  if (at < 0 || at > rows.size()) return;

  Row row;
  row.setText(text);
  rows.insert(rows.begin() + at, row);
}

void Buffer::deleteRow(int at) {
  if (at < 0 || at > rows.size()) return;

  rows.erase(rows.begin() + at);
}

bool Buffer::isDirty() {
  return dirty > 0;
}

void Buffer::fromFile(std::fstream* file) {
  std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> converter;

  std::string line;
  while(getline(*file, line)) {
    std::wstring w_line = converter.from_bytes(line);
    insertRow(rows.size(), w_line);
  }

  dirty = 0;
}

void Buffer::toFile(std::fstream* file) {
  std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;

  int row;
  for (row = 0; row < rows.size(); row++) {
    std::wstring w_text = getRow(row)->getText();
    std::string text = converter.to_bytes(w_text);
    *file << text << '\n';
  }

  dirty = 0;
}

void Buffer::insertCharacter(int row, int column, wchar_t character) {
  if (row == rows.size()) insertRow(rows.size(), L"");
  getRow(row)->insertCharacter(column, character);

  dirty++;
}

void Buffer::insertNewline(int row, int column) {
  if (column == 0) {
    insertRow(row, L"");
  } else {
    insertRow(row + 1, getRow(row)->getText().substr(column));
    getRow(row)->resizeText(column);
  }

  dirty++;
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

  dirty++;
}
