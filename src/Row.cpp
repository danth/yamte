#include "Row.h"
#include <string>

#define TAB_STOP 8


void Row::render() {
  rendered = "";
  int j;
  for (j = 0; j < text.size(); j++) {
    if (text[j] == '\t') {
      // Replace tabs with spaces up to the next tab stop
      rendered.push_back(' ');
      while (rendered.size() % TAB_STOP != 0) rendered.push_back(' ');
    } else {
      rendered.push_back(text[j]);
    }
  }
}

std::string Row::getText() {
  return text;
}

std::string Row::getRendered() {
  return rendered;
}

int Row::size() {
  return text.size();
}

void Row::setText(std::string t) {
  text = t;
  render();
}

void Row::appendText(std::string t) {
  text.append(t);
  render();
}

void Row::resizeText(int length) {
  text.resize(length);
  render();
}

void Row::insertCharacter(int at, int character) {
  if (at < 0 || at > text.size()) at = text.size();

  text.insert(text.begin() + at, character);
  render();
}

void Row::deleteCharacter(int at) {
  if (at < 0 || at >= text.size()) return;

  text.erase(text.begin() + at);
  render();
}

int Row::renderedColumn(int column) {
  int rendered_column = 0;
  int j;
  for (j = 0; j < column; j++) {
    if (text[j] == '\t')
      rendered_column += (TAB_STOP - 1) - (rendered_column % TAB_STOP);
    rendered_column++;
  }
  return rendered_column;
}
