#include "Display.h"
#include "Cursor.h"
#include "Buffer.h"
#include "Editor.h"
#include <ncurses.h>
#include <locale.h>
#include <string>


Display::Display() {
  row_offset = 0;
  column_offset = 0;
}

Editor* Display::getEditor() {
  return &editor;
}

void Display::initialiseScreen() {
  setlocale(LC_ALL, "");

  initscr();
  raw(); // Disable line buffering
  noecho(); // Don't echo typed characters
  nonl(); // Disable translation of \r into \n
  intrflush(stdscr, FALSE);
  keypad(stdscr, TRUE); // Replace F1, F2, F3... with token values
  refresh();

  status_window = newwin(1, COLS, 0, 0);
  sidebar_window = newwin(LINES-2, 4, 1, 0);
  buffer_window = newwin(LINES-2, COLS-4, 1, 4);
  message_window = newwin(1, COLS, LINES-1, 0);
}

void Display::clampScroll(int lines, int columns) {
  // Vertical
  int cursor_row = editor.getCursor()->getRow();

  if (cursor_row < row_offset)
    row_offset = cursor_row;
  if (cursor_row >= row_offset + lines)
    row_offset = cursor_row - lines + 1;

  // Horizontal
  int rendered_column =
    editor.getBuffer()->getRow(cursor_row)->renderedColumn(editor.getCursor()->getColumn());

  if (rendered_column < column_offset)
    column_offset = rendered_column;
  if (rendered_column >= column_offset + columns)
    column_offset = rendered_column - columns + 1;
}

void Display::drawSidebar() {
  werase(sidebar_window);

  int lines, columns;
  getmaxyx(sidebar_window, lines, columns);

  wattron(sidebar_window, WA_STANDOUT);

  int screen_row;
  for (screen_row = 0; screen_row < lines; screen_row++) {
    int file_row = screen_row + row_offset;

    if (file_row >= editor.getBuffer()->countRows()) {
      mvwaddstr(sidebar_window, screen_row, 0, " ~ ");
    } else {
      mvwprintw(sidebar_window, screen_row, 0, "%.3i", file_row);
    }

    mvwaddch(sidebar_window, screen_row, 3, ' ');
  }

  wattroff(sidebar_window, WA_STANDOUT);

  wrefresh(sidebar_window);
}

void Display::drawBuffer() {
  werase(buffer_window);

  int lines, columns;
  getmaxyx(buffer_window, lines, columns);

  clampScroll(lines, columns);

  int screen_row;
  for (screen_row = 0; screen_row < lines; screen_row++) {
    int file_row = screen_row + row_offset;

    if (file_row < editor.getBuffer()->countRows()) {
      std::wstring rendered = editor.getBuffer()->getRow(file_row)->getRendered();
      if (column_offset < rendered.size()) {
        std::wstring visible_rendered = rendered.substr(
            column_offset, column_offset + columns);
        mvwaddwstr(buffer_window, screen_row, 0, visible_rendered.c_str());
      }
    }
  }

  wrefresh(buffer_window);
}

void Display::positionCursor() {
  int row = editor.getCursor()->getRow();
  int column = editor.getCursor()->getColumn();
  wmove(
    buffer_window,
    row - row_offset,
    editor.getBuffer()->getRow(row)->renderedColumn(column) - column_offset
  );
  wrefresh(buffer_window);
}

std::string basename(std::string path) {
  size_t last_separator = path.find_last_of("/\\");
  if (last_separator != std::string::npos)
    return path.substr(last_separator + 1);
  else
    return path;
}

void Display::drawStatus() {
  werase(status_window);

  int lines, columns;
  getmaxyx(status_window, lines, columns);

  wattron(status_window, WA_STANDOUT);

  // Fill with spaces to create background
  int j;
  for (j = 0; j < columns; j++) mvwaddch(status_window, 0, j, ' ');

  // Overwrite some of the spaces with the status
  mvwprintw(
    status_window, 0, 0,
    "%.40s%s • %d lines • %s mode",
    editor.getFilename().size() ? basename(editor.getFilename()).c_str() : "[No name]",
    editor.getBuffer()->isDirty() ? " • Unsaved" : "",
    editor.getBuffer()->countRows(),
    editor.getModeName().c_str()
  );

  wattroff(status_window, WA_STANDOUT);

  wrefresh(status_window);
}

void Display::drawMessage() {
  werase(message_window);

  int lines, columns;
  getmaxyx(message_window, lines, columns);

  wattron(message_window, WA_STANDOUT);

  // Fill with spaces to create background
  int j;
  for (j = 0; j < columns; j++) mvwaddch(message_window, 0, j, ' ');

  mvwaddnstr(message_window, 0, 0, editor.getMessage().c_str(), columns);

  wattroff(message_window, WA_STANDOUT);

  wrefresh(message_window);
}

void Display::draw() {
  drawStatus();
  drawSidebar();
  drawBuffer();
  drawMessage();
  positionCursor();
}

void Display::processKey() {
  wint_t key;
  get_wch(&key);

  editor.processKey(key);
}
