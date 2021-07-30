#include "Display.h"
#include "Editor.h"
#include <ncurses.h>
#include <string>


Display::Display(Editor* e) {
  editor = e;
  row_offset = 0;
  column_offset = 0;
}

void Display::initialiseScreen() {
  initscr();
  raw(); // Disable line buffering
  noecho(); // Don't echo typed characters
  nonl(); // Disable translation of \r into \n
  intrflush(stdscr, FALSE);
  keypad(stdscr, TRUE); // Replace F1, F2, F3... with token values
  refresh();

  status_window = newwin(1, COLS, 0, 0);
  buffer_window = newwin(LINES-2, COLS, 1, 0);
  message_window = newwin(1, COLS, LINES-1, 0);
}

void Display::clampScroll() {
  int lines, columns;
  getmaxyx(buffer_window, lines, columns);

  // Vertical
  int cursor_row = editor->getCursor()->getRow();

  if (cursor_row < row_offset)
    row_offset = cursor_row;
  if (cursor_row >= row_offset + lines)
    row_offset = cursor_row - lines + 1;

  // Horizontal
  int cursor_column = editor->getCursor()->getColumn();
  int rendered_column = editor->getBuffer()->getRow(cursor_row)->renderedColumn(cursor_column);

  if (rendered_column < column_offset)
    column_offset = rendered_column;
  if (rendered_column >= column_offset + columns)
    column_offset = rendered_column - columns + 1;
}

void Display::drawBuffer() {
  wclear(buffer_window);

  int lines, columns;
  getmaxyx(buffer_window, lines, columns);

  int screen_row;
  for (screen_row = 0; screen_row < lines; screen_row++) {
    int file_row = screen_row + row_offset;

    if (file_row >= editor->getBuffer()->countRows()) {
      mvwaddch(buffer_window, screen_row, 0, '~');
    } else {
      std::string rendered = editor->getBuffer()->getRow(file_row)->getRendered();
      if (column_offset < rendered.size()) {
        std::string visible_rendered = rendered.substr(
            column_offset, column_offset + columns);
        mvwaddstr(buffer_window, screen_row, 0, visible_rendered.c_str());
      }
    }
  }

  wnoutrefresh(buffer_window);
}

void Display::drawStatus() {
  wclear(status_window);

  int lines, columns;
  getmaxyx(status_window, lines, columns);

  wattron(status_window, A_STANDOUT);

  // Fill with spaces to create background
  int j;
  for (j = 0; j < columns; j++) mvwaddch(status_window, 0, j, ' ');

  // Overwrite some of the spaces with the status
  mvwprintw(status_window, 0, 0, "%.40s - %d lines - %s mode",
    editor->isFileOpen() ? editor->getFilename().c_str() : "[no name]",
    editor->getBuffer()->countRows(),
    editor->getModeName().c_str()
  );

  wattroff(status_window, A_STANDOUT);

  wnoutrefresh(status_window);
}

void Display::drawMessage() {
  wclear(message_window);

  int lines, columns;
  getmaxyx(message_window, lines, columns);

  wattron(message_window, A_STANDOUT);

  // Fill with spaces to create background
  int j;
  for (j = 0; j < columns; j++) mvwaddch(message_window, 0, j, ' ');

  mvwaddnstr(message_window, 0, 0, editor->getStatusMessage().c_str(), columns);

  wattroff(message_window, A_STANDOUT);

  wnoutrefresh(message_window);
}

void Display::refreshScreen() {
  clampScroll();

  drawBuffer();
  drawStatus();
  drawMessage();

  move(
    editor->getCursor()->getRow() - row_offset,
    editor->getBuffer()->getRow(editor->getCursor()->getRow())->renderedColumn(editor->getCursor()->getColumn()) - column_offset
  );

  doupdate();
}

int Display::getKey() {
  return getch();
}
