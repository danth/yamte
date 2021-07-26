#include "Display.h"
#include "Editor.h"
#include <ncurses.h>
#include <string>


Display::Display(Editor e) {
  editor = e;
  lines = LINES - 1;
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
}

void Display::clampScroll() {
  // Vertical
  int cursor_row = editor.getCursor()->getRow();

  if (cursor_row < row_offset)
    row_offset = cursor_row;
  if (cursor_row >= row_offset + lines)
    row_offset = cursor_row - lines + 1;

  // Horizontal
  int cursor_column = editor.getCursor()->getColumn();
  int rendered_column = editor.getBuffer()->getRow(cursor_row)->renderedColumn(cursor_column);

  if (rendered_column < column_offset)
    column_offset = rendered_column;
  if (rendered_column >= column_offset + COLS)
    column_offset = rendered_column - COLS + 1;
}

void Display::drawRows() {
  int screen_row;
  for (screen_row = 0; screen_row < lines; screen_row++) {
    int file_row = screen_row + row_offset;

    if (file_row >= editor.getBuffer()->countRows()) {
      mvaddch(screen_row, 0, '~');
    } else {
      std::string rendered = editor.getBuffer()->getRow(file_row)->getRendered();
      std::string visible_rendered = rendered.substr(
          column_offset, column_offset + COLS);
      mvaddstr(screen_row, 0, visible_rendered.c_str());
    }
  }
}

void Display::drawStatus() {
  attron(A_STANDOUT);

  // Fill with spaces to create background
  int j;
  for (j = 0; j < COLS; j++) mvaddch(LINES-1, j, ' ');

  // Overwrite some of the spaces with the status
  mvprintw(LINES-1, 0, "%.40s - %d lines",
    editor.isFileOpen() ? editor.getFilename().c_str() : "[no name]",
    editor.getBuffer()->countRows()
  );

  attroff(A_STANDOUT);
}

void Display::refreshScreen() {
  clear();
  lines = LINES - 1;
  clampScroll();
  drawRows();
  drawStatus();
  move(
    editor.getCursor()->getRow() - row_offset,
    editor.getBuffer()->getRow(editor.getCursor()->getRow())->renderedColumn(editor.getCursor()->getColumn()) - column_offset
  );
  refresh();
}

int Display::getKey() {
  return getch();
}
