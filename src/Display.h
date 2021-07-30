#pragma once

#include "Editor.h"
#include <ncurses.h>


class Display {
	private:
    Editor* editor;
    int row_offset;
    int column_offset;
    WINDOW* buffer_window;
    WINDOW* status_window;
    WINDOW* message_window;

	public:
		Display(Editor* e);
    void initialiseScreen();
    void clampScroll();

    void drawBuffer();
    void drawStatus();
    void drawMessage();
    void refreshScreen();

    int getKey();
};
