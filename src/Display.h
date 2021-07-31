#pragma once

#include "Cursor.h"
#include "Buffer.h"
#include "Editor.h"
#include <ncurses.h>
#include <string>


class Display {
	private:
    int row_offset;
    int column_offset;
    Editor editor;
    WINDOW* buffer_window;
    WINDOW* sidebar_window;
    WINDOW* status_window;
    WINDOW* message_window;

    void clampScroll(int lines, int columns);

    void drawStatus();
    void drawSidebar();
    void drawBuffer();
    void drawMessage();
    void positionCursor();

	public:
    Display();

    Editor* getEditor();

    void initialiseScreen();
    void draw();

    void setMessage(std::string m);

    void processKey();
};
