#pragma once

#include "Cursor.h"
#include "Buffer.h"
#include <ncurses.h>
#include <string>


class Display {
	private:
    int row_offset;
    int column_offset;
    WINDOW* buffer_window;
    WINDOW* status_window;
    WINDOW* message_window;

    void clampScroll(Cursor* cursor, Buffer* buffer, int lines, int columns);

	public:
		Display();
    void initialiseScreen();

    void drawBuffer(Cursor* cursor, Buffer* buffer);
    void drawCursor(Cursor* cursor, Buffer* buffer);
    void drawStatus(Buffer* buffer, std::string filename, std::string mode);
    void drawMessage(std::string message);

    int getKey();
};
