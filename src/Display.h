#pragma once

#include "Editor.h"


class Display {
	private:
    Editor editor;
    int lines;
    int row_offset;
    int column_offset;

	public:
		Display(Editor e);
    void initialiseScreen();
    void clampScroll();
    void drawRows();
    void drawStatus();
    void refreshScreen();
    int getKey();
};
