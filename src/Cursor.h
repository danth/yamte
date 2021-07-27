#pragma once

#include "Buffer.h"


class Cursor {
	private:
    Buffer* buffer;
		int cursor_row;
		int cursor_column;

		void moveLeft();
		void moveRight();
		void moveUp();
		void moveDown();
		void moveHome();
		void moveEnd();

	public:
		Cursor(Buffer* b);
		int getRow();
		void setRow(int row);
		void adjustRow(int by);
		int getColumn();
		void setColumn(int column);
		void adjustColumn(int by);
		void move(int key);
};
