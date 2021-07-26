#pragma once

#include "Buffer.h"


class Cursor {
	private:
    Buffer* buffer;
		int cursor_row;
		int cursor_column;

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
