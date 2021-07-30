#pragma once

#include "Row.h"
#include <vector>
#include <string>
#include <fstream>


class Buffer {
	private:
		std::vector<Row> rows;
		int dirty = 0;
	
	public:
		int countRows();
		Row* getRow(int at);
		void insertRow(int at, std::wstring text);
		void deleteRow(int at);

		bool isDirty();

		void fromFile(std::fstream* file);
		void toFile(std::fstream* file);

		void insertCharacter(int row, int column, wchar_t character);
		void insertNewline(int row, int column);
		void deleteCharacter(int row, int column);
};
