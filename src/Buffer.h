#pragma once

#include "Row.h"
#include <vector>
#include <string>
#include <fstream>


class Buffer {
	private:
		std::vector<Row> rows;
	
	public:
		int countRows();
		Row* getRow(int at);
		void insertRow(int at, std::wstring text);
		void deleteRow(int at);

		void fromFile(std::fstream* file);
		void toFile(std::fstream* file);

		void insertCharacter(int row, int column, wchar_t character);
		void insertNewline(int row, int column);
		void deleteCharacter(int row, int column);
};
