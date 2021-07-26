#pragma once

#include "Row.h"
#include <vector>
#include <string>


class Buffer {
	private:
		std::vector<Row> rows;
	
	public:
		int countRows();
		Row* getRow(int at);
		void insertRow(int at, std::string text);
		void deleteRow(int at);
		void insertCharacter(int row, int column, char character);
		void insertNewline(int row, int column);
		void deleteCharacter(int row, int column);
};
