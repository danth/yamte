#pragma once

#include <string>


class Row {
	private:
		std::wstring text;
		std::wstring rendered;
		void render();
	
	public:
		std::wstring getText();
		std::wstring getRendered();
		int size();
		void setText(std::wstring t);
		void appendText(std::wstring t);
		void resizeText(int length);
		void insertCharacter(int at, wchar_t character);
		void deleteCharacter(int at);
		int renderedColumn(int column);
};
