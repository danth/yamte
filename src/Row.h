#pragma once

#include <string>


class Row {
	private:
		std::string text;
		std::string rendered;
		void render();
	
	public:
		std::string getText();
		std::string getRendered();
		int size();
		void setText(std::string t);
		void appendText(std::string t);
		void resizeText(int length);
		void insertCharacter(int at, int character);
		void deleteCharacter(int at);
		int renderedColumn(int column);
};
