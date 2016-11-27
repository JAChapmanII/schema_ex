#ifndef PARSER_HPP
#define PARSER_HPP

#include <string>
#include <vector>
#include <map>
#include <set>

namespace parser {
	struct Field {
		bool composite{false}; // TODO: used?
		std::string type{""};
		std::string name{""};

		Field(std::string itype, std::string iname);
	};

	struct Context;

	struct Type {
		std::string name{"{null}"};
		bool composite{true};
		bool pureChild{false};

		std::vector<std::string> typeArguments{};
		std::vector<Field> fields{};

		Type(std::string iname, bool icomposite = true);
		Type();

		std::string toString(Context *context);
	};

	struct Context {
		std::map<std::string, Type> types{};
		std::string src{""};
		size_t idx{0};
		std::map<std::string, std::string> parentTypes{};
		std::map<std::string, int> typeDepth{};
		// TODO: need to account for ::id_t usage as weak dependency
		std::map<std::string, std::set<std::string>> weakDeps{};
		std::map<std::string, int> weakDepOrder{};
		std::map<std::string, std::string> pureChildFieldNames{};
		std::map<std::string, std::string> setChildFieldNames{};
		bool usedTranslatable{false};


		Context(std::string isrc);

		std::vector<std::string> getOrderedNonIdTypes();

		int getTypeDepth(std::string t);
		void buildTypeDepths();

		void buildParentTypes();
		std::vector<std::string> getParentTypeChain(std::string type);

		void skipSpaces();
		bool is(std::string token);
		void consume(std::string token);

		std::string parseAttribute();
		std::string parseIdentifier();
		std::string parseCompoundIdentifier();
		Field parseField();
		void parseType();
		void parse(); // TODO: uh, uses src in constructor param?

		void makeTranslatable(std::string typeName);
		void declType(std::string type, bool composite = true);
		void declStructType(std::string typeName, bool translatable = false);
	};
}

#endif // PARSER_HPP
