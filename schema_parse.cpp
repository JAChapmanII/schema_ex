#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
using namespace std;

bool isupper(char c) {
	return (c >= 'A' && c <= 'Z');
}
char tolower(char c) {
	if(isupper(c))
		return c - 'A' + 'a';
	return c;
}
string tableCase(string name) {
	string tCase = "";
	tCase += tolower(name[0]);
	for(int i = 1; i < name.size(); ++i) {
		if(isupper(name[i]) && (tCase.empty() || tCase.back() != '_'))
			tCase += "_";
		tCase += tolower(name[i]);
	}
	return tCase;
}
string pluralize(string name) {
	if(name.length() >= 2 && name.substr(name.size() - 2) == "ss")
		return name + "es";
	else if(name.length() >= 1 && name.substr(name.size() - 1) == "s")
		return name;
	return name + "s";
}
string typedField(string type, string name) {
	if(type == "int" || type == "bool") {
		return "`" + name + "` INT NOT NULL";
	}
	if(type == "string") {
		return "`" + name + "` TEXT NOT NULL";
	}
	throw "unknown field type: " + type;
}
string foreignKeyField(string name, string fprefix = "") {
	string fname = (fprefix.empty() ? name : fprefix + "_" + name);
	return "`" + tableCase(fname) + "_id` UNIQUEIDENTIFIER NOT NULL";
}
string foreignKeyConstraint(string name, string fprefix = "") {
	string fname = (fprefix.empty() ? name : fprefix + "_" + name);
	string ourField = tableCase(fname) + "_id",
			idField = tableCase(name) + "_id",
			tableName = pluralize(tableCase(name));
	return "FOREIGN KEY(" + ourField + ") REFERENCES "
		+ tableName + "(" + idField + ")";
}
string createTableHeader(string name) {
	return "CREATE TABLE `" + pluralize(tableCase(name)) + "` (";
}
string createTableFooter() {
	return ");";
}
string primaryKeyConstraint(vector<string> tables) {
	string res = tableCase(tables.front()) + "_id";
	for(int i = 1; i < tables.size(); ++i)
		res += ", " + tableCase(tables[i]) + "_id";
	return "PRIMARY KEY(" + res + ")";
}
template<typename... Args> string primaryKeyConstraint(Args... args) {
	vector<string> vals = {args...};
	return primaryKeyConstraint(vals);
}
string formatTable(string header, vector<string> fields,
		vector<string> constraints, string footer) {
	vector<string> parts = fields;
	parts.insert(parts.end(), constraints.begin(), constraints.end());

	string res = header + "\n";
	for(int i = 0; i < parts.size(); ++i) {
		res += "\t" + parts[i];
		if(i != parts.size() - 1)
			res += ",";
		res += "\n";
	}
	res += footer;
	return res;
}

bool isIdType(string type) {
	return type.size() >= 6 && type.substr(type.size() - 6) == "::id_t";
}
string getBaseIdType(string type) {
	return type.substr(0, type.size() - 6);
}


struct Field {
	bool composite{false};

	string type{""};
	string name{""};

	Field(string itype, string iname)
		: type{itype}, name{iname} { }
};

struct Context;

struct Type {
	string name{"{null}"};
	bool composite{true};
	bool pureChild{false};

	vector<string> typeArguments{};
	vector<Field> fields{};

	Type(string iname, bool icomposite = true)
		: name{iname}, composite{icomposite} { }
	Type() { }

	string toString(Context *context);
};

struct Context {
	map<string, Type> types{};
	string src{""};
	size_t idx{0};
	map<string, string> parentTypes{};
	map<string, int> typeDepth{};
	map<string, set<string>> weakDeps{};
	map<string, int> weakDepOrder{};
	map<string, string> pureChildFieldNames{};
	bool usedTranslatable{false};

	Context(string isrc) : src{isrc} {
		types.emplace("int", Type{"int", false});
		types.emplace("bool", Type{"bool", false});
		types.emplace("string", Type{"string", false});
		types.emplace("set", Type{"set", false});
		types.emplace("map", Type{"map", false});
		types.emplace("vector", Type{"vector", false});

		parse();
		buildParentTypes();
		buildTypeDepths();

		cerr << "====" << endl;
		for(auto &pe : parentTypes) {
			cerr << pe.first << " is child of " << pe.second << endl;
		}

		buildTypeDepths();

		auto orderedNonIdTypes = getOrderedNonIdTypes();

		// print pure child types
		for(auto &te : types) {
			if(te.second.pureChild)
				cerr << "pure child: " << te.second.name
					<< " => " << pureChildFieldNames[te.second.name] << endl;
		}

		// in depth order, print create tables for struct types
		for(auto &typeName : orderedNonIdTypes) {
			auto &type = types[typeName];
			if(!type.composite || type.pureChild)
				continue;

			vector<string> tableFields{};
			vector<string> tableConstraints{};

			// handle id/parent ids
			if(parentTypes.find(typeName) == parentTypes.end()) {
				// we're a root type, include our id primary key
				tableFields.push_back("`" + tableCase(typeName) + "_id`"
						+ " UNIQUEIDENTIFIER PRIMARY KEY");
			} else {
				// we're a child type, need to build id refs
				string t = typeName;
				vector<string> pkFields;
				pkFields.push_back(t);

				tableFields.push_back(foreignKeyField(t));
				do {
					t = parentTypes[t];
					pkFields.push_back(t);

					tableFields.push_back(foreignKeyField(t));
					tableConstraints.push_back(foreignKeyConstraint(t));
				} while(parentTypes.find(t) != parentTypes.end());

				tableConstraints.push_back(primaryKeyConstraint(pkFields));
			}

			for(int i = 0; i < type.fields.size(); ++i) {
				auto &field = type.fields[i];
				auto &ft = types[field.type];
				if(ft.composite)
					continue;
				if(field.type == typeName + "::id_t")
					continue; // skip our own id since it is generated above

				if(ft.name == "map" || ft.name == "set" || ft.name == "vector")
					continue; // special subtable types are handled later

				// if we have the id_t of another table, add foreign key
				if(isIdType(field.type)) {
					string otype = getBaseIdType(field.type);
					tableFields.push_back("`" + tableCase(field.name) + "_id`"
							+ " UNIQUEIDENTIFIER NOT NULL");
					tableConstraints.push_back("FOREIGN KEY("
							+ tableCase(field.name) + "_id) REFERENCES "
							+ pluralize(tableCase(otype)) + "("
							+ tableCase(otype) + "_id)");
					continue;
				}

				tableFields.push_back(typedField(
							field.type, tableCase(field.name)));
			}

			cout << formatTable(createTableHeader(typeName),
					tableFields, tableConstraints, createTableFooter()) << endl;
		}

		// in depth order, print child relations that aren't pure
		for(auto &typeName : orderedNonIdTypes) {
			auto &type = types[typeName];
			if(!type.composite || type.pureChild)
				continue;

			for(auto &field : type.fields) {
				auto &ft = types[field.type];
				if(ft.name == "set")
					continue; // sets are actual tables generated above
				if(ft.name != "map" && ft.name != "vector")
					continue; // impure child relations are only map and vector

				string pkSecond;
				vector<string> fields, constraints;
				fields.push_back(foreignKeyField(typeName));
				constraints.push_back(foreignKeyConstraint(typeName));

				if(ft.name == "map") {
					string keyType = ft.typeArguments[0], valType = ft.typeArguments[1];
					auto &kt = types[keyType];
					auto &vt = types[valType];
					if(vt.composite) {
						// this is actually a pure child, will be handled later
						continue;
					}

					string keyTypeType = keyType.substr(0, keyType.size() - 6);

					fields.push_back(foreignKeyField(keyTypeType, field.name));
					fields.push_back(typedField(valType, "val"));

					constraints.push_back(foreignKeyConstraint(
								keyTypeType, field.name));
					pkSecond = field.name + "_" + keyTypeType;
				}
				if(ft.name == "vector") {
					string subType = ft.typeArguments[0];

					fields.push_back(foreignKeyField(subType));
					constraints.push_back(foreignKeyConstraint(subType));
					pkSecond = subType;
				}

				constraints.push_back(primaryKeyConstraint(typeName, pkSecond));

				cerr << "-- impure child relation" << endl;
				cout << formatTable(createTableHeader(typeName + "_" + field.name),
						fields, constraints, createTableFooter()) << endl;
			}
		}

		// generate all pure child relation tables
		for(auto &typeName : orderedNonIdTypes) {
			auto &type = types[typeName];
			if(!type.composite || !type.pureChild)
				continue;

			string parentType = parentTypes[typeName];
			cerr << "-- pure child " << typeName << " of " << parentType << endl;

			string parentFieldName = pureChildFieldNames[typeName];
			Field parentField{"{null}", parentFieldName};
			for(auto &field : types[parentType].fields)
				if(field.name == parentFieldName)
					parentField.type = field.type;

			string parentFieldKeyType = types[parentField.type].typeArguments[0];
			if(!isIdType(parentFieldKeyType))
				throw parentFieldKeyType + " is not id type";

			cerr << "-- " << parentField.name << " ! " << parentField.type
				<< " ! " << parentFieldKeyType << endl;

			vector<string> fields, constraints;

			fields.push_back(foreignKeyField(parentType));
			constraints.push_back(foreignKeyConstraint(parentType));

			string parentFieldKeyTypeType = getBaseIdType(parentFieldKeyType);
			fields.push_back(foreignKeyField(parentFieldKeyTypeType));
			constraints.push_back(foreignKeyConstraint(parentFieldKeyTypeType));

			constraints.push_back(primaryKeyConstraint(
						parentType, parentFieldKeyTypeType));

			for(auto &field : type.fields) {
				auto &ft = types[field.type];
				// should only have built in fields and ::id_t

				if(isIdType(ft.name)) {
					string ftType = getBaseIdType(ft.name);
					if(ftType == typeName)
						continue; // don't print key type for self
					fields.push_back(foreignKeyField(ftType));
					constraints.push_back(foreignKeyConstraint(ftType));
					continue;
				}
				fields.push_back(typedField(ft.name, field.name));
			}

			cout << formatTable(createTableHeader(typeName),
					fields, constraints, createTableFooter()) << endl;
		}
	}

	vector<string> getOrderedNonIdTypes() {
		size_t cnt = 0;
		map<int, vector<string>> levels{};
		for(auto &tde : typeDepth) {
			if(isIdType(tde.first))
				continue;
			levels[tde.second].push_back(tde.first);
			cnt++;
		}

		vector<string> ordered{};
		ordered.reserve(cnt);

		for(auto &level : levels)
			ordered.insert(ordered.end(), level.second.begin(), level.second.end());

		return ordered;
	}

	int getTypeDepth(string t) {
		int depth = 0;
		while(parentTypes.find(t) != parentTypes.end()) {
			depth++;
			t = parentTypes[t];
		}
		return depth;
	}
	void buildTypeDepths() {
		for(auto &te : types) {
			string bt = te.second.name;
			typeDepth[bt] = getTypeDepth(bt);
		}
	}

	void buildParentTypes() {
		for(auto &te : types) {
			if(!te.second.composite)
				continue;
			string bt = te.second.name;
			string idt = bt + "::id_t";
			parentTypes[idt] = bt;
			weakDeps[bt].insert(idt);
			for(auto &field : te.second.fields) {
				cerr << bt << " has " << types[field.type].name
					<< " (" << types[field.type].toString(this) << ")" << endl;
				if(types[field.type].name == "map") {
					string keyType = types[field.type].typeArguments[0];
					string valType = types[field.type].typeArguments[1];
					cerr << "\t" << types[valType].composite << endl;
					if(types[valType].composite) {
						parentTypes[valType] = bt;
						types[valType].pureChild = true;
						pureChildFieldNames[valType] = field.name;
					}
					weakDeps[bt].insert(valType);
					weakDeps[bt].insert(keyType);
				}
				if(types[field.type].name == "set") {
					string cType = types[field.type].typeArguments[0];
					parentTypes[cType] = bt;
					weakDeps[cType].insert(bt);
				}
			}
		}
	}

	bool isspace() {
		char c = src[idx];
		return c == '\n' || c == '\t' || c == ' ';
	}
	void skipSpaces() {
		while(isspace())
			++idx;
		if(is("//")) {
			while(!is("\n"))
				idx++;
		}
	}
	bool is(string token) {
		if(idx + token.size() >= src.size())
			return false;
		for(size_t i = 0; i < token.size(); ++i)
			if(src[idx + i] != token[i])
				return false;
		return true;
	}
	void consume(string token) {
		skipSpaces();
		size_t ctx = 10;
		if(!is(token))
			throw "unable to consume " + token + " at " + to_string(idx) + "\n"
				+ src.substr(max((size_t)0, idx - ctx),
						min(src.size() - idx, ctx)) + "\n"
				+ string((int)ctx, '~') + "^";
		idx += token.size();
		skipSpaces();
	}

	string parseAttribute() {
		consume("[[");
		string ident = parseIdentifier();
		consume("]]");
		return ident;
	}

	string parseIdentifier() {
		skipSpaces();
		string ident = "";
		while(idx < src.size() && !isspace()
				&& src[idx] != '<' && src[idx] != '>'
				&& src[idx] != ']' && src[idx] != ';'
				&& src[idx] != ',') {
			ident += src[idx];
			idx++;
		}
		skipSpaces();
		return ident;
	}

	void declType(string type, bool composite = true) {
		if(types.find(type) != types.end())
			return;

		if(type.size() > 6
				&& type.substr(type.size() - 6) == "::id_t") {
			cerr << "declared type " << type << " as id_t, not composite" << endl;
			types[type] = Type{type, false};
		} else {
			cerr << "declared type " << type << " as composite" << endl;
			types[type] = Type{type, true};
		}
	}

	string parseCompoundIdentifier() {
		string baseIdent = parseIdentifier();
		if(is("<")) {
			consume("<");
			//cerr << "has compound identifier: " << baseIdent << endl;
			bool consumeComma = false;
			Type ttype{baseIdent, false};
			do {
				if(consumeComma)
					consume(",");
				string innerIdent = parseIdentifier();
				//cerr << "\t" << innerIdent << endl;
				declType(innerIdent);
				ttype.typeArguments.push_back(innerIdent);
				consumeComma = true;
			} while(is(","));
			consume(">");
			types[ttype.toString(this)] = ttype;
			return ttype.toString(this);
		}
		return baseIdent;
	}

	Field parseField() {
		string fieldType = parseCompoundIdentifier();
		string fieldName = parseIdentifier();
		consume(";");

		return Field{fieldType, fieldName};
	}

	void makeTranslatable(string typeName) {
		types[typeName].fields.push_back(Field{"string", "identifier"});
		string lmType = "map<Language::id_t, string>";
		types[lmType] = Type{"map", false};
		types[lmType].typeArguments.push_back("Language::id_t");
		types[lmType].typeArguments.push_back("string");
		types[typeName].fields.push_back(Field{lmType, "translations"});
	};
	void declStructType(string typeName, bool translatable = false) {
		declType(typeName);
		declType(typeName + "::id_t");
		types[typeName].fields.push_back(Field{typeName + "::id_t", "id"});

		if(translatable) {
			usedTranslatable = true;
			makeTranslatable(typeName);
		}
	}

	void parseType() {
		if(idx >= src.size()) return;

		bool translatable = false;
		vector<string> attributes{};
		while(src[idx] == '[') {
			string attr = parseAttribute();
			cerr << "attribute: " << attr << endl;
			if(attr == "translatable")
				translatable = true;
		}

		consume("struct");
		string typeName = parseIdentifier();
		cerr << "parsing type " << typeName << endl;

		declStructType(typeName, translatable);

		consume("{");

		skipSpaces();
		while(!is("}")) {
			auto f = parseField();
			types[typeName].fields.push_back(f);
			while(isspace() || is("//"))
				skipSpaces();
		}
		consume("}");
		consume(";");

		for(auto &field : types[typeName].fields) {
			cerr << "\t" << field.type << " => " << field.name << endl;
		}
	}

	void parse() {
		while(idx < src.size()) {
			skipSpaces();
			parseType();
		}

		if(usedTranslatable)
			declStructType("Language", true);
	}
};

string Type::toString(Context *context) {
	if(typeArguments.empty())
		return name;
	string str = name + "<" + context->types[typeArguments[0]].toString(context);
	for(int i = 1; i < typeArguments.size(); ++i)
		str += ", " + context->types[typeArguments[i]].toString(context);
	return str + ">";
}

int main(int argc, char **argv) {
	if(argc != 2) {
		cout << "usage: ./schema_parse <schema.cpp>" << endl;
		return 0;
	}

	ifstream in{argv[1]};
	string text{""}, line{""};
	while(getline(in, line))
		text += line + "\n";

	try {
		Context ctx{text};
	} catch(string &ex) {
		cerr << ex << endl;
	}
	return 0;
}

