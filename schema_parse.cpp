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
template<typename... Args> string primaryKeyConstraint(Args... args) {
	vector<string> vals = {args...};
	string res = tableCase(vals.front()) + "_id";
	for(int i = 1; i < vals.size(); ++i)
		res += ", " + tableCase(vals[i]) + "_id";
	return "PRIMARY KEY(" + res + ")";
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

		// compute maxDepth
		int maxDepth = 0;
		for(auto &tde : typeDepth)
			maxDepth = max(maxDepth, tde.second);

		// print pure child types
		for(auto &te : types) {
			if(te.second.pureChild)
				cerr << "pure child: " << te.second.name
					<< " => " << pureChildFieldNames[te.second.name] << endl;
		}

		// in depth order, print create tables for struct types
		for(int i = 0; i <= maxDepth; ++i) {
			for(auto &tde : typeDepth) {
				if(tde.second != i)
					continue;
				if(isIdType(tde.first))
					continue;
				if(!types[tde.first].composite)
					continue;
				if(types[tde.first].pureChild)
					continue;
				cerr << tde.second << " => " << tde.first << endl;
				vector<string> tableFields{};
				vector<string> tableConstraints{};
				if(parentTypes.find(tde.first) == parentTypes.end()) {
					tableFields.push_back("`" + tableCase(tde.first) + "_id`"
							+ " UNIQUEIDENTIFIER PRIMARY KEY");
				} else {
					vector<string> keyNames{};
					vector<string> ptNames{};
					string t = tde.first;
					bool doAdvance = false;
					do {
						if(doAdvance)
							t = parentTypes[t];
						else
							doAdvance = true;

						ptNames.push_back(pluralize(tableCase(t)));
						keyNames.push_back(tableCase(t) + "_id");
						tableFields.push_back("`" + keyNames.back() + "`"
								+ " UNIQUEIDENTIFIER NOT NULL");
					} while(parentTypes.find(t) != parentTypes.end());
					for(int i = 1; i < ptNames.size(); ++i)
						tableConstraints.push_back("FOREIGN KEY(" + keyNames[i] + ")"
								+ " REFERENCES " + ptNames[i] + "(" + keyNames[i] + ")");
					string pkField = "PRIMARY KEY(" + keyNames[0];
					for(int i = 1; i < keyNames.size(); ++i)
						pkField += ", " + keyNames[i];
					pkField += ")";
					tableConstraints.push_back(pkField);
				}
				auto &type = types[tde.first];
				for(int i = 0; i < type.fields.size(); ++i) {
					auto &field = type.fields[i];
					auto &ft = types[field.type];
					if(ft.composite)
						continue;
					if(field.type == tde.first + "::id_t")
						continue;
					if(field.type.size() >= 6 // TODO: helper method this
							&& field.type.substr(field.type.size() - 6) == "::id_t") {
						string otype = field.type.substr(0, field.type.size() - 6);
						tableFields.push_back("`" + tableCase(field.name) + "_id`"
								+ " UNIQUEIDENTIFIER NOT NULL");
						tableConstraints.push_back(
								"FOREIGN KEY(" + tableCase(field.name) + "_id)"
								+ " REFERENCES " + pluralize(tableCase(otype))
								+ "(" + tableCase(otype) + "_id)");
						continue;
					}
					if(field.type == "int" || field.type == "bool") {
						tableFields.push_back("`" + tableCase(field.name) + "`" + " INT NOT NULL");
						continue;
					}
					if(field.type == "string") {
						tableFields.push_back("`" + tableCase(field.name) + "`" + " TEXT NOT NULL");
						continue;
					}
					if(ft.name == "map") continue;
					if(ft.name == "set") continue;
					if(ft.name == "vector") continue;
					throw "unable to do simple field type " + field.type;
				}
				cout << formatTable(createTableHeader(tde.first),
						tableFields, tableConstraints, createTableFooter()) << endl;
			}
		}

		// in depth order, print child relations that aren't pure
		for(int i = 0; i <= maxDepth; ++i) {
			for(auto &tde : typeDepth) {
				if(tde.second != i)
					continue;
				if(isIdType(tde.first))
					continue;
				if(!types[tde.first].composite)
					continue;
				if(types[tde.first].pureChild)
					continue;

				auto &type = types[tde.first];
				for(auto &field : type.fields) {
					auto &ft = types[field.type];
					if(ft.name == "set") {
						//string subType = ft.typeArguments[0];
						//cerr << "-- field " << field.name << " is table" << endl;
						continue;
					}
					if(ft.name == "map") {
						string keyType = ft.typeArguments[0], valType = ft.typeArguments[1];
						auto &kt = types[keyType];
						auto &vt = types[valType];
						if(vt.composite) {
							// TODO: this is child table but not being done correctly?
							cerr << "-- TODO: field "
								<< tde.first << "::" << field.name
								<< " is table" << endl;
							continue;
						}
						cerr << "-- TODO: " << tde.first << "::" << field.name << endl;
						string keyTypeType = keyType.substr(0, keyType.size() - 6);
						cout << formatTable(
								createTableHeader(tde.first + "_" + field.name),
								{
									foreignKeyField(tde.first),
									foreignKeyField(keyTypeType, field.name),
									typedField(valType, "val")
								},
								{
									foreignKeyConstraint(tde.first),
									foreignKeyConstraint(keyTypeType, field.name),
									primaryKeyConstraint(tde.first, field.name + "_" + keyTypeType)
								}, createTableFooter()
							) << endl;
					}
					if(ft.name == "vector") {
						string subType = ft.typeArguments[0];
						string tn1 = tableCase(tde.first), tn2 = tableCase(subType);
						cerr << "-- look for me =========================" << endl;

						cout << formatTable(
								createTableHeader(tde.first + "_" + field.name),
								{
									foreignKeyField(tde.first),
									foreignKeyField(subType)
								},
								{
									foreignKeyConstraint(tde.first),
									foreignKeyConstraint(subType),
									primaryKeyConstraint(tde.first, subType)
								}, createTableFooter()
							) << endl;
					}
				}
			}
		}

		for(int i = 0; i <= maxDepth; ++i) {
			for(auto &tde : typeDepth) {
				if(tde.second != i)
					continue;
				if(isIdType(tde.first))
					continue;
				if(!types[tde.first].composite)
					continue;
				if(!types[tde.first].pureChild)
					continue;

				auto &type = types[tde.first];
				string parentType = parentTypes[tde.first];
				cerr << "-- pure child " << tde.first << " of " << parentType << endl;

				string parentFieldName = pureChildFieldNames[tde.first];
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

				string parentFieldKeyTypeType = parentFieldKeyType.substr(
						0, parentFieldKeyType.size() - 6);
				fields.push_back(foreignKeyField(parentFieldKeyTypeType));
				constraints.push_back(foreignKeyConstraint(parentFieldKeyTypeType));

				constraints.push_back(primaryKeyConstraint(
							parentType, parentFieldKeyTypeType));

				for(auto &field : type.fields) {
					auto &ft = types[field.type];
					// should only have built in fields and ::id_t

					if(isIdType(ft.name)) {
						string ftType = ft.name.substr(0, ft.name.size() - 6);
						if(ftType == tde.first)
							continue; // don't print key type for self
						fields.push_back(foreignKeyField(ftType));
						constraints.push_back(foreignKeyConstraint(ftType));
						continue;
					}
					fields.push_back(typedField(ft.name, field.name));
				}

				cout << formatTable(
						createTableHeader(tde.first),
						fields, constraints, createTableFooter()) << endl;
			}
		}
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

