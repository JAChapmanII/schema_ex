#include "parser.hpp"
using std::string;
using std::to_string;
using std::vector;
using std::map;
using std::set;

#include <algorithm>
using std::max;
using std::min;

#include <iostream>
using std::cerr;
using std::endl;

#include <oil/util.hpp>

#include "sql.hpp"


parser::Field::Field(string itype, string iname) : type{itype}, name{iname} { }

parser::Type::Type(string iname, bool icomposite)
	: name{iname}, composite{icomposite} { }
parser::Type::Type() { }

string parser::Type::toString(Context *context) {
	if(typeArguments.empty())
		return name;
	string str = name + "<" + context->types[typeArguments[0]].toString(context);
	for(size_t i = 1; i < typeArguments.size(); ++i)
		str += ", " + context->types[typeArguments[i]].toString(context);
	return str + ">";
}


parser::Context::Context(string isrc) : src{isrc} {
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

	// print pure child types
	for(auto &te : types) {
		if(te.second.pureChild)
			cerr << "pure child: " << te.second.name
				<< " => " << pureChildFieldNames[te.second.name] << endl;
	}
}

vector<string> parser::Context::getOrderedNonIdTypes() {
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

int parser::Context::getTypeDepth(string t) {
	int depth = 0;
	while(util::contains(parentTypes, t)) {
		depth++;
		t = parentTypes[t];
	}
	return depth;
}
void parser::Context::buildTypeDepths() {
	for(auto &te : types) {
		string bt = te.second.name;
		typeDepth[bt] = getTypeDepth(bt);
	}
}

void parser::Context::buildParentTypes() {
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
				setChildFieldNames[cType] = field.name;
			}
		}
	}
}
vector<string> parser::Context::getParentTypeChain(string type) {
	vector<string> chain;
	chain.push_back(type);

	while(util::contains(parentTypes, type)) {
		type = parentTypes[type];
		chain.push_back(type);
	}
	return chain;
}

void parser::Context::skipSpaces() {
	while(isspace(src[idx]))
		++idx;
	if(is("//")) {
		while(!is("\n"))
			idx++;
	}
}
bool parser::Context::is(string token) {
	if(idx + token.size() >= src.size())
		return false;
	for(size_t i = 0; i < token.size(); ++i)
		if(src[idx + i] != token[i])
			return false;
	return true;
}
void parser::Context::consume(string token) {
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

string parser::Context::parseAttribute() {
	consume("[[");
	string ident = parseIdentifier();
	consume("]]");
	return ident;
}
string parser::Context::parseIdentifier() {
	skipSpaces();
	string ident = "";
	while(idx < src.size() && !isspace(src[idx])
			&& src[idx] != '<' && src[idx] != '>'
			&& src[idx] != ']' && src[idx] != ';'
			&& src[idx] != ',') {
		ident += src[idx];
		idx++;
	}
	skipSpaces();
	return ident;
}
string parser::Context::parseCompoundIdentifier() {
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
parser::Field parser::Context::parseField() {
	string fieldType = parseCompoundIdentifier();
	string fieldName = parseIdentifier();
	consume(";");

	return Field{fieldType, fieldName};
}
void parser::Context::parseType() {
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
		while(isspace(src[idx]) || is("//"))
			skipSpaces();
	}
	consume("}");
	consume(";");

	for(auto &field : types[typeName].fields) {
		cerr << "\t" << field.type << " => " << field.name << endl;
	}
}
void parser::Context::parse() {
	while(idx < src.size()) {
		skipSpaces();
		parseType();
	}

	if(usedTranslatable)
		declStructType("Language", true);
}

void parser::Context::makeTranslatable(string typeName) {
	types[typeName].fields.push_back(Field{"string", "identifier"});
	string lmType = "map<Language::id_t, string>";
	types[lmType] = Type{"map", false};
	types[lmType].typeArguments.push_back("Language::id_t");
	types[lmType].typeArguments.push_back("string");
	types[typeName].fields.push_back(Field{lmType, "translations"});
};
void parser::Context::declType(string type, bool composite) {
	if(util::contains(types, type))
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
void parser::Context::declStructType(string typeName, bool translatable) {
	declType(typeName);
	declType(typeName + "::id_t");
	types[typeName].fields.push_back(Field{typeName + "::id_t", "id"});

	if(translatable) {
		usedTranslatable = true;
		makeTranslatable(typeName);
	}
}

