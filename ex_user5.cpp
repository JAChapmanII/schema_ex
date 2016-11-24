struct Attribute { string name; };
struct User {
	string name;
	string hash;
	int lastLogin;

	map<Attribute::id_t, string> attributes;
};
