struct Attribute { string name; };
struct UserAttribute {
	string value;
	int lastModified;
};
struct User {
	string name;
	string hash;
	int lastLogin;

	map<Attribute::id_t, UserAttribute> attributes;
};
