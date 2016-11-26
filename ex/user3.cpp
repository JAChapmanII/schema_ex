struct Tag { };
struct User {
	string name;
	string hash;
	int lastLogin;

	vector<Tag> tags;
};
