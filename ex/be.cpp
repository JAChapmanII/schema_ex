[[translatable]] struct DamageClass { };

[[translatable]] struct Stat {
	DamageClass::id_t damageClassId;

	bool isBattleOnly;
	int gameIndex;
};

[[translatable]] struct Type {
	DamageClass::id_t damageClassId;

	map<Type::id_t, int> damageFactors;
};

[[translatable]] struct State { };

struct MoveEffect {
	Stat::id_t statId;
	int stageChange;
	int chance;
	int target;
	int when;
};

struct MoveState {
	State::id_t stateId;
	int chance;
	int target;
	int when;
};

[[translatable]] struct Move {
	Type::id_t typeId; // TODO: allow multiple?

	int power;
	int pp;
	int accuracy;
	int priority;
	int target;
	DamageClass::id_t damageClassId;

	set<MoveState> states;
	set<MoveEffect> effects;
};

struct TitanStat {
	int baseValue;
	int effortValue;
};

struct TitanMove {
	//int titanMoveMethod;
	int level;
	int order;
};

struct Species {
	map<Language::id_t, string> names;
	map<Language::id_t, string> genus;
};

[[translatable]] struct Attribute { };

[[translatable]] struct ExperienceGroup { };

[[translatable]] struct Titan {
	Species::id_t speciesId;

	int baseExperience;
	int order;
	bool isDefault;

	map<Attribute::id_t, string> attributes;
	ExperienceGroup::id_t experienceGroupId;

	vector<Type> types;
	map<Stat::id_t, TitanStat> stats;
	map<Move::id_t, TitanMove> moves;
};

[[translatable]]
struct Game {
	set<DamageClass> damageClasses;
	set<State> states;
	set<Stat> stats;
	set<Type> types;
	set<Move> moves;
	set<Attribute> attributes;
	set<ExperienceGroup> experienceGroups;
	set<Species> species;
	set<Titan> titans;
};

