/*
 * phc -- the open source PHP compiler
 * See doc/license/README.license for licensing information
 *
 * The sole purpose of Object is to guarantee a polymorpic base
 * (i.e., a base that supports RTTI)
 */

#ifndef PHC_OBJECT
#define PHC_OBJECT

class Object
{
// Make Object a virtual base (required for RTTI and dynamic casts)
public:
	virtual ~Object() {}

// Objects should support cloning
public:
	virtual Object* clone() = 0;
};

#endif /* OBJECT_H */
