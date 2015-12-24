#ifndef SCALA_ANYREF_CPP
#define SCALA_ANYREF_CPP

#include "AnyRef.h"

bool scala::AnyRef::eq(std::shared_ptr<scala::Any> ptr) {
	return this == ptr.get();
}

bool scala::AnyRef::ne(std::shared_ptr<scala::Any> ptr) {
	return this != ptr.get();
}

#endif