#ifndef SCALA_ANYREF_H
#define SCALA_ANYREF_H

#include <memory>
#include "Any.h"

namespace scala {

	class AnyRef : public Any {

		public:
			bool eq(std::shared_ptr<scala::Any>);
			bool ne(std::shared_ptr<scala::Any>);

	};
	
	
}


#endif