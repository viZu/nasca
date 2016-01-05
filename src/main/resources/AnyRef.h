#ifndef ROOT_SCALA_ANYREF_H
#define ROOT_SCALA_ANYREF_H

#include <memory>
#include "Any.h"

namespace _root__scala {

	class AnyRef : public Any {

		public:
			bool eq(std::shared_ptr<_root__scala::Any>);
			bool ne(std::shared_ptr<_root__scala::Any>);

	};
	
	
}


#endif