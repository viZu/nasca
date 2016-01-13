#ifndef ROOT_SCALA_ANY_H
#define ROOT_SCALA_ANY_H

#include <memory>
#include <ostream>
#include <sstream>

namespace _root__scala {

	class Any {

		public:
			bool __bang__eq(std::shared_ptr<_root__scala::Any>);
			bool __eq__eq(std::shared_ptr<_root__scala::Any>);
			bool equals(std::shared_ptr<_root__scala::Any>);
			int hashCode();

            /*virtual std::string toString() const {
                const void * address = static_cast<const void*>(this);
                std::stringstream ss;
                ss << typeid(*this).name() << "@" << address;
                return ss.str();
            }

			friend std::ostream& operator<<(std::ostream &strm, const Any &a) {
                return strm << a.toString();
            }

            friend std::ostream& operator<<(std::ostream &strm, Any* a) {
                return strm << a->toString();
            }

            friend std::ostream& operator<<(std::ostream &strm, std::shared_ptr<Any> &a) {
                return strm << a->toString();
            }*/
	};
	
}

#endif