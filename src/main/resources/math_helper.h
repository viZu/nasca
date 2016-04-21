#define _USE_MATH_DEFINES
#include <cmath>

double toDegrees(double radian) {
    return radian * 180.0 / M_PI;
}

double toRadians(double degree) {
    return degree * M_PI / 180;
}