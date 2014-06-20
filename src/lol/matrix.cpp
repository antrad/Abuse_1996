//
// Lol Engine
//
// Copyright: (c) 2010-2011 Sam Hocevar <sam@hocevar.net>
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the Do What The Fuck You Want To
//   Public License, Version 2, as published by Sam Hocevar. See
//   http://sam.zoy.org/projects/COPYING.WTFPL for more details.
//

#if defined HAVE_CONFIG_H
#   include "config.h"
#endif

#include <cstdlib> /* free() */
#include <cstring> /* strdup() */

#include "lol/matrix.h"

using namespace std;

namespace lol
{

static inline float det3(float a, float b, float c,
                         float d, float e, float f,
                         float g, float h, float i)
{
    return a * (e * i - h * f)
         + b * (f * g - i * d)
         + c * (d * h - g * e);
}

static inline float cofact3(mat4 const &mat, int i, int j)
{
    return det3(mat[(i + 1) & 3][(j + 1) & 3],
                mat[(i + 2) & 3][(j + 1) & 3],
                mat[(i + 3) & 3][(j + 1) & 3],
                mat[(i + 1) & 3][(j + 2) & 3],
                mat[(i + 2) & 3][(j + 2) & 3],
                mat[(i + 3) & 3][(j + 2) & 3],
                mat[(i + 1) & 3][(j + 3) & 3],
                mat[(i + 2) & 3][(j + 3) & 3],
                mat[(i + 3) & 3][(j + 3) & 3]) * (((i + j) & 1) ? -1.0f : 1.0f);
}

template<> float mat4::det() const
{
    float ret = 0;
    for (int n = 0; n < 4; n++)
        ret += (*this)[n][0] * cofact3(*this, n, 0);
    return ret;
}

template<> mat4 mat4::invert() const
{
    mat4 ret;
    float d = det();
    if (d)
    {
        d = 1.0f / d;
        for (int j = 0; j < 4; j++)
            for (int i = 0; i < 4; i++)
                ret[j][i] = cofact3(*this, i, j) * d;
    }
    return ret;
}

template<> void mat4::printf() const
{
#if 0
    mat4 const &p = *this;

    Log::Debug("[ %6.6f %6.6f %6.6f %6.6f\n",
               p[0][0], p[1][0], p[2][0], p[3][0]);
    Log::Debug("  %6.6f %6.6f %6.6f %6.6f\n",
               p[0][1], p[1][1], p[2][1], p[3][1]);
    Log::Debug("  %6.6f %6.6f %6.6f %6.6f\n",
               p[0][2], p[1][2], p[2][2], p[3][2]);
    Log::Debug("  %6.6f %6.6f %6.6f %6.6f ]\n",
               p[0][3], p[1][3], p[2][3], p[3][3]);
#endif
}

template<> mat4 mat4::ortho(float left, float right, float bottom,
                            float top, float near, float far)
{
    float invrl = (right != left) ? 1.0f / (right - left) : 0.0f;
    float invtb = (top != bottom) ? 1.0f / (top - bottom) : 0.0f;
    float invfn = (far != near) ? 1.0f / (far - near) : 0.0f;

    mat4 ret(0.0f);
    ret[0][0] = 2.0f * invrl;
    ret[1][1] = 2.0f * invtb;
    ret[2][2] = -2.0f * invfn;
    ret[3][0] = - (right + left) * invrl;
    ret[3][1] = - (top + bottom) * invtb;
    ret[3][2] = - (far + near) * invfn;
    ret[3][3] = 1.0f;
    return ret;
}

template<> mat4 mat4::frustum(float left, float right, float bottom,
                              float top, float near, float far)
{
    float invrl = (right != left) ? 1.0f / (right - left) : 0.0f;
    float invtb = (top != bottom) ? 1.0f / (top - bottom) : 0.0f;
    float invfn = (far != near) ? 1.0f / (far - near) : 0.0f;

    mat4 ret(0.0f);
    ret[0][0] = 2.0f * near * invrl;
    ret[1][1] = 2.0f * near * invtb;
    ret[2][0] = (right + left) * invrl;
    ret[2][1] = (top + bottom) * invtb;
    ret[2][2] = - (far + near) * invfn;
    ret[2][3] = -1.0f;
    ret[3][2] = -2.0f * far * near * invfn;
    return ret;
}

template<> mat4 mat4::perspective(float theta, float width,
                                  float height, float near, float far)
{
    float t1 = tanf(theta / 2.0f);
    float t2 = t1 * height / width;

    return frustum(-near * t1, near * t1, -near * t2, near * t2, near, far);
}

template<> mat4 mat4::translate(float x, float y, float z)
{
    mat4 ret(1.0f);
    ret[3][0] = x;
    ret[3][1] = y;
    ret[3][2] = z;
    return ret;
}

template<> mat4 mat4::rotate(float theta, float x, float y, float z)
{
    float st = sinf(theta);
    float ct = cosf(theta);

    float len = sqrtf(x * x + y * y + z * z);
    float invlen = len ? 1.0f / len : 0.0f;
    x *= invlen;
    y *= invlen;
    z *= invlen;

    float mtx = (1.0f - ct) * x;
    float mty = (1.0f - ct) * y;
    float mtz = (1.0f - ct) * z;

    mat4 ret(1.0f);

    ret[0][0] = x * mtx + ct;
    ret[0][1] = x * mty + st * z;
    ret[0][2] = x * mtz - st * y;

    ret[1][0] = y * mtx - st * z;
    ret[1][1] = y * mty + ct;
    ret[1][2] = y * mtz + st * x;

    ret[2][0] = z * mtx + st * y;
    ret[2][1] = z * mty - st * x;
    ret[2][2] = z * mtz + ct;

    return ret;
}

} /* namespace lol */

