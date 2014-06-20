//
// Lol Engine
//
// Copyright: (c) 2010-2011 Sam Hocevar <sam@hocevar.net>
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the Do What The Fuck You Want To
//   Public License, Version 2, as published by Sam Hocevar. See
//   http://sam.zoy.org/projects/COPYING.WTFPL for more details.
//

//
// The Timer class
// ---------------
//

#if !defined __LOL_TIMER_H__
#define __LOL_TIMER_H__

namespace lol
{

class TimerData;

class Timer
{
public:
    Timer();
    ~Timer();

    float GetMs();
    float PollMs();
    void WaitMs(float deltams);

private:
    TimerData *data;
};

} /* namespace lol */

#endif // __LOL_TIMER_H__

