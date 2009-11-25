﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleTalk.GUI
{
    public interface ICustomLayout
    {
        CustomKeyboard Keyboard
        {
            get;
            set;
        }

        void ConstructLayout();

        void AutoFormat();
    }
}
