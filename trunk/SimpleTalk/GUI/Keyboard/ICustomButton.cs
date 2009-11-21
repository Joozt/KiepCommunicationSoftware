using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleTalk.GUI
{
    public interface ICustomButton
    {
        bool AutoFontSize
        {
            get;
            set;
        }

        string Keys
        {
            get;
            set;
        }

        bool AutoRescan
        {
            get;
            set;
        }
    }
}
