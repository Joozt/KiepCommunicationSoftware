using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Drawing;
using SimpleTalk.Model;

namespace SimpleTalk.GUI
{
    public class MostButton: Button, ICustomButton
    {
        string _Keys = "";
        bool _AutoSizeFont = true;
        bool _AutoRescan = true;
        bool _Skip = false;

        public MostButton()
        {
          Font = Core.DefaultFont;
          FlatStyle = FlatStyle.Flat;
        }

        public MostButton(string keys) : this()
        {
            _Keys = keys;
        }

        #region ICustomButton Members

        public bool AutoFontSize
        {
            get
            {
                return _AutoSizeFont;
            }
            set
            {
                _AutoSizeFont = value;
            }
        }

        public string Keys
        {
            get
            {
                return _Keys;
            }
            set
            {
                _Keys = value;
            }
        }

        public bool AutoRescan
        {
            get
            {
                return _AutoRescan;
            }
            set
            {
                _AutoRescan = value;
            }
        }

        public bool Skip
        {
            get
            {
                return _Skip;
            }
            set
            {
                _Skip = value;
            }
        }

        #endregion

        private void InitializeComponent()
        {
            this.SuspendLayout();
            // 
            // MostButton
            // 
            this.Margin = new System.Windows.Forms.Padding(1);

            this.ResumeLayout(false);
        }

        protected override void OnPaint(PaintEventArgs pevent)
        {
            base.OnPaint(pevent);
        }

    }
}
