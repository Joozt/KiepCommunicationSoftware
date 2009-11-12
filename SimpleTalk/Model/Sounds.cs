using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using WMPLib;

namespace SimpleTalk.Model
{
    class Sounds
    {
        private string _ProjectPath;

        public WindowsMediaPlayer MediaPlayer;

        public Sounds()
        {
            MediaPlayer = new WindowsMediaPlayer();

            _ProjectPath = AppDomain.CurrentDomain.BaseDirectory;
        }


        public void PlaySound(string filename)
        {
            MediaPlayer.URL = System.IO.Path.Combine(_ProjectPath, string.Format(@"AudioFiles\{0}", filename));

        }

        public void StopSound()
        {
            throw new NotImplementedException();
        }

        public void VolumeUp()
        {
            throw new NotImplementedException();
        }

        public void VolumeDown()
        {
            throw new NotImplementedException();
        }
    }
}
