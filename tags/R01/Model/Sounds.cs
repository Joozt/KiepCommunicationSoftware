using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Media;

namespace SimpleTalk.Model
{
    public enum SoundFiles
    {
        Ja,
        Nee
    }

    public class Sounds
    {
        SoundPlayer _soundPlayer;
        
        public Sounds()
        {
        }

        public void PlaySound(SoundFiles soundFiles)
        {
            switch (soundFiles)
            {
                case SoundFiles.Ja:
                    {
                        _soundPlayer = new SoundPlayer(Properties.Resources.Ja);
                        break;
                    }
                case SoundFiles.Nee:
                    {
                        _soundPlayer = new SoundPlayer(Properties.Resources.Nee);
                        break;
                    }
                default:
                    {
                        break;
                    }
            }

            if (_soundPlayer != null)
            {
              _soundPlayer.Play();
            }
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
