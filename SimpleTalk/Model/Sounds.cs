using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using WMPLib;

namespace SimpleTalk.Model
{
    public enum SoundFiles
    {
        Ja,
        Nee
    }

    public class Sounds
    {
        private string _baseDirectory;

        public WindowsMediaPlayer MediaPlayer;

        public Sounds()
        {
            MediaPlayer = new WindowsMediaPlayer();

            _baseDirectory = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "AudioFiles");
        }


        public void PlaySound(SoundFiles soundFiles)
        {
            switch (soundFiles)
            {
                case SoundFiles.Ja:
                    {
                        MediaPlayer.URL = Path.Combine(_baseDirectory, "Ja.wav");
                        break;
                    }
                case SoundFiles.Nee:
                    {
                        MediaPlayer.URL = Path.Combine(_baseDirectory, "Nee.wav");
                        break;
                    }
                default:
                    {
                        break;
                    }
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
