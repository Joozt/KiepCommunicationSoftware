using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using WMPLib;

namespace SimpleTalk.Model
{
  class Sounds
  {
      public WindowsMediaPlayer MediaPlayer;
      
      public Sounds()
      {
      MediaPlayer = new WindowsMediaPlayer();
      }

      
      public void PlaySound(string filename)
    {
        MediaPlayer.URL = filename; 
        
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
