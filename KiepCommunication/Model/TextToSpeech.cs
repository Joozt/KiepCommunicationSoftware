using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Diagnostics;
using System.Runtime.InteropServices;



namespace KiepCommunication.Model
{

    public class TextToSpeech : IDisposable
    {
        public Process _TextToSpeechProcess;

        public TextToSpeech()
        {
            _TextToSpeechProcess = new Process();

            _TextToSpeechProcess.StartInfo.FileName = "cmd.exe";
            _TextToSpeechProcess.StartInfo.RedirectStandardInput = true;
            _TextToSpeechProcess.StartInfo.RedirectStandardOutput = true;
            _TextToSpeechProcess.StartInfo.CreateNoWindow = true;
            _TextToSpeechProcess.StartInfo.UseShellExecute = false;

            _TextToSpeechProcess.Start();

            _TextToSpeechProcess.StandardInput.WriteLine("set PATH=.;.\\lib\\etc");
            _TextToSpeechProcess.StandardInput.WriteLine("set HOME=C:");
            _TextToSpeechProcess.StandardInput.WriteLine("cd .\\FestivalBinaries\\nextens");
            _TextToSpeechProcess.StandardInput.WriteLine("sh -c \"./bin/festival.exe --libdir ./lib");
        }

        #region IDisposable Members

        public void Dispose()
        {
            _TextToSpeechProcess.StandardInput.WriteLine("(quit)");
            _TextToSpeechProcess.StandardInput.WriteLine("exit");
            _TextToSpeechProcess.WaitForExit();

            GC.SuppressFinalize(this);
        }

        #endregion

        public void Say(string text)
        {
            if (!String.IsNullOrEmpty(text))
            {
                _TextToSpeechProcess.StandardInput.WriteLine("(SayText \"" + text + "\");");
                _TextToSpeechProcess.StandardInput.Flush();
            }
        }
    }
}

