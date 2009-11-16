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



namespace SimpleTalk.Model
{
    class ActiveWindow
    {
        [DllImport("user32.dll")]

        static extern int GetForegroundWindow();

        public int GetActiveWindow()
        {
            const int nChars = 256;
            int handle = 0;
            StringBuilder Buff = new StringBuilder(nChars);

            handle = GetForegroundWindow();

            return handle;
        }
    }

  class TextToSpeech
  {
    public void Say(string text, int AW1)
    {
        
        if (text != "")
        {
            ActiveWindow AW = new ActiveWindow();
            int W2 = AW.GetActiveWindow();
            SetForegroundWindow(AW1);

            SendKeys.SendWait("{(}SayText \" " + text + "\"{)} {enter}");

            SetForegroundWindow(W2);
        }
    }
    [DllImport("User32.dll")]
    public static extern bool DestroyWindow(int hWnd);

    public void StopSpeaking(int W2)
    {
        
        //SetForegroundWindow(W2);
        //SendKeys.SendWait("{(}quit{)} {enter}");
        //System.Threading.Thread.Sleep(3000);
        //initFestival();
        
    }

    //public void CreateText(string text1)
    //{

    //    string[] lines = { "(set! u0000 (SynthText \"" + text1 + "\"));", "(ResynthCurrentInton u0000);" };

    //    string folder = @"C:\Project\festivalbinaries\nextens\test.txt";
    //    FileStream file = new FileStream(folder, FileMode.Create, FileAccess.Write);
    //    StreamWriter sw = new StreamWriter(file);

    //    foreach (string line in lines)
    //    {
    //        sw.WriteLine(line);
    //    }

    //    sw.Close();
    //    file.Close();
    //}

    [DllImport("User32.dll")]
    public static extern Int32 SetForegroundWindow(int hWnd);

    public void initFestival()
    {
        SendKeys.SendWait("cd festivalbinaries\\nextens {enter}");
        SendKeys.SendWait("set PATH=.;.\\lib\\etc{enter}");
        SendKeys.SendWait("set HOME=C:{enter}");

        SendKeys.SendWait("sh -c \"./bin/festival.exe --libdir ./lib\"{enter}");
    }

    public void MakeProcess()
    {
        
        try
        {
            Process myProcess = Process.Start("cmd.exe");
            

            if (myProcess.Responding)
            {
                initFestival(); 
                
            }
            else
            {
                myProcess.Kill();
            
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine("Exception Occurred :{0},{1}", ex.Message, ex.StackTrace.ToString());
            
        }
    }
  }
}

