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

  public class TextToSpeech
  {
      public Process P;

      public TextToSpeech()
      {
          P = CreateProcess();
      }
//      private System.IO.StreamWriter c_StreamInput = null;

    public Process CreateProcess()
    {


        Process cmd = new Process();

        cmd.StartInfo.FileName = "cmd.exe";
        cmd.StartInfo.RedirectStandardInput = true;
        cmd.StartInfo.RedirectStandardOutput = true;
        cmd.StartInfo.CreateNoWindow = true;
        cmd.StartInfo.UseShellExecute = false;

        cmd.Start();

        cmd.StandardInput.WriteLine("cd .\\FestivalBinaries\\nextens");
        cmd.StandardInput.WriteLine(".\\bin\\festival.exe --libdir \"./lib/\"");
        
        return cmd;
    }

    public void Say(string text, Process cmd)
    {

        //Process cmd = new Process();

        //cmd.StartInfo.FileName = "cmd.exe";
        //cmd.StartInfo.RedirectStandardInput = true;
        //cmd.StartInfo.RedirectStandardOutput = true;
        //cmd.StartInfo.CreateNoWindow = true;
        //cmd.StartInfo.UseShellExecute = false;

        //cmd.Start();

        

        //cmd.StandardInput.WriteLine("cd .\\FestivalBinaries\\nextens");
        //cmd.StandardInput.WriteLine(".\\bin\\festival.exe --libdir \"./lib/\"");
        cmd.StandardInput.WriteLine("(set! u0000 (SynthText \"" + text + "\"));");
        cmd.StandardInput.WriteLine("(ResynthCurrentInton u0000);");
        cmd.StandardInput.Flush();
        //cmd.StandardInput.Close();
        
        //string test = cmd.StandardOutput.ReadToEnd();
       // TextWriter tw = new StreamWriter(@"C:\Projectspraak\SimpleTalk\bin\Debug\FestivalBinaries\nextens\test.txt");
       // tw.WriteLine("(set! u0000 (SynthText \"" + text + "\"));");
       // tw.WriteLine("(ResynthCurrentInton u0000);");
       // tw.Close();

        //P.StartInfo.FileName = @".\test.bat";
        
        //P.StartInfo.WorkingDirectory = @"C:\Projectspraak\SimpleTalk\bin\Debug\FestivalBinaries\nextens";

        
        //P.StartInfo.CreateNoWindow = true;
        //if(P.Responding)
       //     P.StandardInput.WriteLine("(SayText \"" + text + "\")");
        

        
        //P.WaitForExit();
        
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

