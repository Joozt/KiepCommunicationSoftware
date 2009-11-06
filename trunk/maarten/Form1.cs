using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using SpeechLib;


namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        bool PlusPressed;
        public string path;
        public SpVoice VoiceOfJoost = new SpVoice();
        private Timer timer;
        [DllImport("user32.dll")]
        public static extern ushort GetKeyState(short nVirtKey);
        public const ushort keyDownBit = 0x80;


        public Form1()
        {
            InitializeComponent();
            WMP1.settings.volume = 50;
            VoiceOfJoost.Rate = -2;
            PlusPressed = false;
            this.timer = new Timer();
            timer.Interval = 1000;
            timer.Tick += new EventHandler(timer_Tick);
            timer.Start();

        }

        void timer_Tick(object sender, EventArgs e)
        {
            //var PlusPressed = 
            if ((GetKeyState((short)Keys.Add) & GetKeyState((short)Keys.Subtract) & keyDownBit) == keyDownBit) ChangeLabel(" +-");
            else if ((GetKeyState((short)Keys.Add) & keyDownBit) == keyDownBit ) ChangeLabel(" +");
            else if ((GetKeyState((short)Keys.Subtract) & keyDownBit) == keyDownBit) ChangeLabel(" -");
            
            //throw new NotImplementedException();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            WMP1.URL = this.path;            

        }        
        
        private void button2_Click(object sender, EventArgs e)
        {
            OpenFileDialog dialog = new OpenFileDialog();
            dialog.Filter = "Audio Files |*.*";


            if (dialog.ShowDialog() == DialogResult.OK)
            {
                this.path = dialog.FileName;                
            }
        }

        private void button_stop_Click(object sender, EventArgs e)
        {
            WMP1.Ctlcontrols.stop();

        }

        private void button_voldown_Click(object sender, EventArgs e)
        {
            volDOWN();
        }

        private void button_volup_Click(object sender, EventArgs e)
        {
            volUP();
        }



        private void volUP()
        {
            if (WMP1.settings.volume <= 90) WMP1.settings.volume = WMP1.settings.volume + 10;
            else WMP1.settings.volume = 100;
        }

        private void volDOWN()
        {
            if (WMP1.settings.volume >= 10) WMP1.settings.volume = WMP1.settings.volume - 10;
            else WMP1.settings.volume = 0;

        }

        private void button_speak_Click(object sender, EventArgs e)
        {
            VoiceOfJoost.Speak(textBox_speak.Text, SpeechVoiceSpeakFlags.SVSFDefault);
        }





        

        
        public void ChangeLabel(String strs)
        {

            textBox_typed.Text = textBox_typed.Text + strs;
        }

        
        


    }


   

}
