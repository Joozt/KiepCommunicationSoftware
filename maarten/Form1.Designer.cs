namespace WindowsFormsApplication1
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
            this.button_play = new System.Windows.Forms.Button();
            this.button_open = new System.Windows.Forms.Button();
            this.button_stop = new System.Windows.Forms.Button();
            this.button_volup = new System.Windows.Forms.Button();
            this.button_voldown = new System.Windows.Forms.Button();
            this.WMP1 = new AxWMPLib.AxWindowsMediaPlayer();
            this.button_speak = new System.Windows.Forms.Button();
            this.textBox_speak = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.textBox_typed = new System.Windows.Forms.TextBox();
            ((System.ComponentModel.ISupportInitialize)(this.WMP1)).BeginInit();
            this.SuspendLayout();
            // 
            // button_play
            // 
            this.button_play.Location = new System.Drawing.Point(28, 151);
            this.button_play.Name = "button_play";
            this.button_play.Size = new System.Drawing.Size(324, 23);
            this.button_play.TabIndex = 0;
            this.button_play.Text = "Play";
            this.button_play.UseVisualStyleBackColor = true;
            this.button_play.Click += new System.EventHandler(this.button1_Click);
            // 
            // button_open
            // 
            this.button_open.Location = new System.Drawing.Point(28, 100);
            this.button_open.Name = "button_open";
            this.button_open.Size = new System.Drawing.Size(324, 23);
            this.button_open.TabIndex = 1;
            this.button_open.Text = "Open File";
            this.button_open.UseVisualStyleBackColor = true;
            this.button_open.Click += new System.EventHandler(this.button2_Click);
            // 
            // button_stop
            // 
            this.button_stop.Location = new System.Drawing.Point(28, 180);
            this.button_stop.Name = "button_stop";
            this.button_stop.Size = new System.Drawing.Size(324, 26);
            this.button_stop.TabIndex = 2;
            this.button_stop.Text = "Stop";
            this.button_stop.UseVisualStyleBackColor = true;
            this.button_stop.Click += new System.EventHandler(this.button_stop_Click);
            // 
            // button_volup
            // 
            this.button_volup.Location = new System.Drawing.Point(28, 228);
            this.button_volup.Name = "button_volup";
            this.button_volup.Size = new System.Drawing.Size(164, 23);
            this.button_volup.TabIndex = 3;
            this.button_volup.Text = "Vol UP";
            this.button_volup.UseVisualStyleBackColor = true;
            this.button_volup.Click += new System.EventHandler(this.button_volup_Click);
            // 
            // button_voldown
            // 
            this.button_voldown.Location = new System.Drawing.Point(198, 228);
            this.button_voldown.Name = "button_voldown";
            this.button_voldown.Size = new System.Drawing.Size(154, 23);
            this.button_voldown.TabIndex = 4;
            this.button_voldown.Text = "Vol Down";
            this.button_voldown.UseVisualStyleBackColor = true;
            this.button_voldown.Click += new System.EventHandler(this.button_voldown_Click);
            // 
            // WMP1
            // 
            this.WMP1.Enabled = true;
            this.WMP1.Location = new System.Drawing.Point(358, 40);
            this.WMP1.Name = "WMP1";
            this.WMP1.OcxState = ((System.Windows.Forms.AxHost.State)(resources.GetObject("WMP1.OcxState")));
            this.WMP1.Size = new System.Drawing.Size(405, 211);
            this.WMP1.TabIndex = 5;
            // 
            // button_speak
            // 
            this.button_speak.Location = new System.Drawing.Point(595, 303);
            this.button_speak.Name = "button_speak";
            this.button_speak.Size = new System.Drawing.Size(168, 23);
            this.button_speak.TabIndex = 6;
            this.button_speak.Text = "Speak!";
            this.button_speak.UseVisualStyleBackColor = true;
            this.button_speak.Click += new System.EventHandler(this.button_speak_Click);
            // 
            // textBox_speak
            // 
            this.textBox_speak.Location = new System.Drawing.Point(28, 305);
            this.textBox_speak.Name = "textBox_speak";
            this.textBox_speak.Size = new System.Drawing.Size(561, 20);
            this.textBox_speak.TabIndex = 7;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(25, 334);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(67, 13);
            this.label1.TabIndex = 9;
            this.label1.Text = "Typed Input:";
            // 
            // textBox_typed
            // 
            this.textBox_typed.Location = new System.Drawing.Point(98, 331);
            this.textBox_typed.Name = "textBox_typed";
            this.textBox_typed.Size = new System.Drawing.Size(167, 20);
            this.textBox_typed.TabIndex = 10;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(775, 438);
            this.Controls.Add(this.textBox_typed);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.textBox_speak);
            this.Controls.Add(this.button_speak);
            this.Controls.Add(this.WMP1);
            this.Controls.Add(this.button_voldown);
            this.Controls.Add(this.button_volup);
            this.Controls.Add(this.button_stop);
            this.Controls.Add(this.button_open);
            this.Controls.Add(this.button_play);
            this.Name = "Form1";
            this.Text = "Form1";
            //this.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.Form1_KeyPress);
            //this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Form1_KeyDown);
            ((System.ComponentModel.ISupportInitialize)(this.WMP1)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button button_play;
        private System.Windows.Forms.Button button_open;
        private System.Windows.Forms.Button button_stop;
        private System.Windows.Forms.Button button_volup;
        private System.Windows.Forms.Button button_voldown;
        private AxWMPLib.AxWindowsMediaPlayer WMP1;
        private System.Windows.Forms.Button button_speak;
        private System.Windows.Forms.TextBox textBox_speak;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox textBox_typed;
    }
}

