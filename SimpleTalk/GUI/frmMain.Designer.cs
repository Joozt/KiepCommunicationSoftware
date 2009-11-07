using System.Windows.Forms;

namespace SimpleTalk.GUI
{
    partial class frmMain
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
          this.pnlKeyboard = new System.Windows.Forms.Panel();
          this.txtOutput = new System.Windows.Forms.TextBox();
          this.gbTest = new System.Windows.Forms.GroupBox();
          this.label1 = new System.Windows.Forms.Label();
          this.nSelectionTime = new System.Windows.Forms.NumericUpDown();
          this.cbButton2 = new System.Windows.Forms.CheckBox();
          this.cbButton1 = new System.Windows.Forms.CheckBox();
          this.button8 = new System.Windows.Forms.Button();
          this.button7 = new System.Windows.Forms.Button();
          this.button6 = new System.Windows.Forms.Button();
          this.button5 = new System.Windows.Forms.Button();
          this.button4 = new System.Windows.Forms.Button();
          this.button3 = new System.Windows.Forms.Button();
          this.button2 = new System.Windows.Forms.Button();
          this.button1 = new System.Windows.Forms.Button();
          this.lbAutoSuggestions = new System.Windows.Forms.ListBox();
          this.btnGetAutoList = new System.Windows.Forms.Button();
          this.pnlAutoComplete = new System.Windows.Forms.Panel();
          this.button9 = new System.Windows.Forms.Button();
          this.button10 = new System.Windows.Forms.Button();
          this.gbTest.SuspendLayout();
          ((System.ComponentModel.ISupportInitialize)(this.nSelectionTime)).BeginInit();
          this.SuspendLayout();
          // 
          // pnlKeyboard
          // 
          this.pnlKeyboard.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                      | System.Windows.Forms.AnchorStyles.Left)
                      | System.Windows.Forms.AnchorStyles.Right)));
          this.pnlKeyboard.Location = new System.Drawing.Point(12, 142);
          this.pnlKeyboard.Name = "pnlKeyboard";
          this.pnlKeyboard.Size = new System.Drawing.Size(377, 446);
          this.pnlKeyboard.TabIndex = 5;
          // 
          // txtOutput
          // 
          this.txtOutput.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                      | System.Windows.Forms.AnchorStyles.Right)));
          this.txtOutput.Location = new System.Drawing.Point(12, 12);
          this.txtOutput.Multiline = true;
          this.txtOutput.Name = "txtOutput";
          this.txtOutput.Size = new System.Drawing.Size(549, 124);
          this.txtOutput.TabIndex = 6;
          // 
          // gbTest
          // 
          this.gbTest.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                      | System.Windows.Forms.AnchorStyles.Right)));
          this.gbTest.Controls.Add(this.button10);
          this.gbTest.Controls.Add(this.button9);
          this.gbTest.Controls.Add(this.label1);
          this.gbTest.Controls.Add(this.nSelectionTime);
          this.gbTest.Controls.Add(this.cbButton2);
          this.gbTest.Controls.Add(this.cbButton1);
          this.gbTest.Controls.Add(this.button8);
          this.gbTest.Controls.Add(this.button7);
          this.gbTest.Controls.Add(this.button6);
          this.gbTest.Controls.Add(this.button5);
          this.gbTest.Controls.Add(this.button4);
          this.gbTest.Controls.Add(this.button3);
          this.gbTest.Controls.Add(this.button2);
          this.gbTest.Controls.Add(this.button1);
          this.gbTest.Controls.Add(this.lbAutoSuggestions);
          this.gbTest.Controls.Add(this.btnGetAutoList);
          this.gbTest.Location = new System.Drawing.Point(567, 12);
          this.gbTest.Name = "gbTest";
          this.gbTest.Size = new System.Drawing.Size(150, 576);
          this.gbTest.TabIndex = 9;
          this.gbTest.TabStop = false;
          this.gbTest.Text = "Test area";
          // 
          // label1
          // 
          this.label1.AutoSize = true;
          this.label1.Location = new System.Drawing.Point(6, 497);
          this.label1.Name = "label1";
          this.label1.Size = new System.Drawing.Size(73, 13);
          this.label1.TabIndex = 22;
          this.label1.Text = "Sel. time (sec)";
          // 
          // nSelectionTime
          // 
          this.nSelectionTime.DecimalPlaces = 1;
          this.nSelectionTime.Increment = new decimal(new int[] {
            1,
            0,
            0,
            65536});
          this.nSelectionTime.Location = new System.Drawing.Point(85, 495);
          this.nSelectionTime.Maximum = new decimal(new int[] {
            5,
            0,
            0,
            0});
          this.nSelectionTime.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            65536});
          this.nSelectionTime.Name = "nSelectionTime";
          this.nSelectionTime.Size = new System.Drawing.Size(57, 20);
          this.nSelectionTime.TabIndex = 21;
          this.nSelectionTime.Value = new decimal(new int[] {
            1,
            0,
            0,
            65536});
          // 
          // cbButton2
          // 
          this.cbButton2.AutoSize = true;
          this.cbButton2.Location = new System.Drawing.Point(6, 553);
          this.cbButton2.Name = "cbButton2";
          this.cbButton2.Size = new System.Drawing.Size(96, 17);
          this.cbButton2.TabIndex = 20;
          this.cbButton2.Text = "Second button";
          this.cbButton2.UseVisualStyleBackColor = true;
          // 
          // cbButton1
          // 
          this.cbButton1.AutoSize = true;
          this.cbButton1.Location = new System.Drawing.Point(6, 530);
          this.cbButton1.Name = "cbButton1";
          this.cbButton1.Size = new System.Drawing.Size(78, 17);
          this.cbButton1.TabIndex = 19;
          this.cbButton1.Text = "First button";
          this.cbButton1.UseVisualStyleBackColor = true;
          // 
          // button8
          // 
          this.button8.Location = new System.Drawing.Point(6, 466);
          this.button8.Name = "button8";
          this.button8.Size = new System.Drawing.Size(136, 23);
          this.button8.TabIndex = 18;
          this.button8.Text = "Test selection";
          this.button8.UseVisualStyleBackColor = true;
          // 
          // button7
          // 
          this.button7.Location = new System.Drawing.Point(6, 401);
          this.button7.Name = "button7";
          this.button7.Size = new System.Drawing.Size(136, 23);
          this.button7.TabIndex = 17;
          this.button7.Text = "Stop sound";
          this.button7.UseVisualStyleBackColor = true;
          this.button7.Click += new System.EventHandler(this.button7_Click);
          // 
          // button6
          // 
          this.button6.Location = new System.Drawing.Point(6, 372);
          this.button6.Name = "button6";
          this.button6.Size = new System.Drawing.Size(136, 23);
          this.button6.TabIndex = 16;
          this.button6.Text = "Play sound";
          this.button6.UseVisualStyleBackColor = true;
          this.button6.Click += new System.EventHandler(this.button6_Click);
          // 
          // button5
          // 
          this.button5.Location = new System.Drawing.Point(6, 331);
          this.button5.Name = "button5";
          this.button5.Size = new System.Drawing.Size(136, 23);
          this.button5.TabIndex = 15;
          this.button5.Text = "Stop speaking";
          this.button5.UseVisualStyleBackColor = true;
          this.button5.Click += new System.EventHandler(this.button5_Click);
          // 
          // button4
          // 
          this.button4.Location = new System.Drawing.Point(6, 302);
          this.button4.Name = "button4";
          this.button4.Size = new System.Drawing.Size(136, 23);
          this.button4.TabIndex = 14;
          this.button4.Text = "Say text";
          this.button4.UseVisualStyleBackColor = true;
          this.button4.Click += new System.EventHandler(this.button4_Click);
          // 
          // button3
          // 
          this.button3.Location = new System.Drawing.Point(6, 263);
          this.button3.Name = "button3";
          this.button3.Size = new System.Drawing.Size(136, 23);
          this.button3.TabIndex = 13;
          this.button3.Text = "Get interpreter text";
          this.button3.UseVisualStyleBackColor = true;
          this.button3.Click += new System.EventHandler(this.button3_Click);
          // 
          // button2
          // 
          this.button2.Location = new System.Drawing.Point(6, 234);
          this.button2.Name = "button2";
          this.button2.Size = new System.Drawing.Size(136, 23);
          this.button2.TabIndex = 12;
          this.button2.Text = "Process command";
          this.button2.UseVisualStyleBackColor = true;
          this.button2.Click += new System.EventHandler(this.button2_Click);
          // 
          // button1
          // 
          this.button1.Location = new System.Drawing.Point(6, 48);
          this.button1.Name = "button1";
          this.button1.Size = new System.Drawing.Size(136, 23);
          this.button1.TabIndex = 11;
          this.button1.Text = "Reset database";
          this.button1.UseVisualStyleBackColor = true;
          this.button1.Click += new System.EventHandler(this.button1_Click);
          // 
          // lbAutoSuggestions
          // 
          this.lbAutoSuggestions.FormattingEnabled = true;
          this.lbAutoSuggestions.Location = new System.Drawing.Point(6, 77);
          this.lbAutoSuggestions.Name = "lbAutoSuggestions";
          this.lbAutoSuggestions.Size = new System.Drawing.Size(136, 147);
          this.lbAutoSuggestions.TabIndex = 10;
          // 
          // btnGetAutoList
          // 
          this.btnGetAutoList.Location = new System.Drawing.Point(6, 19);
          this.btnGetAutoList.Name = "btnGetAutoList";
          this.btnGetAutoList.Size = new System.Drawing.Size(136, 23);
          this.btnGetAutoList.TabIndex = 9;
          this.btnGetAutoList.Text = "Get autocomplete list";
          this.btnGetAutoList.UseVisualStyleBackColor = true;
          this.btnGetAutoList.Click += new System.EventHandler(this.btnGetAutoList_Click);
          // 
          // pnlAutoComplete
          // 
          this.pnlAutoComplete.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                      | System.Windows.Forms.AnchorStyles.Right)));
          this.pnlAutoComplete.Location = new System.Drawing.Point(395, 142);
          this.pnlAutoComplete.Name = "pnlAutoComplete";
          this.pnlAutoComplete.Size = new System.Drawing.Size(166, 446);
          this.pnlAutoComplete.TabIndex = 10;
          // 
          // button9
          // 
          this.button9.Location = new System.Drawing.Point(6, 430);
          this.button9.Name = "button9";
          this.button9.Size = new System.Drawing.Size(62, 23);
          this.button9.TabIndex = 23;
          this.button9.Text = "Vol. up";
          this.button9.UseVisualStyleBackColor = true;
          this.button9.Click += new System.EventHandler(this.button9_Click);
          // 
          // button10
          // 
          this.button10.Location = new System.Drawing.Point(80, 430);
          this.button10.Name = "button10";
          this.button10.Size = new System.Drawing.Size(62, 23);
          this.button10.TabIndex = 24;
          this.button10.Text = "Vol. down";
          this.button10.UseVisualStyleBackColor = true;
          this.button10.Click += new System.EventHandler(this.button10_Click);
          // 
          // frmMain
          // 
          this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
          this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
          this.ClientSize = new System.Drawing.Size(729, 600);
          this.Controls.Add(this.pnlAutoComplete);
          this.Controls.Add(this.gbTest);
          this.Controls.Add(this.txtOutput);
          this.Controls.Add(this.pnlKeyboard);
          this.Name = "frmMain";
          this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
          this.Text = " ";
          this.gbTest.ResumeLayout(false);
          this.gbTest.PerformLayout();
          ((System.ComponentModel.ISupportInitialize)(this.nSelectionTime)).EndInit();
          this.ResumeLayout(false);
          this.PerformLayout();

        }

        #endregion

        private Panel pnlKeyboard;
        private TextBox txtOutput;
        private GroupBox gbTest;
        private ListBox lbAutoSuggestions;
        private Button btnGetAutoList;
        private Button button8;
        private Button button7;
        private Button button6;
        private Button button5;
        private Button button4;
        private Button button3;
        private Button button2;
        private Button button1;
        private Panel pnlAutoComplete;
        private NumericUpDown nSelectionTime;
        private CheckBox cbButton2;
        private CheckBox cbButton1;
        private Label label1;
        private Button button10;
        private Button button9;
    }
}

