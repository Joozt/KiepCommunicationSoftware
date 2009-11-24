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
            this.buttonClearDatabase = new System.Windows.Forms.Button();
            this.numericUpDownCountOffset = new System.Windows.Forms.NumericUpDown();
            this.buttonImport = new System.Windows.Forms.Button();
            this.textBoxFilePathName = new System.Windows.Forms.TextBox();
            this.button10 = new System.Windows.Forms.Button();
            this.button9 = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.nSelectionTime = new System.Windows.Forms.NumericUpDown();
            this.cbButton2 = new System.Windows.Forms.CheckBox();
            this.cbButton1 = new System.Windows.Forms.CheckBox();
            this.button7 = new System.Windows.Forms.Button();
            this.button5 = new System.Windows.Forms.Button();
            this.button4 = new System.Windows.Forms.Button();
            this.button1 = new System.Windows.Forms.Button();
            this.pnlAutoComplete = new System.Windows.Forms.Panel();
            this.gbTest.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownCountOffset)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.nSelectionTime)).BeginInit();
            this.SuspendLayout();
            // 
            // pnlKeyboard
            // 
            this.pnlKeyboard.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.pnlKeyboard.Location = new System.Drawing.Point(4, 163);
            this.pnlKeyboard.Name = "pnlKeyboard";
            this.pnlKeyboard.Size = new System.Drawing.Size(487, 559);
            this.pnlKeyboard.TabIndex = 5;
            // 
            // txtOutput
            // 
            this.txtOutput.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.txtOutput.BackColor = System.Drawing.SystemColors.Window;
            this.txtOutput.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.txtOutput.Location = new System.Drawing.Point(4, 4);
            this.txtOutput.Multiline = true;
            this.txtOutput.Name = "txtOutput";
            this.txtOutput.ReadOnly = true;
            this.txtOutput.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.txtOutput.ShortcutsEnabled = false;
            this.txtOutput.Size = new System.Drawing.Size(1104, 153);
            this.txtOutput.TabIndex = 6;
            this.txtOutput.TabStop = false;
            this.txtOutput.TextChanged += new System.EventHandler(this.txtOutput_TextChanged);
            // 
            // gbTest
            // 
            this.gbTest.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.gbTest.Controls.Add(this.buttonClearDatabase);
            this.gbTest.Controls.Add(this.numericUpDownCountOffset);
            this.gbTest.Controls.Add(this.buttonImport);
            this.gbTest.Controls.Add(this.textBoxFilePathName);
            this.gbTest.Controls.Add(this.button10);
            this.gbTest.Controls.Add(this.button9);
            this.gbTest.Controls.Add(this.label1);
            this.gbTest.Controls.Add(this.nSelectionTime);
            this.gbTest.Controls.Add(this.cbButton2);
            this.gbTest.Controls.Add(this.cbButton1);
            this.gbTest.Controls.Add(this.button7);
            this.gbTest.Controls.Add(this.button5);
            this.gbTest.Controls.Add(this.button4);
            this.gbTest.Controls.Add(this.button1);
            this.gbTest.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.gbTest.Location = new System.Drawing.Point(1122, 12);
            this.gbTest.Name = "gbTest";
            this.gbTest.Size = new System.Drawing.Size(150, 710);
            this.gbTest.TabIndex = 9;
            this.gbTest.TabStop = false;
            this.gbTest.Text = "Test area";
            // 
            // buttonClearDatabase
            // 
            this.buttonClearDatabase.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.buttonClearDatabase.Location = new System.Drawing.Point(6, 186);
            this.buttonClearDatabase.Name = "buttonClearDatabase";
            this.buttonClearDatabase.Size = new System.Drawing.Size(75, 23);
            this.buttonClearDatabase.TabIndex = 30;
            this.buttonClearDatabase.Text = "Clear";
            this.buttonClearDatabase.UseVisualStyleBackColor = true;
            this.buttonClearDatabase.Click += new System.EventHandler(this.buttonClearDatabase_Click);
            // 
            // numericUpDownCountOffset
            // 
            this.numericUpDownCountOffset.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.numericUpDownCountOffset.Location = new System.Drawing.Point(6, 91);
            this.numericUpDownCountOffset.Minimum = new decimal(new int[] {
            100,
            0,
            0,
            -2147483648});
            this.numericUpDownCountOffset.Name = "numericUpDownCountOffset";
            this.numericUpDownCountOffset.Size = new System.Drawing.Size(120, 20);
            this.numericUpDownCountOffset.TabIndex = 29;
            // 
            // buttonImport
            // 
            this.buttonImport.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.buttonImport.Location = new System.Drawing.Point(6, 143);
            this.buttonImport.Name = "buttonImport";
            this.buttonImport.Size = new System.Drawing.Size(75, 23);
            this.buttonImport.TabIndex = 28;
            this.buttonImport.Text = "Import";
            this.buttonImport.UseVisualStyleBackColor = true;
            this.buttonImport.Click += new System.EventHandler(this.buttonImportWords_Click);
            // 
            // textBoxFilePathName
            // 
            this.textBoxFilePathName.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.textBoxFilePathName.Location = new System.Drawing.Point(6, 117);
            this.textBoxFilePathName.Name = "textBoxFilePathName";
            this.textBoxFilePathName.Size = new System.Drawing.Size(138, 20);
            this.textBoxFilePathName.TabIndex = 27;
            // 
            // button10
            // 
            this.button10.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.button10.Location = new System.Drawing.Point(80, 430);
            this.button10.Name = "button10";
            this.button10.Size = new System.Drawing.Size(62, 23);
            this.button10.TabIndex = 24;
            this.button10.Text = "Vol. down";
            this.button10.UseVisualStyleBackColor = true;
            this.button10.Click += new System.EventHandler(this.button10_Click);
            // 
            // button9
            // 
            this.button9.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.button9.Location = new System.Drawing.Point(6, 430);
            this.button9.Name = "button9";
            this.button9.Size = new System.Drawing.Size(62, 23);
            this.button9.TabIndex = 23;
            this.button9.Text = "Vol. up";
            this.button9.UseVisualStyleBackColor = true;
            this.button9.Click += new System.EventHandler(this.button9_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(6, 567);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(73, 13);
            this.label1.TabIndex = 22;
            this.label1.Text = "Sel. time (sec)";
            this.label1.Click += new System.EventHandler(this.label1_Click);
            // 
            // nSelectionTime
            // 
            this.nSelectionTime.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.nSelectionTime.DecimalPlaces = 1;
            this.nSelectionTime.Increment = new decimal(new int[] {
            1,
            0,
            0,
            65536});
            this.nSelectionTime.Location = new System.Drawing.Point(85, 565);
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
            15,
            0,
            0,
            65536});
            this.nSelectionTime.ValueChanged += new System.EventHandler(this.nSelectionTime_ValueChanged);
            // 
            // cbButton2
            // 
            this.cbButton2.AutoSize = true;
            this.cbButton2.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.cbButton2.Location = new System.Drawing.Point(6, 606);
            this.cbButton2.Name = "cbButton2";
            this.cbButton2.Size = new System.Drawing.Size(93, 17);
            this.cbButton2.TabIndex = 20;
            this.cbButton2.Text = "Second button";
            this.cbButton2.UseVisualStyleBackColor = true;
            // 
            // cbButton1
            // 
            this.cbButton1.AutoSize = true;
            this.cbButton1.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.cbButton1.Location = new System.Drawing.Point(6, 583);
            this.cbButton1.Name = "cbButton1";
            this.cbButton1.Size = new System.Drawing.Size(75, 17);
            this.cbButton1.TabIndex = 19;
            this.cbButton1.Text = "First button";
            this.cbButton1.UseVisualStyleBackColor = true;
            // 
            // button7
            // 
            this.button7.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.button7.Location = new System.Drawing.Point(6, 401);
            this.button7.Name = "button7";
            this.button7.Size = new System.Drawing.Size(136, 23);
            this.button7.TabIndex = 17;
            this.button7.Text = "Stop sound";
            this.button7.UseVisualStyleBackColor = true;
            this.button7.Click += new System.EventHandler(this.button7_Click);
            // 
            // button5
            // 
            this.button5.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
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
            this.button4.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.button4.Location = new System.Drawing.Point(6, 302);
            this.button4.Name = "button4";
            this.button4.Size = new System.Drawing.Size(136, 23);
            this.button4.TabIndex = 14;
            this.button4.Text = "Say text";
            this.button4.UseVisualStyleBackColor = true;
            this.button4.Click += new System.EventHandler(this.button4_Click);
            // 
            // button1
            // 
            this.button1.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.button1.Location = new System.Drawing.Point(6, 48);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(136, 23);
            this.button1.TabIndex = 11;
            this.button1.Text = "Reset database";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // pnlAutoComplete
            // 
            this.pnlAutoComplete.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.pnlAutoComplete.Location = new System.Drawing.Point(509, 163);
            this.pnlAutoComplete.Name = "pnlAutoComplete";
            this.pnlAutoComplete.Size = new System.Drawing.Size(599, 559);
            this.pnlAutoComplete.TabIndex = 10;
            // 
            // frmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.Window;
            this.ClientSize = new System.Drawing.Size(1284, 734);
            this.Controls.Add(this.pnlAutoComplete);
            this.Controls.Add(this.gbTest);
            this.Controls.Add(this.txtOutput);
            this.Controls.Add(this.pnlKeyboard);
            this.Name = "frmMain";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = " ";
            this.Load += new System.EventHandler(this.frmMain_Load);
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.frmMain_FormClosed);
            this.gbTest.ResumeLayout(false);
            this.gbTest.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownCountOffset)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.nSelectionTime)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private Panel pnlKeyboard;
        private TextBox txtOutput;
        private GroupBox gbTest;
        private Button button7;
        private Button button5;
        private Button button4;
        private Button button1;
        private Panel pnlAutoComplete;
        private NumericUpDown nSelectionTime;
        private CheckBox cbButton2;
        private CheckBox cbButton1;
        private Label label1;
        private Button button10;
        private Button button9;
        private Button buttonImport;
        private TextBox textBoxFilePathName;
        private NumericUpDown numericUpDownCountOffset;
        private Button buttonClearDatabase;
    }
}

