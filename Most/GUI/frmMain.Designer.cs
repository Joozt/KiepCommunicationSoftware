using System.Windows.Forms;

namespace Most.GUI
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
          this.pbTest = new System.Windows.Forms.ProgressBar();
          this.pnlKeyboard = new System.Windows.Forms.Panel();
          this.btnTest = new Most.GUI.MostButton();
          this.listBoxAutoSuggestions = new System.Windows.Forms.ListBox();
          this.textBoxOutput = new System.Windows.Forms.TextBox();
          this.buttonResetDatabase = new System.Windows.Forms.Button();
          this.SuspendLayout();
          // 
          // pbTest
          // 
          this.pbTest.Location = new System.Drawing.Point(12, 608);
          this.pbTest.Name = "pbTest";
          this.pbTest.Size = new System.Drawing.Size(720, 23);
          this.pbTest.TabIndex = 2;
          // 
          // pnlKeyboard
          // 
          this.pnlKeyboard.Location = new System.Drawing.Point(12, 142);
          this.pnlKeyboard.Name = "pnlKeyboard";
          this.pnlKeyboard.Size = new System.Drawing.Size(545, 431);
          this.pnlKeyboard.TabIndex = 5;
          // 
          // btnTest
          // 
          this.btnTest.AutoFontSize = true;
          this.btnTest.Keys = "";
          this.btnTest.Location = new System.Drawing.Point(12, 579);
          this.btnTest.Name = "btnTest";
          this.btnTest.Size = new System.Drawing.Size(75, 23);
          this.btnTest.TabIndex = 3;
          this.btnTest.Text = "Golve Test";
          this.btnTest.UseVisualStyleBackColor = true;
          // 
          // listBoxAutoSuggestions
          // 
          this.listBoxAutoSuggestions.Font = new System.Drawing.Font("Microsoft Sans Serif", 27.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
          this.listBoxAutoSuggestions.FormattingEnabled = true;
          this.listBoxAutoSuggestions.ItemHeight = 42;
          this.listBoxAutoSuggestions.Location = new System.Drawing.Point(563, 142);
          this.listBoxAutoSuggestions.Name = "listBoxAutoSuggestions";
          this.listBoxAutoSuggestions.Size = new System.Drawing.Size(423, 424);
          this.listBoxAutoSuggestions.TabIndex = 7;
          this.listBoxAutoSuggestions.KeyDown += new System.Windows.Forms.KeyEventHandler(this.listBoxAutoSuggestions_KeyDown);
          // 
          // textBoxOutput
          // 
          this.textBoxOutput.Font = new System.Drawing.Font("Microsoft Sans Serif", 36F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
          this.textBoxOutput.Location = new System.Drawing.Point(12, 12);
          this.textBoxOutput.Multiline = true;
          this.textBoxOutput.Name = "textBoxOutput";
          this.textBoxOutput.Size = new System.Drawing.Size(974, 124);
          this.textBoxOutput.TabIndex = 6;
          this.textBoxOutput.TextChanged += new System.EventHandler(this.textBoxOutput_TextChanged);
          // 
          // buttonResetDatabase
          // 
          this.buttonResetDatabase.Location = new System.Drawing.Point(136, 579);
          this.buttonResetDatabase.Name = "buttonResetDatabase";
          this.buttonResetDatabase.Size = new System.Drawing.Size(75, 23);
          this.buttonResetDatabase.TabIndex = 8;
          this.buttonResetDatabase.Text = "Reset database";
          this.buttonResetDatabase.UseVisualStyleBackColor = true;
          this.buttonResetDatabase.Click += new System.EventHandler(this.buttonResetDatabase_Click);
          // 
          // frmMain
          // 
          this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
          this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
          this.ClientSize = new System.Drawing.Size(998, 639);
          this.Controls.Add(this.buttonResetDatabase);
          this.Controls.Add(this.listBoxAutoSuggestions);
          this.Controls.Add(this.textBoxOutput);
          this.Controls.Add(this.pnlKeyboard);
          this.Controls.Add(this.btnTest);
          this.Controls.Add(this.pbTest);
          this.Name = "frmMain";
          this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
          this.Text = " ";
          this.Load += new System.EventHandler(this.frmMain_Load);
          this.ResumeLayout(false);
          this.PerformLayout();

        }

        #endregion

        private ProgressBar pbTest;
        private MostButton btnTest;
        private Panel pnlKeyboard;
        private ListBox listBoxAutoSuggestions;
        private TextBox textBoxOutput;
        private Button buttonResetDatabase;
    }
}

