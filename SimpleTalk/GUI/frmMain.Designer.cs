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
        this.panel1 = new System.Windows.Forms.Panel();
        this.txtOutput = new System.Windows.Forms.TextBox();
        this.splitContainer1 = new System.Windows.Forms.SplitContainer();
        this.pnlKeyboard = new System.Windows.Forms.Panel();
        this.pnlAutoComplete = new System.Windows.Forms.Panel();
        this.panel1.SuspendLayout();
        this.splitContainer1.Panel1.SuspendLayout();
        this.splitContainer1.Panel2.SuspendLayout();
        this.splitContainer1.SuspendLayout();
        this.SuspendLayout();
        // 
        // panel1
        // 
        this.panel1.Controls.Add(this.txtOutput);
        this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
        this.panel1.Location = new System.Drawing.Point(0, 0);
        this.panel1.Name = "panel1";
        this.panel1.Size = new System.Drawing.Size(1264, 157);
        this.panel1.TabIndex = 11;
        // 
        // txtOutput
        // 
        this.txtOutput.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                    | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.txtOutput.BackColor = System.Drawing.SystemColors.Window;
        this.txtOutput.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
        this.txtOutput.Location = new System.Drawing.Point(12, 3);
        this.txtOutput.Multiline = true;
        this.txtOutput.Name = "txtOutput";
        this.txtOutput.ReadOnly = true;
        this.txtOutput.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
        this.txtOutput.ShortcutsEnabled = false;
        this.txtOutput.Size = new System.Drawing.Size(1240, 154);
        this.txtOutput.TabIndex = 7;
        this.txtOutput.TabStop = false;
        this.txtOutput.TextChanged += new System.EventHandler(this.txtOutput_TextChanged);
        // 
        // splitContainer1
        // 
        this.splitContainer1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                    | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.splitContainer1.FixedPanel = System.Windows.Forms.FixedPanel.Panel1;
        this.splitContainer1.IsSplitterFixed = true;
        this.splitContainer1.Location = new System.Drawing.Point(0, 163);
        this.splitContainer1.Name = "splitContainer1";
        // 
        // splitContainer1.Panel1
        // 
        this.splitContainer1.Panel1.Controls.Add(this.pnlKeyboard);
        // 
        // splitContainer1.Panel2
        // 
        this.splitContainer1.Panel2.Controls.Add(this.pnlAutoComplete);
        this.splitContainer1.Size = new System.Drawing.Size(1264, 579);
        this.splitContainer1.SplitterDistance = 690;
        this.splitContainer1.TabIndex = 12;
        // 
        // pnlKeyboard
        // 
        this.pnlKeyboard.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                    | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.pnlKeyboard.Location = new System.Drawing.Point(12, 3);
        this.pnlKeyboard.Name = "pnlKeyboard";
        this.pnlKeyboard.Size = new System.Drawing.Size(781, 573);
        this.pnlKeyboard.TabIndex = 6;
        // 
        // pnlAutoComplete
        // 
        this.pnlAutoComplete.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                    | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.pnlAutoComplete.BackColor = System.Drawing.SystemColors.Window;
        this.pnlAutoComplete.Location = new System.Drawing.Point(3, 3);
        this.pnlAutoComplete.Name = "pnlAutoComplete";
        this.pnlAutoComplete.Size = new System.Drawing.Size(567, 573);
        this.pnlAutoComplete.TabIndex = 11;
        // 
        // frmMain
        // 
        this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
        this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        this.BackColor = System.Drawing.SystemColors.Window;
        this.ClientSize = new System.Drawing.Size(1264, 741);
        this.Controls.Add(this.splitContainer1);
        this.Controls.Add(this.panel1);
        this.Name = "frmMain";
        this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
        this.WindowState = System.Windows.Forms.FormWindowState.Maximized;
        this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.frmMain_FormClosed);
        this.panel1.ResumeLayout(false);
        this.panel1.PerformLayout();
        this.splitContainer1.Panel1.ResumeLayout(false);
        this.splitContainer1.Panel2.ResumeLayout(false);
        this.splitContainer1.ResumeLayout(false);
        this.ResumeLayout(false);

    }

    #endregion

    private Panel panel1;
    private TextBox txtOutput;
    private SplitContainer splitContainer1;
    private Panel pnlKeyboard;
    private Panel pnlAutoComplete;
  }
}

