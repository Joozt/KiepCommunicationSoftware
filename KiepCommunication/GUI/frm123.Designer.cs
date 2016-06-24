namespace KiepCommunication.GUI
{
  partial class frm123
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
      this.pnlControls = new System.Windows.Forms.Panel();
      this.SuspendLayout();
      // 
      // pnlControls
      // 
      this.pnlControls.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.pnlControls.Location = new System.Drawing.Point(10, 10);
      this.pnlControls.Name = "pnlControls";
      this.pnlControls.Size = new System.Drawing.Size(612, 570);
      this.pnlControls.TabIndex = 6;
      // 
      // frm123
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.BackColor = System.Drawing.SystemColors.ControlLightLight;
      this.ClientSize = new System.Drawing.Size(563, 570);
      this.ControlBox = false;
      this.Controls.Add(this.pnlControls);
      this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
      this.Location = new System.Drawing.Point(20, 170);
      this.MaximizeBox = false;
      this.MinimizeBox = false;
      this.Name = "frm123";
      this.RightToLeft = System.Windows.Forms.RightToLeft.No;
      this.ShowInTaskbar = false;
      this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
      this.Text = "Numbers";
      this.VisibleChanged += new System.EventHandler(this.frm123_VisibleChanged);
      this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.frm123_FormClosing);
      this.ResumeLayout(false);

    }

    #endregion

    private System.Windows.Forms.Panel pnlControls;
  }
}