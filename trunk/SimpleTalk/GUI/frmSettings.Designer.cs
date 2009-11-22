namespace SimpleTalk.GUI
{
  partial class frmSettings
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
        this.txtScanTime = new System.Windows.Forms.TextBox();
        this.txtNextWord = new System.Windows.Forms.TextBox();
        this.txtAutoComplete = new System.Windows.Forms.TextBox();
        this.SuspendLayout();
        // 
        // pnlControls
        // 
        this.pnlControls.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                    | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.pnlControls.Location = new System.Drawing.Point(10, 10);
        this.pnlControls.Name = "pnlControls";
        this.pnlControls.Size = new System.Drawing.Size(850, 600);
        this.pnlControls.TabIndex = 6;
        // 
        // txtScanTime
        // 
        this.txtScanTime.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.txtScanTime.BackColor = System.Drawing.SystemColors.Window;
        this.txtScanTime.Font = new System.Drawing.Font("Consolas", 45F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.txtScanTime.Location = new System.Drawing.Point(860, 10);
        this.txtScanTime.MaximumSize = new System.Drawing.Size(200, 100);
        this.txtScanTime.MinimumSize = new System.Drawing.Size(200, 100);
        this.txtScanTime.Name = "txtScanTime";
        this.txtScanTime.ReadOnly = true;
        this.txtScanTime.Size = new System.Drawing.Size(200, 78);
        this.txtScanTime.TabIndex = 7;
        this.txtScanTime.Text = "1,2";
        this.txtScanTime.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
        // 
        // txtNextWord
        // 
        this.txtNextWord.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.txtNextWord.BackColor = System.Drawing.SystemColors.Window;
        this.txtNextWord.Font = new System.Drawing.Font("Consolas", 45F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.txtNextWord.Location = new System.Drawing.Point(860, 230);
        this.txtNextWord.MaximumSize = new System.Drawing.Size(200, 100);
        this.txtNextWord.MinimumSize = new System.Drawing.Size(200, 100);
        this.txtNextWord.Name = "txtNextWord";
        this.txtNextWord.ReadOnly = true;
        this.txtNextWord.Size = new System.Drawing.Size(200, 78);
        this.txtNextWord.TabIndex = 8;
        this.txtNextWord.Text = "UIT";
        this.txtNextWord.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
        // 
        // txtAutoComplete
        // 
        this.txtAutoComplete.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.txtAutoComplete.BackColor = System.Drawing.SystemColors.Window;
        this.txtAutoComplete.Font = new System.Drawing.Font("Consolas", 45F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.txtAutoComplete.Location = new System.Drawing.Point(860, 340);
        this.txtAutoComplete.MaximumSize = new System.Drawing.Size(200, 100);
        this.txtAutoComplete.MinimumSize = new System.Drawing.Size(200, 100);
        this.txtAutoComplete.Name = "txtAutoComplete";
        this.txtAutoComplete.ReadOnly = true;
        this.txtAutoComplete.Size = new System.Drawing.Size(200, 78);
        this.txtAutoComplete.TabIndex = 9;
        this.txtAutoComplete.Text = "AAN";
        this.txtAutoComplete.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
        // 
        // frmSettings
        // 
        this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
        this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        this.BackColor = System.Drawing.SystemColors.ControlLightLight;
        this.ClientSize = new System.Drawing.Size(1264, 732);
        this.ControlBox = false;
        this.Controls.Add(this.txtAutoComplete);
        this.Controls.Add(this.txtNextWord);
        this.Controls.Add(this.txtScanTime);
        this.Controls.Add(this.pnlControls);
        this.MaximizeBox = false;
        this.MinimizeBox = false;
        this.Name = "frmSettings";
        this.Text = "Settings";
        this.WindowState = System.Windows.Forms.FormWindowState.Maximized;
        this.VisibleChanged += new System.EventHandler(this.frmSettings_VisibleChanged);
        this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.frmSettings_FormClosing);
        this.ResumeLayout(false);
        this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Panel pnlControls;
    private System.Windows.Forms.TextBox txtScanTime;
    private System.Windows.Forms.TextBox txtNextWord;
    private System.Windows.Forms.TextBox txtAutoComplete;
  }
}