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
        this.textNextWord = new System.Windows.Forms.TextBox();
        this.textAutoComplete = new System.Windows.Forms.TextBox();
        this.SuspendLayout();
        // 
        // pnlControls
        // 
        this.pnlControls.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                    | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.pnlControls.Location = new System.Drawing.Point(10, 10);
        this.pnlControls.Name = "pnlControls";
        this.pnlControls.Size = new System.Drawing.Size(750, 600);
        this.pnlControls.TabIndex = 6;
        // 
        // txtScanTime
        // 
        this.txtScanTime.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.txtScanTime.BackColor = System.Drawing.SystemColors.Window;
        this.txtScanTime.Font = new System.Drawing.Font("Microsoft Sans Serif", 61F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.txtScanTime.Location = new System.Drawing.Point(760, 10);
        this.txtScanTime.Name = "txtScanTime";
        this.txtScanTime.ReadOnly = true;
        this.txtScanTime.Size = new System.Drawing.Size(200, 100);
        this.txtScanTime.TabIndex = 7;
        this.txtScanTime.Text = "1,2";
        this.txtScanTime.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
        // 
        // textNextWord
        // 
        this.textNextWord.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.textNextWord.BackColor = System.Drawing.SystemColors.Window;
        this.textNextWord.Font = new System.Drawing.Font("Microsoft Sans Serif", 61F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.textNextWord.Location = new System.Drawing.Point(760, 230);
        this.textNextWord.Name = "textNextWord";
        this.textNextWord.ReadOnly = true;
        this.textNextWord.Size = new System.Drawing.Size(200, 100);
        this.textNextWord.TabIndex = 8;
        this.textNextWord.Text = "UIT";
        this.textNextWord.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
        // 
        // textAutoComplete
        // 
        this.textAutoComplete.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.textAutoComplete.BackColor = System.Drawing.SystemColors.Window;
        this.textAutoComplete.Font = new System.Drawing.Font("Microsoft Sans Serif", 61F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.textAutoComplete.Location = new System.Drawing.Point(760, 340);
        this.textAutoComplete.Name = "textAutoComplete";
        this.textAutoComplete.ReadOnly = true;
        this.textAutoComplete.Size = new System.Drawing.Size(200, 100);
        this.textAutoComplete.TabIndex = 9;
        this.textAutoComplete.Text = "AAN";
        this.textAutoComplete.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
        // 
        // frmSettings
        // 
        this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
        this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        this.BackColor = System.Drawing.SystemColors.ControlLightLight;
        this.ClientSize = new System.Drawing.Size(1264, 732);
        this.Controls.Add(this.textAutoComplete);
        this.Controls.Add(this.textNextWord);
        this.Controls.Add(this.txtScanTime);
        this.Controls.Add(this.pnlControls);
        this.Name = "frmSettings";
        this.Text = "Settings";
        this.WindowState = System.Windows.Forms.FormWindowState.Maximized;
        this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.frmSettings_FormClosing);
        this.ResumeLayout(false);
        this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Panel pnlControls;
    private System.Windows.Forms.TextBox txtScanTime;
    private System.Windows.Forms.TextBox textNextWord;
    private System.Windows.Forms.TextBox textAutoComplete;
  }
}