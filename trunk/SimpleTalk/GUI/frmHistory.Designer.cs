namespace SimpleTalk.GUI
{
  partial class frmHistory
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
      this.txtNumberOfPages = new System.Windows.Forms.TextBox();
      this.txtPageNumber = new System.Windows.Forms.TextBox();
      this.SuspendLayout();
      // 
      // pnlControls
      // 
      this.pnlControls.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.pnlControls.Location = new System.Drawing.Point(10, 10);
      this.pnlControls.Name = "pnlControls";
      this.pnlControls.Size = new System.Drawing.Size(1336, 710);
      this.pnlControls.TabIndex = 6;
      // 
      // txtNumberOfPages
      // 
      this.txtNumberOfPages.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.txtNumberOfPages.BackColor = System.Drawing.SystemColors.Window;
      this.txtNumberOfPages.Font = new System.Drawing.Font("Consolas", 45F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
      this.txtNumberOfPages.Location = new System.Drawing.Point(1196, 12);
      this.txtNumberOfPages.MaximumSize = new System.Drawing.Size(150, 80);
      this.txtNumberOfPages.MinimumSize = new System.Drawing.Size(150, 80);
      this.txtNumberOfPages.Name = "txtNumberOfPages";
      this.txtNumberOfPages.ReadOnly = true;
      this.txtNumberOfPages.Size = new System.Drawing.Size(150, 78);
      this.txtNumberOfPages.TabIndex = 8;
      this.txtNumberOfPages.Text = "30";
      this.txtNumberOfPages.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
      // 
      // txtPageNumber
      // 
      this.txtPageNumber.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.txtPageNumber.BackColor = System.Drawing.SystemColors.Window;
      this.txtPageNumber.Font = new System.Drawing.Font("Consolas", 45F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
      this.txtPageNumber.Location = new System.Drawing.Point(1040, 12);
      this.txtPageNumber.MaximumSize = new System.Drawing.Size(150, 80);
      this.txtPageNumber.MinimumSize = new System.Drawing.Size(150, 80);
      this.txtPageNumber.Name = "txtPageNumber";
      this.txtPageNumber.ReadOnly = true;
      this.txtPageNumber.Size = new System.Drawing.Size(150, 78);
      this.txtPageNumber.TabIndex = 9;
      this.txtPageNumber.Text = "1";
      this.txtPageNumber.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
      // 
      // frmHistory
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.BackColor = System.Drawing.SystemColors.ControlLightLight;
      this.ClientSize = new System.Drawing.Size(1358, 732);
      this.ControlBox = false;
      this.Controls.Add(this.txtPageNumber);
      this.Controls.Add(this.txtNumberOfPages);
      this.Controls.Add(this.pnlControls);
      this.MaximizeBox = false;
      this.MinimizeBox = false;
      this.Name = "frmHistory";
      this.Text = "History";
      this.WindowState = System.Windows.Forms.FormWindowState.Maximized;
      this.Load += new System.EventHandler(this.frmSettings_Load);
      this.VisibleChanged += new System.EventHandler(this.frmSettings_VisibleChanged);
      this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.frmSettings_FormClosing);
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Panel pnlControls;
    private System.Windows.Forms.TextBox txtNumberOfPages;
    private System.Windows.Forms.TextBox txtPageNumber;
  }
}