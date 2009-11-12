using System.Windows;
using System.Windows.Input;
using System.Windows.Controls;
using System.Windows.Media;
using System.Threading;
using System.Windows.Media.Animation;

namespace SimpleTalkWPFUI
{
    /// <summary>
    /// Interaction logic for Main.xaml
    /// </summary>
    public partial class Main : Window
    {
        private System.Timers.Timer _Timer = new System.Timers.Timer(1000);
        private int _SelectedRow;
        private int _SelectedColumn;
        private bool _RowSelected;

        public Main()
        {
            InitializeComponent();
            _Timer.Elapsed += new System.Timers.ElapsedEventHandler(_Timer_Elapsed);
        }

        void _Timer_Elapsed(object sender, System.Timers.ElapsedEventArgs e)
        {
            if (_RowSelected)
            {
                _SelectedColumn++;
                HighlightCell(_SelectedRow, _SelectedColumn);

                if (_SelectedColumn >= _Grid.ColumnDefinitions.Count)
                {
                    Reset();
                }
            }
            else
            {
                _SelectedRow++;
                HighlightRow(_SelectedRow);

                if (_SelectedRow >= _Grid.RowDefinitions.Count)
                {
                    Reset();
                }
            }
        }

        delegate void HighlightRowDelegate(int rowNumber);

        private void HighlightRow(int rowNumber)
        {
            if (this.Dispatcher.Thread == Thread.CurrentThread)
            {
                foreach (object child in _Grid.Children)
                {
                    if (child is DockPanel)
                    {
                        DockPanel dockPanel = child as DockPanel;

                        if (Grid.GetRow(dockPanel) == rowNumber)
                        {
                            dockPanel.Background = Brushes.White;

                            //ColorAnimation animation = new ColorAnimation(Colors.Transparent, Colors.White, new Duration(new System.TimeSpan(0,0,0,0,200)));
                            //dockPanel.Background.BeginAnimation(SolidColorBrush.ColorProperty, animation);
                        }
                        else
                        {
                            dockPanel.Background = Brushes.Transparent;
                        }
                    }
                }
            }
            else
            {
                this.Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal, new HighlightRowDelegate(this.HighlightRow), rowNumber);
            }
        }

        delegate void HighlightCellDelegate(int rowNumber, int columnNumber);

        private void HighlightCell(int rowNumber, int columnNumber)
        {
            if (this.Dispatcher.Thread == Thread.CurrentThread)
            {
                foreach (object child in _Grid.Children)
                {
                    if (child is DockPanel)
                    {
                        DockPanel dockPanel = child as DockPanel;

                        if (Grid.GetRow(dockPanel) == rowNumber)
                        {
                            if (Grid.GetColumn(dockPanel) == columnNumber)
                            {
                                dockPanel.Background = Brushes.White;
                            }
                            else
                            {
                                dockPanel.Background = Brushes.Transparent;
                            }
                        }
                        else
                        {
                            dockPanel.Background = Brushes.Transparent;
                        }
                    }
                }
            }
            else
            {
                this.Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal, new HighlightCellDelegate(this.HighlightCell), rowNumber, columnNumber);
            }
        }

        private void Window_KeyDown(object sender, System.Windows.Input.KeyEventArgs e)
        {
            if (e.Key == Key.Escape)
            {
                Close();
            }

            if (e.Key == Key.Space || e.Key == Key.Add)
            {
                KeyPress();
            }
        }

        private string GetSelection(int rowNumber, int columnNumber)
        {
            string result = "";
            foreach (object child in _Grid.Children)
            {
                if (child is DockPanel)
                {
                    DockPanel dockPanel = child as DockPanel;

                    if (Grid.GetRow(dockPanel) == rowNumber && Grid.GetColumn(dockPanel) == columnNumber)
                    {
                        Button button = dockPanel.Children[0] as Button;
                        if (button != null)
                        {
                            result = button.Content.ToString().ToLower();
                            break;
                        }
                    }
                }
            }
            return result;
        }

        private void Reset()
        {
            _RowSelected = false;
            _Timer.Stop();
            _SelectedRow = 2;
            _SelectedColumn = 0;
            HighlightCell(_Grid.RowDefinitions.Count, _Grid.ColumnDefinitions.Count);
        }

        private void KeyPress()
        {
            if (_Timer.Enabled)
            {
                if (_RowSelected)
                {
                    _Timer.Stop();
                    _RowSelected = false;
                    txtOutput.AppendText(GetSelection(_SelectedRow, _SelectedColumn));
                    Reset();
                }
                else
                {
                    _Timer.Stop();
                    _RowSelected = true;
                    HighlightCell(_SelectedRow, 0);
                    _SelectedColumn = 0;
                    _Timer.Start();
                }
            }
            else
            {
                HighlightRow(2);
                _SelectedRow = 2;
                _Timer.Start();
            }
        }
    }
}
