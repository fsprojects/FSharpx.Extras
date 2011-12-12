using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;

namespace TimeFliesLikeAnArrow
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            TrackMouse("Time flies like an arrow");
        }

        private void TrackMouse(string message)
        {
            var mouseMove =
                Observable.FromEvent<MouseEventArgs, MouseEventHandler>(
                    (f) => new MouseEventHandler((sender, args) => f(args)),
                    handler => MouseMove += handler,
                    handler => MouseMove -= handler);
            var chars = message.ToCharArray();
            for (int i = 0; i < chars.Length; i++)
            {
                var c = message[i].ToString();
                var label = new Label() { Content = c };
                canvas.Children.Add(label);
                int closure = i;
                mouseMove
                    .Select(e => e.GetPosition(canvas))
                    .Delay(closure * 100)
                    .OnDispatcher()
                    .Subscribe(pos =>
                    {
                        Canvas.SetLeft(label, pos.X + closure * 10);
                        Canvas.SetTop(label, pos.Y);
                    });
            }
        }
    }
}
