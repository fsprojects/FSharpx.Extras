using System;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows;
using FSharp.Control;

namespace TimeFliesLikeAnArrow.Silverlight
{
    public partial class MainPage : UserControl
    {
        public MainPage()
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
                var label = new TextBlock() { Text = c };
                canvas.Children.Add(label);
                int closure = i+1;
                Action<Point> charMove = p =>
                    {   Canvas.SetLeft(label, p.X + closure * 10);
                        Canvas.SetTop(label, p.Y); };
                charMove(new Point(closure * 10, 0));
                mouseMove
                    .Select(e => e.GetPosition(canvas))
                    .Delay(closure * 100)
                    .OnDispatcher()
                    .Subscribe(charMove);
            }
        }
    }
}
