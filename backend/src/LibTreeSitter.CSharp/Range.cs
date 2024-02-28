namespace LibTreeSitter.CSharp
{
  public class Range
  {
    public Point StartPoint { get; }
    public Point EndPoint { get; }

    public Range(Point startPoint, Point endPoint)
    {
      StartPoint = startPoint;
      EndPoint = endPoint;
    }
  }
}