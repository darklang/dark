using System;

namespace LibTreeSitter.CSharp
{
  public class Point
  {
    public int Row { get; }
    public int Column { get; }

    public Point(int row, int column)
    {
      Row = row;
      Column = column;
    }
  }
}