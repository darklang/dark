using System;

namespace LibTreeSitter.CSharp
{
  [Serializable]
  public class TreeSitterException : System.Exception
  {
    public TreeSitterException(string message) : base(message)
    {
    }
  }
}