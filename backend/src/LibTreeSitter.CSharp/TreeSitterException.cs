using System;
using System.Runtime.Serialization;

namespace LibTreeSitter.CSharp
{
  [Serializable]
  public class TreeSitterException : Exception
  {
    public TreeSitterException(string message) : base(message)
    {
    }
  }
}