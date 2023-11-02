using System;
using System.Runtime.Serialization;

namespace LibTreeSitter
{
  [Serializable]
  public class TreeSitterException : Exception
  {
    //
    // For guidelines regarding the creation of new exception types, see
    //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cpgenref/html/cpconerrorraisinghandlingguidelines.asp
    // and
    //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dncscol/html/csharp07192001.asp
    //

    public TreeSitterException()
    {
    }

    public TreeSitterException(string message) : base(message)
    {
    }

    public TreeSitterException(string message, Exception inner) : base(message, inner)
    {
    }

    protected TreeSitterException(
        SerializationInfo info,
        StreamingContext context) : base(info, context)
    {
    }
  }
}