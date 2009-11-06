using System;

namespace AutoComplete.Model
{
  /// <summary>
  /// Object model class for NextWords
  /// </summary>
  [Serializable]
  public partial class NextWord
  {
    private const string CLASSNAME = "NextWord";

    public NextWord()
    {
      //Empty constructor
    }

    public NextWord(Int32 wordID, Int32 nextWordID, Int32 count)
    {
      _wordID = wordID;
      _nextWordID = nextWordID;
      _count = count;
    }

    public Int32 WordID
    {
      set { _wordID = value; }
      get { return _wordID; }
    } private Int32 _wordID;

    public Int32 NextWordID
    {
      set { _nextWordID = value; }
      get { return _nextWordID; }
    } private Int32 _nextWordID;

    public Int32 Count
    {
      set { _count = value; }
      get { return _count; }
    } private Int32 _count;

    public override string ToString()
    {
      return string.Format("NextWord: WordID={0}, NextWordID={1}, Count={2}", _wordID, _nextWordID, _count);
    }

    public override bool Equals(object obj)
    {
      if (obj.GetType().IsArray)
      {
        if ((((object[])obj).Length == 2) && (((object[])obj)[0] is Int32) && (((object[])obj)[1] is Int32))
        {
          return (WordID == (Int32)((object[])obj)[0] && NextWordID == (Int32)((object[])obj)[1]);
        }
      }
      else
      {
        if (obj is NextWord)
        {
          return Equals((NextWord)obj);
        }
      }

      return false;
    }

    public bool Equals(NextWord obj)
    {
      return ((this.WordID == ((NextWord)obj).WordID) && (this.NextWordID == ((NextWord)obj).NextWordID));
    }

    public override int GetHashCode()
    {
      int result = 0;

      result = WordID;

      result ^= NextWordID;

      return result;
    }
  }
}
