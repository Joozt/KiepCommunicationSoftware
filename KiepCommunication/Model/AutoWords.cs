using System;

namespace KiepCommunication.Model
{
  /// <summary>
  /// Object model class for Words
  /// </summary>
  [Serializable]
  public partial class AutoWord
  {
    private const string CLASSNAME = "AutoWord";

    public AutoWord()
    {
      //Empty constructor
    }

    public AutoWord(Int32 wordID, String word, Int32 count, Boolean deleted, Boolean added)
    {
      _wordID = wordID;
      _word = word;
      _count = count;
      _deleted = deleted;
      _added = added;
    }

    public Int32 WordID
    {
      set { _wordID = value; }
      get { return _wordID; }
    } private Int32 _wordID;

    public String Word
    {
      set { _word = value; }
      get { return _word; }
    } private String _word;

    public Int32 Count
    {
      set { _count = value; }
      get { return _count; }
    } private Int32 _count;

    public Boolean Deleted
    {
      set { _deleted = value; }
      get { return _deleted; }
    } private Boolean _deleted;

    public Boolean Added
    {
      set { _added = value; }
      get { return _added; }
    } private Boolean _added;

    public override string ToString()
    {
      return string.Format("Word: WordID={0}, Word={1}, Count={2}, Deleted={3}, Added={4}", _wordID, _word, _count, _deleted, _added);
    }

    public override bool Equals(object obj)
    {
      if (obj is Int32)
      {
        return (WordID == (Int32)obj);
      }
      else
      {
        if (obj is AutoWord)
        {
          return Equals((AutoWord)obj);
        }
      }

      return false;
    }

    public bool Equals(AutoWord obj)
    {
      return ((this.WordID == ((AutoWord)obj).WordID));
    }

    public override int GetHashCode()
    {
      int result = 0;

      result = WordID;

      return result;
    }
  }
}
