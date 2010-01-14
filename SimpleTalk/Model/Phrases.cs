using System;

namespace SimpleTalk.Model
{
	/// <summary>
	/// Object model class for Phrases
	/// </summary>
	[Serializable]
	public partial class Phrase
	{
		private const string CLASSNAME = "Phrase";

		public Phrase()
		{
		    //Empty constructor
		}

		public Phrase(Int32 PhraseID, DateTime dateTime, String Phrase)
		{
			_PhraseID = PhraseID;
			_dateTime = dateTime;
			_Phrase = Phrase;
		}

			  public Int32 PhraseID
		{
			set { _PhraseID = value;}
			get { return _PhraseID;}
		} private Int32 _PhraseID;


    							  public DateTime DateTime
		{
			set { _dateTime = value;}
			get { return _dateTime;}
		} private DateTime _dateTime;


    							  public String PhraseLine
		{
			set { _Phrase = value;}
			get { return _Phrase;}
		} private String _Phrase;


        public override string ToString()
        {
            return string.Format( "Phrase: PhraseID={0}, DateTime={1}, Phrase={2}", 
			    _PhraseID, _dateTime, _Phrase);
        }
        
       
    
    public override bool Equals(object obj)
		{
		  if (obj is Int32)
{
return (PhraseID == (Int32)obj);
}
else
if (obj is Phrase )
return Equals((Phrase)obj);
return false;

		}

		public bool Equals(Phrase  obj)
		{
			
		
			return ((this.PhraseID == ((Phrase)obj).PhraseID) );
			
		}
		
		public override int GetHashCode()
		{
		  
		  int result = 0;
		  
		      result = PhraseID;
		    
		  return result;
		}
		
		   
	}
}
