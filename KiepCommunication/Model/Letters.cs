using System;

namespace KiepCommunication.Model
{
	/// <summary>
	/// Object model class for Letters
	/// </summary>
	[Serializable]
	public partial class Letter
	{
		private const string CLASSNAME = "Letter";

		public Letter()
		{
		    //Empty constructor
		}

		public Letter(Int32 letterID, String letter, Int32 count)
		{
			_letterID = letterID;
			_letter = letter;
			_count = count;
		}

			  public Int32 LetterID
		{
			set { _letterID = value;}
			get { return _letterID;}
		} private Int32 _letterID;


    							  public String LetterChar
		{
			set { _letter = value;}
			get { return _letter;}
		} private String _letter;


    							  public Int32 Count
		{
			set { _count = value;}
			get { return _count;}
		} private Int32 _count;


        public override string ToString()
        {
            return string.Format( "Letter: LetterID={0}, Letter={1}, Count={2}", 
			    _letterID, _letter, _count);
        }
        
       
    
    public override bool Equals(object obj)
		{
		  if (obj is Int32)
{
return (LetterID == (Int32)obj);
}
else
if (obj is Letter )
return Equals((Letter)obj);
return false;

		}

		public bool Equals(Letter  obj)
		{
			
		
			return ((this.LetterID == ((Letter)obj).LetterID) );
			
		}
		
		public override int GetHashCode()
		{
		  
		  int result = 0;
		  
		      result = LetterID;
		    
		  return result;
		}
		
		   
	}
}
