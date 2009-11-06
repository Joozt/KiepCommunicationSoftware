using System;
using System.Data;
using System.Data.Common;
using System.Collections.Generic;
using Voets.Diagnostics;
using Voets.Data.Common;
using Voets.Data.Exceptions;

namespace AutoComplete.DataAccess
{
	/// <summary>
	/// Data access to the data in table NextWords in the database
	/// </summary>

	public partial class NextWords
	{
		private const string CLASSNAME = "DataAccess.NextWords";

		public void Add(Model.NextWord newNextWord)
		{
			try
			{
                DbHelper helper = new DbHelper(Database.GetConnectionString());

		        
		        int recordsAffected = helper.ExecuteSPNonQuery("NextWords_Add",
			DbHelper.CreateInputParameter("@WordID", newNextWord.WordID),
			DbHelper.CreateInputParameter("@NextWordID", newNextWord.NextWordID),
			DbHelper.CreateInputParameter("@Count", newNextWord.Count));

                if (recordsAffected == 0)
                    throw new DalNothingUpdatedException("Unable to add NextWord with WordID={0}", newNextWord);

return;    
			}
			catch( Exception ex)
			{
				Trace.WriteError("({0})", "Add", CLASSNAME, ex, newNextWord.ToString());
				throw DbHelper.TranslateException(ex);
			}
		}


		public void Delete(Model.NextWord nextWord)
		{
			try
			{
                DbHelper helper = new DbHelper(Database.GetConnectionString());
				int recordsAffected = helper.ExecuteSPNonQuery("NextWords_Delete",
				          				DbHelper.CreateInputParameter("@WordID", nextWord.WordID),
								DbHelper.CreateInputParameter("@NextWordID", nextWord.NextWordID));

			}
			catch( Exception ex)
			{
				Trace.WriteError("({0})", "Delete", CLASSNAME, ex, nextWord.ToString());
				throw DbHelper.TranslateException(ex);
			}
		}



		public void Modify(Model.NextWord modifiedNextWord)
		{
			try
			{

				DbHelper helper = new DbHelper(Database.GetConnectionString());
                int recordsAffected = helper.ExecuteSPNonQuery("NextWords_Modify",
				          				DbHelper.CreateInputParameter("@WordID", modifiedNextWord.WordID),
								DbHelper.CreateInputParameter("@NextWordID", modifiedNextWord.NextWordID),
								DbHelper.CreateInputParameter("@Count", modifiedNextWord.Count));

				if (recordsAffected == 0)
				{
					throw new DalNothingUpdatedException( "No records were updated (Table: NextWords). NextWord=" + modifiedNextWord.ToString());
				}
			}
			catch( Exception ex)
			{
				Trace.WriteError("({0})", "Modify", CLASSNAME, ex, modifiedNextWord.ToString());
				throw DbHelper.TranslateException(ex);
			}
		}



		public Model.NextWord GetById(Int32 wordID ,Int32 nextWordID )
		{
			DbDataReader reader = null;
			try
			{
				
                DbHelper helper = new DbHelper(Database.GetConnectionString());
                
                reader = helper.ExecuteSPReader("NextWords_GetById",
                      DbHelper.CreateInputParameter("@WordID", wordID),
                      DbHelper.CreateInputParameter("@NextWordID", nextWordID));
				
				Model.NextWord result = null;
				if (reader.Read())
                    result = CreateNextWord(reader);
				return result;
			}
			catch( Exception ex)
			{
				Trace.WriteError("{0},{1}", "GetById", CLASSNAME, ex,wordID,nextWordID);
				throw DbHelper.TranslateException(ex);
			}
			finally 
			{
				if (reader != null)
					reader.Close();
			}
		}


		
		public List<Model.NextWord> GetByWordID(Int32 wordID)
		{
			DbDataReader reader = null;
			try
			{
                DbHelper helper = new DbHelper(Database.GetConnectionString());
                reader = helper.ExecuteSPReader("NextWords_GetByWordID", 
								DbHelper.CreateInputParameter("@WordID", wordID));
                
                List<Model.NextWord> result = new List<Model.NextWord>();
				while (reader.Read())
				{
				    result.Add( CreateNextWord( reader));
				}
				return result;

			}
			catch( Exception ex)
			{
				Trace.WriteError("({0})", "GetByWordID", CLASSNAME, ex, wordID);
				throw DbHelper.TranslateException(ex);
			}
			finally 
			{
				if (reader != null)
					reader.Close();
			}
		}

		public List<Model.NextWord> GetByNextWordID(Int32 nextWordID)
		{
			DbDataReader reader = null;
			try
			{
                DbHelper helper = new DbHelper(Database.GetConnectionString());
                reader = helper.ExecuteSPReader("NextWords_GetByNextWordID", 
								DbHelper.CreateInputParameter("@NextWordID", nextWordID));
                
                List<Model.NextWord> result = new List<Model.NextWord>();
				while (reader.Read())
				{
				    result.Add( CreateNextWord( reader));
				}
				return result;

			}
			catch( Exception ex)
			{
				Trace.WriteError("({0})", "GetByNextWordID", CLASSNAME, ex, nextWordID);
				throw DbHelper.TranslateException(ex);
			}
			finally 
			{
				if (reader != null)
					reader.Close();
			}
		}

		
		public List<Model.NextWord> GetAll()
		{
			DbDataReader reader = null;
			try
			{
                DbHelper helper = new DbHelper(Database.GetConnectionString());
                reader = helper.ExecuteSPReader("NextWords_GetAll");
								
				List<Model.NextWord> result = new List<Model.NextWord>();
				while (reader.Read())
				{
				    result.Add( CreateNextWord( reader));
				}
				
				return result;
			}
			catch( Exception ex)
			{
				Trace.WriteError("()", "GetAll", CLASSNAME, ex);
				throw DbHelper.TranslateException(ex);
			}
			finally 
			{
				if (reader != null)
					reader.Close();
			}
		}
		
		private Model.NextWord CreateNextWord( DbDataReader reader)
		{
			try
			{	
        Model.NextWord result = new Model.NextWord(
          (Int32)reader["WordID"], 
          (Int32)reader["NextWordID"], 
          (Int32)reader["Count"]
						);
				return result;
			}
			catch( Exception ex)
			{
				Trace.WriteError("", "CreateNextWord", CLASSNAME, ex);
				throw DbHelper.TranslateException(ex);
			}
		}
	}
}