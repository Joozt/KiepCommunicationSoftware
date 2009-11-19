using System;
using System.Data;
using System.Data.Common;
using System.Collections.Generic;
using System.Data.SqlClient;

namespace SimpleTalk.DataAccess
{
	/// <summary>
	/// Data access to the data in table NextWords in the database
	/// </summary>

	public partial class NextWords
	{
		private const string CLASSNAME = "DataAccess.NextWords";

    private Database _database = new Database();
    private DbDataReader _dataReader = null;
    private DataSet _dataSet = null;

    public void Dispose()
    {
      try
      {
        if (_dataSet != null)
        {
          _dataSet.Dispose();
        }

        if (_dataReader != null)
        {
          _dataReader.Close();
          _dataReader.Dispose();
        }

        if (_database != null)
        {
          _database.Disconnect();
        }
      }
      catch
      {
        throw;
      }
    }

		public void Add(Model.NextWord newNextWord)
		{
			try
			{
        _database.ExecuteQuery("NextWords_Add",
                               new SqlParameter("@WordID", newNextWord.WordID),
                               new SqlParameter("@NextWordID", newNextWord.NextWordID),
                               new SqlParameter("@Count", newNextWord.Count));
			}
			catch
			{
				throw;
			}
      finally
      {
        if (_dataReader != null)
        {
          _dataReader.Close();
        }
      }
		}

		public void Delete(Model.NextWord nextWord)
		{
			try
			{
        int recordsAffected = _database.ExecuteQuery("NextWords_Delete",
                                                     new SqlParameter("@WordID", nextWord.WordID),
                                                     new SqlParameter("@NextWordID", nextWord.NextWordID));

        if (recordsAffected == 0)
        {
          throw new Exception(string.Format("Unable to delete NextWord with ID '{0}' and NextID '{1}'", nextWord.WordID, nextWord.NextWordID));
        }
			}
			catch
			{
        throw;
			}
      finally
      {
        if (_dataReader != null)
        {
          _dataReader.Close();
        }
      }
		}

		public void Modify(Model.NextWord modifiedNextWord)
		{
			try
			{
        int recordsAffected = _database.ExecuteQuery("NextWords_Modify",
                                                     new SqlParameter("@WordID", modifiedNextWord.WordID),
                                                     new SqlParameter("@NextWordID", modifiedNextWord.NextWordID),
                                                     new SqlParameter("@Count", modifiedNextWord.Count));

        if (recordsAffected == 0)
        {
          throw new Exception(string.Format("Unable to modify NextWord with ID '{0}' and NextID '{1}'", modifiedNextWord.WordID, modifiedNextWord.NextWordID));
        }
			}
			catch
			{
				throw;
			}
      finally
      {
        if (_dataReader != null)
        {
          _dataReader.Close();
        }
      }
		}

		public Model.NextWord GetById(Int32 wordID ,Int32 nextWordID )
		{
			try
			{
        _dataReader = _database.GetDataReader("NextWords_GetById",
                                              new SqlParameter("@WordID", wordID),
                                              new SqlParameter("@NextWordID", nextWordID));

        Model.NextWord nextWord = null;

        if (_dataReader.Read())
        {
          nextWord = CreateNextWord(_dataReader);
        }

        return nextWord;
			}
			catch
			{
				throw;
			}
      finally
      {
        if (_dataReader != null)
        {
          _dataReader.Close();
        }
      }
		}

		public List<Model.NextWord> GetByWordID(Int32 wordID)
		{
			try
			{
        _dataReader = _database.GetDataReader("NextWords_GetByWordID",
                                              new SqlParameter("@WordID", wordID));

        List<Model.NextWord> nextWordList = new List<Model.NextWord>();

        if (_dataReader.Read())
        {
          nextWordList.Add(CreateNextWord(_dataReader));
        }

        return nextWordList;
			}
			catch
			{
				throw;
			}
      finally
      {
        if (_dataReader != null)
        {
          _dataReader.Close();
        }
      }
		}

		public List<Model.NextWord> GetByNextWordID(Int32 nextWordID)
		{
			try
			{
        _dataReader = _database.GetDataReader("NextWords_GetByNextWordID",
                                              new SqlParameter("@NextWordID", nextWordID));

        List<Model.NextWord> nextWordList = new List<Model.NextWord>();

        if (_dataReader.Read())
        {
          nextWordList.Add(CreateNextWord(_dataReader));
        }

        return nextWordList;
			}
			catch
			{
				throw;
			}
      finally
      {
        if (_dataReader != null)
        {
          _dataReader.Close();
        }
      }
		}
	
		public List<Model.NextWord> GetAll()
		{
			try
			{
        _dataReader = _database.GetDataReader("NextWords_GetAll");

        List<Model.NextWord> nextWordList = new List<Model.NextWord>();

        while (_dataReader.Read())
        {
          nextWordList.Add(CreateNextWord(_dataReader));
        }

        return nextWordList;
			}
			catch
			{
				throw;
			}
      finally
      {
        if (_dataReader != null)
        {
          _dataReader.Close();
        }
      }
		}
		
		private Model.NextWord CreateNextWord( DbDataReader reader)
		{
			try
			{	
        return new Model.NextWord((Int32)reader["WordID"], 
                                  (Int32)reader["NextWordID"], 
                                  (Int32)reader["Count"]);
			}
			catch
			{
				throw;
			}
		}

    public void UpdateNextWordCount(string word, string nextWord)
    {
      try
      {
        int recordsAffected = _database.ExecuteQuery("NextWords_UpdateNextWordCount",
                                                     new SqlParameter("@Word", word),
                                                     new SqlParameter("@NextWord", nextWord));

        if (recordsAffected == 0)
        {
          AddNextWord(word, nextWord);
        }
      }
      catch
      {
        throw;
      }
      finally
      {
        if (_dataReader != null)
        {
          _dataReader.Close();
        }
      }
    }

    public void AddNextWord(string word, string nextWord)
    {
      try
      {
        _database.ExecuteQuery("NextWords_AddNextWord",
                               new SqlParameter("@Word", word),
                               new SqlParameter("@NextWord", nextWord));
      }
      catch
      {
        throw;
      }
      finally
      {
        if (_dataReader != null)
        {
          _dataReader.Close();
        }
      }
    }

    public List<string> GetNextWordList(string word)
    {
      try
      {
        _dataReader = _database.GetDataReader("NextWords_GetNextWordList",
                                              new SqlParameter("@Word", word));

        List<string> wordList = new List<string>();

        while (_dataReader.Read())
        {
          wordList.Add(CreateWord(_dataReader));
        }

        return wordList;
      }
      catch
      {
        throw;
      }
      finally
      {
        if (_dataReader != null)
        {
          _dataReader.Close();
        }
      }
    }

    public void Reset()
    {
      try
      {
        _database.ExecuteQuery("NextWords_Reset");
      }
      catch
      {
        throw;
      }
      finally
      {
        if (_dataReader != null)
        {
          _dataReader.Close();
        }
      }
    }

    private string CreateWord(DbDataReader reader)
    {
      try
      {
        return (String)reader["Word"];
      }
      catch
      {
        throw;
      }
    }
	}
}