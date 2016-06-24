using System;
using System.Data;
using System.Data.Common;
using System.Collections.Generic;
using System.Data.SqlClient;

namespace KiepCommunication.DataAccess
{
  /// <summary>
  /// Data access to the data in table Words in the database
  /// </summary>

  public partial class AutoWords
  {
    private const string CLASSNAME = "DataAccess.AutoWords";

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
      }
      catch
      {
        throw;
      }
    }

    public void Add(Model.AutoWord newWord)
    {
      try
      {
        Database.ExecuteQuery("AutoWords_Add", CommandType.StoredProcedure,
                              new SqlParameter("@WordID", newWord.WordID),
                              new SqlParameter("@Word", newWord.Word),
                              new SqlParameter("@Count", newWord.Count),
                              new SqlParameter("@Deleted", newWord.Deleted),
                              new SqlParameter("@Added", newWord.Added));
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

    public void Delete(Model.AutoWord word)
    {
      try
      {
        int recordsAffected = Database.ExecuteQuery("AutoWords_Delete", CommandType.StoredProcedure,
                                                    new SqlParameter("@WordID", word.WordID));

        if (recordsAffected == 0)
        {
          throw new Exception(string.Format("Unable to delete Word with ID '{0}'", word.WordID));
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

    public void Modify(Model.AutoWord modifiedWord)
    {
      try
      {
        int recordsAffected = Database.ExecuteQuery("AutoWords_Modify", CommandType.StoredProcedure,
                                                    new SqlParameter("@WordID", modifiedWord.WordID),
                                                    new SqlParameter("@Word", modifiedWord.Word),
                                                    new SqlParameter("@Count", modifiedWord.Count),
                                                    new SqlParameter("@Deleted", modifiedWord.Deleted),
                                                    new SqlParameter("@Added", modifiedWord.Added));
 
        if (recordsAffected == 0)
        {
          throw new Exception(string.Format("Unable to modify Word with ID '{0}'", modifiedWord.WordID));
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

    public Model.AutoWord GetById(Int32 wordID)
    {
      try
      {
        _dataReader = Database.GetDataReader("AutoWords_GetById", CommandType.StoredProcedure,
                                             new SqlParameter("@WordID", wordID));

        Model.AutoWord autoWord = null;

        if (_dataReader != null)
        {
          if (_dataReader.Read())
          {
            autoWord = CreateAutoWord(_dataReader);
          }
        }

        return autoWord;
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

    public List<Model.AutoWord> GetAll()
    {
      try
      {
        _dataReader = Database.GetDataReader("AutoWords_GetAll", CommandType.StoredProcedure);

        List<Model.AutoWord> autoWordList = new List<Model.AutoWord>();

        if (_dataReader != null)
        {
          while (_dataReader.Read())
          {
            autoWordList.Add(CreateAutoWord(_dataReader));
          }
        }

        return autoWordList;
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

    private Model.AutoWord CreateAutoWord(DbDataReader reader)
    {
      try
      {
        return new Model.AutoWord((Int32)reader["WordID"],
                                  (String)reader["Word"],
                                  (Int32)reader["Count"],
                                  (Boolean)reader["Deleted"],
                                  (Boolean)reader["Added"]);
      }
      catch
      {
        throw;
      }
    }

    public void DeleteByWord(string word)
    {
      try
      {
        int recordsAffected = Database.ExecuteQuery("AutoWords_DeleteByWord", CommandType.StoredProcedure,
                                                    new SqlParameter("@Word", word));

        if (recordsAffected == 0)
        {
          throw new Exception(string.Format("Unable to delete Word '{0}'", word));
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

    public Model.AutoWord GetByWord(string word)
    {
      try
      {
        _dataReader = Database.GetDataReader("AutoWords_GetByWord", CommandType.StoredProcedure,
                                             new SqlParameter("@Word", word));

        Model.AutoWord autoWord = null;

        if (_dataReader != null)
        {
          if (_dataReader.Read())
          {
            autoWord = CreateAutoWord(_dataReader);
          }
        }

        return autoWord;
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

    public List<string> GetWordList(string word)
    {
      try
      {
        _dataReader = Database.GetDataReader("AutoWords_GetWordList", CommandType.StoredProcedure,
                                             new SqlParameter("@Word", word));

        List<string> wordList = new List<string>();

        if (_dataReader != null)
        {
          while (_dataReader.Read())
          {
            wordList.Add(CreateWord(_dataReader));
          }
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

    public void UpdateWordCount(string word)
    {
      try
      {
        int recordsAffected = Database.ExecuteQuery("AutoWords_UpdateWordCount", CommandType.StoredProcedure,
                                                    new SqlParameter("@Word", word));

        if (recordsAffected == 0)
        {
          Add(new Model.AutoWord(default(int), word, -3, false, true));
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

    public void Reset()
    {
      try
      {
        Database.ExecuteQuery("AutoWords_Reset", CommandType.StoredProcedure);
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

    public void Clear()
    {
      try
      {
        Database.ExecuteQuery("AutoWords_Clear",  CommandType.StoredProcedure);
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