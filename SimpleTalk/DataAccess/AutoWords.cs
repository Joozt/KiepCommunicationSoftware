using System;
using System.Data;
using System.Data.Common;
using System.Collections.Generic;
using System.Data.SqlClient;

namespace SimpleTalk.DataAccess
{
  /// <summary>
  /// Data access to the data in table Words in the database
  /// </summary>

  public partial class AutoWords
  {
    private const string CLASSNAME = "DataAccess.AutoWords";

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

    public void Add(Model.AutoWord newWord)
    {
      try
      {
        _database.ExecuteQuery("AutoWords_Add",
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
        int recordsAffected = _database.ExecuteQuery("AutoWords_Delete",
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
        int recordsAffected = _database.ExecuteQuery("AutoWords_Modify",
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
        _dataReader = _database.GetDataReader("AutoWords_GetById",
                                              new SqlParameter("@WordID", wordID));

        Model.AutoWord autoWord = null;

        if (_dataReader.Read())
        {
          autoWord = CreateAutoWord(_dataReader);
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
        _dataReader = _database.GetDataReader("AutoWords_GetAll");

        List<Model.AutoWord> autoWordList = new List<Model.AutoWord>();

        while (_dataReader.Read())
        {
          autoWordList.Add(CreateAutoWord(_dataReader));
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
        int recordsAffected = _database.ExecuteQuery("AutoWords_DeleteByWord",
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
        _dataReader = _database.GetDataReader("AutoWords_GetByWord",
                                              new SqlParameter("@Word", word));

        Model.AutoWord autoWord = null;

        if (_dataReader.Read())
        {
          autoWord = CreateAutoWord(_dataReader);
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
        _dataReader = _database.GetDataReader("AutoWords_GetWordList",
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

    public void UpdateWordCount(string word)
    {
      try
      {
        int recordsAffected = _database.ExecuteQuery("AutoWords_UpdateWordCount",
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
        _database.ExecuteQuery("AutoWords_Reset");
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