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
  /// Data access to the data in table Words in the database
  /// </summary>

  public partial class AutoWords
  {
    public void DeleteByWord(string word)
    {
      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());

        int recordsAffected = helper.ExecuteSPNonQuery("AutoWords_DeleteByWord",
                          DbHelper.CreateInputParameter("@Word", word));

      }
      catch (Exception ex)
      {
        Trace.WriteError("({0})", "DeleteByWord", CLASSNAME, ex, word.ToString());
        throw DbHelper.TranslateException(ex);
      }
    }

    public Model.AutoWord GetByWord(string word)
    {
      DbDataReader reader = null;
      
      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());

        reader = helper.ExecuteSPReader("AutoWords_GetByWord",
              DbHelper.CreateInputParameter("@Word", word));

        Model.AutoWord result = null;
        if (reader.Read())
        {
          result = CreateWord(reader);
        }

        return result;
      }
      catch (Exception ex)
      {
        Trace.WriteError("{0}", "GetByWord", CLASSNAME, ex, word);
        throw DbHelper.TranslateException(ex);
      }
      finally
      {
        if (reader != null)
        {
          reader.Close();
        }
      }
    }

    public List<string> GetWordList(string word)
    {
      DbDataReader reader = null;

      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());
        reader = helper.ExecuteSPReader("AutoWords_GetWordList",
              DbHelper.CreateInputParameter("@Word", word));

        List<string> result = new List<string>();

        while (reader.Read())
        {
          result.Add(CreateOnlyWord(reader));
        }

        return result;
      }
      catch (Exception ex)
      {
        Trace.WriteError("()", "GetWordList", CLASSNAME, ex);
        throw DbHelper.TranslateException(ex);
      }
      finally
      {
        if (reader != null)
        {
          reader.Close();
        }
      }
    }

    public void UpdateWordCount(string word)
    {
      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());

        int recordsAffected = helper.ExecuteSPNonQuery("AutoWords_UpdateWordCount",
                  DbHelper.CreateInputParameter("@Word", word));

        if (recordsAffected == 0)
        {
          Add(new Model.AutoWord(default(int), word, -3, false, true));
        }
      }
      catch (Exception ex)
      {
        Trace.WriteError("({0})", "UpdateWordCount", CLASSNAME, ex, word);
        throw DbHelper.TranslateException(ex);
      }
    }

    public void Reset()
    {
      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());

        helper.ExecuteSPNonQuery("AutoWords_Reset");
      }
      catch (Exception ex)
      {
        Trace.WriteError("({0}, {1})", "Reset", CLASSNAME, ex);
        throw DbHelper.TranslateException(ex);
      }
    }

    private string CreateOnlyWord(DbDataReader reader)
    {
      try
      {
        return (String)reader["Word"];
      }
      catch (Exception ex)
      {
        Trace.WriteError("", "CreateOnlyWord", CLASSNAME, ex);
        throw DbHelper.TranslateException(ex);
      }
    }
  }
}