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
    public void UpdateNextWordCount(string word, string nextWord)
    {
      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());

        int recordsAffected = helper.ExecuteSPNonQuery("NextWords_UpdateNextWordCount",
                  DbHelper.CreateInputParameter("@Word", word),
                  DbHelper.CreateInputParameter("@NextWord", nextWord));

        if (recordsAffected == 0)
        {
          AddNextWord(word, nextWord);
        }
      }
      catch (Exception ex)
      {
        Trace.WriteError("({0}, {1})", "UpdateNextWordCount", CLASSNAME, ex, word, nextWord);
        throw DbHelper.TranslateException(ex);
      }
    }

    public void AddNextWord(string word, string nextWord)
    {
      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());

        int recordsAffected = helper.ExecuteSPNonQuery("NextWords_AddNextWord",
                  DbHelper.CreateInputParameter("@Word", word),
                  DbHelper.CreateInputParameter("@NextWord", nextWord));

        if (recordsAffected == 0)
        {
          throw new DalNothingUpdatedException("Unable to add NextWord with Word = {0}", word);
        }
      }
      catch (Exception ex)
      {
        Trace.WriteError("({0}, {1})", "AddNextWord", CLASSNAME, ex, word, nextWord);
        throw DbHelper.TranslateException(ex);
      }
    }

    public List<string> GetNextWordList(string word)
    {
      DbDataReader reader = null;

      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());
        reader = helper.ExecuteSPReader("NextWords_GetNextWordList",
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
        Trace.WriteError("()", "GetNextWordList", CLASSNAME, ex);
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

    public void Reset()
    {
      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());

        helper.ExecuteSPNonQuery("NextWords_Reset");
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