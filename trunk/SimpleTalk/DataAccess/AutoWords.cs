using System;
using System.Data;
using System.Data.Common;
using System.Collections.Generic;

namespace SimpleTalk.DataAccess
{
  /// <summary>
  /// Data access to the data in table Words in the database
  /// </summary>

  public partial class AutoWords
  {
    private const string CLASSNAME = "DataAccess.AutoWords";

    public Int32 Add(Model.AutoWord newWord)
    {
      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());
        DbParameter WordIDParam = DbHelper.CreateOutputParameter("@WordID", SqlDbType.Int);

        int recordsAffected = helper.ExecuteSPNonQuery("AutoWords_Add",
  WordIDParam,
  DbHelper.CreateInputParameter("@Word", newWord.Word),
  DbHelper.CreateInputParameter("@Count", newWord.Count),
  DbHelper.CreateInputParameter("@Deleted", newWord.Deleted),
  DbHelper.CreateInputParameter("@Added", newWord.Added));

        if (recordsAffected == 0)
          throw new Exception(String.Format("Could not add Word with ID '{0}'", newWord.WordID));

        return (Int32)WordIDParam.Value;
      }
      catch (Exception ex)
      {
        throw;
      }
    }


    public void Delete(Model.AutoWord word)
    {
      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());
        int recordsAffected = helper.ExecuteSPNonQuery("AutoWords_Delete",
                          DbHelper.CreateInputParameter("@WordID", word.WordID));

      }
      catch (Exception ex)
      {
        throw;
      }
    }



    public void Modify(Model.AutoWord modifiedWord)
    {
      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());
        int recordsAffected = helper.ExecuteSPNonQuery("AutoWords_Modify",
                  DbHelper.CreateInputParameter("@WordID", modifiedWord.WordID),
        DbHelper.CreateInputParameter("@Word", modifiedWord.Word),
        DbHelper.CreateInputParameter("@Count", modifiedWord.Count),
        DbHelper.CreateInputParameter("@Deleted", modifiedWord.Deleted),
        DbHelper.CreateInputParameter("@Added", modifiedWord.Added));

        if (recordsAffected == 0)
        {
          throw new Exception(string.Format("Could not modify Word with ID '{0}'", modifiedWord.WordID));
        }
      }
      catch (Exception ex)
      {
        throw;
      }
    }



    public Model.AutoWord GetById(Int32 wordID)
    {
      DbDataReader reader = null;
      try
      {

        DbHelper helper = new DbHelper(Database.GetConnectionString());

        reader = helper.ExecuteSPReader("AutoWords_GetById",
              DbHelper.CreateInputParameter("@WordID", wordID));

        Model.AutoWord result = null;
        if (reader.Read())
          result = CreateWord(reader);
        return result;
      }
      catch (Exception ex)
      {
        throw;
      }
      finally
      {
        if (reader != null)
          reader.Close();
      }
    }




    public List<Model.AutoWord> GetAll()
    {
      DbDataReader reader = null;
      try
      {
        DbHelper helper = new DbHelper(Database.GetConnectionString());
        reader = helper.ExecuteSPReader("AutoWords_GetAll");

        List<Model.AutoWord> result = new List<Model.AutoWord>();
        while (reader.Read())
        {
          result.Add(CreateWord(reader));
        }

        return result;
      }
      catch (Exception ex)
      {
        throw;
      }
      finally
      {
        if (reader != null)
          reader.Close();
      }
    }


    private Model.AutoWord CreateWord(DbDataReader reader)
    {
      try
      {
        Model.AutoWord result = new Model.AutoWord(
          (Int32)reader["WordID"],
          (String)reader["Word"],
          (Int32)reader["Count"],
          (Boolean)reader["Deleted"],
          (Boolean)reader["Added"]
            );
        return result;
      }
      catch (Exception ex)
      {
        throw;
      }
    }

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
        throw;
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
        throw;
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
        throw;
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
        throw;
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
        throw;
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
        throw;
      }
    }
  }
}