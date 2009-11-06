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
          throw new DalNothingUpdatedException("Unable to add Word with WordID={0}", newWord);

        return (Int32)WordIDParam.Value;
      }
      catch (Exception ex)
      {
        Trace.WriteError("({0})", "Add", CLASSNAME, ex, newWord.ToString());
        throw DbHelper.TranslateException(ex);
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
        Trace.WriteError("({0})", "Delete", CLASSNAME, ex, word.ToString());
        throw DbHelper.TranslateException(ex);
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
          throw new DalNothingUpdatedException("No records were updated (Table: Words). Word=" + modifiedWord.ToString());
        }
      }
      catch (Exception ex)
      {
        Trace.WriteError("({0})", "Modify", CLASSNAME, ex, modifiedWord.ToString());
        throw DbHelper.TranslateException(ex);
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
        Trace.WriteError("{0}", "GetById", CLASSNAME, ex, wordID);
        throw DbHelper.TranslateException(ex);
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
        Trace.WriteError("()", "GetAll", CLASSNAME, ex);
        throw DbHelper.TranslateException(ex);
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
        Trace.WriteError("", "CreateWord", CLASSNAME, ex);
        throw DbHelper.TranslateException(ex);
      }
    }

  }
}