using System;
using System.Data;
using System.Data.Common;
using System.Collections.Generic;
using System.Data.SqlClient;

namespace KiepCommunication.DataAccess
{
	/// <summary>
	/// Data access to the data in table Letters in the database
	/// </summary>

	public partial class Letters
	{
		private const string CLASSNAME = "DataAccess.Letters";

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

		public void Add(Model.Letter newLetter)
		{
      try
      {
        Database.ExecuteQuery("Letters_Add", CommandType.StoredProcedure,
                              new SqlParameter("@LetterID", newLetter.LetterID),
                              new SqlParameter("@Letter", newLetter.LetterChar),
                              new SqlParameter("@Count", newLetter.Count));
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


		public void Delete(Model.Letter letter)
		{
      try
      {
        int recordsAffected = Database.ExecuteQuery("Letters_Delete", CommandType.StoredProcedure,
                                                    new SqlParameter("@LetterID", letter.LetterID));

        if (recordsAffected == 0)
        {
          throw new Exception(string.Format("Unable to delete Letter with ID '{0}'", letter.LetterID));
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

		public void Modify(Model.Letter modifiedLetter)
		{
      try
      {
        int recordsAffected = Database.ExecuteQuery("Letters_Modify", CommandType.StoredProcedure,
                                                    new SqlParameter("@LetterID", modifiedLetter.LetterID),
                                                    new SqlParameter("@Letter", modifiedLetter.LetterChar),
                                                    new SqlParameter("@Count", modifiedLetter.Count));

        if (recordsAffected == 0)
        {
          throw new Exception(string.Format("Unable to modify Letter with ID '{0}'", modifiedLetter.LetterID));
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

		public Model.Letter GetById(Int32 letterID )
		{
      try
      {
        _dataReader = Database.GetDataReader("Letters_GetById", CommandType.StoredProcedure,
                                             new SqlParameter("@LetterID", letterID));

        Model.Letter letter = null;

        if (_dataReader != null)
        {
          if (_dataReader.Read())
          {
            letter = CreateLetter(_dataReader);
          }
        }

        return letter;
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
	
		public List<Model.Letter> GetAll()
		{
      try
      {
        _dataReader = Database.GetDataReader("Letters_GetAll", CommandType.StoredProcedure);

        List<Model.Letter> letterList = new List<Model.Letter>();

        if (_dataReader != null)
        {
          while (_dataReader.Read())
          {
            letterList.Add(CreateLetter(_dataReader));
          }
        }

        return letterList;
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
		
		private Model.Letter CreateLetter(DbDataReader reader)
		{
      try
      {
        return new Model.Letter((Int32)reader["LetterID"],
                                (String)reader["Letter"],
                                (Int32)reader["Count"]);
      }
      catch
      {
        throw;
      }
		}

    public void UpdateLetterCount(string letter)
    {
      try
      {
        int recordsAffected = Database.ExecuteQuery("Letters_UpdateLetterCount", CommandType.StoredProcedure,
                                                    new SqlParameter("@Letter", letter));
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
        Database.ExecuteQuery("Letters_Reset", CommandType.StoredProcedure);
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
	}
}