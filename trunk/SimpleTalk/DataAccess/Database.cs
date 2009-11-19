using System;
using System.Data.Common;
using System.Collections.Generic;
using System.Text;
using System.Data;
using System.Data.SqlClient;

namespace SimpleTalk.DataAccess
{
  public class Database
  {
    private const string CLASSNAME = "Database";

    private string _connectionString = string.Empty;
    private SqlConnection _sqlConnection = null;
    private SqlCommand _sqlCommand = null;
    private DataSet _dataSet = null;

    private string _serverName = @"(local)";
    private string _databaseName = "AutoComplete";
    private string _userID = string.Empty;
    private string _password = string.Empty;

    public Database()
    {
      try
      {
        if (string.IsNullOrEmpty(_connectionString))
        {
          string authorisation = "Integrated Security = SSPI";

          if (!string.IsNullOrEmpty(_userID) && !string.IsNullOrEmpty(_password))
          {
            authorisation = string.Format("User ID = {0};Password = {1}", _userID, _password);
          }

          _connectionString = string.Format("Data Source = {0};Initial Catalog = {1};{2}", _serverName, _databaseName, authorisation);
        }

        if (string.IsNullOrEmpty(_connectionString))
        {
          throw new Exception("No connectionstring found!");
        }
      }
      catch
      {
        throw;
      }
    }

    private void Connect()
    {
      try
      {
        if (_dataSet == null)
        {
          _dataSet = new DataSet();
        }

        if (_sqlConnection == null)
        {
          _sqlConnection = new SqlConnection(_connectionString);
        }

        if (_sqlConnection.State != ConnectionState.Open)
        {
          _sqlConnection.Open();
        }
      }
      catch
      {
        throw;
      }
    }

    public void Disconnect()
    {
      try
      {
        if (_dataSet != null)
        {
          _dataSet.Dispose();
        }

        if (_sqlCommand != null)
        {
          _sqlCommand.Dispose();
        }

        if ((_sqlConnection != null) && (_sqlConnection.State != ConnectionState.Closed))
        {
          _sqlConnection.Close();
          _sqlConnection.Dispose();
        }

        if (_sqlConnection != null)
        {
          _sqlConnection.Dispose();
        }
      }
      catch
      {
        throw;
      }
    }

    private void SetCommand(string sqlString, params SqlParameter[] parameters)
    {
      try
      {
        Connect();

        if (_sqlCommand == null)
        {
          _sqlCommand = new SqlCommand();
        }
        else
        {
          _sqlCommand.Parameters.Clear();
        }

        _sqlCommand.Connection = _sqlConnection;
        _sqlCommand.CommandType = CommandType.StoredProcedure;
        _sqlCommand.CommandText = sqlString;

        _sqlCommand.Parameters.AddRange(parameters);
      }
      catch
      {
        throw;
      }
    }

    public DbDataReader GetDataReader(string sqlString, params SqlParameter[] parameters)
    {
      try
      {
        SetCommand(sqlString, parameters);

        return _sqlCommand.ExecuteReader(CommandBehavior.SingleResult);
      }
      catch
      {
        throw;
      }
    }

    public DataSet GetDataSet(string sqlString, params SqlParameter[] parameters)
    {
      try
      {
        SetCommand(sqlString, parameters);

        using (SqlDataAdapter dataAdapter = new SqlDataAdapter(_sqlCommand))
        {
          dataAdapter.Fill(_dataSet);

          return _dataSet;
        }
      }
      catch
      {
        throw;
      }
    }

    public int ExecuteQuery(string sqlString, params SqlParameter[] parameters)
    {
      try
      {
        SetCommand(sqlString, parameters);

        return _sqlCommand.ExecuteNonQuery();
      }
      catch
      {
        throw;
      }
    }
  }
}
