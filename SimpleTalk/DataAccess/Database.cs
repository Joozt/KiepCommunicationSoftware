using System;
using System.Data.Common;
using System.Collections.Generic;
using System.Text;
using System.Data;
using System.Data.SqlClient;
using System.Runtime.InteropServices;
using System.Media;
using System.IO;

namespace SimpleTalk.DataAccess
{
  public static class Database
  {
    private const string CLASSNAME = "Database";

    private static string _connectionString = string.Empty;
    private static SqlConnection _sqlConnection = null;
    private static SqlCommand _sqlCommand = null;
    private static DataSet _dataSet = null;

    private static string _serverName = string.Empty;
    private static string _instanceName = "SQLEXPRESS";
    private static string _databaseName = "AutoComplete";
    private static string _userID = string.Empty;
    private static string _password = string.Empty;

    private static bool _connected = false;

    public static void Connect(string databaseName)
    {
      try
      {
        if (_sqlConnection == null)
        {
          if (string.IsNullOrEmpty(_connectionString))
          {
            _connectionString = GetConnectionString(databaseName);
          }

          if (string.IsNullOrEmpty(_connectionString))
          {
            throw new Exception("No connectionstring found!");
          }

          _sqlConnection = new SqlConnection(_connectionString);
        }

        if ((_sqlConnection != null) && (_sqlConnection.State != ConnectionState.Open))
        {
          _sqlConnection.Open();

          _connected = true;
        }
      }
      catch (SqlException ex)
      {
        Disconnect();

        _connected = false;
      }
      catch
      {
        throw;
      }
    }

    public static void Disconnect()
    {
      try
      {
        if (_dataSet != null)
        {
          _dataSet.Dispose();

          _dataSet = null;
        }

        if (_sqlCommand != null)
        {
          _sqlCommand.Dispose();

          _sqlCommand = null;
        }

        if (_sqlConnection != null)
        {
          if (_sqlConnection.State != ConnectionState.Closed)
          {
            _sqlConnection.Close();
          }

          _sqlConnection.Dispose();

          _sqlConnection = null;
        }
      }
      catch
      {
        throw;
      }
    }

    private static void GenerateDatabase()
    {
      try
      {
        _connectionString = string.Empty;

        Disconnect();
        Connect("master");

        Database.ExecuteQuery("CREATE DATABASE AutoComplete", CommandType.Text);
        Database.ExecuteQuery(Properties.Resources.GenerateAutoCompleteDatabase, CommandType.Text);

        Disconnect();
        Connect(string.Empty);
      }
      catch
      {
        throw;
      }
    }

    private static string GetConnectionString(string databaseName)
    {
      try
      {
        string authorisation = "Integrated Security = SSPI";

        if (!string.IsNullOrEmpty(_userID) && !string.IsNullOrEmpty(_password))
        {
          authorisation = string.Format("User ID = {0}; Password = {1}", _userID, _password);
        }

        if (string.IsNullOrEmpty(_serverName))
        {
          _serverName = Path.Combine(Environment.MachineName, _instanceName);
        }

        return string.Format("Data Source = {0};Initial Catalog = {1}; {2}", _serverName, (string.IsNullOrEmpty(databaseName) ? _databaseName : databaseName), authorisation);
      }
      catch
      {
        throw;
      }
    }

    private static void SetCommand(string sqlString, CommandType commandType, params SqlParameter[] parameters)
    {
      try
      {
        if (!_connected)
        {
          Connect(string.Empty);
        }

        if (_sqlCommand == null)
        {
          _sqlCommand = new SqlCommand();
        }
        else
        {
          _sqlCommand.Parameters.Clear();
        }

        _sqlCommand.Connection = _sqlConnection;
        _sqlCommand.CommandType = commandType;
        _sqlCommand.CommandText = sqlString;

        _sqlCommand.Parameters.AddRange(parameters);
      }
      catch
      {
        throw;
      }
    }

    public static DbDataReader GetDataReader(string sqlString, CommandType commandType, params SqlParameter[] parameters)
    {
      try
      {
        SetCommand(sqlString, commandType, parameters);

        return _sqlCommand.ExecuteReader(CommandBehavior.SingleResult);
      }
      catch (SqlException ex)
      {
        Disconnect();

        _connected = false;

        return null;
      }
      catch
      {
        throw;
      }
    }

    public static DataSet GetDataSet(string sqlString, CommandType commandType, params SqlParameter[] parameters)
    {
      try
      {
        SetCommand(sqlString, commandType, parameters);

        using (SqlDataAdapter dataAdapter = new SqlDataAdapter(_sqlCommand))
        {
          if (_dataSet == null)
          {
            _dataSet = new DataSet();
          }

          dataAdapter.Fill(_dataSet);

          return _dataSet;
        }
      }
      catch (SqlException ex)
      {
        Disconnect();

        _connected = false;

        return null;
      }
      catch
      {
        throw;
      }
    }

    public static int ExecuteQuery(string sqlString, CommandType commandType, params SqlParameter[] parameters)
    {
      try
      {
        SetCommand(sqlString, commandType, parameters);

        return _sqlCommand.ExecuteNonQuery();
      }
      catch (SqlException ex)
      {
        Disconnect();

        _connected = false;

        return 0;
      }
      catch
      {
        throw;
      }
    }
  }
}
