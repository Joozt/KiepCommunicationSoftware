﻿using System;
using System.Data.Common;
using System.Collections.Generic;
using System.Text;
using System.Data;
using System.Data.SqlClient;
using System.Runtime.InteropServices;

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
    private static string _databaseName = "AutoComplete";
    private static string _userID = string.Empty;
    private static string _password = string.Empty;

    public static void Connect()
    {
      try
      {
        if (_dataSet == null)
        {
          _dataSet = new DataSet();
        }

        if (_sqlConnection == null)
        {
          if (string.IsNullOrEmpty(_connectionString))
          {
            string authorisation = "Integrated Security = SSPI";

            if (!string.IsNullOrEmpty(_userID) && !string.IsNullOrEmpty(_password))
            {
              authorisation = string.Format("User ID = {0};Password = {1}", _userID, _password);
            }

            if (string.IsNullOrEmpty(_serverName))
            {
              _serverName = string.Format(@"{0}\SQLEXPRESS", System.Environment.MachineName);
            }

            _connectionString = string.Format("Data Source = {0};Initial Catalog = {1};{2}", _serverName, _databaseName, authorisation);
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
        }
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
        }

        if (_sqlCommand != null)
        {
          _sqlCommand.Dispose();
        }

        if (_sqlConnection != null)
        {
          if (_sqlConnection.State != ConnectionState.Closed)
          {
            _sqlConnection.Close();
          }

          _sqlConnection.Dispose();
        }
      }
      catch
      {
        throw;
      }
    }

    private static void SetCommand(string sqlString, params SqlParameter[] parameters)
    {
      try
      {
        //Connect();

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

    public static DbDataReader GetDataReader(string sqlString, params SqlParameter[] parameters)
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

    public static DataSet GetDataSet(string sqlString, params SqlParameter[] parameters)
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

    public static int ExecuteQuery(string sqlString, params SqlParameter[] parameters)
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