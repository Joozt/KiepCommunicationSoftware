using System;
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
              //List<string> serverList = GetServers();

              //if ((serverList != null) && serverList.Count > 0)
              //{
              //  _serverName = serverList[0];
              //}
              //else
              //{
              //  
              //}

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

    [DllImport("odbc32.dll")]
		private static extern short SQLAllocHandle(short hType, IntPtr inputHandle, out IntPtr outputHandle);
		[DllImport("odbc32.dll")]
		private static extern short SQLSetEnvAttr(IntPtr henv, int attribute, IntPtr valuePtr, int strLength);
		[DllImport("odbc32.dll")]
		private static extern short SQLFreeHandle(short hType, IntPtr handle); 
		[DllImport("odbc32.dll",CharSet=CharSet.Ansi)]
		private static extern short SQLBrowseConnect(IntPtr hconn, StringBuilder inString, 
			short inStringLength, StringBuilder outString, short outStringLength,
			out short outLengthNeeded);

		private const short SQL_HANDLE_ENV = 1;
		private const short SQL_HANDLE_DBC = 2;
		private const int SQL_ATTR_ODBC_VERSION = 200;
		private const int SQL_OV_ODBC3 = 3;
		private const short SQL_SUCCESS = 0;
		
		private const short SQL_NEED_DATA = 99;
		private const short DEFAULT_RESULT_SIZE = 1024;
		private const string SQL_DRIVER_STR = "DRIVER=SQL SERVER";

    public static List<string> GetServers()
    {
      List<string> serverList = new List<string>();
      string[] retval = null;
      string txt = string.Empty;
      IntPtr henv = IntPtr.Zero;
      IntPtr hconn = IntPtr.Zero;
      StringBuilder inString = new StringBuilder(SQL_DRIVER_STR);
      StringBuilder outString = new StringBuilder(DEFAULT_RESULT_SIZE);
      short inStringLength = (short)inString.Length;
      short lenNeeded = 0;

      try
      {
        if (SQL_SUCCESS == SQLAllocHandle(SQL_HANDLE_ENV, henv, out henv))
        {
          if (SQL_SUCCESS == SQLSetEnvAttr(henv, SQL_ATTR_ODBC_VERSION, (IntPtr)SQL_OV_ODBC3, 0))
          {
            if (SQL_SUCCESS == SQLAllocHandle(SQL_HANDLE_DBC, henv, out hconn))
            {
              if (SQL_NEED_DATA == SQLBrowseConnect(hconn, inString, inStringLength, outString, DEFAULT_RESULT_SIZE, out lenNeeded))
              {
                if (DEFAULT_RESULT_SIZE < lenNeeded)
                {
                  outString.Capacity = lenNeeded;
                  if (SQL_NEED_DATA != SQLBrowseConnect(hconn, inString, inStringLength, outString, lenNeeded, out lenNeeded))
                  {
                    throw new ApplicationException("Unabled to aquire SQL Servers from ODBC driver.");
                  }
                }

                txt = outString.ToString();

                int start = txt.IndexOf("{") + 1;
                int len = txt.IndexOf("}") - start;

                if ((start > 0) && (len > 0))
                {
                  txt = txt.Substring(start, len);
                }
                else
                {
                  txt = string.Empty;
                }
              }
            }
          }
        }
      }
      catch
      {
        txt = string.Empty;

        serverList.Add("<No servers found>");
      }
      finally
      {
        if (hconn != IntPtr.Zero)
        {
          SQLFreeHandle(SQL_HANDLE_DBC, hconn);
        }
        if (henv != IntPtr.Zero)
        {
          SQLFreeHandle(SQL_HANDLE_ENV, hconn);
        }
      }

      if (txt.Length > 0)
      {
        retval = txt.Split(",".ToCharArray());

        if (retval != null)
        {
          foreach (string serverName in retval)
          {
            serverList.Add(serverName);
          }
        }
      }

      return serverList;
    }
  }
}
