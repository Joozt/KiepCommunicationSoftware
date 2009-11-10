﻿using System;
using System.Data.Common;
using System.Collections.Generic;
using System.Text;
using System.Data;
using System.Data.SqlClient;
using System.Configuration;

namespace AutoComplete.DataAccess
{
  public class Database
  {
    public static DbConnection Connect()
    {
      DbConnection conn = Database.GetConnection();
      conn.Open();
      return conn;
    }

    public static DbConnection GetConnection()
    {
      if (ConfigurationManager.ConnectionStrings["AutoComplete"] == null)
        throw new Exception("Can't find connectionstring 'AutoComplete' in configuration file");

      DbConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["AutoComplete"].ConnectionString);
      return conn;
    }

    internal static string GetConnectionString()
    {
      return GetConnection().ConnectionString;
    }
  }
}