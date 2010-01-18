using System;
using System.Data;
using System.Data.Common;
using System.Collections.Generic;
using System.Data.SqlClient;

namespace SimpleTalk.DataAccess
{
    /// <summary>
    /// Data access to the data in table Phrases in the database
    /// </summary>

    public partial class Phrases
    {
        private const string CLASSNAME = "DataAccess.Phrases";

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

        public void Add(Model.Phrase newPhrase)
        {
            try
            {
                Database.ExecuteQuery("Phrases_Add", CommandType.StoredProcedure,
                                      new SqlParameter("@PhraseID", newPhrase.PhraseID),
                                      new SqlParameter("@DateTime", newPhrase.DateTime),
                                      new SqlParameter("@Phrase", newPhrase.PhraseLine));
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

        public void Delete(Model.Phrase phrase)
        {
            try
            {
                int recordsAffected = Database.ExecuteQuery("Phrases_Delete", CommandType.StoredProcedure,
                                                            new SqlParameter("@PhraseID", phrase.PhraseID));

                if (recordsAffected == 0)
                {
                    throw new Exception(string.Format("Unable to delete Phrase with ID '{0}'", phrase.PhraseID));
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

        public void Modify(Model.Phrase modifiedPhrase)
        {
            try
            {
                int recordsAffected = Database.ExecuteQuery("Phrases_Modify", CommandType.StoredProcedure,
                                                            new SqlParameter("@PhraseID", modifiedPhrase.PhraseID),
                                                            new SqlParameter("@DateTime", modifiedPhrase.DateTime),
                                                            new SqlParameter("@Phrase", modifiedPhrase.PhraseLine));

                if (recordsAffected == 0)
                {
                    throw new Exception(string.Format("Unable to modify Phrase with ID '{0}'", modifiedPhrase.PhraseID));
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

        public Model.Phrase GetById(Int32 PhraseID)
        {
            try
            {
                _dataReader = Database.GetDataReader("Phrases_GetById", CommandType.StoredProcedure,
                                                     new SqlParameter("@PhraseID", PhraseID));

                Model.Phrase phrase = null;

                if (_dataReader != null)
                {
                    if (_dataReader.Read())
                    {
                        phrase = CreatePhrase(_dataReader);
                    }
                }

                return phrase;
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

        public List<Model.Phrase> GetAll()
        {
            try
            {
                _dataReader = Database.GetDataReader("Phrases_GetAll", CommandType.StoredProcedure);

                List<Model.Phrase> phraseList = new List<Model.Phrase>();

                if (_dataReader != null)
                {
                    while (_dataReader.Read())
                    {
                        phraseList.Add(CreatePhrase(_dataReader));
                    }
                }

                return phraseList;
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

        public int Count()
        {
            try
            {
                _dataReader = Database.GetDataReader("Phrases_CountRecords", CommandType.StoredProcedure);

                if (_dataReader != null)
                {
                    if (_dataReader.Read())
                    {
                        return (Int32)_dataReader["Count"];
                    }
                }

                return 0;
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

        private Model.Phrase CreatePhrase(DbDataReader reader)
        {
            try
            {
                return new Model.Phrase((Int32)reader["PhraseID"],
                                        (DateTime)reader["DateTime"],
                                        (String)reader["Phrase"]);
            }
            catch
            {
                throw;
            }
        }

        public List<KeyValuePair<DateTime, string>> GetPhraseList(DateTime dateTime)
        {
            try
            {
                _dataReader = Database.GetDataReader("Phrases_GetPhraseList", CommandType.StoredProcedure,
                                                     new SqlParameter("@DateTime", dateTime));

                List<KeyValuePair<DateTime, string>> phraseList = new List<KeyValuePair<DateTime, string>>();

                if (_dataReader != null)
                {
                    while (_dataReader.Read())
                    {
                        phraseList.Add(CreatePhraseLine(_dataReader));
                    }
                }

                return phraseList;
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

        public void DeleteByDateTime(DateTime dateTime)
        {
            try
            {
                int recordsAffected = Database.ExecuteQuery("Phrases_DeleteByDateTime", CommandType.StoredProcedure,
                                                            new SqlParameter("@DateTime", dateTime));

                if (recordsAffected == 0)
                {
                    throw new Exception(string.Format("Unable to delete Phrase with DateTime '{0}'", dateTime));
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

        public void Reset()
        {
            try
            {
                Database.ExecuteQuery("Phrases_Reset", CommandType.StoredProcedure);
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

        private KeyValuePair<DateTime, string> CreatePhraseLine(DbDataReader reader)
        {
            try
            {
                return new KeyValuePair<DateTime, string>((DateTime)reader["DateTime"], (String)reader["Phrase"]);
            }
            catch
            {
                throw;
            }
        }
    }
}