using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace KiepCommunication.Model
{
  public static class DatabaseFunctions
  {
      private static List<string> _Suggestions = new List<string>();
    //remember length of word list, because items are only stored with increasing amount of text. 
      private static int textLength = 0;
      private static int previousTextLength = 0;

      public static event EventHandler SuggestionsChanged;
      private static void OnSuggestionsChanged(object sender, EventArgs e)
    {
      if (SuggestionsChanged != null)
      {
        SuggestionsChanged(sender, e);
      }
    }

    private static DataAccess.AutoWords _autoWords;
    private static DataAccess.NextWords _nextWords;
    private static DataAccess.Letters _letters;
    private static DataAccess.Phrases _phrases;

    public static List<string> Suggestions
    {
      get
      {
        return _Suggestions;
      }
    }

    public static void Initialize()
    {
        try
        {
            DataAccess.Database.Connect(string.Empty);

            _autoWords = new DataAccess.AutoWords();
            _nextWords = new DataAccess.NextWords();
            _letters = new DataAccess.Letters();
            _phrases = new DataAccess.Phrases();
        }
        catch //(Exception ex)
        {
            //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
        }
    }

    public static void ImportWords(string fileNamePath, int countOffset, int countOffsetWhenDouble)
    {
      try
      {
        string[] wordArray = File.ReadAllLines(fileNamePath);

        Model.AutoWord autoWord = null;

        foreach (string word in wordArray)
        {
          autoWord = _autoWords.GetByWord(word);

          if (autoWord == null)
          {
            _autoWords.Add(new Model.AutoWord(default(int), word, countOffset, false, false));
          }
          else
          {
            autoWord.Word = autoWord.Word.ToLower();
            autoWord.Count = countOffsetWhenDouble;

            _autoWords.Modify(autoWord);
          }
        }
      }
      catch //(Exception ex)
      {
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }

    private static void SetWordsCount(string word)
    {
      try
      {
        _autoWords.UpdateWordCount(word);
      }
      catch //(Exception ex)
      {
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }

    private static void SetLettersCount(string letter)
    {
      try
      {
        _letters.UpdateLetterCount(letter);
      }
      catch //(Exception ex)
      {
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }

    private static void SetNextWordsCount(string secondLastWord, string lastWord)
    {
      try
      {
        _nextWords.UpdateNextWordCount(secondLastWord, lastWord);
      }
      catch //(Exception ex)
      {
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }

    private static void GetWordList(string word)
    {
      try
      {
        //disable autoWord suggestion if autoWord is switched off
        if (Core.Instance.AutoWordCompeltionOn)
          _Suggestions = _autoWords.GetWordList(word);
        
        else
          _Suggestions.Clear();

          OnSuggestionsChanged(null, EventArgs.Empty);
        
      }
      catch //(Exception ex)
      {
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }

    public static List<KeyValuePair<DateTime, string>> GetPhraseList(DateTime dateTime)
    {
      try
      {
          return _phrases.GetPhraseList(dateTime);
      }
      catch //(Exception ex)
      {
        return null;
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }

    public static int PhrasesCount()
    {
        try
        {
            return _phrases.Count();
        }
        catch //(Exception ex)
        {
            return 0;
            //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
        }
    }

    private static void GetNextWordList(string word)
    {
      try
      {
        //disable nextWord suggestion if nextWord or autoWord is switched off
        if (Core.Instance.NextWordSuggestionOn && Core.Instance.AutoWordCompeltionOn)
          _Suggestions = _nextWords.GetNextWordList(word);
        else
          _Suggestions.Clear();

        if (_Suggestions.Count < 3)
        {
          _Suggestions.Clear();
        }

        OnSuggestionsChanged(null, EventArgs.Empty);
      }
      catch //(Exception ex)
      {
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }

    public static void AddPhrase(string phrase)
    {
      try
      {
        _phrases.Add(new Phrase(default(int), Convert.ToDateTime(DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")), phrase));
      }
      catch //(Exception ex)
      {
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }

    public static void DeletePhrase(DateTime dateTime)
    {
        try
        {
            _phrases.DeleteByDateTime(dateTime);
        }
        catch //(Exception ex)
        {
            //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
        }
    }

    public static void OnTextChanged(string text)
    {
      try
      {
        //note that C# adds "\r\n" to the end of a line. 
        //"\r\n" should also be added when a on screen buton "carriage return" is implemented

        if (string.IsNullOrEmpty(text))
        {
          textLength = 0;
        }
        else
        {
          textLength = text.Length;
        }

        if (string.IsNullOrEmpty(text))
        {
          //return empty list in case of empty string
          Suggestions.Clear();  //This line should be uncommented in the final code 
          OnSuggestionsChanged(null, EventArgs.Empty);
        }
        else
        {
          List<string> lineArray = new List<string>(text.Split(new string[] { "\r\n" }, StringSplitOptions.None));
          List<string> wordArray = new List<string>();

          if (lineArray != null && lineArray.Count > 0)//if is required for handling empty list exception
          {
            foreach (string word in lineArray[lineArray.Count - 1].Split(new string[] { " " }, StringSplitOptions.RemoveEmptyEntries))
            {
              wordArray.Add(word);
            }
          }

          if (text[text.Length - 1].Equals(' '))
          {
            //Last word is completed (with space)

            //Count only words if text size is increasing to prevent double counting when line is cleared with backspace
            if (textLength > previousTextLength && wordArray.Count > 0 && text.Length >= 2)
            {
              //filter for two successive split comments to prevent double word counting
              //note that C# adds "\r\n" to the end of a line.
              if (!text[text.Length - 2].Equals(' ') && !text[text.Length - 2].Equals('\n'))
              {
                SetWordsCount(wordArray[wordArray.Count - 1]);

                if (wordArray.Count >= 2)
                {
                  //Two or more complete words
                  SetNextWordsCount(wordArray[wordArray.Count - 2], wordArray[wordArray.Count - 1]);
                }
              }
            }
          }
          else
          {
            if (text[text.Length - 1].Equals('\n'))
            {
              //Last word is completed (with enter)

              //Count only words if text size is increasing to prevent double counting when line is cleared with backspace
              if (textLength > previousTextLength && text.Length >= 3)
              {
                //filter for two successive split comments to prevent double word counting
                //note that C# adds "\r\n" to the end of a line.
                if (!text[text.Length - 3].Equals(' ') && !text[text.Length - 3].Equals('\n'))
                {
                  //Get words from previous line
                  if (lineArray != null && lineArray.Count > 1)//if is required for handling empty list exception
                  {
                    wordArray.Clear();
                    foreach (string word in lineArray[lineArray.Count - 2].Split(new string[] { " " }, StringSplitOptions.RemoveEmptyEntries))
                    {
                      wordArray.Add(word);
                    }
                  }
                  if (wordArray.Count > 0)
                  {
                    SetWordsCount(wordArray[wordArray.Count - 1]);

                    if (wordArray.Count >= 2)
                    {
                      //Two or more complete words
                      SetNextWordsCount(wordArray[wordArray.Count - 2], wordArray[wordArray.Count - 1]);
                    }
                  }
                }
              }
            }
          }

          if (text[text.Length - 1].Equals(' ') && wordArray.Count > 0)
          {
            //Get suggestions for next word only is space is added and there is a word on the last line
            GetNextWordList(wordArray[wordArray.Count - 1]);
          }
          else
          {
            if (wordArray.Count > 0 && !text[text.Length - 1].Equals('\n'))
            {
              //Incomplete last word (do not count it)
              GetWordList(wordArray[wordArray.Count - 1]);
            }
          }
        }
        //remember text length
        previousTextLength = textLength;
      }
      catch //(Exception ex)
      {
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }

    public static List<string> GetAutoCompleteList()
    {
      return Suggestions;
    }

    public static void Dispose()
    {
      try
      {
        _nextWords.Dispose();
        _autoWords.Dispose();
        _letters.Dispose();
        _phrases.Dispose();

        DataAccess.Database.Disconnect();
      }
      catch //(Exception ex)
      {
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }

    public static void ResetDatabase()
    {
      try
      {
        _nextWords.Reset();
        _autoWords.Reset();
        _letters.Reset();
        _phrases.Reset();
      }
      catch //(Exception ex)
      {
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }

    public static void ClearDatabase()
    {
      try
      {
        _nextWords.Reset();
        _autoWords.Clear();
        _letters.Reset();
        _phrases.Reset();
      }
      catch //(Exception ex)
      {
        //System.Windows.Forms.MessageBox.Show(ex.Message, "Error", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Error);
      }
    }
  }
}
