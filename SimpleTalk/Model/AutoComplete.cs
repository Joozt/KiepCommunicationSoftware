//#define DATABASE_ENABLED //uncomment this line to enable autocompletion database

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace SimpleTalk.Model
{
  class AutoComplete
  {
    private List<string> _Suggestions = new List<string>();
    //remember length of word list, because items are only stored with increasing amount of text. 
    private int textLength = 0;
    private int previousTextLength = 0;

    public event EventHandler SuggestionsChanged;
    private void OnSuggestionsChanged(object sender, EventArgs e)
    {
      if (SuggestionsChanged != null)
      {
        SuggestionsChanged(sender, e);
      }
    }

#if (DATABASE_ENABLED)
    private DataAccess.AutoWords _autoWords;
    private DataAccess.NextWords _nextWords;
#endif


    public AutoComplete()
    {
#if (DATABASE_ENABLED)
      _autoWords = new DataAccess.AutoWords();
      _nextWords = new DataAccess.NextWords();
#endif
    }

    public List<string> Suggestions
    {
      get
      {
        return _Suggestions;
      }
    }

    public void ImportWords(string fileNamePath)
    {
      string[] wordArray = File.ReadAllLines(fileNamePath);

      foreach (string word in wordArray)
      {
        // Todo:  make database connection and add word row to database
        // Todo: if word row does exists skip word

#if (DATABASE_ENABLED)
        _autoWords.Add(new Model.AutoWord(default(int), word, 0, false, false));
#endif

      }
    }

    private void SetWordsCount(string word)
    {
      //Todo: make database connection and update word counter.
      //Todo: if word row does not exists create row

#if (DATABASE_ENABLED)
      _autoWords.UpdateWordCount(word);
#endif

      //for debugging:
      //_listSuggestions.Add("SetWordsCount(" + word + ")");
    }

    private void SetNextWordsCount(string secondLastWord, string lastWord)
    {
      //Todo: make database connection and update next word counter.
      //Todo: if nextWord row does not exists create it

#if (DATABASE_ENABLED)
      _nextWords.UpdateNextWordCount(secondLastWord, lastWord);
#endif

      //for debugging:
      //_listSuggestions.Add("SetNextWordsCount(" + secondLastWord + "," + lastWord + ")");
    }

    private void GetWordList(string word)
    {
      //Todo: make database connection and return word list.

#if (DATABASE_ENABLED)
      _Suggestions = _autoWords.GetWordList(word);
#endif

      //for debugging:
      _Suggestions.Add(word);

      OnSuggestionsChanged(this, new EventArgs());
    }

    private void GetNextWordList(string word)
    {
      
#if (DATABASE_ENABLED)
      _Suggestions = _nextWords.GetNextWordList(word);
#endif

      //Gives only next word suggestions when there are 3 or more options in the database to make it hard for regenerate the last typed sentences. (Privacy issue)
      if (_Suggestions.Count() < 3)
      {
        _Suggestions.Clear();
      }


      //for debugging:
      //_listSuggestions.Add("GetNextWord(" + word + ")");

      OnSuggestionsChanged(this, new EventArgs());
    }

    public void OnTextChanged(string text)
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
      //for debugging:
      //_listSuggestions.Clear();

      if (string.IsNullOrEmpty(text))
      {
        //return empty list in case of empty string
        Suggestions.Clear();  //This line should be uncommented in the final code 
        OnSuggestionsChanged(this, new EventArgs());

        //for debugging:
        //_listSuggestions.Add("Clear list");
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

    public List<string> GetAutoCompleteList()
    {
      return Suggestions;
    }

    public void Reset()
    {
      //Todo: add database reset function
#if (DATABASE_ENABLED)
      _nextWords.Reset();
      _autoWords.Reset();
#endif

      //throw new NotImplementedException();
    }
  }
}
