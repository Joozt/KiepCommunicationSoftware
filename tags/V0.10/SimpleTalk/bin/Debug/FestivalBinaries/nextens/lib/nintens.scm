;;; $Id: nintens.scm,v 1.6 2003/03/13 08:35:07 joopk Exp $

;;; this is the initialisation file that is read by the nextens gui


;;; use the Dutch voice
(voice_net_nl_ib_mbrola)

;;; Linux font settings

(if (string-matches *ostype* ".*Linux.*")
    (begin
      (set! gui::OrthFont "times")
      (set! gui::OrthFontType "medium")
      (set! gui::OrthFontSize "18")
      
      (set! gui::ToDIFont "lucida")
      (set! gui::ToDIFontType "bold")
      (set! gui::ToDIFontSize "14")
      
      (set! gui::F0Font "lucida")
      (set! gui::F0FontType "bold")
      (set! gui::F0FontSize "14")
      ))

(provide 'nintens)






