;; from: https://github.com/ekaschalk/.spacemacs.d/blob/master/layers/display/local/pretty-fonts/pretty-fonts.el

(defconst fira-code-regexs
  '(
    (#Xe100 "www"       "\\(www\\)")
    ;; it's difficult to make e101 through e105 work with e16f
    ;; (#Xe101 "**")
    ;; (#Xe102 "***")
    ;; (#Xe103 "**/")
    ;; (#Xe104 "*>")
    ;; (#Xe105 "*/")
    (#Xe106 "\\\\"      "\\(\\\\\\\\\\)")
    ;; removed e107 because grouping long strings of backslashes in pairs is more useful than triples
    ;; (#Xe107 "\\\\\\")
    (#Xe108 "{-"        "\\({-\\)")
    (#Xe109 "[]"        "\\(\\[\\]\\)")
    (#Xe10a "::"        "\\(::\\)")
    (#Xe10b ":::"       "\\(:::\\)")
    (#Xe10c ":="        "^\\(:=\\)")
    (#Xe10c ":="        "[^=]\\(:=\\)")
    (#Xe10d "!!"        "\\(!!\\)")
    (#Xe10e "!="        "\\(!=\\)")
    (#Xe10f "!=="       "\\(!==\\)")
    (#Xe110 "-}"        "\\(-}\\)")
    (#Xe111 "--"        "\\(--\\)")
    (#Xe112 "---"       "\\(---\\)")
    (#Xe113 "-->"       "\\(-->\\)")
    (#Xe114 "->"        "^\\(->\\)")
    (#Xe114 "->"        "[^-]\\(->\\)")
    (#Xe115 "->>"       "\\(->>\\)")
    (#Xe116 "-<"        "\\(-<\\)" )
    (#Xe117 "-<<"       "\\(-<<\\)")
    (#Xe118 "-~"        "\\(-~\\)")
    (#Xe119 "#{"        "\\(#{\\)")
    (#Xe11a "#["      "\\(#\\[\\)")
    (#Xe11b "##"        "\\(##\\)")
    (#Xe11c "###"       "\\(###\\)")
    (#Xe11d "####"      "\\(####\\)")
    (#Xe11e "#("        "\\(#(\\)")
    (#Xe11f "#?"      "\\(#\\?\\)")
    (#Xe120 "#_"        "\\(#_\\)")
    (#Xe121 "#_("       "\\(#_(\\)")
    (#Xe122 ".-"        "\\(\\.-\\)")
    (#Xe123 ".="        "\\(\\.=\\)")
    (#Xe124 ".."        "\\(\\.\\.\\)")
    (#Xe125 "..<"       "\\(\\.\\.<\\)")
    (#Xe126 "..."       "\\(\\.\\.\\.\\)")
    (#Xe127 "?="        "\\(\\?=\\)")
    (#Xe128 "??"        "\\(\\?\\?\\)")
    (#Xe129 ";;"        "\\(;;\\)")
    (#Xe12a "/*"        "\\(\\/\\*\\)")
    (#Xe12b "/**"       "\\(\\/\\*\\*\\)")
    (#Xe12c "/="        "\\(/=\\)")
    (#Xe12d "/=="       "\\(/==\\)")
    (#Xe12e "/>"        "\\(/>\\)")
    (#Xe12f "//"        "\\(//\\)")
    (#Xe130 "///"       "\\(///\\)")
    (#Xe131 "&&"        "\\(&&\\)")
    (#Xe132 "||"        "\\(||\\)")
    (#Xe133 "||="       "\\(||=\\)")
    (#Xe134 "|="        "^\\(|=\\)")
    (#Xe134 "|="        "[^|]\\(|=\\)")
    (#Xe135 "|>"        "\\(|>\\)")
    (#Xe136 "^="        "\\(\\^=\\)")
    (#Xe137 "$>"        "\\(\\$>\\)")
    (#Xe138 "++"        "\\(\\+\\+\\)")
    (#Xe139 "+++"       "\\(\\+\\+\\+\\)")
    (#Xe13a "+>"        "\\(\\+>\\)")
    (#Xe13b "=:="       "\\(=:=\\)")
    (#Xe13c "=="        "^\\(==\\)[^>]")
    (#Xe13c "=="        "[^!/]\\(==\\)[^>]")
    (#Xe13d "==="       "\\(===\\)")
    (#Xe13e "==>"       "\\(==>\\)")
    (#Xe13f "=>"        "^\\(=>\\)")
    (#Xe13f "=>"        "[^=]\\(=>\\)")
    (#Xe140 "=>>"       "\\(=>>\\)")
    (#Xe141 "<="        "\\(<=\\)")
    (#Xe142 "=<<"       "\\(=<<\\)")
    (#Xe143 "=/="       "\\(=/=\\)")
    (#Xe144 ">-"        "\\(>-\\)" )
    (#Xe145 ">="        "\\(>=\\)")
    (#Xe146 ">=>"       "\\(>=>\\)")
    (#Xe147 ">>"        "^\\(>>\\)")
    (#Xe147 ">>"        "[^-=]\\(>>\\)")
    (#Xe148 ">>-"       "\\(>>-\\)")
    (#Xe149 ">>="       "\\(>>=\\)")
    (#Xe14a ">>>"       "\\(>>>\\)")
    (#Xe14b "<*"        "\\(<\\*\\)")
    (#Xe14c "<*>"       "\\(<\\*>\\)")
    (#Xe14d "<|"        "\\(<|\\)")
    (#Xe14e "<|>"       "\\(<|>\\)")
    (#Xe14f "<$"        "\\(<\\$\\)")
    (#Xe150 "<$>"       "\\(<\\$>\\)")
    (#Xe151 "<!--"      "\\(<!--\\)")
    (#Xe152 "<-"        "\\(<-\\)")
    (#Xe153 "<--"       "\\(<--\\)")
    (#Xe154 "<->"       "\\(<->\\)")
    (#Xe155 "<+"        "\\(<\\+\\)")
    (#Xe156 "<+>"       "\\(<\\+>\\)")
    (#Xe157 "<="        "\\(<=\\)")
    (#Xe158 "<=="       "\\(<==\\)")
    (#Xe159 "<=>"       "\\(<=>\\)")
    (#Xe15a "<=<"       "\\(<=<\\)")
    (#Xe15b "<>"        "\\(<>\\)")
    (#Xe15c "<<"        "^\\(<<\\)")
    (#Xe15c "<<"        "[^-=]\\(<<\\)")
    (#Xe15d "<<-"       "\\(<<-\\)")
    (#Xe15e "<<="       "\\(<<=\\)")
    (#Xe15f "<<<"       "\\(<<<\\)")
    (#Xe160 "<~"        "\\(<~\\)")
    (#Xe161 "<~~"       "\\(<~~\\)")
    (#Xe162 "</"        "\\(</\\)")
    (#Xe163 "</>"       "\\(</>\\)")
    (#Xe164 "~@"        "\\(~@\\)")
    (#Xe165 "~-"        "\\(~-\\)" )
    (#Xe166 "~="        "\\(~=\\)")
    (#Xe167 "~>"        "\\(~>\\)")
    (#Xe168 "~~"        "^\\(~~\\)")
    (#Xe168 "~~"        "[^<]\\(~~\\)")
    (#Xe169 "~~>"       "\\(~~>\\)")
    (#Xe16a "%%"        "\\(%%\\)")
    (#Xe16b "x"         "0\\(x\\)[0-9a-fA-F]")
    (#Xe16c ":"         "^\\(:\\)[^:=]")
    (#Xe16c ":"         "[^:=]\\(:\\)[^:=]")
    (#Xe16d "+"         "^\\(\\+\\)[^\\+<>]")
    (#Xe16d "+"         "[^\\+<>]\\(\\+\\)[^\\+<>]")
    (#Xe16f "*"         "^\\(\\*\\)")
    (#Xe16f "*"         "[^/\\<>]\\(\\*\\)")
    ))

(defun fira-code--pad-codepoint (codepoint)
  (concat "\t" (char-to-string codepoint)))

(defun fira-code--build-keyword (entry)
  (let ((codepoint (first entry))
        (regex (third entry)))
    `(,regex (0 (prog1 nil
                  (compose-region (match-beginning 1)
                                  (match-end 1)
                                  ,(fira-code--pad-codepoint codepoint)))))))

(defun fira-code ()
  (let ((keywords (mapcar #'fira-code--build-keyword fira-code-regexs)))
    (font-lock-add-keywords nil keywords))

(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))
