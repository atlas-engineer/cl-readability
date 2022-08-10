;;;; SPDX-FileCopyrightText: Atlas Engineer LCC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

(defvar *document-url* nil
  "The URL of the document currently being processed.")

(defvar *min-content-length* 140
  "The minimum node content length used to decide if the document is readerable.")

(defvar *min-score* 20
  "The minumum cumulated 'score' used to determine if the document is readerable.")

;;; XXX: Readability.UNLIKELY_ROLES
(defvar *unlikely-roles*
  (list "menu" "menubar" "complementary" "navigation" "alert" "alertdialog" "dialog"))

(defvar *div-to-p-elements*
  (list "BLOCKQUOTE" "DL" "DIV" "IMG" "OL" "P" "PRE" "TABLE" "UL" ))

(defvar *alter-to-div-exceptions*
  (list "DIV" "ARTICLE" "SECTION" "P"))

(defvar *presentation-attributes*
  (list "align" "background" "bgcolor" "border" "cellpadding" "cellspacing"
        "frame" "hspace" "rules" "style" "valign" "vspace"))

(defvar *deprecated-size-attribute-names*
  (list "TABLE" "TH" "TD" "HR" "PRE"))

(defvar *visibility-checker* nil
  "The function used to determine if a node is visible.")

(defvar *keep-classes* nil
  "Whether to keep classes of elements when parsing the document.")

(defvar *preserved-classes* nil
  "The of classes to preserve on HTML elements.
If nil, preserve nothing.
If t, preserve everything.
If a list of strings, preserve this exact list of classes.")

(defvar *phrasing-elements*
  (list
   ;;; The commented out elements qualify as phrasing content but tend to be
   ;;; removed by readability when put into paragraphs, so we ignore them here.
   ;; "CANVAS" "IFRAME" "SVG" "VIDEO"
   "ABBR" "AUDIO" "B" "BDO" "BR" "BUTTON" "CITE" "CODE" "DATA"
   "DATALIST" "DFN" "EM" "EMBED" "I" "IMG" "INPUT" "KBD" "LABEL"
   "MARK" "MATH" "METER" "NOSCRIPT" "OBJECT" "OUTPUT" "PROGRESS" "Q"
   "RUBY" "SAMP" "SCRIPT" "SELECT" "SMALL" "SPAN" "STRONG" "SUB"
   "SUP" "TEXTAREA" "TIME" "VAR" "WBR")
  "Types of tags that usually include sensible text.")

(defvar *max-elements* nil
  "Max number of nodes supported by the parser.
Defaults to nil (no limit).")

(defvar *max-top-candidates* 5
  "The number of top candidates to consider when analysing how tight
  the competition is among candidates.")

(defvar *tags-to-score*
  '("SECTION" "H2" "H3" "H4" "H5" "H6" "P" "TD" "PRE")
  "Element tags to score by default.")

(defvar *char-threshold* 500
  "The default number of chars an article must have in order to return a result.")

;; Regular expressions

(defvar *unlikely-candidate-regex*
  "-ad-|ai2html|banner|breadcrumbs|combx|comment|community|cover-wrap|disqus|extra|footer|gdpr|header|legends|menu|related|remark|replies|rss|shoutbox|sidebar|skyscraper|social|sponsor|supplemental|ad-break|agegate|pagination|pager|popup|yom-remote")

(defvar *maybe-candidate-regex*
  "and|article|body|column|content|main|shadow")

(defvar *positive-regex*
  "article|body|content|entry|hentry|h-entry|main|page|pagination|post|text|blog|story")

(defvar *negative-regex*
  "-ad-|hidden|^hid$| hid$| hid |^hid |banner|combx|comment|com-|contact|foot|footer|footnote|gdpr|masthead|media|meta|outbrain|promo|related|scroll|share|shoutbox|sidebar|skyscraper|sponsor|shopping|tags|tool|widget")

(defvar *extraneous-regex*
  "print|archive|comment|discuss|e[\\-]?mail|share|reply|all|login|sign|single|utility")

(defvar *byline-regex*
  "byline|author|dateline|writtenby|p-author")

(defvar *normalize-regex* "\\s{2,}")

;; TODO: replaceFonts

(defvar *videos-regex*
  "\\/\\/(www\\.)?((dailymotion|youtube|youtube-nocookie|player\\.vimeo|v\\.qq)\\.com|(archive|upload\\.wikimedia)\\.org|player\\.twitch\\.tv)")

(defvar *share-element-regex*
  "(\\b|_)(share|sharedaddy)(\\b|_)")

(defvar *next-link-regex*
  "(next|weiter|continue|>([^\\|]|$)|»([^\\|]|$))")

(defvar *prev-link-regex*
  "(prev|earl|old|new|<|«)")

;; TODO: tokenize, whitespace, hasContent, hashUrl

(defvar *srcset-url-regex*
  "(\S+)(\s+[\d.]+[xw])?(\s*(?:,|$))")

(defvar *base64-data-url-regex*
  "^data:\s*([^\s;,]+)\s*;\s*base64\s*,")

(defvar *json-ld-article-types*
  "^Article|AdvertiserContentArticle|NewsArticle|AnalysisNewsArticle|AskPublicNewsArticle|BackgroundNewsArticle|OpinionNewsArticle|ReportageNewsArticle|ReviewNewsArticle|Report|SatiricalArticle|ScholarlyArticle|MedicalScholarlyArticle|SocialMediaPosting|BlogPosting|LiveBlogPosting|DiscussionForumPosting|TechArticle|APIReference$")
