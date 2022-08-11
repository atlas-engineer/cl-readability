;;;; SPDX-FileCopyrightText: Atlas Engineer LCC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

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
