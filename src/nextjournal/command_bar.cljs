(ns nextjournal.command-bar
  (:require ["@lezer/highlight" :as highlight :refer [tags]]
            ["@codemirror/language" :as language :refer [LRLanguage LanguageSupport]]
            ["@nextjournal/lezer-clojure" :as lezer-clj]
            [applied-science.js-interop :as j]
            [nextjournal.clojure-mode.extensions.close-brackets :as close-brackets]
            [nextjournal.clojure-mode.extensions.match-brackets :as match-brackets]
            [nextjournal.clojure-mode.extensions.formatting :as format]
            [nextjournal.clojure-mode.extensions.selection-history :as sel-history]
            [nextjournal.clojure-mode.keymap :as keymap]
            [nextjournal.clojure-mode.node :as n]
            [nextjournal.clojure-mode.test-utils :as test-utils]))
