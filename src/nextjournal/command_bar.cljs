(ns nextjournal.command-bar
  (:require ["@lezer/highlight" :as highlight :refer [tags]]
            ["@codemirror/state" :refer [StateField]]
            ["@codemirror/view" :refer [keymap]]
            ["@codemirror/language" :as language :refer [LRLanguage LanguageSupport]]
            ["@nextjournal/lezer-clojure" :as lezer-clj]
            [applied-science.js-interop :as j]
            [nextjournal.clojure-mode.extensions.close-brackets :as close-brackets]
            [nextjournal.clojure-mode.extensions.match-brackets :as match-brackets]
            [nextjournal.clojure-mode.extensions.formatting :as format]
            [nextjournal.clojure-mode.extensions.selection-history :as sel-history]
            [nextjournal.clojure-mode.keymap :as keymap]
            [nextjournal.clojure-mode.node :as n]
            [nextjournal.clojure-mode.test-utils :as test-utils]
            [reagent.core :as reagent]))

(defonce !bindings (reagent/atom {}))

(defn get-bindings [^js state]
  (.filter
   (.flat (.facet state keymap))
   (fn [binding]
     (let [{:keys [key mac run]} (j/lookup binding)]
       (and (or key mac) run)))))

(def extension
  (.-extension (.define StateField
                        (j/lit {:create #(reset! !bindings (get-bindings %))
                                :update (fn [_value tr]
                                          #(reset! !bindings (get-bindings (.-state tr))))}))))

@!bindings
