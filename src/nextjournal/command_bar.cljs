(ns nextjournal.command-bar
  (:require ["@lezer/highlight" :as highlight :refer [tags]]
            ["@codemirror/state" :refer [StateField]]
            ["@codemirror/view" :refer [keymap]]
            ["@codemirror/language" :as language :refer [LRLanguage LanguageSupport]]
            ["@nextjournal/lezer-clojure" :as lezer-clj]
            ["w3c-keyname" :refer [keyName]]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [nextjournal.clerk.render.hooks :as hooks]
            [nextjournal.clojure-mode.extensions.close-brackets :as close-brackets]
            [nextjournal.clojure-mode.extensions.match-brackets :as match-brackets]
            [nextjournal.clojure-mode.extensions.formatting :as format]
            [nextjournal.clojure-mode.extensions.selection-history :as sel-history]
            [nextjournal.clojure-mode.keymap :as keymap]
            [nextjournal.clojure-mode.node :as n]
            [nextjournal.clojure-mode.test-utils :as test-utils]
            [reagent.core :as reagent]))

(defonce !bindings (reagent/atom []))
(defonce !global-bindings (reagent/atom []))
(defonce !codemirror-bindings (reagent/atom []))

(defn global-unset-key! [keybinding]
  (swap! !global-bindings (fn [bindings]
                            (remove #(= keybinding (j/get % :key)) bindings))))

(defn global-set-key! [keybinding f]
  (swap! !global-bindings conj #js {:key keybinding :run f}))

(defn get-global-binding [key]
  (first (filter #(= key (j/get % :key)) @!global-bindings)))

(defn get-codemirror-bindings [^js state]
  (.filter
   (.flat (.facet state keymap))
   (fn [binding]
     (let [{:keys [key mac run]} (j/lookup binding)]
       (and (or key mac) run (.-name run)))))) ;; TODO: removing everything that doesn't have a fn name for now. Let's revisit.

(def extension
  (.-extension (.define StateField
                        (j/lit {:create #(reset! !codemirror-bindings (get-codemirror-bindings %))
                                :update (fn [_value tr]
                                          #(reset! !codemirror-bindings (get-codemirror-bindings (.-state tr))))}))))

(defonce modifiers #{"Alt" "Control" "Meta" "Shift"})

(defn get-event-modifier [event]
  (cond
    (.-metaKey event) "Meta"
    (.-altKey event) "Alt"
    (.-ctrlKey event) "Control"
    (.-shiftKey event) "Shift"))

(defn use-global-keybindings []
  (hooks/use-effect
   (fn []
     (let [handle-global-key (fn [event]
                               (when-not (contains? modifiers (.-key event))
                                 (let [modifier (get-event-modifier event)
                                       key (cond->> (-> (.-code event) (str/replace #"Key" "") str/lower-case)
                                             modifier (str modifier "-"))]
                                   (when-let [binding (get-global-binding key)]
                                     (.preventDefault event)
                                     (j/call binding :run)))))]
       (.addEventListener js/document "keypress" handle-global-key)
       #(.removeEventListener js/document "keypress" handle-global-key)))))

(defn use-watches []
  (hooks/use-effect (fn []
                      (let [reset-bindings! #(reset! !bindings (concat @!global-bindings @!codemirror-bindings))]
                        (add-watch !global-bindings :global reset-bindings!)
                        (add-watch !codemirror-bindings :codemirror reset-bindings!)
                        #(do (remove-watch !global-bindings :global)
                             (remove-watch !codemirror-bindings :codemirror))))))

(defn get-fn-name [f]
  (-> (str/split (.-name f) #"\$[_]") last (str/replace #"_" "-")))

(defn cmd-view [binding]
  (let [{:keys [key mac run]} (j/lookup binding)
        fn-name (.-name run)]
    [:div.flex.items-center.flex-shrink-0.font-inter.gap-1.text-white
     {:class "text-[12px]"}
     [:div (get-fn-name run)]
     [:div.text-slate-300 (or key mac)]]))

(defn view []
  (let [!el (hooks/use-ref nil)
        !focus? (hooks/use-state false)]
    (use-global-keybindings)
    (use-watches)
    (hooks/use-effect (fn []
                        (let [toggle-command-bar (fn toggle-command-bar []
                                                   (swap! !focus? not))]
                          (global-set-key! "Alt-x" toggle-command-bar)
                          #(global-unset-key! "Alt-x"))))
    [:div.bg-slate-950.px-4.overflow-x-auto.flex.items-center
     {:ref !el :class "h-[26px]"}
     (if @!focus?
       [:<>
        [:input.bg-transparent.font-inter.text-white.focus:outline-none
         {:autoFocus true
          :class "text-[12px]"
          :type "text"
          :placeholder "Search for commands"}]
        (into [:div.flex.items-center.gap-3]
              (map cmd-view)
              @!bindings)]
       [:div.text-slate-300 {:class "text-[12px]"}
        (when-let [binding (get-global-binding "Alt-x")]
          [cmd-view binding])])]))

