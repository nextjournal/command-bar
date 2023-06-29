(ns nextjournal.command-bar.demo
  (:require ["@codemirror/commands" :refer [history historyKeymap]]
            ["@codemirror/language" :refer [syntaxHighlighting HighlightStyle]]
            ["@codemirror/state" :refer [Compartment EditorState]]
            ["@codemirror/view" :as view :refer [EditorView]]
            ["@lezer/highlight" :refer [tags]]
            ["react" :as react]
            ["react-dom/client" :as react-client]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [nextjournal.command-bar :as command-bar]
            [nextjournal.command-bar.demo.sci :as demo.sci]
            [nextjournal.clerk.render :as render]
            [nextjournal.clerk.render.hooks :as hooks]
            [nextjournal.clerk.viewer :as v]
            [nextjournal.clojure-mode :as clojure-mode]
            [nextjournal.clojure-mode.extensions.eval-region :as eval-region]
            [nextjournal.clojure-mode.keymap :as keymap]
            [nextjournal.clojure-mode.test-utils :as test-utils]
            [reagent.core :as r]
            [shadow.resource :as rc]))

(def highlight-style
  (.define HighlightStyle
           (clj->js [{:tag (.-meta tags) :class "cmt-meta"}
                     {:tag (.-link tags) :class "cmt-link"}
                     {:tag (.-heading tags) :class "cmt-heading"}
                     {:tag (.-emphasis tags) :class "cmt-italic"}
                     {:tag (.-strong tags) :class "cmt-strong"}
                     {:tag (.-strikethrough tags) :class "cmt-strikethrough"}
                     {:tag (.-keyword tags) :class "cmt-keyword"}
                     {:tag (.-atom tags) :class "cmt-atom"}
                     {:tag (.-bool tags) :class "cmt-bool"}
                     {:tag (.-url tags) :class "cmt-url"}
                     {:tag (.-contentSeparator tags) :class "cmt-contentSeparator"}
                     {:tag (.-labelName tags) :class "cmt-labelName"}
                     {:tag (.-literal tags) :class "cmt-literal"}
                     {:tag (.-inserted tags) :class "cmt-inserted"}
                     {:tag (.-string tags) :class "cmt-string"}
                     {:tag (.-deleted tags) :class "cmt-deleted"}
                     {:tag (.-regexp tags) :class "cmt-regexp"}
                     {:tag (.-escape tags) :class "cmt-escape"}
                     {:tag (.. tags (special (.-string tags))) :class "cmt-string"}
                     {:tag (.. tags (definition (.-variableName tags))) :class "cmt-variableName"}
                     {:tag (.. tags (local (.-variableName tags))) :class "cmt-variableName"}
                     {:tag (.-typeName tags) :class "cmt-typeName"}
                     {:tag (.-namespace tags) :class "cmt-namespace"}
                     {:tag (.-className tags) :class "cmt-className"}
                     {:tag (.. tags (special (.-variableName tags))) :class "cmt-variableName"}
                     {:tag (.-macroName tags) :class "cmt-macroName"}
                     {:tag (.. tags (definition (.-propertyName tags))) :class "cmt-propertyName"}
                     {:tag (.-comment tags) :class "cmt-comment"}
                     {:tag (.-invalid tags) :class "cmt-invalid"}])))

(def theme
  (.theme EditorView
          (j/lit {"&.cm-focused" {:outline "none"}
                  ".cm-line" {:padding "0"
                              :line-height "1.6"
                              :font-size "15px"
                              :font-family "\"Fira Mono\", monospace"}
                  ".cm-cursor" {:visibility "hidden"}
                  "&.cm-focused .cm-cursor" {:visibility "visible"
                                             :animation "steps(1) cm-blink 1.2s infinite"}
                  "&.cm-focused .cm-selectionBackground" {:background-color "Highlight"}
                  ".cm-tooltip" {:border "1px solid rgba(0,0,0,.1)"
                                 :border-radius "3px"
                                 :overflow "hidden"}
                  ".cm-tooltip > ul > li" {:padding "3px 10px 3px 0 !important"}
                  ".cm-tooltip > ul > li:first-child" {:border-top-left-radius "3px"
                                                       :border-top-right-radius "3px"}
                  ".cm-tooltip.cm-tooltip-autocomplete" {:border "0"
                                                         :border-radius "6px"
                                                         :box-shadow "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                                                         "& > ul" {:font-size "12px"
                                                                   :font-family "'Fira Code', monospace"
                                                                   :background "rgb(241 245 249)"
                                                                   :border "1px solid rgb(203 213 225)"
                                                                   :border-radius "6px"}}
                  ".cm-tooltip-autocomplete ul li[aria-selected]" {:background "rgb(79 70 229)"
                                                                   :color "#fff"}
                  ".cm-matchingBracket" {:border-bottom "1px solid var(--teal-color)"
                                         :color "inherit"}})))

(defn editor [{:keys [on-result on-reset-result source] :or {on-result #() on-reset-result #()}}]
  (let [!el (hooks/use-ref nil)
        !view (atom nil)]
    (hooks/use-effect
     (fn []
       (reset! !view (new EditorView
                          (j/obj :state
                                 (test-utils/make-state
                                  #js [theme
                                       (history)
                                       clojure-mode/default-extensions
                                       (syntaxHighlighting highlight-style)
                                       (eval-region/extension {:modifier "Meta"})
                                       (demo.sci/extension {:modifier "Meta" :on-result on-result})
                                       (view/drawSelection)
                                       (.of view/keymap clojure-mode/complete-keymap)
                                       (.of view/keymap historyKeymap)
                                       (.. EditorState -transactionExtender
                                           (of (fn [^js tr]
                                                 (when (.-selection tr)
                                                   (on-reset-result))
                                                 #js {})))]
                                  source)
                                 :parent @!el)))
       (fn []
         (j/call @!view :destroy)
         (reset! !view nil))))
    [:div.w-full.h-full.font-mono {:ref !el}]))

(defn root []
  (let [!last-result (hooks/use-state nil)]
    [:div
     [:div.absolute.left-0.top-0.w-full.bg-slate-200.p-4
      {:class "bottom-[24px]"}
      [editor {:source "(+ 1 1)"
               :on-result #(reset! !last-result %)
               :on-reset-result #(reset! !last-result nil)}]]
     [:div.absolute.left-0.bottom-0.w-full
      (when-some [{:keys [error result]} @!last-result]
        [:div.bg-white.border-t.border-slate-300.p-3
         (cond
           error [:div.red error]
           (render/valid-react-element? result) result
           'else (render/inspect result))])
      [:div.bg-slate-950.px-4 {:class "h-[26px]"}]]]))

(defonce react-root
  (react-client/createRoot (js/document.getElementById "root")))

(defn ^:dev/after-load render []
  (when react-root
    (.render react-root (r/as-element [root]))))
