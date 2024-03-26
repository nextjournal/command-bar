(ns nextjournal.command-bar.demo.editor
  (:require ["@codemirror/commands" :refer [history historyKeymap]]
            ["@codemirror/language" :refer [syntaxHighlighting HighlightStyle]]
            ["@codemirror/state" :refer [Compartment EditorState StateField]]
            ["@codemirror/view" :as view :refer [keymap EditorView ViewPlugin]]
            ["@lezer/highlight" :refer [tags]]
            ["react" :as react]
            ["react-dom/client" :as react-client]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [nextjournal.command-bar :as command-bar]
            [nextjournal.command-bar.keybind :as keybind]
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

(defn get-codemirror-bindings [^js state]
  ;; Removing everything that doesn't have a fn name for now. Not sure about this yet.
  (->> (.. state (facet keymap) flat (filter #(and (or (.-key %) (.-mac %)) (.-run %) (.. % -run -name))))
       (map
        (fn [binding]
          (let [{:keys [key mac run shift]} (j/lookup binding)
                key (or key mac)]
            (concat
             [{:local? true
               :spec (command-bar/normalize-spec key)
               :run run
               :name (command-bar/get-fn-name run)}]
             (when shift
               [{:local? true
                 :spec (command-bar/normalize-spec (str "Shift-" key))
                 :run shift
                 :name (command-bar/get-fn-name run)}])))))
       flatten
       (remove nil?)))

(def extension
  #js [(.-extension (.define StateField
                             (j/lit {:create #(reset! command-bar/!local-bindings (get-codemirror-bindings %))
                                     :update (fn [_value tr]
                                               #(reset! command-bar/!local-bindings (get-codemirror-bindings (.-state tr))))})))
       (.define ViewPlugin (fn [view]
                             #js {:update (fn [^js update]
                                            (when (and (.-focusChanged update) (.. update -view -hasFocus))
                                              (reset! command-bar/!local-bindings (get-codemirror-bindings (.-state update)))
                                              (reset! command-bar/!local-view (.-view update))))}))])

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
                                       extension
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

(defonce !last-result (r/atom nil))

(defn keys-view [spec]
  (into [:span.inline-flex {:class "gap-[2px]"}]
        (map (fn [k] [:span.rounded-sm.shadow.border.border-slate-300.shadow-inner.font-bold.leading-none.text-center
                     {:class "px-[3px] py-[1px] min-w-[16px]"} k]))
        (str/split (command-bar/get-pretty-spec spec) #" ")))

(defn key-description [{:keys [local? run spec var]}]
  [:div.font-mono {:class "text-[12px]"}
   [:div
    [keys-view spec]
    " is bound to "
    (when local?
      [:span "CodeMirror's "])
    [:span
     (when-let [ns (some-> var meta :ns)]
       [:span ns "/"])
     [:span.font-bold (command-bar/get-fn-name run)]]
    (when local?
      " command")]
   (when-let [docs (some-> var meta :doc)]
     [:div.mt-3 docs])])

#_nextjournal.command-bar/toggle-command-bar
#_nextjournal.command-bar.demo.editor/describe-key

(defn doc-view [sym]
  [:div.font-mono {:class "text-[12px]"}
   (let [{:as info :keys [arglists-str doc ns name]} (demo.sci/info {:sym sym :ns "user" :ctx (sci.ctx-store/get-ctx)})]
     (if ns
       [:div
        [:div.flex.gap-2
         [:div.font-bold ns "/" name]
         [:div arglists-str]]
        ;; TODO: Revisit once we figure out how to keep symbols stable in advanced compilation
        #_(let [bindings (keep (fn [{:as binding :keys [var]}]
                                 (when-let [{:keys [ns name]} (meta var)]
                                   (when (and (= ns (:ns info)) (= name (:name info)))
                                     binding)))
                               @command-bar/!global-bindings)]
            (when (seq bindings)
              (into [:div.mt-3 "It is bound to "]
                    (map-indexed (fn [i {:keys [spec]}]
                                   [:<>
                                    (when-not (zero? i)
                                      [:span ", and "])
                                    [keys-view spec]]))
                    bindings)))
        (when doc
          [:div.mt-3 doc])]
       [:div "No docs found for " [:span.font-bold sym] "."]))])

(defn doc
  "Shows bindings and doc for a given function."
  []
  (command-bar/toggle-interactive!
   (fn [!state]
     (hooks/use-effect
      (fn []
        (keybind/disable!)
        #(keybind/enable!)))
     [:<>
      [command-bar/label {:text "Which name:"}]
      [command-bar/input !state {:placeholder "Enter name…"
                                 :default-value (or (:doc/name @!state) "")
                                 :on-key-down (fn [event]
                                                (when (= (.-key event) "Enter")
                                                  (swap! !last-result assoc :result (r/as-element [doc-view (:doc/name @!state)])))
                                                (when (contains? #{"Escape" "Enter"} (.-key event))
                                                  (.preventDefault event)
                                                  (.stopPropagation event)
                                                  (command-bar/kill-interactive!)
                                                  (swap! !state dissoc :doc/name)))
                                 :on-input (fn [event]
                                             (swap! !state assoc :doc/name (.. event -target -value)))}]])))

(defn describe-key
  "Describes which function a key or sequence of keys is bound to. Shows the bound function's docstring when available."
  []
  (command-bar/toggle-interactive!
   (fn [!state]
     (hooks/use-effect
      (fn []
        (keybind/disable!)
        #(keybind/enable!)))
     [:<>
      [command-bar/label {:text "Describe key:"}]
      [command-bar/input !state {:placeholder "Press a key or binding…"
                                 :default-value (if-let [spec (:describe-key/spec @!state)]
                                                  (command-bar/get-pretty-spec spec)
                                                  "")
                                 :on-key-down (fn [event]
                                                (.preventDefault event)
                                                (.stopPropagation event)
                                                (if (= (.-key event) "Escape")
                                                  (command-bar/kill-interactive!)
                                                  (let [chord (keybind/e->chord event)
                                                        spec (cond-> chord
                                                               (command-bar/mod-only-chord? chord) (dissoc :button)
                                                               true command-bar/chord->spec)]
                                                    (swap! !state assoc :describe-key/spec spec))))
                                 :on-key-up (fn [event]
                                              (when-let [spec (:describe-key/spec @!state)]
                                                (when-let [binding (command-bar/get-binding-by-spec spec)]
                                                  (swap! !last-result assoc :result (r/as-element [key-description binding])))
                                                (swap! !state dissoc :describe-key/spec)
                                                (command-bar/kill-interactive!)))}]])))

(defn root []
  [:div
   [:div.absolute.left-0.top-0.w-full.bg-slate-200.p-4
    {:class "bottom-[24px]"}
    [editor {:source "(+ 1 1)"
             :on-result #(reset! !last-result %)
             :on-reset-result #(reset! !last-result nil)}]]
   [:div.absolute.left-0.bottom-0.w-full
    (when-some [{:keys [error result]} @!last-result]
      [:div.bg-white.border-t.border-slate-300.px-4.py-3
       (cond
         error [:div.red error]
         (render/valid-react-element? result) result
         :else (render/inspect result))])
    [command-bar/view {:describe-key {:binding "Alt-d" :run describe-key}
                       :doc {:binding "Shift-Alt-d" :run doc}}]]])
