(ns nextjournal.command-bar
  (:require ["@codemirror/state" :refer [StateField]]
            ["@codemirror/view" :refer [keymap EditorView ViewPlugin]]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [nextjournal.clerk.render.hooks :as hooks]
            [nextjournal.command-bar.fuzzy :as fuzzy]
            [reagent.core :as reagent]))

(defonce !bindings (reagent/atom []))
(defonce !global-bindings (reagent/atom []))
(defonce !codemirror-bindings (reagent/atom []))
(defonce !codemirror-view (reagent/atom nil))

(defn global-unset-key! [keybinding]
  (swap! !global-bindings (fn [bindings]
                            (remove #(= keybinding (j/get % :key)) bindings))))

(defn global-set-key! [keybinding f]
  (swap! !global-bindings conj #js {:key keybinding :run f}))

(defn get-global-binding [key]
  (first (filter #(= key (j/get % :key)) @!global-bindings)))

(defn get-codemirror-bindings [^js state]
  (-> (.facet state keymap)
      (.flat)
      (.filter
       (fn [binding]
         ;; TODO: removing everything that doesn't have a fn name for now. Let's revisit.
         (let [{:keys [key mac run]} (j/lookup binding)]
           (and (or key mac) run (.-name run)))))
      (.map #(j/assoc! % :codemirror true))))

(def extension
  #js [(.-extension (.define StateField
                             (j/lit {:create #(reset! !codemirror-bindings (get-codemirror-bindings %))
                                     :update (fn [_value tr]
                                               #(reset! !codemirror-bindings (get-codemirror-bindings (.-state tr))))})))
       (.define ViewPlugin (fn [view]
                             #js {:update (fn [^js update]
                                            (when (and (.-focusChanged update) (.. update -view -hasFocus))
                                              (reset! !codemirror-bindings (get-codemirror-bindings (.-state update)))
                                              (reset! !codemirror-view (.-view update))))}))])

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

(defonce !fn-names (atom {}))

(defn get-fn-name [f]
  ;; TODO: Expose ns too when not default commands, e.g. view/toggle-command-bar
  ;; TODO: Show binding only on first occurence of command, others don't show their (duplicate) bindings
  (let [n (.-name f)]
    (get @!fn-names n
         (let [fn-name (-> (str/split n #"\$[_]") last (str/replace #"_" "-"))]
           (swap! !fn-names assoc n fn-name)
           fn-name))))

(defn get-pretty-key [key]
  (->> (str/split key #"-")
       (map (fn [k]
              ;; TODO: Investigate why Cmd/Meta/Mod and Control/Ctrl are not unified
              (get {"Alt" "⌥"
                    "Control" "^"
                    "Ctrl" "^"
                    "Shift" "⇧"
                    "Cmd" "⌘"
                    "Meta" "⌘"
                    "Mod" "⌘"
                    "ArrowLeft" "←"
                    "ArrowRight" "→"
                    "ArrowUp" "↑"
                    "ArrowDown" "↓"
                    "Backspace" "⌫"
                    "Enter" "↩"} k (str/upper-case k))))
       (str/join " ")))

(defn run-binding [binding]
  (let [{:keys [codemirror run]} (j/lookup binding)]
    (if codemirror
      (do
        (.focus @!codemirror-view)
        (js/requestAnimationFrame #(run @!codemirror-view)))
      (run))))

(defn cmd-view [{:keys [binding selected?]}]
  (let [!el (hooks/use-ref nil)
        {:keys [key mac run]} (j/lookup binding)
        fn-name (.-name run)]
    (hooks/use-effect #(when selected? (.scrollIntoViewIfNeeded @!el)) [selected?])
    [:div.flex.items-center.flex-shrink-0.font-mono.gap-1.relative.transition
     {:class (str "text-[12px] h-[26px] " (if selected? "text-indigo-300" "text-white"))
      :ref !el}
     [:div (get-fn-name run)]
     [:div.font-inter
      {:class (if selected? "text-indigo-300" "text-slate-300")}
      (get-pretty-key (or key mac))]
     [:div.absolute.bottom-0.left-0.w-full.transition.bg-indigo-300
      {:class (str "h-[2px] " (if selected? "opacity-100" "opacity-0"))}]]))

(defn cmd-list [!state]
  (let [{:keys [filtered-bindings query selected-index] :or {selected-index 0}} @!state]
    [:<>
     [:style ".cmd-list::-webkit-scrollbar { height: 0; } .cmd-list { scrollbar-width: none; }"]
     (into [:div.cmd-list.flex.flex-auto.items-center.gap-3.overflow-x-auto]
           (map-indexed (fn [i binding]
                          [cmd-view {:binding binding
                                     :selected? (= i selected-index)}]))
           (or filtered-bindings @!bindings))]))

(defn handle-component-keys [keymap event]
  (doseq [[key f] keymap]
    (when (= (.-key event) key)
      (.preventDefault event)
      (f event))))

(defn query-input [!state]
  (let [{:keys [filtered-bindings query selected-index] :or {selected-index 0}} @!state
        placeholder "Search for commands…"
        bindings* (if (str/blank? query)
                    @!bindings
                    (fuzzy/search @!bindings #(get-fn-name (j/get % :run)) query))]
    [:<>
     [:div.relative.flex-shrink-0.border-r.border-slate-600.pr-3.mr-3
      [:div.whitespace-no-wrap.font-mono.pointer-events-none
       {:class "text-[12px]"}
       (if (str/blank? query) placeholder query)]
      [:input.bg-transparent.font-mono.text-white.focus:outline-none.absolute.left-0.top-0.w-full.h-full
       {:autoFocus true
        :on-input (fn [event]
                    (swap! !state merge {:query (.. event -target -value)
                                         :selected-index 0
                                         :filtered-bindings bindings*}))
        :on-blur #(swap! !state dissoc :query :selected-index :filtered-bindings)
        :on-key-down (fn [event]
                       (handle-component-keys
                        {"ArrowRight" (fn [] (swap! !state update :selected-index #(min (dec (count bindings*)) (inc %))))
                         "ArrowLeft" (fn [] (swap! !state update :selected-index #(max 0 (dec %))))
                         "Escape" #(swap! !state dissoc :focus?)
                         "Enter" (fn []
                                   (swap! !state dissoc :focus?)
                                   (run-binding (nth bindings* selected-index)))}
                        event))
        :class "text-[12px]"
        :type "text"
        :placeholder placeholder}]]]))

(defn view []
  (let [!el (hooks/use-ref nil)
        !state (hooks/use-state {})
        {:keys [focus?]} @!state]
    (use-global-keybindings)
    (use-watches)
    (hooks/use-effect (fn []
                        (global-set-key! "Alt-x" (fn toggle-command-bar [] (swap! !state update :focus? not)))
                        #(global-unset-key! "Alt-x")))
    [:div.bg-slate-950.px-4.flex.items-center
     {:ref !el}
     (if focus?
       [:<>
        [query-input !state]
        [cmd-list !state]]
       [:div.text-slate-300 {:class "text-[12px]"}
        (when-let [binding (get-global-binding "Alt-x")]
          [cmd-view {:binding binding}])])]))
