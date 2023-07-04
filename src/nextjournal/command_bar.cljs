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
(defonce !state (reagent/atom {}))

(defn global-unset-key! [keybinding]
  (swap! !global-bindings (fn [bindings]
                            (remove #(= keybinding (j/get % :key)) bindings))))

(defn global-set-key! [keybinding fvar]
  (swap! !global-bindings conj #js {:key keybinding :run @fvar :var fvar}))

(defn get-binding [key]
  (first (filter #(= key (j/get % :key)) @!bindings)))

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

(defn is-only-modifier? [event]
  (contains? modifiers (.-key event)))

(defn get-full-key [event]
  (let [modifier (get-event-modifier event)]
    (if (is-only-modifier? event)
      (get-event-modifier event)
      (cond->> (-> (.-code event) (str/replace #"Key" "") str/lower-case)
        modifier (str modifier "-")))))

;; TODO: Use keyboard event lib that allows chording/multiple modifiers
(defn use-global-keybindings []
  (hooks/use-effect
   (fn []
     (let [handle-global-key (fn [event]
                               (when-not (is-only-modifier? event)
                                 (when-let [binding (get-global-binding (get-full-key event))]
                                   (.preventDefault event)
                                   (j/call binding :run))))]
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
         (let [fn-name (-> (str/split n #"\$_?") last (str/replace #"_" "-"))]
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

(defn kill-interactive! []
  (swap! !state dissoc :interactive))

(defn make-interactive! [interactive-fn]
  (swap! !state assoc :interactive interactive-fn))

(defn toggle-interactive! [interactive-fn]
  (if (:interactive @!state)
    (kill-interactive!)
    (make-interactive! interactive-fn)))

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

(defn handle-component-keys [keymap event]
  (doseq [[key f] keymap]
    (when (= (.-key event) key)
      (.preventDefault event)
      (f event))))

(def default-component-keys
  {"Escape" #(kill-interactive!)
   "Enter" #(kill-interactive!)})

(defn label [{:keys [text]}]
  [:label.text-white.mr-2.flex.items-center.font-mono {:class "h-[26px] text-[12px]"} text])

;; TODO: Find a more elegant way than on-arrow-, &c
(defn input [!state {:keys [placeholder on-input on-blur on-key-down on-key-up component-keys default-value]}]
  (let [{:input/keys [query]} @!state]
    [:div.relative.flex-shrink-0.border-r.border-slate-600.pr-3.mr-3
     [:div.whitespace-no-wrap.font-mono.pointer-events-none
      {:class "text-[12px]"}
      (if (str/blank? query) placeholder query)]
     [:input.bg-transparent.font-mono.text-white.focus:outline-none.absolute.left-0.top-0.w-full
      {:autoFocus true
       :on-input on-input
       :on-blur on-blur
       :on-key-down (or on-key-down (partial handle-component-keys (merge default-component-keys component-keys)))
       :on-key-up (or on-key-up #())
       :class "text-[12px]"
       :type "text"
       :placeholder placeholder
       :default-value default-value}]]))

(defn pick-list [!state {:keys [placeholder items on-select]}]
  (let [{:pick-list/keys [filtered-items selected-index] :input/keys [query]
         :or {selected-index 0}} @!state
        items* (if (str/blank? query)
                 items
                 (fuzzy/search items #(get-fn-name (j/get % :run)) query))]
    [:<>
     [:style ".cmd-list::-webkit-scrollbar { height: 0; } .cmd-list { scrollbar-width: none; }"]
     [input !state {:placeholder placeholder
                    :on-input (fn [event]
                                (swap! !state merge {:input/query (.. event -target -value)
                                                     :pick-list/selected-index 0
                                                     :pick-list/filtered-items items*}))
                    :on-blur #(swap! !state dissoc :input/query :pick-list/selected-index :pick-list/filtered-items)
                    :component-keys {"ArrowRight" (fn [] (swap! !state update :pick-list/selected-index #(min (dec (count items*)) (inc %))))
                                     "ArrowLeft" (fn [] (swap! !state update :pick-list/selected-index #(max 0 (dec %))))
                                     "Enter" (partial on-select (nth items* selected-index))}}]
     (into [:div.cmd-list.flex.flex-auto.items-center.gap-3.overflow-x-auto]
           (map-indexed (fn [i binding]
                          [cmd-view {:binding binding
                                     :selected? (= i selected-index)}]))
           (or filtered-items items))]))

(defn toggle-command-bar
  "Shows a searchable list of all available commands. Use ← and → to navigate or ↩ to run the selected command."
  []
  (toggle-interactive! (fn [!state]
                         [pick-list !state {:placeholder "Search for commands…"
                                            :items @!bindings
                                            :on-select (fn [selected-binding _event]
                                                         (kill-interactive!)
                                                         (run-binding selected-binding))}])))

(defn view [commands]
  (let [!el (hooks/use-ref nil)
        {:keys [interactive]} @!state]
    (use-global-keybindings)
    (use-watches)
    (hooks/use-effect (fn []
                        (global-set-key! "Alt-x" #'toggle-command-bar)
                        (doseq [[binding run] commands]
                          (global-set-key! binding run))
                        #(do (global-unset-key! "Alt-x")
                             (doseq [[binding _run] commands]
                               (global-unset-key! binding)))))
    [:div.bg-slate-950.px-4.flex.items-center
     {:ref !el}
     (if interactive
       [interactive !state]
       [:div.text-slate-300 {:class "text-[12px]"}
        (when-let [binding (get-global-binding "Alt-x")]
          [cmd-view {:binding binding}])])]))
