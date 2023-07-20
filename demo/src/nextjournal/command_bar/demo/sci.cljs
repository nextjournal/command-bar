(ns nextjournal.command-bar.demo.sci
  (:require ["@codemirror/view" :as view]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [goog.object :as gobject]
            [nextjournal.clojure-mode.extensions.eval-region :as eval-region]
            [nextjournal.clerk.sci-env]
            [sci.core :as sci]
            [sci.ctx-store]))

(defn format [fmt-str x]
  (str/replace fmt-str "%s" x))

(defn fully-qualified-syms [ctx ns-sym]
  (let [syms (sci/eval-string* ctx (format "(keys (ns-map '%s))" ns-sym))
        sym-strs (map #(str "`" %) syms)
        sym-expr (str "[" (str/join " " sym-strs) "]")
        syms (sci/eval-string* ctx sym-expr)
        syms (remove #(str/starts-with? (str %) "nbb.internal") syms)]
    syms))

(defn- ns-imports->completions [ctx query-ns query]
  (let [[_ns-part name-part] (str/split query #"/")
        resolved (sci/eval-string* ctx
                                   (pr-str `(let [resolved# (resolve '~query-ns)]
                                              (when-not (var? resolved#)
                                                resolved#))))]
    (when resolved
      (when-let [[prefix imported] (if name-part
                                     (let [ends-with-dot? (str/ends-with? name-part ".")
                                           fields (str/split name-part #"\.")
                                           fields (if ends-with-dot?
                                                    fields
                                                    (butlast fields))]
                                       [(str query-ns "/" (when (seq fields)
                                                            (let [joined (str/join "." fields)]
                                                              (str joined "."))))
                                        (apply gobject/getValueByKeys resolved
                                               fields)])
                                     [(str query-ns "/") resolved])]
        (let [props (loop [obj imported
                           props []]
                      (if obj
                        (recur (js/Object.getPrototypeOf obj)
                               (into props (js/Object.getOwnPropertyNames obj)))
                        props))
              completions (map (fn [k]
                                 [nil (str prefix k)]) props)]
          completions)))))

(defn- match [_alias->ns ns->alias query [sym-ns sym-name qualifier]]
  (let [pat (re-pattern query)]
    (or (when (and (= :unqualified qualifier) (re-find pat sym-name))
          [sym-ns sym-name])
        (when sym-ns
          (or (when (re-find pat (str (get ns->alias (symbol sym-ns)) "/" sym-name))
                [sym-ns (str (get ns->alias (symbol sym-ns)) "/" sym-name)])
              (when (re-find pat (str sym-ns "/" sym-name))
                [sym-ns (str sym-ns "/" sym-name)]))))))

(defn format-1 [fmt-str x]
  (str/replace-first fmt-str "%s" x))

(defn info [{:keys [sym ctx] ns-str :ns}]
  (if-not sym
    {:status ["no-eldoc" "done"]
     :err "Message should contain a `sym`"}
    (let [code (-> "(when-let [the-var (ns-resolve '%s '%s)] (meta the-var))"
                   (format-1 ns-str)
                   (format-1 sym))
          [kind val] (try [::success (sci/eval-string* ctx code)]
                          (catch :default e
                            [::error (str e)]))
          {:keys [doc file line name arglists]} val]
      (if (and name (= kind ::success))
        (cond-> {:ns (some-> val :ns ns-name)
                 :arglists (pr-str arglists)
                 :eldoc (mapv #(mapv str %) arglists)
                 :arglists-str (.join (apply array arglists) "\n")
                 :status ["done"]
                 :name name}
          doc (assoc :doc doc)
          file (assoc :file file)
          line (assoc :line line))
        {:status ["done" "no-eldoc"]}))))

(defn eval-string
  ([source] (eval-string (sci.ctx-store/get-ctx) source))
  ([ctx source]
   (when-some [code (not-empty (str/trim source))]
     (try {:result (sci/eval-string* ctx code)}
          (catch js/Error e
            {:error (str (.-message e))})))))

(j/defn eval-at-cursor [on-result ^:js {:keys [state]}]
  (some->> (eval-region/cursor-node-string state)
           (eval-string)
           (on-result))
  true)

(j/defn eval-top-level [on-result ^:js {:keys [state]}]
  (some->> (eval-region/top-level-string state)
           (eval-string)
           (on-result))
  true)

(j/defn eval-cell [on-result ^:js {:keys [state]}]
  (-> (.-doc state)
      (str)
      (eval-string)
      (on-result))
  true)

(defn keymap* [modifier]
  {:eval-cell
   [{:key "Mod-Enter"
     :doc "Evaluate cell"}]
   :eval-at-cursor
   [{:key (str modifier "-Enter")
     :doc "Evaluates form at cursor"}]
   :eval-top-level
   [{:key (str modifier "-Shift-Enter")
     :doc "Evaluates top-level form at cursor"}]})

(defn extension [{:keys [modifier
                         on-result]}]
  (.of view/keymap
       (j/lit
        [{:key "Meta-Enter"
          :run (partial eval-cell on-result)}
         {:key (str modifier "-Enter")
          :shift (partial eval-top-level on-result)
          :run (partial eval-at-cursor on-result)}])))
