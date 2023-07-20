(ns nextjournal.command-bar.demo
  (:require-macros [nextjournal.clerk.render.macros :refer [sci-copy-nss]])
  (:require ["react" :as react]
            ["react-dom/client" :as react-client]
            [nextjournal.command-bar.demo.sci :as demo.sci]
            [nextjournal.command-bar.demo.editor :as editor]
            [nextjournal.command-bar :as command-bar]
            [reagent.core :as r]
            [sci.core :as sci]
            [sci.ctx-store]))

(defonce react-root
  (react-client/createRoot (js/document.getElementById "root")))

(defn ^:dev/after-load render []
  (when react-root
    (.render react-root (r/as-element [editor/root]))))

(sci.ctx-store/swap-ctx! sci/merge-opts {:namespaces (sci-copy-nss 'nextjournal.command-bar.demo.editor 'nextjournal.command-bar)})
