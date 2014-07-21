(ns wwrrd.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.events :as events]
            [cljs.core.async :refer [put! <! chan]]
            [cljs-http.client :as http]))

(enable-console-print!)

(def ENTER 13)

(defn enter-listen [el]
  (let [out (chan)]
    (events/listen el "keydown"
      (fn [e]
        (when (== (.-keyCode e) ENTER)
          (put! out (.-value el)))))
    out))

(defn query-submissions [owner]
  (let* [input (om/get-node owner "query-input")
         enters (enter-listen input)]
    (go (while true
          (let [query (<! enters)]
            (http/get "http://localhost:8080" { :with-credentials? false }))))))

(defn rick-view [app owner]
  (reify
   om/IDidMount
   (did-mount [_]
     (query-submissions owner))
   om/IRenderState
   (render-state [_ _]
       (dom/h1 nil "wwrrd")
       (dom/div nil
         (dom/div nil "what would rick ross do?")
         (dom/input
           #js { :type "text" :ref "query-input" })))))

(def app-state
  (atom { :current-track nil :query nil }))

(om/root rick-view app-state
  {:target (. js/document (getElementById "rick"))})
