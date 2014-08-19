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

(defn run-state [app owner]
  (let* [input (om/get-node owner "query-input")
         enters (enter-listen input)]
        (go (while true
              (let* [query (<! enters)
                     response (<! (http/get (+ "query/" query) { :with-credentials? false }))]
                    (om/transact! app :current (fn [_] (.-line (.parse js/JSON (:body response))))))))))

(defn rick-view [app owner]
  (reify
    om/IDidMount
    (did-mount [_]
      (run-state app owner))
   om/IRenderState
   (render-state [_ _]
       (dom/h1 nil "wwrrd")
       (dom/div nil
         (dom/div nil "what would rick ross do?")
         (dom/input
           #js { :type "text" :ref "query-input" })
         (dom/div nil (:current app))))))

(def app-state (atom { :current "foo" }))

(om/root rick-view app-state
  {:target (. js/document (getElementById "rick"))})
