(ns wwrrd.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.events :as events]
            [goog.dom :as goog.dom]
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

(defn parse-line [accessor response]
  (try
    (accessor (.parse js/JSON (:body response)))
    (catch js/Error e (str "something fucked up"))))

(defn parse-response [app response]
    (om/transact! app :current-line (fn [_] (parse-line '.-line response)))
    (om/transact! app :current-phrases (fn [_] (parse-line (fn [b] (.-phrases b)) response))))

(defn listen [app enters]
  (go (while true
        (let [query (<! enters)]
          (om/transact! app :current (fn [_] ""))
          (let [response (<! (http/get (+ "query/" query) { :with-credentials? false }))]
            (parse-response app response))))))

(defn run-state [app owner]
  (let* [input (om/get-node owner "query-input")
         enters (enter-listen input)]
        (listen app enters)))

(defn rick-view [app owner]
  (reify
    om/IDidMount
    (did-mount [_]
      (run-state app owner))
    om/IRenderState
    (render-state [_ _]
      (dom/h1 nil "wwrrd")
      (dom/div #js { :className "container" }
               (dom/div nil "what would rick ross do?")
               (dom/input
                #js { :type "text" :ref "query-input" :id "inp"})
               (dom/div nil (:current-line app))
               (dom/div #js { :className "phrases" } (:current-phrases app))))))

(def app-state (atom { :current-line "" :current-phrases nil }))

(om/root rick-view app-state
         {:target (. js/document (getElementById "rick"))})

(set! (.-onload js/window)
      (fn [_]
        (let [inp (goog.dom/getElement "inp")]
          (.focus inp))))
