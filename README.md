# Hammer

An alternative to Om and Reagent reactive UI. Based on Mithril.js with emphasis on simplicity.

An example program.

```clojure
(ns ^:figwheel-load hammer-examples.example1
  (:require [hammer.ui :as ui]))

(defn view [this _]
  (let [title (ui/s this :some-title "Title Not Found")
        {textf :read text! :write} (ui/g this :global-text)
        {ltextf :read ltext! :write} (ui/l this :local-text)]
    (ui/vd :div {}
           (ui/vd :h3 {:style "color:red;"} title)
           (ui/vd :button {:onclick text!} (str "Global Count: " (textf)))
           (ui/vd :button {:onclick ltext!} (str "Local Count: " (ltextf))))))

(defn- global-counter-reader [app-state]
  #(:counter @app-state))

(defn- global-counter-writer [app-state]
  #(swap! app-state update :counter inc))

(defn- local-counter-reader [local-state]
  #(:counter @local-state))

(defn- local-counter-writer [local-state]
  #(vswap! local-state update :counter inc))

(defn init [{:keys [counter] :as app-state}]
  (let [local-state (volatile! {:counter 0})
        local-text (local-counter-reader local-state)
        local-text! (local-counter-writer local-state)
        global-text (global-counter-reader app-state)
        global-text! (global-counter-writer app-state)
        another-example {:in nil :out nil}]
    ;; Possible State Keys :read :write :update :delete :channels 
    (ui/component
     {:component {:view view}
      :state {:static {:some-title "Hello World!"}
              :global {:global-text {:read global-text :write global-text!}
                       :another-example {:channels another-example}}
              :local {:local-text {:read local-text :write local-text!}}}})))
```

### Component Overview

The `:component` value should be a map with optional overrides. For example, to have an event run before a component is updated, your `:component` value should contain `{:on-before-update (fn [this vnode old])}`

```clojure
(on-before-remove [state vnode])
(on-before-update [state vnode old])
(on-create [state vnode])
(on-init [state vnode])
(on-remove [state vnode])
(on-update [state vnode])
(gchannels [state])
(greaders [state])
(gwriters [state])
(global-v [state k])
(lchannels [state])
(lreaders [state])
(lwriters [state])
(local-v [state k])
(state-params [state])
(static-v [state k])
(view [state vnode])
```
Components can be mounted with `mount!`

```clojure
(ui/mount! (.getElementById js/document "app") component)
```

### State Overview

The `:state` value should be a map containing the keys `:static`, `:global`, and `:local`.

With the exception of `:static`, the values should be a map that provides functions or channels. Literal values should be placed in `:static` and accessed with the `s` function, `g` is used for `:global`, `l` for `:local`.

Example `:state` map.

```clojure
(ui/component
  {:component {:view view}
   :state {:static {:some-title "Hello World!"}
           :global {:global-text {:read global-text :write global-text!}
                    :another-example {:channels another-example}}
           :local {:local-text {:read local-text :write local-text!}}}})
```

Getting value functions.

```clojure
  (let [title (ui/s this :some-title "Title Not Found")
        {textf :read text! :write} (ui/g this :global-text)
        {ltextf :read ltext! :write} (ui/l this :local-text)])
```

### Example Application

```clojure
(ns ^:figwheel-load hammer-examples.todo
  (:require [hammer.ui :as ui]))

;; Example `app-state`
;;
;; {:todo/list [0 1 2]
;;  :todo/list-content ["one" "two" "three"]}

(declare local-reader-input
         local-writer-input
         global-reader-list
         global-writer-list
         view)

(defn init
  [{:keys [todo/list todo/list-content]
    :as app-state}]
  (let [local-state (volatile! {:value ""})
        input-reader (local-reader-input local-state)
        input-writer! (local-writer-input local-state)
        items-reader (global-reader-list app-state)
        items-writer! (global-writer-list input-reader app-state)]
    (ui/component
     {:component {:view view}
      :state {:static {:title "Example Title"}
              :local {:input {:read input-reader
                              :write input-writer!}}
              :global {:items {:read items-reader
                               :write items-writer!}}}})))

(defn view [this _]
  (let [title (ui/s this :title "Title Not Found")
        {input-f :read input-f! :write} (ui/l this :input)
        {items-f :read items-f! :write} (ui/g this :items)]
    (ui/vd :div {}
           (ui/vd :h4 {} title)
           (ui/vd :textarea
                  {:oninput input-f! :value (input-f) :style "width: 100%;"})
           (ui/vd :br)
           (ui/vd :button
                  {:onclick items-f! :style "width: 100%;"}
                  "Add")
           (ui/vd :br)
           (items-f))))

(defn- global-writer-delete-item
  [{:keys [todo/list todo/list-content]
    :as app-state}]
  (fn [id]
    (let [data @app-state
          items (:todo/list data)
          contents (:todo/list-content data)
          size (count items)
          limit (dec size)
          items' (-> limit range vec)
          contents' (->> (range size)
                         (keep
                          (fn [n]
                            (if (not= n id)
                              (get contents n))))
                         vec)]
      (swap! app-state
             assoc
             :todo/list items'
             :todo/list-content contents'))))

(defn- global-writer-move-item-up
  [{:keys [todo/list todo/list-content]
    :as app-state}]
  (fn [id]
    (if-not (zero? id)
      (let [data @app-state
            items (:todo/list data)
            contents (:todo/list-content data)
            limit (-> items count dec)
            old-id id
            old-content (get contents old-id)
            new-id' (dec id)
            new-id (if (neg? new-id') limit new-id')
            new-content (get contents new-id)]
        (swap! app-state
               update
               :todo/list-content
               (fn [x]
                 (-> x
                     (assoc-in [old-id] new-content)
                     (assoc-in [new-id] old-content))))))))

(defn- global-writer-move-item-down
  [{:keys [todo/list todo/list-content]
    :as app-state}]
  (fn [id]
    (let [data @app-state
          items (:todo/list data)
          contents (:todo/list-content data)
          size (count items)
          limit (dec size)]
      (if-not (= id limit)
        (let [old-id id
              old-content (get contents old-id)
              new-id' (inc id)
              new-id (if (> new-id' limit) 0 new-id')
              new-content (get contents new-id)]
          (swap! app-state
                 update
                 :todo/list-content
                 (fn [x]
                   (-> x
                       (assoc-in [old-id] new-content)
                       (assoc-in [new-id] old-content)))))))))

(def ^:private item-style
  (str "border-width: 1px;"
       "border-style: solid;"
       "border-color: black;"
       "padding: 4px;"
       "margin: 4px;"))

(def ^:private item-style-a
  (str "margin-right: 5px;"
       "padding-right: 5px;"))

(defn- global-reader-list
  [{:keys [todo/list todo/list-content]
    :as app-state}]
  (fn [& _]
    (let [data @app-state
          items (:todo/list data)
          contents (:todo/list-content data)
          move-up! (global-writer-move-item-up app-state)
          move-down! (global-writer-move-item-down app-state)
          delete! (global-writer-delete-item app-state)]
      (map (fn [id]
             (let [content (get contents id)]
               (ui/vd :div
                      {:id id :style item-style}
                      [(ui/vd :a
                              {:href "#"
                               :style item-style-a
                               :onclick #(delete! id)}
                              (ui/vd :b "delete"))
                       (ui/vd :a
                              {:href "#"
                               :style item-style-a
                               :onclick #(move-up! id)}
                              (ui/vd :b "up"))
                       (ui/vd :a
                              {:href "#"
                               :style item-style-a
                               :onclick #(move-down! id)}
                              (ui/vd :b "down"))
                       (ui/vd :div {} content)])))
           items))))

(defn- global-writer-list
  [read-value-fn
   {:keys [todo/list todo/listcontent]
    :as app-state}]   
  #(let [data @app-state
         items (:todo/list data)
         contents (:todo/list-content data)
         items' (conj items (count items))
         contents' (into [(read-value-fn)] contents)]
     (swap! app-state
            assoc
            :todo/list items'
            :todo/list-content contents')))

(defn- local-writer-input [{:keys [value] :as local-state}]
  #(this-as this
     (vswap! local-state assoc :value (.-value this))))

(defn- local-reader-input [{:keys [value] :as local-state}]
  #(:value @local-state))
```

## License

Copyright © 2017 Ryan Kelker

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
