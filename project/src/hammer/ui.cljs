(ns hammer.ui
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljsjs.mithril]
            [cljs.core.async :as async]))

(defprotocol IComponentBeforeRemove (on-before-remove [this vnode]))
(defprotocol IComponentBeforeUpdate (on-before-update [this vnode old]))
(defprotocol IComponentCreate (on-create [this vnode]))
(defprotocol IComponentInit (on-init [this vnode]))
(defprotocol IComponentRemove (on-remove [this vnode]))
(defprotocol IComponentUpdate (on-update [this vnode]))
(defprotocol IComponentView (view [this vnode]))

(defprotocol IGlobalStateChannels (gchannels [this] "map of channels"))
(defprotocol IGlobalStateReaders (greaders [this] "map of fns to retrieve values"))
(defprotocol IGlobalStateWriters (gwriters [this] "map fns to write values"))
(defprotocol IGlobalValue (global-v [this k] "returns readers and writers of global value"))

(defprotocol ILocalStateChannels (lchannels [this] "map of channels"))
(defprotocol ILocalStateReaders (lreaders [this] "map of fns to retrieve values"))
(defprotocol ILocalStateWriters (lwriters [this] "map fns to write values"))
(defprotocol ILocalValue (local-v [this k] "returns readers and writers of a local value"))

(defprotocol IStateParams (state-params [this] "Returns global and local param names"))
(defprotocol IStaticValue (static-v [this k] "returns static values"))

(defn gen-state-map [& ks]
  (reduce merge
          {:static {}}
          (map (fn [k]
                 {k {:channels {}
                     :deleters {}
                     :readers {}
                     :updaters {}
                     :writers {}}})
               ks)))

(defn state-map-channels [m]
  (let [m (or (:state m) m)]
    {:local (-> m :local :channels)
     :global (-> m :global :channels)}))

(defn state-map-deleters [m]
  (let [m (or (:state m) m)]
    {:local (-> m :local :deleters)
     :global (-> m :global :deleters)}))

(defn state-map-readers [m]
  (let [m (or (:state m) m)]
    {:local (-> m :local :readers)
     :global (-> m :global :readers)}))

(defn state-map-statics [m]
  (let [m (or (:state m) m)]
    {:static (:static m)}))

(defn state-map-updaters [m]
  (let [m (or (:state m) m)]
    {:local (-> m :local :updaters)
     :global (-> m :global :updaters)}))

(defn state-map-writers [m]
  (let [m (or (:state m) m)]
    {:local (-> m :local :writers)
     :global (-> m :global :writers)}))

(defn state-global-channels [m]
  (-> m state-map-channels :global))

(defn state-global-deleters [m]
  (-> m state-map-deleters :global))

(defn state-global-readers [m]
  (-> m state-map-readers :global))

(defn state-global-updaters [m]
  (-> m state-map-updaters :global))

(defn state-global-writers [m]
  (-> m state-map-writers :global))

(defn state-local-channels [m]
  (-> m state-map-channels :local))

(defn state-local-deleters [m]
  (-> m state-map-deleters :local))

(defn state-local-readers [m]
  (-> m state-map-readers :local))

(defn state-local-updaters [m]
  (-> m state-map-updaters :local))

(defn state-local-writers [m]
  (-> m state-map-writers :local))

(defn state-statics [m]
  (-> m state-map-statics :static))

(defn having-global-channels [m & kv-pairs]
  (let [ret (volatile! m)]
    (doseq [[k v] (partition-all 2 kv-pairs)]
      (vswap! ret update-in [:global :channels] assoc k v))
    @ret))

(defn having-global-deleters [m & kv-pairs]
  (let [ret (volatile! m)]
    (doseq [[k v] (partition-all 2 kv-pairs)]
      (vswap! ret update-in [:global :deleters] assoc k v))
    @ret))

(defn having-global-readers [m & kv-pairs]
  (let [ret (volatile! m)]
    (doseq [[k v] (partition-all 2 kv-pairs)]
      (vswap! ret update-in [:global :readers] assoc k v))
    @ret))

(defn having-global-updaters [m & kv-pairs]
  (let [ret (volatile! m)]
    (doseq [[k v] (partition-all 2 kv-pairs)]
      (vswap! ret update-in [:global :updaters] assoc k v))
    @ret))

(defn having-global-writers [m & kv-pairs]
  (let [ret (volatile! m)]
    (doseq [[k v] (partition-all 2 kv-pairs)]
      (vswap! ret update-in [:global :writers] assoc k v))
    @ret))

(defn having-local-channels [m & kv-pairs]
  (let [ret (volatile! m)]
    (doseq [[k v] (partition-all 2 kv-pairs)]
      (vswap! ret update-in [:local :channels] assoc k v))
    @ret))

(defn having-local-deleters [m & kv-pairs]
  (let [ret (volatile! m)]
    (doseq [[k v] (partition-all 2 kv-pairs)]
      (vswap! ret update-in [:local :deleters] assoc k v))
    @ret))

(defn having-local-readers [m & kv-pairs]
  (let [ret (volatile! m)]
    (doseq [[k v] (partition-all 2 kv-pairs)]
      (vswap! ret update-in [:local :readers] assoc k v))
    @ret))

(defn having-local-updaters [m & kv-pairs]
  (let [ret (volatile! m)]
    (doseq [[k v] (partition-all 2 kv-pairs)]
      (vswap! ret update-in [:local :updaters] assoc k v))
    @ret))

(defn having-local-writers [m & kv-pairs]
  (let [ret (volatile! m)]
    (doseq [[k v] (partition-all 2 kv-pairs)]
      (vswap! ret update-in [:local :writers] assoc k v))
    @ret))

(defn having-statics [m & kv-pairs]
  (let [ret (volatile! m)]
    (doseq [[k v] (partition-all 2 kv-pairs)]
      (vswap! ret assoc-in [:static k] v))
    @ret))

(defn having-global [m data]
  (let [ret (volatile! m)]
    (doseq [[k {:keys [read write update delete channels]}] data]
      (vswap! ret
              (fn [x]
                (-> x
                    (having-global-readers k read)
                    (having-global-writers k write)
                    (having-global-updaters k update)
                    (having-global-deleters k delete)
                    (having-global-channels k channels)))))
    @ret))

(defn having-local [m data]
  (let [ret (volatile! m)]
    (doseq [[k {:keys [read write update delete channels]}] data]
      (vswap! ret
              (fn [x]
                (-> x
                    (having-local-readers k read)
                    (having-local-writers k write)
                    (having-local-updaters k update)
                    (having-local-deleters k delete)
                    (having-local-channels k channels)))))
    @ret))

(defn having-static [m data]
  (let [ret (volatile! m)]
    (doseq [[k v] data]
      (vswap! ret having-statics k v))
    @ret))

(defn having-state [type-creation-f {:keys [static local global]} overrides]
  (let [m (gen-state-map :local :global)
        m' (if-not global m (having-global m global))
        m' (if-not local m' (having-local m' local))
        state (if-not static m' (having-static m' static))]
    (type-creation-f state overrides)))
      
(defn state-global-value
  "Returns global readers and writers for a k"
  [m k]
  {:read (-> m state-global-readers k)
   :write (-> m state-global-writers k)
   :update (-> m state-global-updaters k)
   :delete (-> m state-global-deleters k)
   :channels (-> m state-global-channels k)})

(defn state-local-value
  "Returns local readers and writers for a k"
  [m k]
  {:read (-> m state-local-readers k)
   :write (-> m state-local-writers k)
   :update (-> m state-local-updaters k)
   :delete (-> m state-local-deleters k)
   :channels (-> m state-local-channels k)})

(defn state-static-value
  [m k]
  (-> m
      state-statics
      (get k)))

(defn state-params* [m]
  (->> m
       (map
        (fn [[k {:keys [readers
                        writers
                        updaters
                        deleters
                        channels]}]]
          (let [t (transient [])]
            (if-let [x (-> readers keys seq)]
              (conj! t [:readers (vec x)]))
            (if-let [x (-> writers keys seq)]
              (conj! t [:writers (vec x)]))
            (if-let [x (-> updaters keys seq)]
              (conj! t [:updaters (vec x)]))
            (if-let [x (-> deleters keys seq)]
              (conj! t [:deleters (vec x)]))
            (if-let [x (-> channels keys seq)]
              (conj! t [:channels (vec x)]))
            {k (apply hash-map
                      (mapcat identity
                              (persistent! t)))})))
       (reduce merge)))

(defn ic-before-remove [e]
  (if-not (satisfies? IComponentBeforeRemove e)
    e
    {:onbeforeremove (fn [vnode] (on-before-remove e vnode))}))

(defn ic-before-update [e]
  (if-not (satisfies? IComponentBeforeUpdate e)
    e
    {:onbeforeupdate (fn [vnode old] (on-before-update e vnode old))}))

(defn ic-create [e]
  (if-not (satisfies? IComponentCreate e)
    e
    {:oncreate (fn [vnode] (on-create e vnode))}))

(defn ic-init [e]
  (if-not (satisfies? IComponentInit e)
    e
    {:oninit (fn [vnode] (on-init e vnode))}))
  
(defn ic-remove [e]
  (if-not (satisfies? IComponentRemove e)
    e
    {:onremove (fn [vnode] (on-remove e vnode))}))

(defn ic-update [e]
  (if-not (satisfies? IComponentUpdate e)
    e
    {:onupdate (fn [vnode] (on-update e vnode))}))

(defn ic-view [e]
  (if-not (satisfies? IComponentView e)
    e
    {:view (fn [vnode] (view e vnode))}))

(defn vd
  ([k]
   (vd k {}))
  ([k attr & body]
   (js/m (name k) (clj->js attr) (clj->js body))))

(defn clj->m-element* [e]
  (reduce merge
          e
          (mapcat (fn [f] (f e))
                  [ic-before-remove
                   ic-before-update
                   ic-create
                   ic-init
                   ic-remove
                   ic-update
                   ic-view])))

(defn clj->m-element [e]
  (-> e clj->m-element* clj->js))

(defn mount! [root & coll]
  (go-loop [elements coll]
    (when-first [e elements]
      (.mount js/m
              root
              (if-not (satisfies? IComponentView e)
                e
                (clj->m-element e)))
      (recur
       (rest elements)))))

(defrecord Component [state overrides]
  IComponentBeforeRemove
  (on-before-remove [state vnode]
    (if-let [f (:on-before-remove overrides)]
      (f state vnode)))
  IComponentBeforeUpdate
  (on-before-update [state vnode old]
    (if-let [f (:on-before-update overrides)]
      (f state vnode old)
      true))
  IComponentCreate
  (on-create [state vnode]
    (if-let [f (:on-create overrides)]
      (f state vnode)))
  IComponentInit
  (on-init [state vnode]
    (if-let [f (:on-init overrides)]
      (f state vnode)))
  IComponentRemove
  (on-remove [state vnode]
    (if-let [f (:on-remove overrides)]
      (f state vnode)))
  IComponentUpdate
  (on-update [state vnode]
    (if-let [f (:on-update overrides)]
      (f state vnode)))
  IGlobalStateChannels
  (gchannels [state]
    (if-let [f (:gchannels overrides)]
      (f state)
      (state-global-channels state)))
  IGlobalStateReaders
  (greaders [state]
    (if-let [f (:greaders overrides)]
      (f state)
      (state-global-readers state)))
  IGlobalStateWriters
  (gwriters [state]
    (if-let [f (:gwriters overrides)]
      (f state)
      (state-global-writers state)))
  IGlobalValue
  (global-v [state k]
    (if-let [f (:global-v overrides)]
      (f state k)
      (state-global-value state k)))
  ILocalStateChannels
  (lchannels [state]
    (if-let [f (:lchannels overrides)]
      (f state)
      (state-local-channels state)))
  ILocalStateReaders
  (lreaders [state]
    (if-let [f (:lreaders overrides)]
      (f state)
      (state-local-readers state)))
  ILocalStateWriters
  (lwriters [state]
    (if-let [f (:lwriters overrides)]
      (f state)
      (state-local-writers state)))
  ILocalValue
  (local-v [state k]
    (if-let [f (:local-v overrides)]
      (f state k)
      (state-local-value state k)))
  IStateParams
  (state-params [state]
    (if-let [f (:state-params overrides)]
      (f state)
      (state-params* state)))
  IStaticValue
  (static-v [state k]
    (if-let [f (:static-v overrides)]
      (f state k)
      (state-static-value state k)))
  IComponentView
  (view [state vnode]
    (if-let [f (or (:view overrides)
                   (:body overrides))]
      (f state vnode)
      (vd :span {} "{{Empty Component}}"))))

(defn component [{:keys [state component]}]
  (having-state ->Component state component))

(defn redraw! [] (.redraw js/m))

(defn g
  "global-v helper"
  [x y & [default]]
  (or (global-v x y) default))

(defn l
  "local-v helper"
  [x y & [default]]
  (or (local-v x y) default))

(defn s
  "static-v helper"
  [x y & [default]]
  (or (static-v x y) default))
