;; Copyright (C) 2015, 2019, Jozef Wagner. All rights reserved.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be
;; found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(ns crispin.core
  "Managing system-wide configuration.

  Configuration map is assumed immutable and can be safely cached.
  Configuration is static. When the config changes, service should be
  restarted. Design choice is to have stateless fail-fast services,
  that have their configuration stored on various places."
  {:authors ["Jozef Wagner"]}
  (:require
   [clojure.java.io :as jio]
   [clojure.string :as cs]
   [clojure.walk :as cw]
   [clojure.edn :as ce]
   [cheshire.core :as cc]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def preescape-map
  {"__DASH__" "~~CRISPIN~DASH~~"
   "__UNDERSCORE__" "~~CRISPIN~UNDERSCORE~~"
   "__DOT__" "~~CRISPIN~DOT~~"
   #"__U(\p{XDigit}{4})__" "~~CRISPIN~$1~~"})

(def escape-map
  {"~~CRISPIN~DASH~~" "-"
   "~~CRISPIN~UNDERSCORE~~" "_"
   "~~CRISPIN~DOT~~" "."
   #"~~CRISPIN~(\p{XDigit}{4})~~"
   #(-> % second (Long/parseLong 16) char str)})

;; taken from clojure.java.classpath, Copyright by Stuart Sierra
(defn ^Boolean jar-file?
  "Returns `_true_` if file is a normal file with a .jar or
  .JAR extension."
  [f]
  (let [file (jio/file f)]
    (and (.isFile file) (or (.endsWith (.getName file) ".jar")
                            (.endsWith (.getName file) ".JAR")))))

;; taken from clojure.java.classpath, Copyright by Stuart Sierra
(defn filenames-in-jar
  "Returns a sequence of Strings naming the non-directory entries in
  the `_jar-file_`."
  [^java.util.jar.JarFile jar-file]
  (->> (enumeration-seq (.entries jar-file))
       (filter #(not (.isDirectory ^java.util.jar.JarEntry %)))
       (map #(.getName ^java.util.jar.JarEntry %))))

(defn merge-cfg
  [& maps]
  (let [mf #(cond (and (map? %1) (map? %2)) (merge-cfg %1 %2)
                  (map? %1) (merge-cfg %1 {nil %2})
                  (map? %2) (merge-cfg {nil %1} %2)
                  :else %2)]
    (apply merge-with mf maps)))

(defn unescape
  [s m]
  (let [rf (fn [s [f t]] (cs/replace s f t))]
    (reduce rf s m)))

(defn transform-keys
  [coll separator]
  (let [sf #(-> %
                (unescape preescape-map)
                (cs/split separator))
        cf (comp keyword
                 cs/lower-case
                 #(unescape % escape-map))
        tf (fn [k] (->> (sf k)
                        (map cf)
                        vec))
        rf (fn [m [k v]] (merge-cfg m (assoc-in {} (tf k) v)))]
    (reduce rf {} (seq coll))))

(defn fetch-env
  "Returns map that represents current environment variables.
  Name of the environment variable is translated into sequence
  of keys, e.g. JAVA_HOME will result into
  {:java {:home \"/usr/lib/...\"}} map"
  []
  (transform-keys (java.lang.System/getenv) #"_"))

(defn fetch-sys
  "Returns map that represents current system properties
  Name of the environment variable is translated into sequence
  of keys, e.g. java.home will result into
  {:java {:home \"/usr/lib/...\"}} map"
  []
  (transform-keys (java.lang.System/getProperties) #"\."))

(defn fetch-resource
  "Returns map loaded from resource specified by `_uri_`, parsed with
  `_parser_`. UTF-8 encoding is assumed and the resource must
  produce exactly one parsed value.
  Returns `nil` if resource does not exists"
  ([uri]
   (when-not (map? uri) ;; special case for [:crispin] custom res
     (let [s (str (or (:uri uri) uri))
           u (java.net.URI/create s)
           s (or (.getPath u) (.getSchemeSpecificPart u))
           i (.lastIndexOf s (int \.))
           ext (subs s (inc i))
           p (condp = ext "json" :json "edn" :edn "clj" :clj :edn)]
       (fetch-resource p uri))))
  ([parser uri]
   (when uri
     (try
       (let [^java.net.URI puri (if (instance? java.net.URI uri)
                    uri
                    (java.net.URI/create uri))
             r (condp = (.getScheme puri)
                 "cp" (jio/resource (.getSchemeSpecificPart puri))
                 uri)
             s (when r (slurp r))]
         (condp = parser
           :json (cc/parse-string s)
           :edn (ce/read-string s)
           :clj (load-string s)))
       (catch java.nio.file.NoSuchFileException e nil)
       (catch java.io.FileNotFoundException e nil)
       ;; NPE class will be added in bb 0.3.8
       #?@(:bb [] :clj [(catch java.lang.NullPointerException e nil)])))))

(defn fetch-cp-entry
  [^java.io.File entry ^java.nio.file.Path ppath]
  (let [jar? (jar-file? entry)
        wrapf #(if jar?
                 (str "cp:" (.toString ^java.lang.Object %))
                 (.toString ^java.lang.Object %))
        pf #(fn [^java.nio.file.Path p]
              (when (.endsWith p ^java.lang.String %) (wrapf p)))
        matches-config (fn [^java.nio.file.Path p]
                         (or (.endsWith p "config.edn")
                             (.endsWith p "config.clj")
                             (.endsWith p "config.json")))
        json-file (pf "config.json")
        edn-file (pf "config.edn")
        clj-file (pf "config.clj")
        dir-path (.toPath entry)
        config-path (if jar? ppath (.resolve dir-path ppath))
        matches-path #(.startsWith ^java.nio.file.Path % config-path)
        walk-opts (when-not jar?
                    (into-array
                     java.nio.file.FileVisitOption
                     [java.nio.file.FileVisitOption/FOLLOW_LINKS]))
        file-stream
        (when-not jar?
          (java.nio.file.Files/walk dir-path 10 walk-opts))
        file-seq
        (if jar?
          (map #(java.nio.file.Paths/get
                 % (into-array java.lang.String nil))
               (filenames-in-jar
                (java.util.jar.JarFile. entry)))
          (clojure.core/iterator-seq (.iterator file-stream)))
        extract-ns-vec
        (fn [^java.nio.file.Path x]
          (let [x (.getParent (.relativize config-path x))
                nms (when x (clojure.core/iterator-seq (.iterator x)))
                tf (fn [^java.nio.file.Path p] (.toString p))]
            (vec (map (comp keyword name tf) nms))))
        rf (fn [m [ns-vec paths]]
             (let [json* (fetch-resource :json (some json-file paths))
                   edn* (fetch-resource :edn (some edn-file paths))
                   clj* (fetch-resource :clj (some clj-file paths))
                   nm (merge-cfg json* edn* clj*)
                   nm (if (empty? ns-vec) nm (assoc-in {} ns-vec nm))]
               (merge-cfg m nm)))]
    (->> file-seq
         (filter matches-path)
         (filter matches-config)
         (group-by extract-ns-vec)
         (sort-by #(count (key %)))
         (reduce rf {}))))

(defn provide-sequential
  [x]
  (cond (nil? x) [] (sequential? x) x :else [x]))

(defn fetch-root
  []
  (merge-cfg (fetch-resource "cp:config.json")
             (fetch-resource "cp:config.edn")
             (fetch-resource "cp:config.clj")))

(defn instance?*
  [t m]
  (if (class? t)
    (instance? t)
    (satisfies? t m)))

(def bmap {"true" true "false" false
           "t" true "f" false
           "1" true "0" false
           "yes" true "no" false
           "on" true "off" false
           "enable" true "disable" false
           "enabled" true "disabled" false
           "default" :default})

(defprotocol IResource
  (-get-uri [this]))

(#?(:bb defrecord :clj deftype)
 Resource [uri]
 IResource
 (-get-uri [this] uri))



;;;; Public API

(def ^{:added "1.0"} custom-cfg
  "A reference to map that will be merged into configuration
  map. Used e.g. by boot to inject custom configuration."
  (atom {}))

(defn load-custom-cfg!
  "Load custom configuration in custom-cfg atom.

   Call this function before calling cfg.
   You can call this function multiple times and config will be merged.

   Looks for cfg-name in current directory if not specified otherwise."
  ([cfg-name]
   (load-custom-cfg! (System/getProperty "user.dir") cfg-name))
  ([cfg-dir cfg-name]
   (let [cfg (->> (clojure.java.io/file cfg-dir cfg-name)
                  .getCanonicalFile
                  .toURI)]
    (swap! custom-cfg merge-cfg (fetch-resource cfg)))))

(defn cfg
  "Provides map merged from various sources, in following order:

  * :cp Configuration file in the root classpath
  * :profile Project specific environment variables
    (set e.g. in lein project.clj or profiles.clj)
    Users must use lein-environ plugin to enable this
  * custom-cfg reference
  * :env Environment variables, translating keys
  * :sys Java system properties, translating keys
  * :res Custom configuration resources pointed from [:crispin]
    property, guessing format from filename, defaulting to .edn

  Sources are also optionally put under :raw key.

  Accepted file names for configuration files, loaded in following
  order:
  * config.json
  * config.edn
  * config.clj

  Conflicts when merging branch and non-branch nodes are resolved
  into creation of branch node with previous non-branch node put
  under nil key."
  {:added "1.0"}
  ([]
   (cfg nil false))
  ([search-in]
   (cfg search-in false))
  ([search-in include-raw?]
   (let [env-map (fetch-env)
         sys-map (fetch-sys)
         root-map (fetch-root)
         profile-map (fetch-resource :clj ".lein-env")
         raw {:cp root-map
              :env env-map
              :profile profile-map
              :sys sys-map}
         exf #(if (satisfies? IResource %)
                (fetch-resource (-get-uri %))
                %)
         m (merge-cfg (cw/prewalk exf (:cp raw))
                      (cw/prewalk exf (:profile raw))
                      (cw/prewalk exf @custom-cfg)
                      (:env raw) (:sys raw) {:raw raw})
         res (->> (get-in m [:crispin] nil)
                  provide-sequential
                  (map fetch-resource))
         res (apply merge-cfg res)
         res (merge-cfg (assoc-in m [:raw :res] res) res)
         res (if include-raw? res (dissoc res :raw))]
     res)))

(defn nget-in
  "Like get-in, but parses value with edn reader and asserts that
  it is a number. Does not parse default value. Allows nil values."
  {:added "1.0"}
  ([cfg ks]
   (nget-in cfg ks nil))
  ([cfg ks default-value]
   (let [ks (if (vector? ks) ks (vector ks))]
     (when-let [v (get-in cfg ks default-value)]
       (cond
         (identical? v default-value) v
         (number? v) v
         :else (let [pv (ce/read-string v)]
                 (when-not (number? pv)
                   (throw (IllegalArgumentException.
                           "cannot parse to number")))
                 pv))))))

(defn sget-in
  "Like get-in, but coerces to string. Does not parse default value.
  Nil is not coerced into a string."
  {:added "1.0"}
  ([cfg ks]
   (sget-in cfg ks nil))
  ([cfg ks default-value]
   (let [ks (if (vector? ks) ks (vector ks))
         v (get-in cfg ks default-value)]
     (cond (identical? v default-value) v
           (nil? v) v
           (string? v) v
           :else (str v)))))

(defn bget-in
  "Like get-in, but coerces to boolean. Does not parse default value.
  Defaults to false."
  {:added "1.0"}
  ([cfg ks]
   (bget-in cfg ks false))
  ([cfg ks ^Boolean default-value]
   (if-let [s (sget-in cfg ks nil)]
     (let [r (get bmap (cs/lower-case s))]
       (cond
         (nil? r) (throw (IllegalArgumentException.
                          "value not recognized"))
         (identical? :default r) default-value
         :else r)))))

(defn bget
  "Like get, but coerces to boolean. Does not parse default value.
  Defaults to false"
  {:added "1.0"}
  ([cfg k]
   (bget-in cfg [k]))
  ([cfg k default]
   (bget-in cfg [k] default)))

(defn sget
  "Like get, but coerces to string. Does not parse default value.
  Allows nil values."
  {:added "1.0"}
  ([cfg k]
   (sget-in cfg [k]))
  ([cfg k default]
   (sget-in cfg [k] default)))

(defn nget
  "Like get, but coerces to number. Does not parse default value.
  Allows nil values."
  {:added "1.0"}
  ([cfg k]
   (nget-in cfg [k]))
  ([cfg k default]
   (nget-in cfg [k] default)))


(comment

  (do
    (reset! custom-cfg {})
    (println @custom-cfg)
    (load-custom-cfg! "config.edn")
    (println @custom-cfg)
    (load-custom-cfg! "config.json")

    @custom-cfg)

  0)
