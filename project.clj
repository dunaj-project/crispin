(defproject crispin "0.3.1-SNAPSHOT"
  :description "A library for managing project-wide configuration."
  :url "https://github.com/dunaj-project/crispin"
  :license {:name "Eclipse Public License"
            :distribution :repo
            :comments "same as Clojure"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git" :url "https://github.com/dunaj-project/crispin"}
  :signing {:gpg-key "487FA463"}
  :deploy-repositories [["clojars" {:creds :gpg}]]
  :dependencies [[cheshire "5.6.1"]
                 [org.clojure/clojure "1.8.0"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.8.0"]]}})
