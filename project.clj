(defproject crispin "0.1.0"
  :description "Dunaj library for managing project-wide configuration."
  :url "https://github.com/dunaj-project/crispin"
  :license {:name "Eclipse Public License"
            :distribution :repo
            :comments "same as Clojure"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git" :url "https://github.com/dunaj-project/crispin"}
  :signing {:gpg-key "6A72CBE2"}
  :deploy-repositories [["clojars" {:creds :gpg}]]
  :profiles {:dev {:dependencies [[org.dunaj/dunaj "0.6.0-SNAPSHOT"]]}})
