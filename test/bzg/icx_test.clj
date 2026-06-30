(ns bzg.icx-test
  (:require [babashka.process :refer [shell]]
            [clojure.string :as str]
            [babashka.fs :as fs]))

(def script "src/bzg/icx.clj")
(def test-ics "test/bzg/test.ics")
(def expected-dir "test/bzg/expected")

(def formats
  [["html" "html"] ["text" "txt"]])

;; Pinned environment: reproducible output regardless of the machine.
;; ICX_TODAY pins the computation window; TZ pins the UTC time conversion.
(def pinned-env
  {"TZ"                   "Europe/Paris"
   "ICX_TODAY"            "2026-06-29"
   "ICX_WORK_START"       "13:30"
   "ICX_WORK_END"         "17:00"
   "ICX_LEAD_DAYS"        "3"
   "ICX_BUFFER_MINUTES"   "10"
   "ICX_MIN_SLOT_MINUTES" "44"
   "ICX_TITLE"            "Available Time Slots"
   "ICX_ICAL_URL"         "https://example.org/agenda.ics"
   "ICX_VISIO_URL"        "https://example.org/visio"})

(defn- expected-file [ext] (str expected-dir "/test." ext))

(defn- run-icx [fmt]
  (:out (shell {:out :string :err :string :extra-env pinned-env}
               "bb" script test-ics "-f" fmt)))

(defn- normalize [text]
  ;; The "Generated on ..." footer depends on the generation time.
  (str/replace text
               #"Generated on \d{4}-\d{2}-\d{2} at \d{2}:\d{2}"
               "Generated on NORMALIZED"))

(defn generate []
  (fs/create-dirs expected-dir)
  (doseq [[fmt ext] formats]
    (let [output (run-icx fmt)
          path   (expected-file ext)]
      (spit path output)
      (println (str "  wrote " path " (" (count (str/split-lines output)) " lines)"))))
  (println "Done. Review test/bzg/expected/ before committing."))

(defn test-all []
  (let [results
        (for [[fmt ext] formats]
          (let [path (expected-file ext)]
            (if-not (fs/exists? path)
              (do (println (str "SKIP " fmt " — run: bb test:generate")) :skip)
              (let [actual   (run-icx fmt)
                    expected (slurp path)]
                (if (= (normalize actual) (normalize expected))
                  (do (println (str "  OK " fmt)) :pass)
                  (let [actual-path (str path ".actual")]
                    (spit actual-path actual)
                    (println (str "FAIL " fmt " → diff " path " " actual-path))
                    :fail))))))]
    (let [{:keys [pass fail skip]} (merge {:pass 0 :fail 0 :skip 0} (frequencies results))]
      (println (str "\n" pass "/" (count results) " passed"
                    (when (pos? fail) (str ", " fail " failed"))
                    (when (pos? skip) (str ", " skip " skipped"))))
      (when (pos? fail) (System/exit 1)))))
