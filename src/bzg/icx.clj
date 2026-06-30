#!/usr/bin/env bb

;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bzg.icx
  (:require [clojure.string :as str])
  (:import [java.time LocalDate LocalDateTime LocalTime Duration DayOfWeek ZoneId]
           [java.time.format DateTimeFormatter]
           [java.time.temporal TemporalAdjusters]))

(defn- env [k d] (or (System/getenv k) d))

;; Display time zone: UTC times (Z suffix) are converted into it.
;; TZ is the standard POSIX variable — left unprefixed.
(def ^:private display-zone
  (if-let [tz (System/getenv "TZ")] (ZoneId/of tz) (ZoneId/systemDefault)))

;; Availability computation settings (equivalents of compute_availability.py),
;; prefixed with ICX_ to avoid clashes in the environment.
(defn- parse-hm [s] (let [[h m] (str/split s #":")] (LocalTime/of (parse-long h) (parse-long m))))
(def ^:private work-start (parse-hm   (env "ICX_WORK_START" "13:30")))
(def ^:private work-end   (parse-hm   (env "ICX_WORK_END" "17:00")))
(def ^:private lead-days  (parse-long (env "ICX_LEAD_DAYS" "3")))
(def ^:private buffer-min (parse-long (env "ICX_BUFFER_MINUTES" "10")))
(def ^:private min-slot   (parse-long (env "ICX_MIN_SLOT_MINUTES" "44")))

;; ---------------------------------------------------------------------------
;; Hard-coded names (no locale data, unavailable in the bb native image)
;; ---------------------------------------------------------------------------
(def ^:private months
  ["January" "February" "March" "April" "May" "June"
   "July" "August" "September" "October" "November" "December"])
(def ^:private weekdays
  ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"])

(def ^:private dt-fmt   (DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss"))
(def ^:private date-fmt (DateTimeFormatter/ofPattern "yyyyMMdd"))
(def ^:private gen-fmt  (DateTimeFormatter/ofPattern "yyyy-MM-dd 'at' HH:mm"))

(defn- month-day-year [^LocalDate d]
  (format "%s %02d, %d" (nth months (dec (.getValue (.getMonth d))))
          (.getDayOfMonth d) (.getYear d)))

(defn- weekday-month-day [^LocalDate d]
  (format "%s, %s %02d" (nth weekdays (dec (.getValue (.getDayOfWeek d))))
          (nth months (dec (.getValue (.getMonth d)))) (.getDayOfMonth d)))

(defn- hhmm [^LocalDateTime t] (format "%02d:%02d" (.getHour t) (.getMinute t)))
(defn- time-range [{:keys [start end]}] (str (hhmm start) " - " (hhmm end)))

(defn- duration-label [^LocalDateTime start ^LocalDateTime end]
  (let [mins (.toMinutes (Duration/between start end))
        h    (quot mins 60)
        m    (mod mins 60)]
    (cond
      (and (pos? h) (pos? m)) (format "%dh%02dm" h m)
      (pos? h)                (format "%dh" h)
      :else                   (format "%dmin" m))))

;; ---------------------------------------------------------------------------
;; ICS parsing (VEVENT only — VTIMEZONE is ignored)
;; ---------------------------------------------------------------------------
(defn- unfold
  "Reassemble RFC 5545 folded lines (continuation = leading space/tab)."
  [lines]
  (reduce (fn [acc line]
            (if (and (seq acc) (or (str/starts-with? line " ") (str/starts-with? line "\t")))
              (conj (pop acc) (str (peek acc) (subs line 1)))
              (conj acc line)))
          [] lines))

(defn- parse-prop
  "[NAME params value] from a 'NAME;params:value' line (NAME upper-cased)."
  [line]
  (when-let [idx (str/index-of line ":")]
    [(-> (subs line 0 idx) (str/split #";") first str/upper-case)
     (subs line 0 idx)
     (subs line (inc idx))]))

(defn- parse-ics-dt [params value]
  (cond
    ;; UTC (Z suffix) -> converted to the display time zone
    (str/ends-with? value "Z")
    (-> (LocalDateTime/parse (subs value 0 (dec (count value))) dt-fmt)
        (.atZone (ZoneId/of "UTC"))
        (.withZoneSameInstant display-zone)
        (.toLocalDateTime))
    ;; date only (all-day)
    (or (str/includes? (str/upper-case params) "VALUE=DATE")
        (not (str/includes? value "T")))
    (.atStartOfDay (LocalDate/parse value date-fmt))
    ;; floating local time or with TZID -> as-is
    :else
    (LocalDateTime/parse value dt-fmt)))

(defn parse-events
  "List of {:start :end :summary} for the VEVENTs in the ICS."
  [text]
  (loop [ls (unfold (str/split-lines text)), cur nil, acc []]
    (if (empty? ls)
      acc
      (let [line (first ls), r (rest ls)]
        (cond
          (= line "BEGIN:VEVENT") (recur r {} acc)
          (= line "END:VEVENT")   (recur r nil (if (and (:start cur) (:end cur)) (conj acc cur) acc))
          (nil? cur)              (recur r cur acc)
          :else
          (let [[name params value] (parse-prop line)]
            (recur r (case name
                       "DTSTART" (assoc cur :start (parse-ics-dt params value))
                       "DTEND"   (assoc cur :end   (parse-ics-dt params value))
                       "SUMMARY" (assoc cur :summary value)
                       cur)
                   acc)))))))

;; ---------------------------------------------------------------------------
;; Computing free slots from busy time
;; ---------------------------------------------------------------------------
(defn- add-working-days
  "Advance d by n working days (weekends skipped)."
  [^LocalDate d n]
  (loop [cur d, added 0]
    (if (>= added n)
      cur
      (let [nx (.plusDays cur 1)]
        (recur nx (if (< (.getValue (.getDayOfWeek nx)) 6) (inc added) added))))))

(defn- working-blocks
  "One [start end] working-hours block per working day within [start,end]."
  [^LocalDate start ^LocalDate end]
  (loop [d start, acc []]
    (if (.isAfter d end)
      acc
      (recur (.plusDays d 1)
             (if (< (.getValue (.getDayOfWeek d)) 6)
               (conj acc [(.atTime d work-start) (.atTime d work-end)])
               acc)))))

(defn- longer? [dur mins] (pos? (.compareTo dur (Duration/ofMinutes mins))))

(defn- merge-busy
  "Widen busy intervals by the buffer, then merge overlaps."
  [busy]
  (->> busy
       (map (fn [[s e]] [(.minusMinutes ^LocalDateTime s buffer-min) (.plusMinutes ^LocalDateTime e buffer-min)]))
       (sort-by first)
       (reduce (fn [acc [s e]]
                 (if (and (seq acc) (not (.isAfter ^LocalDateTime s (second (peek acc)))))
                   (conj (pop acc) [(first (peek acc))
                                    (if (.isAfter ^LocalDateTime e (second (peek acc))) e (second (peek acc)))])
                   (conj acc [s e])))
               [])))

(defn- available-slots
  "Subtract busy time from working blocks; keep gaps > min-slot."
  [blocks busy]
  (let [merged (merge-busy busy)]
    (mapcat
     (fn [[ws we]]
       (let [here (filter (fn [[bs be]] (and (.isBefore ^LocalDateTime bs we) (.isAfter ^LocalDateTime be ws))) merged)]
         (loop [cursor ws, bs here, acc []]
           (if (empty? bs)
             (if (and (.isBefore ^LocalDateTime cursor we) (longer? (Duration/between cursor we) min-slot))
               (conj acc {:start cursor :end we}) acc)
             (let [[b-s b-e] (first bs)
                   gap-end (if (.isBefore ^LocalDateTime b-s we) b-s we)
                   acc (if (and (.isBefore ^LocalDateTime cursor gap-end) (longer? (Duration/between cursor gap-end) min-slot))
                         (conj acc {:start cursor :end gap-end}) acc)]
               (recur (if (.isAfter ^LocalDateTime b-e cursor) b-e cursor) (rest bs) acc))))))
     blocks)))

;; Reference date: ICX_TODAY (ISO yyyy-MM-dd) if set, otherwise today.
;; This seam allows producing reproducible outputs (tests, "as of a given
;; date" report).
(defn- today []
  (if-let [t (System/getenv "ICX_TODAY")]
    (LocalDate/parse t)
    (LocalDate/now display-zone)))

(defn available
  "Free slots {:start :end} derived from the busy events of the ICS. The
   window runs from (today + lead-days working days) to the last known busy day."
  [busy-events]
  (if (empty? busy-events)
    []
    (let [today    (today)
          start    (add-working-days today lead-days)
          end-date (->> busy-events
                        (map #(.toLocalDate ^LocalDateTime (:end %)))
                        (reduce (fn [a b] (if (.isAfter ^LocalDate b a) b a))))]
      (available-slots (working-blocks start end-date)
                       (map (juxt :start :end) busy-events)))))

;; ---------------------------------------------------------------------------
;; Grouping: ISO week (Monday) -> day -> events (sorted)
;; ---------------------------------------------------------------------------
(defn- week-monday [^LocalDate d]
  (.with d (TemporalAdjusters/previousOrSame DayOfWeek/MONDAY)))

(defn- group-by-week [events]
  (->> events
       (sort-by :start)
       (group-by #(week-monday (.toLocalDate ^LocalDateTime (:start %))))
       (sort)
       (map (fn [[monday evs]]
              [monday (->> evs
                           (group-by #(.toLocalDate ^LocalDateTime (:start %)))
                           (sort)
                           (map (fn [[date day-evs]] [date (sort-by :start day-evs)])))]))))

;; ---------------------------------------------------------------------------
;; Text rendering
;; ---------------------------------------------------------------------------
(defn- center [s w]
  (let [len (count s)]
    (if (>= len w)
      s
      (let [pad (- w len), left (quot pad 2)]
        (str (apply str (repeat left " ")) s (apply str (repeat (- pad left) " ")))))))

(defn render-text [events title]
  (if (empty? events)
    "No available time slots found.\n"
    (let [rule (apply str (repeat 70 "="))
          thin (apply str (repeat 70 "-"))
          body (reduce
                (fn [lines [idx [monday days]]]
                  (let [lines (cond-> lines (pos? idx) (conj ""))
                        lines (conj lines (str "WEEK OF " (str/upper-case (month-day-year monday))) thin)]
                    (reduce (fn [ls [date day-evs]]
                              (reduce (fn [l ev]
                                        (conj l (str "    " (time-range ev)
                                                     " (" (duration-label (:start ev) (:end ev)) ")")))
                                      (conj ls (str "\n  " (weekday-month-day date)))
                                      day-evs))
                            lines days)))
                [rule (center title 70) rule ""]
                (map-indexed vector (group-by-week events)))
          footer ["" rule (str "Generated on " (.format (LocalDateTime/now) gen-fmt)) ""]]
      (str/join "\n" (concat body footer)))))

;; ---------------------------------------------------------------------------
;; HTML rendering
;; ---------------------------------------------------------------------------
(defn- esc [s]
  (str/escape (str s) {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;"}))

(def ^:private css
"    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto,
                     'Helvetica Neue', Arial, sans-serif;
        line-height: 1.6; color: #333; background: #f5f5f5; padding: 20px;
    }
    .container {
        max-width: 900px; margin: 0 auto; background: white;
        border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        overflow: hidden;
    }
    header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white; padding: 30px; text-align: center;
    }
    header h1 { font-size: 28px; font-weight: 600; margin-bottom: 8px; }
    header p  { opacity: 0.9; font-size: 14px; }
    header a  { color: white; }
    .events   { padding: 20px; }
    .week-group  { margin-bottom: 30px; }
    .week-header {
        font-size: 14px; font-weight: 600; color: #666;
        text-transform: uppercase; letter-spacing: 0.5px;
        margin-bottom: 12px; padding-bottom: 8px;
        border-bottom: 2px solid #e0e0e0;
    }
    .event {
        background: white; border-left: 4px solid #667eea; padding: 16px;
        margin-bottom: 12px; border-radius: 6px; transition: all 0.2s;
        border: 1px solid #e0e0e0;
    }
    .event:hover {
        border-left-color: #764ba2;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        transform: translateX(4px);
    }
    .event-date {
        font-weight: 600; color: #333; font-size: 16px;
        margin-bottom: 12px; padding-bottom: 8px;
        border-bottom: 1px solid #f0f0f0;
    }
    .event-time {
        color: #666; font-size: 14px; padding: 8px 0;
        display: flex; align-items: center; justify-content: space-between;
    }
    .event-time:not(:last-child) { border-bottom: 1px dashed #e8e8e8; }
    .time-range { flex: 1; }
    .event-duration {
        display: inline-block; background: #e8eaf6; color: #667eea;
        padding: 2px 8px; border-radius: 4px; font-size: 12px;
        font-weight: 600; margin-left: 8px;
    }
    .no-events {
        text-align: center; padding: 60px 20px; color: #999;
    }
    footer {
        text-align: center; padding: 20px; color: #999;
        font-size: 12px; border-top: 1px solid #e0e0e0;
    }
    @media (max-width: 600px) { header h1 { font-size: 24px; } }
")

(defn- events-html [events]
  (if (empty? events)
    "        <div class=\"no-events\">\n            <p>No available time slots found.</p>\n        </div>"
    (str/join "\n"
              (concat
               ["        <div class=\"events\">"]
               (mapcat
                (fn [[monday days]]
                  (concat
                   [(str "            <div class=\"week-group\">\n"
                         "                <div class=\"week-header\">Week of "
                         (esc (month-day-year monday)) "</div>")]
                   (mapcat
                    (fn [[date day-evs]]
                      (concat
                       [(str "                <div class=\"event\">\n"
                             "                    <div class=\"event-date\">"
                             (esc (weekday-month-day date)) "</div>")]
                       (map (fn [ev]
                              (str "                    <div class=\"event-time\">\n"
                                   "                        <span class=\"time-range\">"
                                   (esc (time-range ev)) "</span>\n"
                                   "                        <span class=\"event-duration\">"
                                   (esc (duration-label (:start ev) (:end ev))) "</span>\n"
                                   "                    </div>"))
                            day-evs)
                       ["                </div>"]))
                    days)
                   ["            </div>"]))
                (group-by-week events))
               ["        </div>"]))))

(defn render-html [events title]
  (let [ical  (env "ICX_ICAL_URL" "https://bzg.fr/agenda.ics")
        visio (env "ICX_VISIO_URL" "https://rendez-vous.renater.fr/swh-partnerships")
        gen   (.format (LocalDateTime/now) gen-fmt)]
    (str "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n"
         "    <meta charset=\"UTF-8\">\n"
         "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
         "    <title>" (esc title) "</title>\n"
         "    <style>\n" css "    </style>\n"
         "</head>\n<body>\n"
         "    <div class=\"container\">\n"
         "        <header>\n"
         "            <h1>" (esc title) "</h1>\n"
         "            <p><strong><a href=\"" (esc ical) "\">iCal file</a> - "
         "<a href=\"" (esc visio) "\">Visio link</a></strong></p>\n"
         "        </header>\n"
         (events-html events) "\n"
         "        <footer>\n            Generated on " gen "\n        </footer>\n"
         "    </div>\n</body>\n</html>\n")))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------
(defn- log* [& xs] (binding [*out* *err*] (apply println xs)))

(def ^:private usage
  "icx - render an ICS busy feed as free time slots (HTML or text)

Usage: icx [OPTIONS] [INPUT] [OUTPUT]

  INPUT and OUTPUT default to - (stdin/stdout).

Options:
  -f, --format FORMAT   html (default) or text (aliases: txt, ascii)
  -t, --title TITLE     page title
  -h, --help            show this help and exit

Tuning is done through environment variables (ICX_ prefix, plus TZ);
see the README for the full list.")

(defn- parse-args [args]
  (loop [a args, opts {:format "html"}, pos []]
    (if (empty? a)
      (assoc opts :input (or (first pos) "-") :output (or (second pos) "-"))
      (let [x (first a)]
        (case x
          ("-h" "--help")   (assoc opts :help true)
          ("-f" "--format") (recur (drop 2 a)
                                   (assoc opts :format
                                          (let [f (str/lower-case (second a))]
                                            (get {"text" "text" "txt" "text" "ascii" "text"} f f)))
                                   pos)
          ("-t" "--title")  (recur (drop 2 a) (assoc opts :title (second a)) pos)
          (recur (rest a) opts (conj pos x)))))))

(defn -main [& args]
  (let [{:keys [input output title help] fmt :format} (parse-args args)]
   (cond
     help
     (println usage)
     ;; No INPUT given and stdin is an interactive terminal (no pipe/redirect):
     ;; there is nothing to read, so bail out instead of blocking on (slurp *in*).
     (and (= input "-") (System/console))
     (binding [*out* *err*]
       (println "icx: no input. Pass an ICS file, or pipe one on stdin.")
       (println "Try 'icx --help' for usage.")
       (System/exit 2))
     :else
     (let [text   (if (= input "-") (slurp *in*) (slurp input))
           busy   (parse-events text)
           slots  (available busy)
           title  (or title (env "ICX_TITLE" "Available Time Slots"))
           out    (if (= fmt "text") (render-text slots title) (render-html slots title))]
       (log* (format "%d busy event(s) read -> %d free slot(s)" (count busy) (count slots)))
       (if (= output "-")
         (print out)
         (do (spit output out) (log* (str "✓ Written to " output))))))))

;; Runs when the file is launched as a script (bb src/bzg/icx.clj …),
;; not when it is loaded as a namespace (-m bzg.icx, require in tests).
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
