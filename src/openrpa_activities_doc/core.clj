(ns openrpa-activities-doc.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [hiccup.core :as h]
            [hiccup.util :refer [escape-html]]
            [hiccup.page :refer [html5]]
            [medley.core :as m]))

(defn parse-string-resource [f]
  (try
    (-> (xml/parse f)
        :content
        (->> (filter #(= :data (:tag %)))
             (map #(vector (get-in % [:attrs :name])
                           (get-in % [:content 0 :content 0])))
             (into {})))
    (catch Exception _)))


(defn parse-xaml [file]
  (try
    (->> (xml/parse file)
         (xml-seq)
         (keep (fn [e]
                 (when (and (map? e) (= :sapv:ExpressionTextBox (:tag e)))
                   (let [hint (or (some->> (get-in e [:attrs :HintText])
                                           (re-seq #"or.strings\.([\w_]+)")
                                           (first)
                                           (second)))
                         field (some->> (get-in e [:attrs :Expression])
                                        (re-seq #"Path=ModelItem\.(\w+)")
                                        (first)
                                        (second))]
                     (when hint
                       [field hint])))))
         (into {}))
    (catch Exception _)))


(defn parse-annotations [s]
  (->> (str/split-lines (or s ""))
       (keep #(or (when-let [[_ s] (re-matches #".*System.Drawing.ToolboxBitmap.*\"Resources\.([^\"]+).*" %)]
                    {:bitmap s})
                  (when-let [[_ s] (re-matches #".*LocalizedToolboxTooltip\(\"([^\"]+).*" %)]
                    {:tooltip s})
                  (when-let [[_ s] (re-matches #".*LocalizedDisplayName\(\"([^\"]+).*" %)]
                    {:display-name s})
                  (when-let [[_ s] (re-matches #".*LocalizedHelpURL\(\"([^\"]+).*" %)]
                    {:help-url s})))
       (into {})))

(defn flatten-classes
  [classes]
  (let [index (m/index-by :name classes)
        parent? (comp (set (mapcat :parents classes)) :name)]
    (->> classes
         (remove parent?)
         (map (fn [class]
                (-> class
                    (dissoc :parents)
                    (update :fields #(concat (->> (:parents class)
                                                  (map index)
                                                  (mapcat :fields))
                                             %))))))))

(defn parse-file
  [file]
  (let [root (.getParentFile (.getParentFile (io/file file)))]
    (for [[_ class-annotations class-name _ parent class-body] (re-seq #"(?s)\n( {4}\[.+\]\n)* {4}public class (\w+)(<[^>]+>)? : ([\w., <>]+Activit[\w., <>]+)\s+(\{\s+.+?\n {4}\})" (slurp file))]
      {:name        class-name
       :namespace   (.getName root)
       :xaml-hints  (parse-xaml (io/file (str/replace (str file) #"\.cs$" "Designer.xaml")))
       :strings {:en (parse-string-resource (io/file root "Resources/strings.resx"))
                 :ja (parse-string-resource (io/file root "Resources/strings.ja.resx"))}
       :parents     (set (map (fn [xs] (nth xs 2)) (re-seq #"(^| )?([\w.]+)(<[^>]+>)?" parent)))
       :annotations (-> (parse-annotations class-annotations)
                        (m/update-existing :bitmap #(let [bitmap (apply io/file root "Resources" (str/split % #"\." 2))]
                                                      (when (.exists bitmap)
                                                        bitmap))))
       :fields      (->> (re-seq #"\n( {8}\[.+\]\n)* {8}public.*?\s([\w.]+|[\w.]+<[^>]+>) (\w+)\s+\{" class-body)
                         (mapv (fn [[_ field-annotations type name]]
                                 {:name              name
                                  :type              type
                                  :field-annotations (parse-annotations field-annotations)})))})))


(defn copy-bitmap
  [bitmap namespace]
  (let [new-bitmap (str "images/" namespace "/" (.getName bitmap))]
    (io/make-parents (io/file "build" new-bitmap))
    (io/copy bitmap (io/file "build" new-bitmap))
    new-bitmap))



(comment
  (def classes
    (->> (file-seq (io/file "openrpa"))
         (filter #(and (re-matches #".+/Activities/[^.]+.cs" (str %))
                       (re-find #"public class [ \w:.,<>]+Activity" (slurp %))))
         (mapcat parse-file)
         (flatten-classes))))


(def link-svg
  [:svg
   {:xmlns "http://www.w3.org/2000/svg"
    :viewBox "0 0 24 24"
    :fill "none"
    :stroke "currentColor"
    :stroke-width "2"
    :stroke-linecap "round"
    :stroke-linejoin "round"
    :class "w-4"}
   [:path {:d "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"}]
   [:path {:d "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"}]])



(defn render
  [classes]
  (.mkdir (io/file "build"))
  (let [locales {:en "" :ja "_ja"}]
    (for [[locale local-suffix] locales]
      (spit (str "build/index" local-suffix ".html")
            (html5
              [:head
               [:script {:src "https://cdn.tailwindcss.com"}]]
              [:body {:class "container mx-auto p-4 text-slate-800"}
               [:div {:class "flex justify-end gap-2 items-center"}
                (let [ref (slurp ".git/modules/openrpa/HEAD")]
                  [:a {:class "text-sm text-slate-400 mr-4 hover:text-cyan-600" :href (str "https://github.com/open-rpa/openrpa/tree/" ref)} (str "ver: " (subs ref 0 7))])
                (for [[l l-suffix] locales]
                  [:a {:class (str "px-2 py-1 rounded border border-transparent hover:border-slate-100 text-cyan-600"
                                   (when (= l locale) " bg-slate-100"))
                       :href  (str "index" l-suffix ".html")} (str/upper-case (name l))])]
               [:div {:class "space-y-16"}
                (->> (group-by :namespace classes)
                     (map (fn [[namespace classes]]
                            [:div {:class "space-y-8"}
                             [:h2 {:class "group relative text-3xl font-medium text-cyan-700" :id namespace}
                              [:a {:class "text-cyan-600 absolute invisible group-hover:visible p-2 right-full top-1/2 transform -translate-y-1/2" :href (str "#" namespace)}
                               link-svg]
                              namespace]
                             [:div {:class "space-y-8 pl-4"}
                              (->> classes
                                   (map (fn [class]
                                          (let [localize #(or (get-in class [:strings locale %])
                                                              (get-in class [:strings :en %]))]
                                            [:div {:class "space-y-4"}
                                             [:div {:class "relative group flex gap-2 items-center border-b border-slate-200"}
                                              [:a {:class "text-cyan-600 absolute invisible group-hover:visible p-2 right-full top-1/2 transform -translate-y-1/2" :href (str "#" namespace "." (:name class))}
                                               link-svg]
                                              (if-let [bitmap (get-in class [:annotations :bitmap])]
                                                [:img {:width 16 :src (copy-bitmap bitmap namespace)}]
                                                [:div {:class "w-4 h-4 rounded-full bg-orange-500 text-orange-100 text-xs grid place-items-center"} "?"])
                                              [:h3 {:class "text-lg font-medium text-cyan-700" :id (str namespace "." (:name class))} (:name class)]
                                              (when-let [url (localize (get-in class [:annotations :help-url]))]
                                                [:a {:class "text-cyan-600 text-sm" :href url :target "_blank"} "↗"])]
                                             (when-let [tooltip (localize (get-in class [:annotations :tooltip]))]
                                               [:div tooltip])
                                             [:table
                                              (->> (:fields class)
                                                   (map (fn [field]
                                                          [:tr
                                                           [:td {:class "px-4 py-1"} (:name field)]
                                                           [:td [:code {:class "text-xs rounded border bg-slate-100 px-2 py-0.5"} (escape-html (:type field))]]
                                                           [:td {:class "px-4 py-1 text-slate-600"} (or (localize (get-in field [:field-annotations :display-name]))
                                                                                                        (localize (get (:xaml-hints class) (:name field))))]])))]]))))]])))]])))))
(defn -main
  [& args]
  (->> (file-seq (io/file "openrpa"))
       (filter #(and (re-matches #".+/Activities/[^.]+.cs" (str %))
                     (re-find #"public class [ \w:.,<>]+Activity" (slurp %))))
       (mapcat parse-file)
       (flatten-classes)
       (render)
       (dorun))
  (println "Done."))
