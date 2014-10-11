(ns soundfactor.command
  (:use [clojure.core.match :only (match)]))

(defn get-summary [command-or-group]
  ((apply hash-map (second command-or-group)) :summary))

(defn print-usage [command-or-group breadcrumbs]
  ; TODO: stderr
  (let [tag (first command-or-group)]  
    (cond (= tag :command)
            (let [cmd-as-map (apply hash-map (second command-or-group))]
              (printf "%s\n" (cmd-as-map :summary))
              (printf "  argv[0] %s <args>\n" (reverse breadcrumbs))
              (printf "\n")
              (printf "___ flags ___\n")
              (doseq [spec (cmd-as-map :spec-list)]
                (printf "  spec: %s\n" spec))
              (flush))
          (= tag :group) 
            (let [cmd-as-map (apply hash-map (second command-or-group))]
              (printf "%s\n" (cmd-as-map :summary))
              (printf "  argv[0] %s\n" (reverse breadcrumbs))
              (printf "\n")
              (printf "___ subcommands ___\n\n")
              (doseq [[name sub-command] (cmd-as-map :subcommands)]
                (printf "  %s          %s\n" name (get-summary sub-command)))
              (printf "\n")
              (flush))
            :else (assert false "print-usage: did not get :command or :group"))))

(defn print-usage-error [command-or-group breadcrumbs s] 
  (print-usage command-or-group breadcrumbs)
  (printf "error: %s\n" s) ; TODO: stderr
  (flush)
  (System/exit 1))

(defn process-flag [spec-as-map args-for-main argv]
  (let [switch                (spec-as-map :flag)
        argv-before           (take-while (fn [switch'] (not (= switch switch'))) argv)
        argv-middle-and-after (drop (count argv-before) argv)
        type-info             (spec-as-map :type)
        type-tag              (first type-info)]
    (if (= (first argv-middle-and-after) switch) ; was switch found?
      (let [[value argv-after] ; yes! process it
            (if (= type-tag :no-arg) 
              [true (rest argv-middle-and-after)]
              [(second argv-middle-and-after)
               (rest (rest argv-middle-and-after))])]
        [(concat args-for-main [value]) (concat argv-before argv-after)])
      ;; switch not found. maybe it's optional?
      (let [return-value (fn [value] [:ok [(concat args-for-main [value]) argv]])]
        (match type-tag
               [:required] [:usage-error (format "the '%s' flag is required" switch)]
               [:optional] (return-value nil)
               [:optional_with_default] (return-value (nth type-info 2))
               [:no-arg] (return-value false)
               :else (assert false (str "process-flag: unknown type-tag: " type-tag)))))))

(defn process-command [cmd argv breadcrumbs]
  (let [cmd-as-map (apply hash-map cmd)
        spec-list  (cmd-as-map :spec)
        main       (cmd-as-map :main)]
    ; TODO: assert no duplicate switches in spec
    ; TODO: implement 
    (let [[args-for-main argv-leftovers] 
          (reduce (fn [[args-for-main argv] spec]
                    (let [spec-as-map (apply hash-map spec)]
                      (cond (contains? spec-as-map :flag)
                              (match (process-flag spec-as-map args-for-main argv)
                                     [:ok result] result
                                     [:usage-error s] (print-usage-error [:command cmd] breadcrumbs s))
                            (contains? spec-as-map :anon)      [(concat args-for-main [(first argv)]) (rest argv)]
                            (contains? spec-as-map :anon-list) [(concat args-for-main argv) []]
                            :else (assert false (str "bad spec" spec))))) 
              [[] argv]
              spec-list)]
      (if (not (empty? argv-leftovers))
        (print-usage-error (format "unexpected extra arguments: %s" argv))
        (apply main args-for-main)))))

(defn process-command-or-group [command-or-group argv breadcrumbs]
  (let [tag (first command-or-group)]  
    (cond (= tag :command) (process-command (second command-or-group) argv breadcrumbs)
          (= tag :group)
          (let [subcommand-to-run  (first argv)
                matches            (filter (fn [name _command-or-group] (.startsWith name subcommand-to-run)))
                num-matches        (count matches)
                usage-error        (fn [more-detail]
                                     (print-usage-error command-or-group
                                                        breadcrumbs
                                                        (format "specified sub-command \"%s\" %s" 
                                                                subcommand-to-run 
                                                                more-detail)))]
                                        ; TODO: assert no duplicated sub-command names
                                        ; TODO: ensure subcommand-to-run isn't a command-line switch
            (cond (= num-matches 1) (let [match (first matches)] 
                                      (process-command-or-group (second match)
                                                                (rest argv)
                                                                (cons (first matches) breadcrumbs)))
                  (> num-matches 1) (usage-error "is ambiguous")
                  (< num-matches 1) (usage-error "not found"))
          :else (assert false "process-command-or-group: did not get :command or :group")))))

(defn required [type] [:required type])
(defn optional [type] [:optional type])
(defn optional-with-default [type value] [:optional-with-default type value])
(def no-arg [:no-arg])

(defn flag [switch type & {:keys [doc aliases]}]
  ; TODO: ensure flags begin with -
  [:flag switch :type type :doc doc :aliases aliases])

(defn anon [name] [:anon name])
(defn anon-list [name] [:anon-list name])

(defn basic [& {:keys [summary spec main]}]
  [:command [:summary summary
             :spec spec
             :main main]])

(defn group [{:keys [summary names-and-subcommands]}]
  ; TODO: ensure names aren't command-line switch names
  [:group [:summary summary :subcommands names-and-subcommands]])

(defn run [command-or-group]
  "(run ...) processes command-line arguments to the program (argv) according to the specified command or group combinators"
  (process-command-or-group command-or-group 
                            *command-line-args* 
                            []))