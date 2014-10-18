(ns soundfactor.command
  (:use [clojure.core.match :only (match)]))

(defn get-summary [command-or-group]
  ((apply hash-map (second command-or-group)) :summary))

(defn print-usage [command-or-group breadcrumbs]
  ; TODO: stderr
  (let [tag        (first command-or-group)
        cmd-as-map (apply hash-map (second command-or-group))
        crumbs     (clojure.string/join " " (reverse breadcrumbs))]
    (printf "%s\n" (get-summary command-or-group))
    (printf "\n")
    (cond (= tag :command)
          (let [spec-list  (cmd-as-map :spec)]
            (printf "  argv[0] %s<args>\n" crumbs)
            (printf "\n")
            (if (not (empty? spec-list))
              (do (printf "___ flags ___\n\n")
                  (doseq [spec spec-list]
                    (printf "  spec: %s\n" spec))))
            (printf "\n")
            (flush))
          (= tag :group) 
          (let [subcommands (cmd-as-map :subcommands)]
            (printf "  argv[0] %s\n" crumbs)
            (printf "\n")
            (if (not (empty? subcommands))
              (do (printf "___ subcommands ___\n\n")
                  (doseq [[name sub-command] (cmd-as-map :subcommands)]
                    (printf "  %s          %s\n" name (get-summary sub-command)))))
            (printf "\n")
            (flush))
          :else (assert false "print-usage: did not get :command or :group"))))

(defn print-usage-error [command-or-group breadcrumbs s] 
  (print-usage command-or-group breadcrumbs)
  (println s) ; TODO: stderr
  (flush)
  (System/exit 1))

(defn process-flag [spec-as-map args-for-main argv]
  (let [starts-with      (fn [haystack needle] (if (nil? needle) false (.startsWith haystack needle)))
        switch           (spec-as-map :flag)
        partial-matches  (filter (fn [arg] (starts-with switch arg)) argv)]
    (if (> (count partial-matches) 1)
      ;; we allow the user to specify less than the full name of the flag so long as it uniquely
      ;; identifies the flag, so let's just make sure each flag matches once at most
      ;; it's true, this seems N^2, but if your program accepts 10,000 flag arguments you probably suck
      [:usage-error (format "multiple matches for flag '%s': %s" switch (apply str partial-matches))]
      (let [argv-before           (take-while (fn [arg] (not (starts-with switch arg))) argv)
            argv-middle-and-after (drop (count argv-before) argv)
            type-info             (spec-as-map :type)
            type-tag              (first type-info)
            return-ok             (fn [x] [:ok x])]
        (if (starts-with switch (first argv-middle-and-after)) ; was switch found?
          (let [[value argv-after] ; yes! process it
                (if (= type-tag :no-arg) 
                  [true (rest argv-middle-and-after)]
                  [(second argv-middle-and-after)
                   (rest (rest argv-middle-and-after))])]
            (return-ok [(concat args-for-main [value]) 
                        (concat argv-before argv-after)]))
          ;; switch not found. maybe it's optional?
          (let [return-value (fn [value] (return-ok [(concat args-for-main [value]) argv]))]
            (match [type-tag]
                   [:required] [:usage-error (format "the '%s' flag is required" switch)]
                   [:optional] (return-value nil)
                   [:optional_with_default] (return-value (nth type-info 2))
                   [:no-arg] (return-value false)
                   :else (assert false (str "process-flag: unknown type-tag: " type-tag))))))))) 

(defn process-command [cmd argv breadcrumbs]
  (let [cmd-wrap   [:command cmd] ; un-destructure this, for passing
        cmd-as-map (apply hash-map cmd)
        spec-list  (cmd-as-map :spec)
        main       (cmd-as-map :main)]
    ; TODO: assert no duplicate switches in spec
    (let [[args-for-main argv-leftovers] 
          (reduce (fn [[args-for-main argv] spec]
                    (let [spec-as-map (apply hash-map spec)]
                      (cond (contains? spec-as-map :flag)
                            (match (process-flag spec-as-map args-for-main argv)
                                   [:ok result] result
                                   [:usage-error s] (print-usage-error cmd-wrap breadcrumbs s))
                            (contains? spec-as-map :anon)      [(concat args-for-main [(first argv)]) (rest argv)]
                            (contains? spec-as-map :anon-list) [(concat args-for-main argv) []]
                            :else (assert false (str "bad spec" spec))))) 
                  [[] argv]
                  spec-list)]
      (if (not (empty? argv-leftovers))
        (print-usage-error cmd-wrap 
                           breadcrumbs 
                           (let [extra (first argv-leftovers)]
                             (if (.startsWith extra "-")
                               (format "unknown flag: %s" extra)
                               (format "too many anonymous arguments: %s" extra))))
        (apply main args-for-main)))))

(defn process-command-or-group [command-or-group argv breadcrumbs]
  (let [tag (first command-or-group)]
    (cond (= tag :command) (process-command (second command-or-group) argv breadcrumbs)
          (= tag :group)
          (let [subcommands        ((apply hash-map (second command-or-group)) :subcommands)
                subcommand-to-run  (first argv) ; XXX: use that starts-with function instead?
                matches            (filter (fn [name _command-or-group] (.startsWith name subcommand-to-run)) subcommands)
                num-matches        (count matches)
                usage-error        (fn [more-detail]
                                     (print-usage-error command-or-group
                                                        breadcrumbs
                                                        (format "specified sub-command \"%s\" %s" 
                                                                subcommand-to-run 
                                                                more-detail)))]
                                        ; XXX: assert no duplicated sub-command names at this level
                                        ; XXX: ensure subcommand-to-run isn't a command-line switch
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

(defn group [summary names-and-subcommands]
  ; TODO: ensure names aren't command-line switch names
  [:group [:summary summary :subcommands names-and-subcommands]])

(defn run [command-or-group]
  "(run ...) processes command-line arguments to the program (argv) according to the specified command or group combinators"
  (process-command-or-group command-or-group 
                            *command-line-args* 
                            []))
