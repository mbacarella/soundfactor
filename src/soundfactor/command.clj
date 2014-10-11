(ns soundfactor.command)

(defn bomb-out-with-usage-error [s] 
  (printf "error: %s\n" s)
  (flush)
  (System/exit 1))

(defn process-flag [spec-as-map args-for-main argv]
  (let [switch         (spec-as-map :flag)
        before         (take-while (fn [switch'] (not (= switch switch'))) argv)
        type-info      (spec-as-map :type)
        type-tag       (first type-info)]
    ; XXX: this is broken
    (if (not (= before argv)) ; was switch found?
      (let [[value after] ; yes! process it
            (let [middle-and-after (drop (count before) argv)]
              (assert (= (first middle-and-after) switch))
              (if (= type-tag :no-arg) ; no-arg types are boolean switches
                [true (rest middle-and-after)]
                [(second middle-and-after)
                 (rest (rest middle-and-after))]))]
        [(concat args-for-main [value]) (concat before after)])
      (let [value ; no, maybe it's optional?
            (cond (= type-tag :required) (bomb-out-with-usage-error (format "the '%s' flag is required" switch))
                  (= type-tag :optional) nil
                  (= type-tag :optional_with_default) (nth type-info 2)
                  (= type-tag :no-arg) false
                  :else (assert false (str "process-flag: unknown type-tag: " type-tag)))]
        [(concat args-for-main [value]) argv]))))

(defn process-command [cmd argv]
  (let [cmd-as-map (apply hash-map cmd)
        spec-list  (cmd-as-map :spec)
        main       (cmd-as-map :main)]
    ; TODO: ensure no duplicate switches in spec
    ; TODO: implement disambuigation
    (let [[args-for-main argv-leftovers] 
          (reduce (fn [[args-for-main argv] spec]
                    (let [spec-as-map (apply hash-map spec)]
                      (cond (contains? spec-as-map :flag) (process-flag spec-as-map args-for-main argv) 
                            (contains? spec-as-map :anon) [(concat args-for-main [(first argv)]) (rest argv)]
                            (contains? spec-as-map :anon-list) ( ; TODO: usage-error if argv has flags remaining
                                                                [(concat args-for-main argv) []]
                                                                )
                            :else (assert false (str "bad spec" spec))))) 
              [[] argv]
              spec-list)]
      (if (not (empty? argv-leftovers))
        (bomb-out-with-usage-error (format "unexpected extra arguments: %s" argv))
        (apply main args-for-main)))))

(defn process-command-or-group [command-or-group argv]
  (let [tag (first command-or-group)]  
    (cond (= tag :command) (process-command (second command-or-group) argv)
          (= tag :group)
          (let [subcommand-to-run (first argv)]
            ; TODO: ensure no duplicated sub-command names
            ; TODO: ensure subcommand-to-run isn't a command-line switch
            (process-command-or-group (second (first (filter (fn [name command-or-group] (= name subcommand-to-run)))))
                                      (rest argv)))
          :else (assert false "process-command-or-group: did not get :command or :group"))))

(defn required [type] [:required type])
(defn optional [type] [:optional type])
(defn optional-with-default [type value] [:optional-with-default type value])
(def no-arg [:no-arg])

(defn flag [switch type & {:keys [doc aliases]}]
  [:flag switch :type type :doc doc :aliases aliases])

(defn anon [name] [:anon name])
(defn anon-list [name] [:anon-list name])

(defn basic [& {:keys [summary spec main]}]
  [:command [:summary summary
             :spec spec
             :main main]])

(defn group [{:keys [summary names-and-subcommands]}]
  ; TODO: ensure names aren't command-line switch names
  [:group :summary summary :subcommands names-and-subcommands])

(defn run [command-or-group]
  "(run ...) processes command-line arguments to the program (argv) according to the specified command or group combinators"
  (process-command-or-group command-or-group *command-line-args*))

