#|
 This file is a part of Inkwell
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.inkwell)

(define-class battle ()
  (id
   victory-p
   scores
   power
   start-time
   duration
   stage
   mode
   rule
   teams))

(define-unreadable-printer battle
  "~a ~:[VICTORY~;LOSS~] {~a}"
  (rule battle) (victory-p battle) (id battle))

(define-converter battle
  :id (=> "battle_number")
  :victory-p (not (null (string= "victory" (=> "my_team_result" "key"))))
  :scores (list (or (=> "my_team_count") (=> "my_team_percentage"))
                (or (=> "other_team_count") (=> "other_team_percentage")))
  :power (=> "estimate_gachi_power")
  ;; FIXME: Where to stuff weapon_paint_point ?
  :start-time (->date (=> "start_time"))
  :duration (or (=> "elapsed_time") (* 3 60))
  :stage (into 'stage (=> "stage"))
  :mode (->mode (=> "game_mode" "key"))
  :rule (->rule (=> "rule" "key"))
  :teams (list (sort (list*
                      (into 'player-result (=> "player_result"))
                      (into 'player-result (=> "my_team_members")))
                     #'> :key #'sorting)
               (sort (into 'player-result (=> "other_team_members"))
                     #'> :key #'sorting)))

(define-class stage ()
  (id
   name
   image))

(define-unreadable-printer stage
  "~a {~a}" (name stage) (id stage))

(define-converter stage
  :id (=> "id")
  :name (=> "name")
  :image (->url (->url (=> "image"))))

(define-class player-result ()
  (player
   sorting
   paint-points
   specials
   assists
   kills
   deaths))

(define-unreadable-printer player-result
  "~a ~ap ~as ~aa ~ak ~ad"
  (name (player player-result)) (paint-points player-result)
  (specials player-result) (assists player-result) (kills player-result) (deaths player-result))

(define-converter player-result
  :player (into 'player (=> "player"))
  :sorting (=> "sort_score")
  :paint-points (=> "game_paint_point")
  :specials (=> "special_count")
  :assists (=> "assist_count")
  :kills (=> "kill_count")
  :deaths (=> "death_count"))

(define-class player ()
  (head
   clothes
   shoes
   weapon
   player-rank
   star-rank
   id
   rank
   name))

(define-unreadable-printer player
  "~a {~a}" (name player) (id player))

(define-converter player
  :head (into (into 'gear (=> "head"))
              (=> "head_skills"))
  :clothes (into (into 'gear (=> "clothes"))
                 (=> "clothes_skills"))
  :shoes (into (into 'gear (=> "shoes"))
               (=> "shoes_skills"))
  :player-rank (=> "player_rank")
  :star-rank (=> "star_rank")
  :id (=> "principal_id")
  :weapon (into 'weapon (=> "weapon"))
  :name (=> "name")
  :rank (into 'rank (=> "udemae")))

(define-class rank ()
  (name
   progress
   s-plus-number))

(define-unreadable-printer rank
  "~a~@[~a~] ~a"
  (name rank) (s-plus-number rank) (progress rank))

(define-converter rank
  :name (->keyword (=> "name"))
  :progress (=> "number")
  :s-plus-number (=> "s_plus_number"))

(define-class weapon ()
  (id
   name
   image
   thumbnail
   special
   sub))

(define-unreadable-printer weapon
  "~a (~a/~a) {~a}"
  (name weapon) (name (special weapon)) (name (sub weapon)) (id weapon))

(define-converter weapon
  :id (=> "id")
  :name (=> "name")
  :image (->url (=> "image"))
  :thumbnail (->url (=> "thumbnail"))
  :special (into 'special (=> "special"))
  :sub (into 'sub (=> "sub")))

(define-class special ()
  (id
   name
   image-a
   image-b))

(define-unreadable-printer special
  "~a {~a}" (name special) (id special))

(define-converter special
  :id (=> "id")
  :name (=> "name")
  :image-a (->url (=> "image_a"))
  :image-b (->url (=> "image_b")))

(define-class sub ()
  (id
   name
   image-a
   image-b))

(define-unreadable-printer sub
  "~a {~a}" (name sub) (id sub))

(define-converter sub
  :id (=> "id")
  :name (=> "name")
  :image-a (->url (=> "image_a"))
  :image-b (->url (=> "image_b")))

(define-class single-player ()
  (honor
   clear-rate
   cleared-weapons
   stage-info))

(define-unreadable-printer single-player
  "~a (~a%)" (honor single-player) (* 100 (clear-rate single-player)))

(define-converter single-player
  :honor (=> "summary" "honor" "name")
  :clear-rate (=> "summary" "clear_rate")
  :cleared-weapons (loop for k being the hash-keys of (=> "summary" "weapon_cleared_info") using (hash-value v)
                         when v collect (->weapon-name k))
  :stage-info (into 'stage-info (let ((info (=> "stage_infos")))
                                  (dolist (i info info)
                                    (setf (gethash "weapon_map" i) (=> "weapon_map"))))))

(define-class stage-info ()
  (area
   id
   boss-p
   weapon-info))

(define-unreadable-printer stage-info
  "~a-~a~:[ BOSS~;~]" (area stage-info) (id stage-info) (boss-p stage-info))

(define-converter stage-info
  :area (=> "stage" "area")
  :id (=> "stage" "id")
  :boss-p (=> "stage" "is_boss")
  :weapon-info (sort (into 'weapon-info (loop for v being the hash-values of (=> "clear_weapons")
                                              do (setf (gethash "image" v) (=> "weapon_map" (gethash "weapon_category" v)))
                                              collect v))
                     #'< :key #'clear-time))

(define-class weapon-info ()
  (id
   name
   level
   clear-time
   image))

(define-unreadable-printer weapon-info
  "~a ~a:~a" (name weapon-info) (mod (clear-time weapon-info) 60) (floor (clear-time weapon-info) 60))

(define-converter weapon-info
  :id (=> "weapon_category")
  :name (->weapon-name (=> "weapon_category"))
  :level (=> "weapon_level")
  :clear-time (=> "clear_time")
  :image (->url (=> "image")))

(define-class festival ()
  (id
   teams
   announce-time
   start-time
   end-time
   special-stage
   result-time))

(define-unreadable-printer festival
  "~a vs ~a {~a}" (name (first (teams festival))) (name (second (teams festival))) (id festival))

(define-converter festival
  :id (=> "festival_id")
  :announce-time (->date (=> "times" "announce"))
  :start-time (->date (=> "times" "start"))
  :end-time (->date (=> "times" "end"))
  :result-time (->date (=> "times" "result"))
  :special-stage (into 'stage (=> "special_stage"))
  :teams (list (into 'alpha-team (=>))
               (into 'bravo-team (=>))))

(define-class festival-team ()
  (name
   long-name
   color
   image
   participants
   solo
   team))

(define-class alpha-team (festival-team) ())

(define-converter alpha-team
  :name (=> "names" "alpha_short")
  :long-name (=> "names" "alpha_long")
  :color (list (=> "colors" "alpha" "r")
               (=> "colors" "alpha" "g")
               (=> "colors" "alpha" "b")
               (=> "colors" "alpha" "a"))
  :image (->url (=> "images" "alpha"))
  :participants (=> "result" "team_participants" "alpha")
  :solo (=> "result" "team_scores" "alpha_solo")
  :team (=> "result" "team_scores" "alpha_team"))

(define-class bravo-team (festival-team) ())

(define-converter bravo-team
  :name (=> "names" "bravo_short")
  :long-name (=> "names" "bravo_long")
  :color (list (=> "colors" "bravo" "r")
               (=> "colors" "bravo" "g")
               (=> "colors" "bravo" "b")
               (=> "colors" "bravo" "a"))
  :image (->url (=> "images" "bravo"))
  :participants (=> "result" "team_participants" "bravo")
  :solo (=> "result" "team_scores" "bravo_solo")
  :team (=> "result" "team_scores" "bravo_team"))

(define-class merchandise ()
  (gear
   price
   end-time
   kind
   id))

(define-converter merchandise
  :gear (into (into 'gear (=> "gear"))
              (mkhash "main" (=> "skill")))
  :price (=> "price")
  :end-time (->date (=> "end_time"))
  :kind (->keyword (=> "kind"))
  :id (=> "id"))

(define-unreadable-printer merchandise
  "~a ~a¢ {~a}" (name (gear merchandise)) (price merchandise) (id merchandise))

(define-class gear ()
  (name
   kind
   id
   brand
   rarity
   thumbnail
   image
   skills))

(define-unreadable-printer gear
  "~a ~a {~a}" (name (brand gear)) (name gear) (id gear))

(define-converter gear
  :name (=> "name")
  :kind (->keyword (=> "kind"))
  :id (=> "id")
  :brand (into 'brand (=> "brand"))
  :rarity (=> "rarity")
  :thumbnail (->url (=> "thumbnail"))
  :image (->url (=> "image"))
  :skills (list* (into 'skill (=> "main")) (into 'skill (=> "subs"))))

(define-class brand ()
  (frequent-skill
   image
   name
   id))

(define-unreadable-printer brand
  "~a {~a}" (name brand) (id brand))

(define-converter brand
  :frequent-skill (into 'skill (=> "frequent_skill"))
  :image (->url (=> "image"))
  :name (=> "name")
  :id (=> "id"))

(define-class skill ()
  (id
   name
   image))

(define-unreadable-printer skill
  "~a {~a}" (name skill) (id skill))

(define-converter skill
  :id (=> "id")
  :name (=> "name")
  :image (->url (=> "image")))

(define-class user ()
  (id
   name
   thumbnail))

(define-unreadable-printer user
  "~a {~a}" (name user) (id user))

(define-converter user
  :id (=> "nsa_id")
  :name (=> "nickname")
  :thumbnail (=> "thumbnail_url"))

(define-class ranking ()
  (id
   unique-id
   score
   cheater-p
   player
   updated-time
   sorting))

(define-unreadable-printer ranking
  "#~a ~a ~ap {~a}" (sorting ranking) (name (player ranking)) (score ranking) (id ranking))

(define-converter ranking
  :id (=> "principal_id")
  :unique-id (=> "unique_id")
  :score (=> "score")
  :cheater-p (=> "cheater_p")
  :player (into 'player (=> "info"))
  :updated-time (->date (=> "updated_time"))
  :sorting (=> "order"))

(define-class schedule ()
  (id
   mode
   rule
   start-time
   end-time
   stages))

(define-unreadable-printer schedule
  "~a-~a {~a}"
  (fmttime (start-time schedule) :format :hour)
  (fmttime (end-time schedule) :format :hour)
  (id schedule))

(define-converter schedule
  :id (=> "id")
  :mode (->mode (=> "mode" "key"))
  :rule (->rule (=> "rule" "key"))
  :start-time (->date (=> "start_time"))
  :end-time (->date (=> "end_time"))
  :stages (remove NIL (list (into 'stage (=> "stage_a"))
                            (into 'stage (=> "stage_b"))
                            (into 'stage (=> "stage_c")))))

(define-class timeline ()
  (id
   salmon-run
   stats
   schedule
   challenge
   paint-points
   merchandise
   rank-up-match
   dlc-available-p
   new-weapons))

(define-unreadable-printer timeline
  "{~a}" (id timeline))

(define-converter timeline
  :id (=> "unique_id")
  :salmon-run (into 'salmon-run (=> "coop"))
  :stats (into 'battle (=> "stats" "recents"))
  :schedule (list :regular (into 'schedule (=> "schedule" "schedules" "regular"))
                  :ranked (into 'schedule (=> "schedule" "schedules" "gachi"))
                  :league (into 'schedule (=> "schedule" "schedules" "league")))
  :challenge (list :next (into 'challenge (=> "challenge" "next_challenge"))
                   :previous (into 'challenge (=> "challenge" "last_archived_challenge")))
  :paint-points (=> "challenge" "total_paint_point")
  :merchandise (into 'merchandise (=> "onlineshop" "merchandise"))
  :rank-up-match (into 'battle (=> "udemae" "stat"))
  :dlc-available-p (=> "download_contents" "is_available")
  :new-weapons (into 'weapon-release (=> "weapon_availability" "availabilities")))

(define-class weapon-release ()
  (weapon
   release-time))

(define-unreadable-printer weapon-release
  "~a ~a" (fmttime (release-time weapon-release) :format :date) (name (weapon weapon-release)))

(define-converter weapon-release
  :weapon (into 'weapon (=> "weapon"))
  :release-time (->date (=> "release_time")))

(define-class salmon-run ()
  (stage
   end-time
   start-time
   weapons
   reward))

(define-unreadable-printer salmon-run
  "~a-~a ~a"
  (fmttime (start-time salmon-run) :format :hour)
  (fmttime (end-time salmon-run) :format :hour)
  (name (stage salmon-run)))

(define-converter salmon-run
  :stage (into 'stage (=> "schedule" "stage"))
  :end-time (->date (=> "schedule" "end_time"))
  :start-time (->date (=> "schedule" "start_time"))
  :weapons (into 'weapon (=> "schedule" "weapons"))
  :reward (into 'gear (=> "reward_gear" "gear")))

(define-class challenge ()
  (id
   name
   paint-points
   image))

(define-unreadable-printer challenge
  "~a ~ap" (name challenge) (paint-points challenge))

(define-converter challenge
  :id (=> "key")
  :name (=> "name")
  :paint-points (=> "paint_points")
  :image (->url (=> "image")))
