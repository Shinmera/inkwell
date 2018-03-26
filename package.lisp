#|
 This file is a part of Inkwell
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:inkwell
  (:nicknames #:org.shirakumo.inkwell)
  (:use #:cl)
  (:shadow #:special)
  ;; api.lisp
  (:export
   #:*base-url*
   #:*session*
   #:api-request-failed
   #:url
   #:code
   #:body
   #:request
   #:list-battles
   #:battle
   #:single-player
   #:list-stages
   #:timeline
   #:user
   #:active-festivals
   #:list-festivals
   #:votes
   #:rankings
   #:shop-info
   #:order)
  ;; objects.lisp
  (:export
   ;; types
   #:battle
   #:stage
   #:player-result
   #:player
   #:rank
   #:weapon
   #:special
   #:sub
   #:single-player
   #:stage-info
   #:weapon-info
   #:festival
   #:festival-team
   #:alpha-team
   #:bravo-team
   #:merchandise
   #:gear
   #:brand
   #:skill
   #:user
   #:ranking
   #:schedule
   #:timeline
   #:weapon-release
   #:salmon-run
   #:challenge
   ;; readers
   #:id
   #:victory-p
   #:scores
   #:power
   #:start-time
   #:duration
   #:stage
   #:mode
   #:rule
   #:teams
   #:name
   #:image
   #:player
   #:sorting
   #:paint-points
   #:specials
   #:assists
   #:kills
   #:deaths
   #:head
   #:clothes
   #:shoes
   #:weapon
   #:player-rank
   #:star-rank
   #:rank
   #:nickname
   #:progress
   #:s-plus-number
   #:thumbnail
   #:special
   #:sub
   #:image-a
   #:image-b
   #:honor
   #:clear-rate
   #:cleared-weapons
   #:stage-info
   #:area
   #:boss-p
   #:weapon-info
   #:level
   #:clear-time
   #:announce-time
   #:end-time
   #:special-stage
   #:result-time
   #:long-name
   #:color
   #:participants
   #:solo
   #:team
   #:gear
   #:price
   #:kind
   #:brand
   #:rarity
   #:skills
   #:frequent-skill
   #:unique-id
   #:score
   #:cheater-p
   #:updated-time
   #:stages
   #:salmon-run
   #:stats
   #:schedule
   #:challenge
   #:merchandise
   #:rank-up-match
   #:dlc-available-p
   #:new-weapons
   #:release-time
   #:weapons
   #:reward)
  ;; toolkit.lisp
  (:export))
