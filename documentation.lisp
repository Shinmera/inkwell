#|
This file is a part of Inkwell
(c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.inkwell)

;;; api.lisp
(docs:define-docs
  (variable *base-url*
    "The base URL for all Splatnet resources.")
  
  (variable *session*
    "The iksm_session cookie value required for Splatnet requests.

There is currently no automated procedure to generate
or fetch such a token. You will have to use some form
of MITM packet sniffing tool to extract it from requests
from your Nintendo Switch mobile app. If you google for
iskm_session you should find a couple of tutorials and
tools to do it.")
  
  (type api-request-failed
    "Error signalled if a request fails for some reason.

See URL
See CODE
See BODY")
  
  (function url
    "The URL to which the request failed.

See API-REQUEST-FAILED")
  
  (function code
    "The HTTP status code that was returned.

See API-REQUEST-FAILED")
  
  (function body
    "The HTTP body as a string.

See API-REQUEST-FAILED")
  
  (function request
    "Perform a request against the specified API endpoint.

Note that the endpoint must begin with a slash. If you need
to parameterise the URL, use the urlparts argument. Note that
slashes are automatically inserted before each urlpart, so
you will not need to do so yourself.

In case of an error code being returned from the server, a
condition of type API-REQUEST-FAILED is signalled.

See API-REQUEST-FAILED
See *SESSION*")
  
  (function list-battles
    "Retrieves a list of recent battle results.

See BATTLE")
  
  (function battle
    "Retrieves a specific battle result.

Takes either a battle object or an ID.

See BATTLE")
  
  (function single-player
    "Retrieves single-player mode statistics.

See SINGLE-PLAYER")

  (function schedules
    "Retrieves schedule information for upcoming online rotations.

Returns a plist of three keys, :REGULAR :RANKED and :LEAGUE.
Each value is a list of SCHEDULE entries.

See SCHEDULE")
  
  (function list-stages
    "Lists all available stages/maps.

See STAGE")
  
  (function timeline
    "Retrieves the overall timeline object.

See TIMELINE")
  
  (function user
    "Retrieves information about a specific user.

See USER")
  
  (function active-festivals
    "Retrieves active festivals (splatfests).

See FESTIVAL")
  
  (function list-festivals
    "Retrieves past festivals (splatfests).

See FESTIVAL")
  
  (function votes
    "Retrieves information about votes cast by your friends for a specific festival (splatfest).

Takes a FESTIVAL instance, or a festival ID.

Returns a list of two entries, the first being a list of all
friends that voted for team alpha, and the second being a list
of all friends that voted for team bravo.

See USER")
  
  (function rankings
    "Retrieves top-100 rank listings for a specific festival (splatfest).

Takes a FESTIVAL instance, or a festival ID.

Returns a list of two entries, the first being a list of the
top 100 rankings for team alpha, the second being a list of the
top 100 rankings for team bravo. The lists are sorted by rank.

See RANKING")
  
  (function shop-info
    "Retrieves online shop information.

Returns two values: a list of available merchandise, and, if
the user has already ordered something, the ordered merchandise.

See MERCHANDISE")
  
  (function order
    "Order a marchandise item from the online shop.

Takes a MERCHANDISE instance, or a merchandise ID.

If the user has already ordered something, and override is NIL,
the requested merchandise will not be ordered.

See MERCHANDISE"))

;;; objects.lisp
(docs:define-docs
  ;; types
  (type battle
    "Representation of a battle result.

See ID
See VICTORY-P
See SCORES
See POWER
See START-TIME
See DURATION
See STAGE
See MODE
See RULE
See TEAMS")

  (type stage
    "Representation of a stage on which matches take place.

This counts both for regular matches and Salmon Run.

See ID
See NAME
See IMAGE")

  (type player-result
    "Representation of a player's performance in an online match.

See PLAYER
See SORTING
See PAINT-POINTST
See SPECIALS
See ASSISTS
See KILLS
See DEATHS")

  (type player
    "Representation of a player character in online matches.

See HEAD
See CLOTHES
See SHOES
See WEAPON
See PLAYER-RANK
See STAR-RANK
See ID
See RANK
See NAME")

  (type rank
    "Representation of a player's rank in a ranked or league mode.

See NAME
See PROGRESS
See S-PLUS-NUMBER")

  (type weapon
    "Representation of a weapon.

See ID
See NAME
See IMAGE
See THUMBNAIL
See SPECIAL
See SUB")

  (type special
    "Representation of a special ability.

See ID
See NAME
See IMAGE-A
See IMAGE-B")

  (type sub
    "Representation of a sub-weapon.

See ID
See NAME
See IMAGE-A
See IMAGE-B")

  (type single-player
    "Representation of single-player mode performance statistics.

See HONOR
See CLEAR-RATE
See CLEARED-WEAPONS
See STAGE-INFO")

  (type stage-info
    "Representation of performance statistics on a single-player stage.

See AREA
See ID
See BOSS-P
See WEAPON-INFO")

  (type weapon-info
    "Representation of performance statistics of a weapon on a particular single-player stage.

See ID
See NAME
See LEVEL
See CLEAR-TIME
See IMAGE")

  (type festival
    "Representation of a festival (splatfest).

This is used for both past and future festivals, though
some fields will not be available until the festival is
over.

See ID
See TEAMS
See ANNOUNCE-TIME
See START-TIME
See END-TIME
See SPECIAL-STAGE
See RESULT-TIME")

  (type festival-team
    "Representation of a team for a festival (splatfest).

This is used for both past and future festivals, though
some fields will not be available until the festival is
over.

See NAME
See LONG-NAME
See COLOR
See IMAGE
See PARTICIPANTS
See SOLO
See TEAM")

  (type alpha-team
    "Representation of team alpha for a festival (splatfest).

See FESTIVAL-TEAM")

  (type bravo-team
    "Representation of team bravo for a festival (splatfest).

See FESTIVAL-TEAM")

  (type merchandise
    "Representation of a piece of merchandise from the online shop.

See GEAR
See PRICE
See END-TIME
See KIND
See ID")

  (type gear
    "Representation of a piece of gear a player character can wear.

See NAME
See KIND
See ID
See BRAND
See RARITY
See THUMBNAIL
See IMAGE
See SKILLS")

  (type brand
    "Representation of a gear brand.

See FREQUENT-SKILL
See IMAGE
See NAME
See ID")

  (type skill
    "Representation of a game power up skill.

See ID
See NAME
See IMAGE")

  (type user
    "Representation of a user account.

See ID
See NAME
See THUMBNAIL")

  (type ranking
    "Representation of a festival (splatfest) ranking.

See ID
See UNIQUE-ID
See SCORE
See CHEATER-P
See PLAYER
See UPDATED-TIME
See SORTING")

  (type schedule
    "Representation of an online mode rotation schedule item.

See ID
See MODE
See RULE
See START-TIME
See END-TIME
See STAGES")

  (type timeline
    "Representation of the overall timeline object.

This object contains bits and pieces of information about
the overall status of the splatnet for the user. It is
used to present the \"home page\" on the Switch app for
Splatoon.

See ID
See SALMON-RUN
See STATS
See SCHEDULE
See CHALLENGE
See PAINT-POINTST
See MERCHANDISE
See RANK-UP-MATCH
See DLC-AVAILABLE-P
See NEW-WEAPONS")

  (type weapon-release
    "Representation of a new weapon release.

See WEAPON
See RELEASE-TIME")

  (type salmon-run
    "Representation of a Salmon Run shift.

See STAGE
See END-TIME
See START-TIME
See WEAPONS
See REWARD")

  (type challenge
    "Representation of an ink points challenge on the app.

See ID
See NAME
See PAINT-POINTS
See IMAGE")

  ;; readers
  (function id
    "Returns some kind of ID (may be a string or number) to identify the object.

Note that some IDs are not globally unique.

See BATTLE
See STAGE
See PLAYER
See WEAPON
See SPECIAL
See SUB
See STAGE-INFO
See WEAPON-INFO
See FESTIVAL
See MERCHANDISE
See GEAR
See BRAND
See SKILL
See USER
See RANKING
See SCHEDULE
See TIMELINE
See CHALLENGE")

  (function victory-p
    "Returns whether the battle was a victory for the user or not.

See BATTLE")

  (function scores
    "Returns a list of scores for the user's team and the opponent team.

The scores are either a point count for ranked modes, or
a percentage for turf war.

See BATTLE")

  (function power
    "Returns the battle power.

See BATTLE")

  (function start-time
    "Returns the starting date timestamp.

See BATTLE
See FESTIVAL
See SCHEDULE
See SALMON-RUN")

  (function duration
    "Returns the duration of the battle in seconds.

See BATTLE")

  (function stage
    "Returns the stage where the battle took place.

See STAGE
See BATTLE
See SALMON-RUN")

  (function mode
    "Returns the game mode of the battle or schedule.

Can be one of :REGULAR :RANKED :LEAGUE
In case of new modes (super unlikely), :UNKNOWN may be returned.

See BATTLE
See SCHEDULE")

  (function rule
    "Returns the game rules of the battle or schedule

Can be one of :TURF-WAR :SPLAT-ZONES :TOWER-CONTROL :CLAM-BLITZ
In case of new rules (unlikely), :UNKNOWN may be returned.

See BATTLE
See SCHEDULE")

  (function teams
    "Returns a list of two items for the teams for the battle.

Each team list is sorted by the player-result's sorting.
The first list is always the user's own team.

See PLAYER-RESULT
See BATTLE")

  (function name
    "Returns a string for the name of the object.

See STAGE
See PLAYER
See RANK
See WEAPON
See SPECIAL
See SUB
See WEAPON-INFO
See FESTIVAL-TEAM
See GEAR
See BRAND
See SKILL
See USER
See CHALLENGE")

  (function image
    "Returns a full URL to an image resource showing the object.

See STAGE
See WEAPON
See WEAPON-INFO
See FESTIVAL-TEAM
See GEAR
See BRAND
See SKILL
See CHALLENGE")

  (function player
    "Returns the player associated with this object.

See PLAYER
See PLAYER-RESULT
See RANKING")

  (function sorting
    "Returns an integer to sort the objects of this kind by.

See PLAYER-RESULT
See RANKING")

  (function paint-points
    "Returns the number of paint points the player gained in the battle.

See PLAYER-RESULT
See CHALLENGE
See TIMELINE")

  (function specials
    "Returns the number of times the player used their special in the battle.

See BATTLE")

  (function assists
    "Returns the number of assists the player achieved in the battle.

See BATTLE")

  (function kills
    "Returns the number of kills the player achieved in the battle.

See BATTLE")

  (function deaths
    "Returns the number of times the player died in the battle.

See BATTLE")

  (function head
    "Returns the head gear item.

See GEAR
See PLAYER")

  (function clothes
    "Returns the chest clothes gear item.

See GEAR
See PLAYER")

  (function shoes
    "Returns the shoes gear item.

See GEAR
See PLAYER")

  (function weapon
    "Returns the weapon the player used or the weapon that is being released.

See WEAPON
See PLAYER
See WEAPON-RELEASE")

  (function player-rank
    "Returns the player's rank number.

See PLAYER")

  (function star-rank
    "Returns the player's star rank number.

See PLAYER")

  (function rank
    "Returns the player's ranked/league rank information.

See RANK
See PLAYER")

  (function progress
    "Returns the progress the player has made in the rank.

See RANK")

  (function s-plus-number
    "Returns the S+ rank number, if any.

See RANK")

  (function thumbnail
    "Returns a full URL for a thumbnail image representing the object.

See WEAPON
See GEAR
See USER")

  (function special
    "Returns the special ability the weapon allows.

See SPECIAL
See WEAPON")

  (function sub
    "Returns the sub-weapon ability the weapon allows.

See SUB
See WEAPON")

  (function image-a
    "Returns a full URL for an image describing the object.

See SPECIAL
See SUB")

  (function image-b
    "Returns a full URL for an image describing the object.

See SPECIAL
See SUB")

  (function honor
    "Returns the single-player honor description.

See SINGLE-PLAYER")

  (function clear-rate
    "Returns the overall clear-rate.

1.0 means 100%, but it can go up to 10.

See SINGLE-PLAYER")

  (function cleared-weapons
    "Returns a list of cleared weapons.

The list is composed of strings of the weapon names.

See SINGLE-PLAYER")

  (function stage-info
    "Returns a list of statistics about each single-player stage.

See STAGE-INFO
See SINGLE-PLAYER")

  (function area
    "Returns the ID of the area the stage-info is about.

See STAGE-INFO")

  (function boss-p
    "Returns whether this stage is a boss.

See STAGE-INFO")

  (function weapon-info
    "Returns a list of weapon statistics for the stage.

The list is sorted by the clear time, lowest first.

See WEAPON-INFO
See STAGE-INFO")

  (function level
    "Returns the level of the weapon.

See WEAPON-INFO")

  (function clear-time
    "Returns the clear time using the weapon for a particular stage.

The time is in seconds.

See WEAPON-INFO")

  (function announce-time
    "Returns the timestamp on which the festival (splatfest) was announced.")

  (function end-time
    "Returns the timestamp on which this object becomes invalid/outdated.

See SALMON-RUN
See SCHEDULE
See MERCHANDISE
See FESTIVAL")

  (function special-stage
    "Returns the stage for the festival (splatfest)'s special stage.

See STAGE
See FESTIVAL")

  (function result-time
    "Returns the timestamp on which the festival (splatfest) results are announced.

See FESTIVAL")

  (function long-name
    "Returns the long, descriptive name of the festival (splatfest) team.

See FESTIVAL-TEAM")

  (function color
    "Returns the colour of the festival (splatfest) team.

This is a list with components R G B A.

See FESTIVAL-TEAM")

  (function participants
    "Returns the number of people that voted for this festival (splatfest) team.

See FESTIVAL-TEAM")

  (function solo
    "Returns the number of won solo battles for this festival (splatfest) team.

See FESTIVAL-TEAM")

  (function team
    "Returns the number of won team battles for this festival (splatfest) team.

See FESTIVAL-TEAM")

  (function gear
    "Returns the piece of gear this merchandise is selling.

See GEAR
See MERCHANDISE")

  (function price
    "Returns the price of the merchandise.

See MERCHANDISE")

  (function kind
    "Returns the kind of gear this is as a keyword.

See MERCHANDISE
See GEAR")

  (function brand
    "Returns the brand of the gear.

See BRAND
See GEAR")

  (function rarity
    "Returns a rarity index of the gear.

See GEAR")

  (function skills
    "Returns a list of allocated skill slots.

The first item is the primary skill slot.

See SKILL
See GEAR")

  (function frequent-skill
    "Returns the skill that this brand usually puts on its primary slot.

See SKILL
See BRAND")

  (function unique-id
    "Returns the unique id of this ranking.

See RANKING")

  (function score
    "Returns the total score of this ranking.

See RANKING")

  (function cheater-p
    "Returns whether the player was deemed to be a cheater or not.

See RANKING")

  (function updated-time
    "Returns a timestamp of the last time this ranking was updated.

See RANKING")

  (function stages
    "Returns the stages that will be played on for this schedule item.

See STAGE
See SCHEDULE")

  (function salmon-run
    "Returns a salmon-run object if there is currently a Salmon Run shift open.

See SALMON-RUN
See TIMELINE")

  (function stats
    "Returns a few of the most recent battle results.

See BATTLE
See TIMELINE")

  (function schedule
    "Returns a few of the next schedule items.

This is a plist of three keys, :REGULAR :RANKED and :LEAGUE.
Each value is a list of SCHEDULE instances.

See SCHEDULE
See TIMELINE")

  (function challenge
    "Returns information about the current ink point challenge.

This is a plist of two keys, :NEXT and :PREVIOUS.
Each value is a CHALLENGE instance.

See CHALLENGE
See TIMELINE")

  (function merchandise
    "Returns a single piece of merchandise currently available for purchase.

See MERCHANDISE
See TIMELINE")

  (function rank-up-match
    "Returns the last battle that caused the user to rank up, if any.

See BATTLE
See TIMELINE")

  (function dlc-available-p
    "Returns whether there is DLC ready to download.

See TIMELINE")

  (function new-weapons
    "Returns a list of new weapons that will be released soon / have been released recently.

See WEAPON-RELEASE
See TIMELINE")

  (function release-time
    "Returns a timestamp of the date on which this weapon will become available.

See WEAPON-RELEASE")

  (function weapons
    "Returns a list of weapons that will be handed out for this Salmon Run shift.

See WEAPON
See SALMON-RUN")

  (function reward
    "Returns the gear that can be won in this Salmon Run shift.

See GEAR
See SALMON-RUN"))
