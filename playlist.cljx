(ns playlist-rest.playlist
  (:use [playlist-rest.prime-factoring :only [factor merge-primes]])
  (#+clj :use #+cljs :use-macros [playlist-rest.macros :only (=ToClass?)]))

(defprotocol PlaylistType
  (playlist-length [this])
  (get-item [this idx]))

(defrecord Single [item-list]
  PlaylistType
  (playlist-length [this] (count item-list))
  (get-item [this idx]
	    (nth item-list (mod idx (count item-list)))))

(defn single? [maybe]
  #+cljs (js/alert (str "maybe? " (class maybe)))
  (=ToClass? Single maybe))

(defrecord Merge [singles]
  PlaylistType
  (playlist-length [this] (reduce + (map (fn [x] (playlist-length x)) singles)))
  (get-item [this idx]
    (letfn [(get-item-recurse [idx items]
             (let [hd (first items)
                   hd-length (playlist-length hd)
                   tl (rest items)]
               (if (< (- hd-length 1) idx)
                 (get-item-recurse (- idx hd-length) tl)
                 (get-item hd idx))))]
      (get-item-recurse (mod idx (playlist-length this)) singles))))

(defn merge? [maybe] (=ToClass? Merge maybe))

(defrecord Complex [singles]
  PlaylistType
  (playlist-length [this]
    (* (count singles)
       (reduce * (reduce merge-primes (map (comp factor playlist-length) singles)))))
  (get-item [this idx] (get-item (nth singles (mod idx (count singles)))
                                 (/ idx (count singles)))))

(defn complex? [maybe] (=ToClass? Complex maybe))

(defrecord Playlist [rows]
  PlaylistType
  (playlist-length [this]
    (* (count rows)
       (reduce * (reduce merge-primes (map (comp factor playlist-length) rows)))))
  (get-item [this idx]
    (map (fn [x] (get-item x idx)) rows)))

(defn playlist? [maybe]
  (=ToClass? Playlist maybe))
