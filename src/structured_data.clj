(ns structured-data)

(defn do-a-thing [x]
  (let [plus-x (+ x x)] (Math/pow plus-x plus-x))
)

(defn spiff [v] (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1] [x2]] rectangle] (- x2 x1)) )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle 
        [p1 p2] point] 
        (and (<= x1 p1 x2) (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer 
    [[x3 y3] [x4 y4]] inner] 
    (and 
      (contains-point? outer [x3 y3] )
      (contains-point? outer [x4 y4]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  
  (let [existing-authors (get book :authors)] (assoc book :authors (conj existing-authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (let [get-second (fn [v] (get v 1))] 
   (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (get book :authors)] (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)) )

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [full-name (get author :name)
        birth (get author :birth-year)
        death (get author :death-year)
        lifespan-part (if (nil? birth) "" (str " (" birth " - " death ")"))]
        (str full-name lifespan-part)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)) ))

(defn book->string [book]
  (let [title (get book :title)
        authors (get book :authors)] (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (let [book-count (count books) 
        book-part (cond 
                  (= book-count 0) "No books."
                  (> book-count 1) (str book-count " books.")
                  :else "1 book.")] 
   (str book-part (if (= book-count 0) nil (str " " (apply str (interpose ". " (map book->string books))) ".")))))


(defn books-by-author [author books]
  ())

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
