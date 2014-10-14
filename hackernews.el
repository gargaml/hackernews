(require 'json)

(defconst url-top-stories "https://hacker-news.firebaseio.com/v0/topstories")
(defconst url-item "https://hacker-news.firebaseio.com/v0/item/")

(defun build-top-stories-request ()
  (concat url-top-stories ".json"))

(defun build-item-request (item xs)
  (let ((args (mapconcat (lambda (x)
			   (let ((k (url-hexify-string (car x)))
				 (v (url-hexify-string (cdr x))))
			     (concat k "=" v)))
			 xs
			 "&")))
    (concat url-item item ".json?" args)))

(defun get-item (item)
  (let* ((url (build-item-request item '()))
	 (b (url-retrieve-synchronously url)))
    (switch-to-buffer b)
    (goto-char (point-max))
    (let* ((s (line-beginning-position))
	   (e (line-end-position))
	   (j (buffer-substring-no-properties s e))
	   (d (json-read-from-string j)))
      (switch-to-buffer (previous-buffer))
      d)))

(defun get-by (item)
  (cdr (assoc 'by item)))

(defun get-title (item)
  (cdr (assoc 'title item)))

(defun get-top-stories ()
  (let ((b (url-retrieve-synchronously (build-top-stories-request))))
    (switch-to-buffer b)
    (goto-char (point-max))
    (let* ((s (line-beginning-position))
	   (e (line-end-position))
	   (is (buffer-substring-no-properties s e))
	   (j (json-read-from-string is)))
      (switch-to-buffer (previous-buffer))
      (mapcar (lambda (i) (get-item (int-to-string i))) j))))
      

(get-top-stories)


