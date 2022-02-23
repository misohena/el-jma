;;; jma-utils.el --- JMA Utilities                   -*- lexical-binding: t; -*-

;; Copyright (C) 2022 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)
(require 'calendar)

;;;; Customize

(defgroup jma nil
  "Access to Japan Meteorological Agency"
  :prefix "jma-"
  :group 'comm)

;;;; HTTP

(defun jma-http-parse-response (buffer)
  (with-current-buffer buffer
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (unless (looking-at "^HTTP/\\([^ ]+\\) +\\([0-9]+\\) +\\(.*\\)$")
      (error "Invalid HTTP response : %s" (buffer-substring (line-beginning-position) (line-end-position))))
    (let ((http-ver (match-string 1))
          (status (string-to-number (match-string 2)))
          (message (match-string 3))
          (headers (cl-loop while (progn (forward-line) (not (eolp)))
                            when (looking-at "^\\([^:\n]+\\): \\([^\n]*\\)$")
                            collect (cons (match-string 1) (match-string 2))))
          (body (progn
                  (forward-line)
                  (buffer-substring (point) (point-max)))))
      (list http-ver status message headers body))))

(defun jma-http-ver (res) (nth 0 res))
(defun jma-http-status (res) (nth 1 res))
(defun jma-http-message (res) (nth 2 res))
(defun jma-http-headers (res) (nth 3 res))
(defun jma-http-body (res) (nth 4 res))

(defun jma-http-get (url)
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (jma-http-parse-response buffer)
      (kill-buffer buffer))))

;;;; JSON

(defun jma-json-get (url)
  (let* ((res (jma-http-get url))
         (status (jma-http-status res)))
    (unless (= status 200)
      (error "HTTP status is not OK : %s" status))
    (json-parse-string (jma-http-body res) :object-type 'alist)))

;;;; 日付処理

(defun jma-date (y m d)
  (list m d y))

(defun jma-date-today (&optional tz)
  (let ((dt (decode-time nil tz)))
    (jma-date
     (decoded-time-year dt)
     (decoded-time-month dt)
     (decoded-time-day dt))))

(defun jma-date-to-time (date)
  (if (= (length date) 3)
      (encode-time
       (list
        0 0 0
        (calendar-extract-day date)
        (calendar-extract-month date)
        (calendar-extract-year date)
        nil nil
        32400 ;;常にJST(+09:00)で考える。
        ))
    ;; @todo support decoded time
    ;; encoded time?
    date))

(defun jma-date-inc-day (date &optional n)
  (jma-date
   (calendar-extract-year date)
   (calendar-extract-month date)
   (+ (calendar-extract-day date) (or n 1))))

;;;; 時刻処理

(defun jma-time-next-hour (time hours)
  "TIMEの次の時を返します。"
  (let* ((dt (decode-time time 32400))
         (y (decoded-time-year dt))
         (m (decoded-time-month dt))
         (d (decoded-time-day dt))
         (hour
          (cl-find-if
           (lambda (hour)
             (time-less-p
              time
              (encode-time (list 0 0 hour d m y nil nil 32400))))
           hours)))
    (if hour
        (encode-time (list 0 0 hour d m y nil nil 32400))
      (encode-time (list 0 0 (car hours) (1+ d) m y nil nil 32400)))))
;;(format-time-string "%Y-%m-%d %H:%M:%S" (jma-time-next-hour (encode-time '(0 0 17 22 2 2022 nil nil 32400)) '(5 11 17)))

;;;; 時系列データ処理

(defun jma-not-empty-range-p (pair)
  (and pair
       (< (car pair) (cdr pair))))

(defun jma-time-series-range-in-time (time-series lower-time upper-time)
  "時系列データ TIME-SERIES 内で時間が LOWER-TIME 以上 UPPER-TIME 未満の範囲を要素インデックス値のペアで返します。"
  (let ((times (mapcar #'parse-iso8601-time-string (alist-get 'timeDefines time-series)))
        (i 0) lower-index upper-index)
    (while (and times
                (time-less-p (car times) lower-time))
      (cl-incf i)
      (setq times (cdr times)))
    (setq lower-index i)
    (while (and times
                (time-less-p (car times) upper-time))
      (cl-incf i)
      (setq times (cdr times)))
    (setq upper-index i)
    (cons lower-index upper-index)))

(defun jma-time-series-range-in-date (time-series date)
  "時系列データ TIME-SERIES 内の DATE で指定した日付に該当する範囲を要素インデックス値のペアで返します。"
  (jma-time-series-range-in-time
   time-series
   (jma-date-to-time date)
   (jma-date-to-time (jma-date-inc-day date))))

(defun jma-time-series-area (time-series area-code)
  "時系列データ TIME-SERIES 内の AREA-CODE で指定された場所のデータを返します。"
  (cond
   ;; シーケンスの場合は要素のコードから探す。
   ((listp area-code)
    (seq-some
     (lambda (code) (jma-time-series-area time-series code))
     area-code))
   (t
    (seq-find
     (lambda (area) (equal (alist-get 'code (alist-get 'area area)) area-code))
     (alist-get 'areas time-series)))))

(defun jma-time-series-area-at (time-series area-index)
  (elt (alist-get 'areas time-series) area-index))

(defun jma-time-series-area-value-at (area field-name time-index)
  "時系列データの場所 AREA 内の FIELD-NAME で指定したデータの TIME-INDEX 番目を返します。"
  (elt (alist-get field-name area) time-index))

(defun jma-time-series-value-at (time-series area-code field-name time-index)
  "時系列データ TIME-SERIES から一つの値を取り出します。"
  (elt (alist-get field-name (jma-time-series-area time-series area-code)) time-index))

;;;; 文字列テンプレート

(defun jma-expand-template (template params)
  "TEMPLATEをPARAMSを使って展開します。

TEMPLATEが関数の場合、単にTEMPLATEにPARAMSを引き渡します。

TEMPLATEが文字列の場合、文字列中の展開指定を展開後文字列に置き換えます。

展開指定は {{{PNAME}}} または {{{PNAME:FMT}}} の形式です。"
  (cond
   ((functionp template)
    (funcall template params))
   ((stringp template)
    (let ((result "")
          (pos 0))
      (while (string-match "{{{\\([^:}]+\\)\\(?::\\([^}]+\\)\\)?}}}" template pos)
        (let* ((placeholder-beg (match-beginning 0))
               (placeholder-end (match-end 0))
               (pname (match-string 1 template))
               (fmt (match-string 2 template))
               ;; paramsのキーは文字列でもシンボルでもどちらでもOK
               (pvalue (cdr (or (assoc pname params)
                                (assq (intern pname) params))))
               ;; pvalueが関数なら、その関数を呼び出す
               (pvalue (if (functionp pvalue)
                           (funcall pvalue params)
                         pvalue))
               ;; pvalueが非nilのときだけfmtを使って文字列化
               ;; {{{xxx: %s度}}}のように前後の文字を制御できるようにする
               (fmted-pvalue
                (if pvalue
                    (format (or fmt "%s") pvalue)
                  "")))
          (setq result
                (concat
                 result
                 (substring template pos placeholder-beg)
                 fmted-pvalue))
          (setq pos placeholder-end)))
      (setq result (concat result (substring template pos)))
      result))))

(provide 'jma-utils)
;;; jma-utils.el ends here
