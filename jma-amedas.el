;;; jma-amedas.el --- 気象庁アメダス関連処理        -*- lexical-binding: t; -*-

;; Copyright (C) 2022 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: comm

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

;; 気象庁からアメダスに関する情報を取得するためのコードです。

;; * 使用例
;; ** 選択したアメダス観測所の最新の気温を取得する
;; (jma-amedas-sample-temp ;;気温を取得
;;   (jma-amedas-point-latest ;;最新のデータを取得
;;     (jma-amedas-read-amedas-code))) ;;観測所を選んでもらう
;;
;; ** 最新のデータから過去1日分のデータを取得する
;; (let ((latest-time (jma-amedas-latest-time)))
;;   (jma-amedas-point-samples-between "44116" (time-add latest-time (* -24 60 60)) latest-time))


;;; Code:

(require 'jma-utils)
(require 'jma-area)



;;;; 観測所情報
;; jma-area.elに書くべきか迷う。jma-forecast.el等他でも使用するので。

;;;;; 観測所情報取得

(defvar jma-amedas-points nil)

(defun jma-amedas-points ()
  (or jma-amedas-points
      (setq jma-amedas-points
            (jma-json-get
             "https://www.jma.go.jp/bosai/amedas/const/amedastable.json"))))

(defun jma-amedas-point (amedas-code)
  "AMEDAS-CODEで指定されたコードを持つ観測所の情報を返します。"
  (assq (jma-ensure-symbol amedas-code) (jma-amedas-points)))

(defun jma-amedas-point-code (amedas-point)
  (symbol-name (car amedas-point)))

(defun jma-amedas-point-name (amedas-point)
  (alist-get 'kjName (cdr amedas-point)))

;;;;; 観測所検索
;; jma-area.elに書くべきか迷う。jma-forecast.el等他でも使用するので。

(defvar jma-amedas-class20-list nil)

(defun jma-amedas-class20-list ()
  "二次細分区域から近いアメダス観測所のリストを返します。"
  (or jma-amedas-class20-list
      (setq
       jma-amedas-class20-list
       (jma-json-get
        "https://www.jma.go.jp/bosai/amedas/const/amedas_class20_list.json"))))

(defun jma-amedas-near-class20 (class20-code)
  "二次細分区域CLASS20から近いアメダス観測所番号をいくつか返します。"
  (alist-get (jma-ensure-symbol class20-code) (jma-amedas-class20-list)))
;;Example (jma-amedas-near-class20 "0120200")

(defun jma-amedas-read-amedas-code (&optional amedas-codes prompt)
  "複数のアメダス観測所コードから一つを選びます。"
  (jma-choose-from-alist
   (or prompt "アメダス観測所: ")
   (mapcar
    (lambda (amedas-code)
      (cons
       (jma-amedas-point-name (jma-amedas-point amedas-code))
       amedas-code))
    (or amedas-codes
        ;; 省略時は府県予報区、二次細分区域を選んでもらう。
        (jma-amedas-near-class20
         (jma-area-read-class20-in-office (jma-area-read-office)))))))
;;(jma-amedas-read-amedas-code)



;;;; 計測データサンプル処理

(defun jma-amedas-sample-proc-aqc (value-vec)
  "シーケンスの第2要素が0のときだけ第1要素の値を返し、それ以外の時nilを返します。"
  ;; 参考:
  ;; https://qiita.com/e_toyoda/items/7a293313a725c4d306c0#:~:text=%E5%90%84%E8%A6%81%E7%B4%A0%E3%81%8C2%E3%81%A4%E3%81%AE%E6%95%B0%E5%80%A4%E3%81%AE%E9%85%8D%E5%88%97%E3%81%AB%E3%81%AA%E3%81%A3%E3%81%A6%E3%81%84%E3%81%BE%E3%81%99%E3%81%8C%E3%80%81%E5%85%88%E3%81%AEfloat%E3%81%8C%E5%80%A4%E3%81%9D%E3%81%AE%E3%82%82%E3%81%AE%E3%80%81%E5%BE%8C%E3%81%AE0%E3%81%B0%E3%81%8B%E3%82%8A%E3%81%AA%E3%81%AE%E3%81%8CAQC%E3%83%95%E3%83%A9%E3%82%B0%E3%81%A7%E3%81%99%E3%80%82
  ;; > 各要素が2つの数値の配列になっていますが、先のfloatが値そのもの、後の0ばかりなのがAQCフラグです。

  ;; 参考:
  ;; https://www.wxbc.jp/wp-content/uploads/2018/07/seminar_180706_03.pdf
  ;; > 0 正常
  ;; > 1 準正常（やや疑わしい）
  ;; > 2 非常に疑わしい
  ;; > 3 利用に適さない
  ;; > 4 観測値は期間内で資料数が不足している
  ;; > 5 点検又は計画休止のため欠測
  ;; > 6 障害のため欠測
  ;; > 7 この要素の観測はしていない
  (when (eq (elt value-vec 1) 0)
    (elt value-vec 0)))

(defun jma-amedas-sample-time (sample)
  "サンプルの取得時刻を返します。"
  (let* ((date-str (symbol-name (car sample)))
         (year (string-to-number (substring date-str 0 4)))
         (month (string-to-number (substring date-str 4 6)))
         (day (string-to-number (substring date-str 6 8)))
         (hour (string-to-number (substring date-str 8 10)))
         ;; 必ず10分毎か分からないので
         (minute (string-to-number (substring date-str 10 12)))
         ;; 必ず00秒か分からないので
         (second (string-to-number (substring date-str 12 14))))
    (encode-time (list second minute hour day month year nil nil 32400))))

(defun jma-amedas-sample-value (sample key)
  (jma-amedas-sample-proc-aqc
   (alist-get key (cdr sample))))

(defun jma-amedas-sample-temp (sample)
  "気温(℃)"
  (jma-amedas-sample-value sample 'temp))

(defun jma-amedas-sample-wind-direction (sample)
  "風向"
  (jma-amedas-sample-value sample 'windDirection))

(defun jma-amedas-sample-wind (sample)
  "風速(m/s)"
  (jma-amedas-sample-value sample 'wind))

(defun jma-amedas-sample-precipitation1h (sample)
  "(前1h)降水量(mm)"
  (jma-amedas-sample-value sample 'precipitation1h))



;;;; 計測データ時間軸処理

(defun jma-amedas-trim-sorted-samples-between (samples min-time max-time)
  "時刻でソート済みの計測データリストSAMPLESから、MIN-TIMEからMAX-TIME以外のデータを削除します。

新しいリストの先頭を返します。リストSAMPLESは変更されます。"
  (while (and samples
              (time-less-p (jma-amedas-sample-time (car samples)) min-time))
    (setq samples (cdr samples)))
  (let ((first samples)
        last)
    (while (and samples
                (not (time-less-p max-time (jma-amedas-sample-time (car samples)))))
      (setq last samples)
      (setq samples (cdr samples)))
    (when last
      (setcdr last nil)
      first)))



;;;; 最新時刻

(defun jma-amedas-latest-time ()
  (let* ((res (jma-http-get "https://www.jma.go.jp/bosai/amedas/data/latest_time.txt"))
         (status (jma-http-status res)))
    (unless (= status 200)
      (error "HTTP status is not OK : %s" status))
    (parse-iso8601-time-string (jma-http-body res))))
;;Example: (format-time-string "%Y-%m-%d %H:%M:%S" (jma-amedas-latest-time))



;;;; 全国計測データ取得

(defun jma-amedas-map-samples (time)
  "全国にある観測所の指定時刻の計測データを返します。"
  (jma-json-get
   (format
    "https://www.jma.go.jp/bosai/amedas/data/map/%s.json"
    (format-time-string "%Y%m%d%H%M%S" time))))
;;Example: (jma-amedas-map-samples (jma-amedas-latest-time))



;;;; 観測所計測データ取得

(defun jma-amedas-point-samples (amedas-code time)
  "指定地点、指定時間帯の計測データ(サンプル)のリストを返します。"
  (let* ((dt (decode-time time 32400))
         (year (decoded-time-year dt))
         (month (decoded-time-month dt))
         (day (decoded-time-day dt))
         (hour (decoded-time-hour dt)))
    (jma-json-get
     (format
      "https://www.jma.go.jp/bosai/amedas/data/point/%s/%04d%02d%02d_%02d.json"
      amedas-code
      year
      month
      day
      (- hour (% hour 3))))))

(defun jma-amedas-point-latest (amedas-code)
  "指定地点の最新の計測データを返します。"
  (car
   (last
    (jma-amedas-point-samples amedas-code (jma-amedas-latest-time)))))
;;Example: (jma-amedas-point-latest "44116")
;;Example: (jma-amedas-sample-temp (jma-amedas-point-latest "44116"))

(defun jma-amedas-point-samples-between (amedas-code min-time max-time)
  (jma-amedas-trim-sorted-samples-between
   (cl-loop for time = min-time then (time-add time 10800) ;;3hours
            while (not (time-less-p max-time time)) ;; time <= max-time
            append (jma-amedas-point-samples amedas-code time))
   min-time max-time))

(provide 'jma-amedas)
;;; jma-amedas.el ends here
