;;; jma-area.el --- JMA Area                         -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'jma-utils)

;;;; 発表区域

(defvar jma-area-data-cache nil)

(defun jma-area-data-get ()
  (or jma-area-data-cache
      (setq jma-area-data-cache
            (jma-json-get
             "https://www.jma.go.jp/bosai/common/const/area.json"))))

;;;;; 一覧取得

(defun jma-area-centers ()
  (alist-get 'centers (jma-area-data-get)))

(defun jma-area-offices ()
  (alist-get 'offices (jma-area-data-get)))

(defun jma-area-class10s ()
  (alist-get 'class10s (jma-area-data-get)))

(defun jma-area-class15s ()
  (alist-get 'class15s (jma-area-data-get)))

(defun jma-area-class20s ()
  (alist-get 'class20s (jma-area-data-get)))

;;;;; 検索

(defun jma-area-center (center-code)
  (assq
   (jma-ensure-symbol center-code)
   (jma-area-centers)))

(defun jma-area-office (office-code)
  (assq
   (jma-ensure-symbol office-code)
   (jma-area-offices)))

(defun jma-area-class10 (class10-code)
  (assq
   (jma-ensure-symbol class10-code)
   (jma-area-class10s)))

(defun jma-area-class15 (class15-code)
  (assq
   (jma-ensure-symbol class15-code)
   (jma-area-class15s)))

(defun jma-area-class20 (class20-code)
  (assq
   (jma-ensure-symbol class20-code)
   (jma-area-class20s)))

;;;;; 区域オブジェクトに対するアクセッサ

(defun jma-area-code-symbol (area-obj)
  (car area-obj))

(defun jma-area-code (area-obj)
  (symbol-name (jma-area-code-symbol area-obj)))

(defun jma-area-name (area-obj)
  (alist-get 'name (cdr area-obj)))

(defun jma-area-office-name (area-obj)
  (alist-get 'officeName (cdr area-obj)))

(defun jma-area-parent-code (area-obj)
  (alist-get 'parent (cdr area-obj)))

(defun jma-area-child-codes (area-obj)
  (alist-get 'children (cdr area-obj)))

;;;;; 府県予報区-二次細分区域 対応関係

(defun jma-area-class20-codes-in-office (office-code)
  "府県予報区OFFICE-CODEの中の全二次細分区域コードを返します。"
  (seq-mapcat
   (lambda (class15-code)
     (jma-area-child-codes (jma-area-class15 class15-code)))
   (seq-mapcat
    (lambda (class10-code)
      (jma-area-child-codes (jma-area-class10 class10-code)))
    (jma-area-child-codes (jma-area-office office-code)))))

;;;;; ユーザー入力

(defun jma-area-read-center (&optional prompt)
  "ミニバッファから地方予報区を読み取ります。"
  (jma-choose-from-alist
   (or prompt "地方予報区: ")
   (mapcar (lambda (center)
             (cons
              (jma-area-name center)
              (jma-area-code center)))
           (jma-area-centers))))
;;(jma-area-read-center)

(defun jma-area-read-office (&optional prompt)
  "ミニバッファから府県予報区(概ね都道府県)を読み取ります。"
  (jma-choose-from-alist
   (or prompt "府県予報区: ")
   (mapcar (lambda (office)
             (cons
              (jma-area-name office)
              (jma-area-code office)))
           (jma-area-offices))))
;;(jma-area-read-office)

(defun jma-area-read-class10 (office-code &optional prompt)
  "ミニバッファから一次細分区域を読み取ります。"
  (jma-choose-from-alist
   (or prompt "一次細分区域: ")
   (mapcar (lambda (class10-code)
             (cons
              (jma-area-name (jma-area-class10 class10-code))
              class10-code))
           (jma-area-child-codes (jma-area-office office-code)))))
;;(jma-area-read-class10 "130000")

(defun jma-area-read-class20-in-office (office-code &optional prompt)
  "ミニバッファから二次細分区域(概ね市区町村)を読み取ります。"
  (jma-choose-from-alist
   (or prompt "二次細分区域: ")
   (mapcar (lambda (class20-code)
             (cons
              (jma-area-name (jma-area-class20 class20-code))
              class20-code))
           (jma-area-class20-codes-in-office office-code))))
;;(jma-area-read-class20-in-office "130000")


(provide 'jma-area)
;;; jma-area.el ends here
