;;; bbdb2tbird.el --- export Emacs BBDB to Thunderbird Address Book via LDIF

;; Copyright (C) 2007 Neil W. Van Dyke

;; Author:   Neil Van Dyke <neil@neilvandyke.org>
;; Keywords: bbdb addressbook export thunderbird icedove ldif ldap
;; X-URL:    http://www.neilvandyke.org/bbdb2tbird/
;; X-CVS:    $Id: bbdb2tbird.el,v 1.26 2007-06-01 21:40:03 neil Exp $

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.  This
;; is distributed in the hope that it will be useful, but without any warranty;
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose.  See the GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License along with
;; Emacs; see the file `COPYING'.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.")

;;; Commentary:

;; "bbdb2tbird.el" is an Emacs Lisp program I hacked up mostly on 2007-05-22 to
;; migrate my BBDB contact database of over 3000 entries to the Address Book of
;; Mozilla Thunderbird.

;; This was a one-shot conversion for my own personal use, and I'll not be
;; maintaining the program.  That said, I suspect the program will also do the
;; job for most other people who are migrating from VM or Gnus to Thunderbird.
;; Documentation and user interface are minimal, but I figure most people with
;; substantial BBDB databases are pretty Emacs-savvy and can figure it out.

;; Note that this program does not properly convert non-ASCII characters.
;; Support for non-ASCII characters can be added by someone with Emacs Lisp
;; experience, especially if they used Emacs 22 rather than 21.

;; The basic usage procedure is:
;;
;;   1. Load "bbdb2tbird.el" into an Emacs that's configured to use your BBDB
;;      database.
;;
;;   2. M-x bbdb2tbird RET
;;
;;   3. The "*bbdb2tbird*" buffer should now be in your window.  Write this
;;      buffer to a file name ending in ".ldif", such as "addressbook.ldif".
;;
;;   4. From the Thunderbird Address Book window, select the "Tools -> Import"
;;      menu item and follow the prompts to import the ".ldif" file you just
;;      wrote.
;;
;;   5. Once you've examined the imported address book entries and are
;;      satisfied, you can move them from the category Thunderbird created
;;      during import to the "Personal Address Book" category.

;; Warning: The Thunderbird Address Book is very limited and dumbed-down
;; compared to BBDB.

;; For purposes of future debugging, here are the versions of various involved
;; packages when I used when this program worked for me:
;;
;;   * GNU Emacs 21.4
;;   * BBDB 2.35
;;   * Icedove 1.5.0.10.dfsg1-3 (Debian packaging of a fork of Thunderbird)

;; HISTORY:
;;
;; Version 1.1 (2007-06-01): Documentation changes.
;;
;; Version 1.0 (2007-05-24): First and probably only release by Neil Van Dyke.

;;; Code:

(require 'bbdb)
(require 'cl)                           ; for "remove"

(defvar bbdb2tbird-inhibit-base64-p nil
  "This can be set to non-NIL for visually inspecting generated \"description\"
fields, etc.")

;; Quick & Dirty Utilities:

(defun bbdb2tbird-base64-encoded (str)
  (if bbdb2tbird-inhibit-base64-p
      str
    (save-excursion
      (set-buffer (generate-new-buffer "*bbdb2tbird-base64*"))
      (buffer-disable-undo)
      ;; TODO: We are losing some multibyte character info here, since in Emacs
      ;; 21 we can't set to UTF-8 before the BASE-64 encode.
      (set-buffer-multibyte t)
      (insert str)
      (set-buffer-multibyte nil)
      (base64-encode-region (point-min) (point-max) t)
      (prog1 (buffer-substring (point-min) (point-max))
        (kill-buffer (current-buffer))))))

(defun bbdb2tbird-escaped (str)
  ;; TODO: We are losing some information with the "string-make-unibyte".
  ;; Proper way to do this would be to use UTF-8.
  (if (string= str "")
      str
    (let ((str (string-make-unibyte str)))
      (save-match-data
        ;; TODO: This is overly aggressive in escaping.
        (let ((regexp           "[^ -_.A-Za-z0-9]")
              (subs             '())
              (len              (length str))
              (start            0)
              (trailing-space-p nil))
          (if (and (> len 1) (= (aref str (- len 1)) 32))
              (setq trailing-space-p t
                    str              (substring str 0 -1)
                    len              (- len 1)))
          (if (string-match "\\`[# ]" str)
              (setq subs  '("\\20")
                    start 1))
          (while (and (> len start)
                      (string-match regexp str start))
            (let ((beg (match-beginning 0))
                  (end (match-end       0)))
              (when (> beg start)
                (setq subs (cons (substring str start beg) subs)))
              (let ((chr (aref str beg)))
                (assert (< chr 256))
                (setq subs (cons (format "\\%X" chr) subs)))
              (setq start end)))
          (if trailing-space-p
              (setq subs (cons "\\20" subs)))
          (if subs
              (apply 'concat (reverse (if (> len start)
                                          (cons (substring str start len) subs)
                                        subs)))
            str))))))

(defconst bbdb2tbird-needs-escaping-rx
  (concat "\\`[# ]"
          "\\|"
          "[,+\"\\<>\;]"
          "\\|"
          " \\'")
  "Per RFC 2253 sec. 2.4.")

(defun bbdb2tbird-value (str)
  (if (and str (string-match bbdb2tbird-needs-escaping-rx str))
      (bbdb2tbird-escaped str)
    str))

(defun bbdb2tbird-fooremove-fun (keys foolist &optional func)
  (let ((rest  foolist)
        (found nil))
    (while (and (not found) rest)
      (let ((obj (car rest)))
        (assert (vectorp obj))
        (let ((label (aref obj 0)))
          (if (member (downcase label) keys)
              (setq found obj)
            (setq rest (cdr rest))))))
    (if found
        (values (if func (funcall func found) found)
                (remove found foolist))
      (values nil foolist))))

(defmacro bbdb2tbird-fooremove (keys foolist &optional func)
  (assert (symbolp foolist))
  (let ((val      (gensym))
        (new-list (gensym)))
    `(multiple-value-bind (,val ,new-list)
         (bbdb2tbird-fooremove-fun ,keys ,foolist ,func)
       (prog1 ,val
         (setq ,foolist ,new-list)))))

(defun bbdb2tbird-alistremove-fun (keys alist &optional func)
  (let ((rest  alist)
        (found nil))
    (while (and (not found) rest)
      (let ((obj (car rest)))
        (assert (consp obj))
        (let ((label (car obj)))
          (if (member label keys)
              (setq found obj)
            (setq rest (cdr rest))))))
    (if found
        (values (if func (funcall func found) (cdr found))
                (remove found alist))
      (values nil alist))))

(defmacro bbdb2tbird-alistremove (keys alist &optional func)
  (assert (symbolp alist))
  (let ((val      (gensym))
        (new-list (gensym)))
    `(multiple-value-bind (,val ,new-list)
         (bbdb2tbird-alistremove-fun ,keys ,alist ,func)
       (prog1 ,val
         (setq ,alist ,new-list)))))

(defmacro bbdb2tbird-popany (alist)
  (assert (symbolp alist))
  `(if ,alist
       (prog1 (car ,alist)
         (setq ,alist (cdr ,alist)))
     nil))

;; Main Stuff:

(defun bbdb2tbird ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*bbdb2tbird*"))
  ;; TODO: We have to disable multibyte, since we can't necessarily match the
  ;; expected character encoding otherwise.
  (set-buffer-multibyte nil)
  (buffer-disable-undo)
  (erase-buffer)
  (mapcar 'bbdb2tbird-insert-record (bbdb-records))
  (goto-char (point-min))
  (buffer-disable-undo)
  (set-buffer-modified-p nil))

(defun bbdb2tbird-insert (attr val &optional already-escaped-p)
  ;; TODO: Possibly use BASE-64 encoding in circumstances in addition to when
  ;; the value has a newline.
  (cond
   ((not val)                   nil)
   (already-escaped-p           (insert attr ": " val "\n"))
   ((string-match "[\n\r]" val) (insert attr ":: "
                                        (bbdb2tbird-base64-encoded val)
                                        "\n"))
   (t                           (insert attr ": "
                                        (bbdb2tbird-value val)
                                        "\n"))))

(defun bbdb2tbird-insert-address (address n-street1 n-street2 n-city n-state
                                          n-zip n-country)
  (if address
      (progn
        (let ((streets (bbdb-address-streets address)))
          (if streets
              (progn (bbdb2tbird-insert n-street1 (car streets))
                     (if (cdr streets)
                         (bbdb2tbird-insert n-street2 (mapconcat 'identity
                                                                 (cdr streets)
                                                                 ", "))))))
        (bbdb2tbird-insert n-city    (bbdb-address-city    address))
        (bbdb2tbird-insert n-state   (bbdb-address-state   address))
        (bbdb2tbird-insert n-zip     (bbdb-address-zip     address))

        (bbdb2tbird-insert n-country
                           (let ((country (bbdb-address-country
                                           address)))
                             (cond
                              ((string= country "Emacs") nil)
                              ((string= country "")      nil)
                              (t                         country)))))))

(defun bbdb2tbird-address-as-string (address)
  (mapconcat 'identity
             (remove nil
                     (remove ""
                             (append (bbdb-address-streets address)
                                     (list (bbdb-address-city    address)
                                           (bbdb-address-state   address)
                                           (bbdb-address-zip     address)
                                           (bbdb-address-country address)))))
             ", "))

(defun bbdb2tbird-insert-record (rec)

  (let* ((unhandled '())
         ;;
         (cn           (bbdb-record-name rec))
         (given-name   (bbdb-record-firstname rec))
         (sn           (bbdb-record-lastname rec))
         (mail         nil)
         (second-email nil))

    ;; Assign email variables
    (let ((net (mapcar 'downcase (bbdb-record-net rec))))
      (setq mail (bbdb2tbird-popany net))
      (setq second-email (bbdb2tbird-popany net))
      (if net
          (setq unhandled (cons (cons "Other Email Addresses"
                                      (mapconcat 'identity
                                                 net
                                                 ", "))
                                unhandled))))

    (if (not (or cn mail))
        (message "Skipping record with neither \"cn\" nor \"mail\".")

      ;; dn: cn=MyFirstName MyLastName,mail=MyEmailAddress
      (bbdb2tbird-insert "dn"
                         (concat "cn="
                                 (bbdb2tbird-value cn)
                                 ",mail="
                                 (bbdb2tbird-value mail))
                         t)

      ;; objectclass: top
      ;; objectclass: person
      ;; objectclass: organizationalPerson
      ;; objectclass: inetOrgPerson
      ;; objectclass: mozillaAbPersonAlpha
      (bbdb2tbird-insert "objectclass" "top")
      (bbdb2tbird-insert "objectclass" "person")
      (bbdb2tbird-insert "objectclass" "organizationalPerson")
      (bbdb2tbird-insert "objectclass" "inetOrgPerson")
      (bbdb2tbird-insert "objectclass" "mozillaAbPersonAlpha")

      ;; givenName: MyFirstName
      (bbdb2tbird-insert "givenName" given-name)

      ;; sn: MyLastName
      (bbdb2tbird-insert "sn" sn)

      ;; cn: MyFirstName MyLastName
      (bbdb2tbird-insert "cn" cn)

      ;; mozillaNickname: MyNickName

      ;; mail: MyEmailAddress
      (bbdb2tbird-insert "mail" mail)

      ;; mozillaSecondEmail: MyAdditionalEmailAddress
      (bbdb2tbird-insert "mozillaSecondEmail" second-email)

      (let (
            ;;
            (phones     (bbdb-record-phones rec))
            (addresses  (bbdb-record-addresses rec))
            ;;
            (raw-notes  (bbdb-record-raw-notes rec))
            (free-notes nil)
            ;;
            )

        (if (stringp raw-notes)
            (progn (setq free-notes raw-notes)
                   (setq raw-notes  nil))
          (setq free-notes (bbdb2tbird-alistremove '(notes) raw-notes)))

        ;; nsAIMid: MyScreenName
        (bbdb2tbird-insert "nsAIMid"
                           (bbdb2tbird-alistremove '(aim AIM) raw-notes))

        ;; modifytimestamp: 0Z

        ;; telephoneNumber: MyWorkPhone
        (bbdb2tbird-insert "telephoneNumber"
                           (bbdb2tbird-fooremove '("office" "work")
                                                 phones
                                                 'bbdb-phone-string))

        ;; homePhone: MyHomePhone
        (bbdb2tbird-insert "homePhone"
                           (bbdb2tbird-fooremove '("home")
                                                 phones
                                                 'bbdb-phone-string))

        ;; fax: MyFax
        (bbdb2tbird-insert "fax"
                           (bbdb2tbird-fooremove '("fax")
                                                 phones
                                                 'bbdb-phone-string))

        ;; pager: MyPager
        (bbdb2tbird-insert "pager"
                           (bbdb2tbird-fooremove '("pager")
                                                 phones
                                                 'bbdb-phone-string))

        ;; mobile: MyMobile
        (bbdb2tbird-insert "mobile"
                           (bbdb2tbird-fooremove '("mobile" "cell")
                                                 phones
                                                 'bbdb-phone-string))

        ;; Note: the names "homeStreet" and "mozillaHomeStreet2", as exported
        ;; Icedove 1.5.0.10 (200070329), are inconsistent.

        ;; homeStreet: MyHomeLine1
        ;; mozillaHomeStreet2: MyHomeLine2
        ;; mozillaHomeLocalityName: MyHomeCity
        ;; mozillaHomeState: XX
        ;; mozillaHomePostalCode: 12345
        ;; mozillaHomeCountryName: MyHomeCountry

        (bbdb2tbird-insert-address (bbdb2tbird-fooremove '("home") addresses)
                                   "homeStreet"
                                   "mozillaHomeStreet2"
                                   "mozillaHomeLocalityName"
                                   "mozillaHomeState"
                                   "mozillaHomePostalCode"
                                   "mozillaHomeCountryName")

        ;; street: MyWorkLine1
        ;; mozillaWorkStreet2: MyWorkLine2
        ;; l: MyWorkCity
        ;; st: XX
        ;; postalCode: 12345
        ;; c: MyWorkCountry
        (bbdb2tbird-insert-address (bbdb2tbird-fooremove '("office" "work")
                                                         addresses)
                                   "street"
                                   "mozillaWorkStreet2"
                                   "l"
                                   "st"
                                   "postalCode"
                                   "c")

        ;; title: MyTitle
        (bbdb2tbird-insert "title"
                           (bbdb2tbird-alistremove '(title Title) raw-notes))

        ;; department: MyDepartment
        (bbdb2tbird-insert "department"
                           (bbdb2tbird-alistremove '(department
                                                     Department
                                                     dept
                                                     Dept
                                                     group
                                                     Group)
                                                   raw-notes))

        ;; company: MyOrganization
        (bbdb2tbird-insert "company" (bbdb-record-company rec))

        ;; mozillaWorkUrl: http://MyWorkWebPage

        ;; mozillaHomeUrl: http://MyHomeWebPage
        (bbdb2tbird-insert "mozillaHomeUrl"
                           (bbdb2tbird-alistremove
                            '(web Web www WWW homepage Homepage HomePage)
                            raw-notes))

        ;; mozillaCustom1: MyCustom1
        ;; mozillaCustom2: MyCustom2
        ;; mozillaCustom3: MyCustom3
        ;; mozillaCustom4: MyCustom4

        ;; Add BBDB "AKA" to unhandled.
        (let ((aka (bbdb-record-aka rec)))
          (if aka
              (setq unhandled (cons (cons "AKA"

                                          (if (stringp aka)
                                              aka
                                            (mapconcat 'identity aka ", ")))
                                    unhandled))))

        ;; Add unused phone numbers to unhandled.
        (mapcar (function
                 (lambda (phone)
                   (setq unhandled
                         (cons (cons (format "Other Phone \"%s\""
                                             (aref phone 0))
                                     (bbdb-phone-string phone))
                               unhandled))))
                phones)

        ;; Add unused addresses to unhandled.
        (mapcar
         (function
          (lambda (address)
            (setq unhandled
                  (cons (cons (format "Other Address \"%s\""
                                      (bbdb-address-location address))
                              (bbdb2tbird-address-as-string address))
                        unhandled))))
         addresses)

        ;; Process remaining raw-notes.
        (let ((mapping '((birthdate     . "Birthday")
                         (birthday      . "Birthday")
                         (creation-date . nil)
                         (irc           . "IRC")
                         (model-stats   . "Model Stats")
                         (old_net       . "Old Email Addresses")
                         (palm          . nil)
                         (timestamp     . nil)
                         (yahoo         . "Yahoo"))))
          (mapcar
           (function
            (lambda (note)
              (let* ((a     (car note))
                     (v     (cdr note))
                     (cell  (assoc a mapping))
                     (label (if cell
                                (cdr cell)
                              (format "%s" a))))
                (if label
                    (setq unhandled
                          (cons (cons label v) unhandled))))))
           raw-notes))

        ;; TODO: What about moving "mail-alias" to the "Nickname"?

        ;; description:: TXlOb3Rlc0xpbmUxCk15Tm90ZXNMaW5lMg==
        (bbdb2tbird-insert
         "description"
         (let ((lines (mapcar (function (lambda (x)
                                          (concat (car x) ": " (cdr x))))
                              (reverse unhandled))))
           (if free-notes (setq lines (cons free-notes lines)))
           (if lines
               (mapconcat 'identity lines "\n")))))

      (insert "\n"))))

(provide 'bbdb2tbird)

;;; bbdb2tbird.el ends here
