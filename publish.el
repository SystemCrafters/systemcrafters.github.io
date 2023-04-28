;;; publish.el --- Build systemcrafters.net

;; Copyright (C) 2021, 2023 David Wilson <david@systemcrafters.net>

;; Author: David Wilson <david@systemcrafters.net>
;; Maintainer: David Wilson <david@systemcrafters.net>
;; URL: https://codeberg.org/SystemCrafters/systemcrafters.net
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.2"))
;; Keywords: hypermedia, blog, feed, rss

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Docs License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Docs License for more details.
;;
;; You should have received a copy of the GNU General Docs License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;; emacs -Q --batch -l ./publish.el --funcall dw/publish

;;; Code:

;; Initialize package sources
(require 'package)

;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(setq package-user-dir (expand-file-name "./.packages"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Require built-in dependencies
(require 'vc-git)
(require 'ox-publish)
(require 'subr-x)

;; Unfortunately this is necessary for now...
(load-file "./ox-slimhtml.el")

;; Install other dependencies
(use-package esxml
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package webfeeder
  :ensure t)

(defvar yt-iframe-format
  ;; TODO: Change this after switching from Bootstrap
  (concat "<div class=\"embed-responsive embed-responsive-16by9\">"
          " <iframe class=\"embed-responsive-item\" src=\"https://www.youtube.com/embed/%s\" allowfullscreen></iframe>"
          " </div>"))

(setq user-full-name "David Wilson")
(setq user-mail-address "david@systemcrafters.net")

(defvar dw/site-url (if (string-equal (getenv "CI") "true")
                        "https://systemcrafters.codeberg.page/systemcrafters.net"
                        ;"https://systemcrafters.net"
                      "http://localhost:8080")
  "The URL for the site being generated.")

(org-link-set-parameters
 "yt"
 :follow
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/watch?v="
            handle)))
 :export
 (lambda (path desc backend channel)
   (when (eq backend 'html)
     (format yt-iframe-format
             path (or desc "")))))

(defun dw/site-header (info)
  (list '(header (@ (class "blog-header"))
                 (div (@ (class "container"))
                      (div (@ (class "blog-title"))
                           (img (@ (class "logo") (src "/img/sc_logo.png") (alt "System Crafters")))))

                 (div (@ (class "blog-masthead"))
                      (div (@ (class "container"))
                           (nav (@ (class "nav"))
                                (a (@ (class "nav-link") (href "/")) "Home") " "
                                (a (@ (class "nav-link") (href "/start-here")) "Start Here!") " "
                                (a (@ (class "nav-link") (href "/guides")) "Guides") " "
                                (a (@ (class "nav-link") (href "/news")) "News") " "
                                (a (@ (class "nav-link") (href "https://store.systemcrafters.net?utm_source=sc-site-nav")) "Store") " "
                                (a (@ (class "nav-link") (href "/support-the-channel")) "Support Us")))))))

(defun dw/site-footer (info)
  (list `(footer (@ (class "blog-footer"))
                 (div (@ (class "container"))
                      (div (@ (class "row"))
                           (div (@ (class "column"))
                                (p (a (@ (href ,(concat dw/site-url "/privacy-policy/"))) "Privacy Policy")
                                   " · "
                                   (a (@ (href ,(concat dw/site-url "/credits/"))) "Credits"))
                                (p "© 2021-2023 System Crafters LLC"))
                           (div (@ (class "column align-right"))
                                (p (a (@ (href "https://codeberg.org/SystemCrafters/systemcrafters.net"))
                                      (img (@ (src "/img/codeberg.png")
                                              (style "width: 120px")
                                              (alt "Contribute on Codeberg")))))))))))

(defun get-article-output-path (org-file pub-dir)
  (let ((article-dir (concat pub-dir
                             (downcase
                              (file-name-as-directory
                               (file-name-sans-extension
                                (file-name-nondirectory org-file)))))))

    (if (string-match "\\/index.org$" org-file)
        pub-dir
        (progn
          (unless (file-directory-p article-dir)
            (make-directory article-dir t))
          article-dir))))

(defun dw/get-commit-hash ()
  "Get the short hash of the latest commit in the current repository."
  (string-trim-right
   (with-output-to-string
    (with-current-buffer standard-output
      (vc-git-command t nil nil "rev-parse" "--short" "HEAD")))))

(defun dw/org-html-template (contents info)
  (concat
   "<!-- Generated from " (dw/get-commit-hash)  " on " (format-time-string "%Y-%m-%d @ %H:%M") " with " (plist-get info :creator) " -->\n"
   "<!DOCTYPE html>"
   (sxml-to-xml
    `(html (@ (lang "en"))
           (head
            (meta (@ (charset "utf-8")))
            (meta (@ (author "System Crafters - David Wilson")))
            (meta (@ (name "viewport")
                     (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
            (link (@ (rel "stylesheet") (href "/fonts/iosevka-aile/iosevka-aile.css")))
            (link (@ (rel "stylesheet") (href "/fonts/jetbrains-mono/jetbrains-mono.css")))
            (link (@ (rel "stylesheet") (href "/css/code.css")))
            (link (@ (rel "stylesheet") (href "/css/site.css")))
            (script (@ (defer "defer")
                       (data-domain "systemcrafters.net")
                       (src "https://plausible.io/js/plausible.js"))
                    ;; Empty string to cause a closing </script> tag
                    "")
            (title ,(concat (org-export-data (plist-get info :title) info) " - System Crafters")))
           (body ,@(dw/site-header info)
                 (div (@ (class "container"))
                      (div (@ (class "blog-post"))
                           (h1 (@ (class "blog-post-title"))
                               ,(org-export-data (plist-get info :title) info))
                           (p (@ (class "blog-post-meta"))
                              ,(org-export-data (org-export-get-date info "%B %e, %Y") info))
                           ,contents))
                 ,@(dw/site-footer info))))))

;; Thanks Ashraz!
(defun dw/org-html-link (link contents info)
  "Removes file extension and changes the path into lowercase file:// links."
  (when (string= 'file (org-element-property :type link))
    (org-element-put-property link :path
                              (downcase
                               (file-name-sans-extension
                                (org-element-property :path link)))))

  (let ((exported-link (org-export-custom-protocol-maybe link contents 'html info)))
    (cond
     (exported-link exported-link)
     ((equal contents nil)
      (format "<a href=\"%s\">%s</a>"
              (org-element-property :raw-link link)
              (org-element-property :raw-link link)))
     (t (org-export-with-backend 'slimhtml link contents info)))))

(defun dw/make-heading-anchor-name (headline-text)
  (thread-last headline-text
    (downcase)
    (replace-regexp-in-string " " "-")
    (replace-regexp-in-string "[^[:alnum:]_-]" "")))

(defun dw/org-html-headline (headline contents info)
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (level (org-export-get-relative-level headline info))
         (level (min 7 (when level (1+ level))))
         (anchor-name (dw/make-heading-anchor-name text))
         (attributes (org-element-property :ATTR_HTML headline))
         (container (org-element-property :HTML_CONTAINER headline))
         (container-class (and container (org-element-property :HTML_CONTAINER_CLASS headline))))
    (when attributes
      (setq attributes
            (format " %s" (org-html--make-attribute-string
                           (org-export-read-attribute 'attr_html `(nil
                                                                   (attr_html ,(split-string attributes))))))))
    (concat
     (when (and container (not (string= "" container)))
       (format "<%s%s>" container (if container-class (format " class=\"%s\"" container-class) "")))
     (if (not (org-export-low-level-p headline info))
         (format "<h%d%s><a id=\"%s\" class=\"anchor\" href=\"#%s\">¶</a>%s</h%d>%s"
                 level
                 (or attributes "")
                 anchor-name
                 anchor-name
                 text
                 level
                 (or contents ""))
       (concat
        (when (org-export-first-sibling-p headline info) "<ul>")
        (format "<li>%s%s</li>" text (or contents ""))
        (when (org-export-last-sibling-p headline info) "</ul>")))
     (when (and container (not (string= "" container)))
       (format "</%s>" (cl-subseq container 0 (cl-search " " container)))))))

(org-export-define-derived-backend 'site-html
    'slimhtml
  :translate-alist
  '((template . dw/org-html-template)
    (link . dw/org-html-link)
    (code . ox-slimhtml-verbatim)
    (headline . dw/org-html-headline))
  :options-alist
  '((:page-type "PAGE-TYPE" nil nil t)
    (:html-use-infojs nil nil nil)))

(defun org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, using the FILENAME as the output directory."
  (let ((article-path (get-article-output-path filename pub-dir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 (concat article-path "index" extension))))
      (org-publish-org-to 'site-html
                          filename
                          (concat "." (or (plist-get plist :html-extension)
                                          "html"))
                          plist
                          article-path))))

(defun dw/sitemap-entry (entry style project)
  (format "<h4><em>%s</em> - <a href=\"%s\">%s</a></h4>"
          (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
          (concat (file-name-sans-extension entry) "/")
          (org-publish-find-title entry project)))

(defun dw/generate-sitemap (title list)
  (concat "#+TITLE: " title "\n\n"
          "#+BEGIN_EXPORT html\n"
          (mapconcat (lambda (item)
                       (car item))
                     (cdr list)
                     "\n")
          "\n#+END_EXPORT\n"))
(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./.org-cache/"
      org-export-with-section-numbers nil
      org-export-use-babel nil
      org-export-with-smart-quotes t
      org-export-with-sub-superscripts nil
      org-export-with-tags 'not-in-toc
      org-html-htmlize-output-type 'css
      ;; org-html-metadata-timestamp-format "%Y-%m-%d"
      ;; org-html-checkbox-type 'site-html
      ;; org-html-html5-fancy nil
      ;; org-html-self-link-headlines t
      ;; org-html-validation-link nil
      ;; org-html-doctype "html5"
      org-export-with-toc t
      make-backup-files nil)

(setq org-html-link-home dw/site-url
      org-html-link-use-abs-url t
      org-html-link-org-files-as-html t)

(defun dw/video-series-config (series-path)
  (list (format "systemcrafters:%s" series-path)
        :base-directory (format "./content/%s" series-path)
        :base-extension "org"
        :publishing-directory (format "./public/%s" series-path)
        :publishing-function 'org-html-publish-to-html
        :with-title nil
        :with-timestamps nil))

(defun dw/format-live-stream-entry (entry style project)
  "Format posts with author and published data in the index page."
  (cond ((not (directory-name-p entry))
         (format "*[[file:%s][%s]]*
                 #+HTML: <p class='pubdate'>by %s on %s.</p>"
                 entry
                 (org-publish-find-title entry project)
                 (car (org-publish-find-property entry :author project))
                 (format-time-string "%b %d, %Y"
                                     (org-publish-find-date entry project))))
        ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(setq org-publish-project-alist
      (list '("systemcrafters:main"
              :base-directory "./content"
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :with-title nil
              :with-timestamps nil)
            '("systemcrafters:assets"
              :base-directory "./assets"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|woff2\\|ttf"
              :publishing-directory "./public"
              :recursive t
              :publishing-function org-publish-attachment)
            ;; '("systemcrafters:live-streams"
            ;;   :base-directory "./content/live-streams"
            ;;   :base-extension "org"
            ;;   :publishing-directory "./public/live-streams"
            ;;   :publishing-function org-html-publish-to-html
            ;;   :auto-sitemap t
            ;;   :sitemap-filename "index.org"
            ;;   :sitemap-title "Live Streams"
            ;;   :sitemap-format-entry dw/format-live-stream-entry
            ;;   :sitemap-style list
            ;;   :sitemap-sort-files anti-chronologically
            ;;   :with-title nil
            ;;   :with-timestamps nil)
            (dw/video-series-config "advanced-package-management")
            ;; (dw/video-series-config "build-a-second-brain-in-emacs")
            ;; (dw/video-series-config "chatting-with-emacs")
            ;; (dw/video-series-config "craft-your-system-with-guix")
            ;; (dw/video-series-config "effective-emacs-workflow")
            ;; (dw/video-series-config "emacs-essentials")
            ;; (dw/video-series-config "emacs-from-scratch")
            ;; (dw/video-series-config "emacs-ide")
            ;; (dw/video-series-config "emacs-mail")
            ;; (dw/video-series-config "emacs-shorts")
            ;; (dw/video-series-config "emacs-tips")
            ;; (dw/video-series-config "learning-emacs-lisp")
            ;; (dw/video-series-config "managing-your-dotfiles")
            ;; (dw/video-series-config "mastering-git-with-magit")
            ;; (dw/video-series-config "publishing-websites-with-org-mode")
            ))

(defun dw/publish ()
  "Publish the entire site."
  (interactive)
  (org-publish-all (string-equal (or (getenv "FORCE")
                                     (getenv "CI"))
                                 "true")))

            ;;  :rss-extension "xml"
            ;;  :rss-image-url "http://example.com/logo.png"
            ;;  :rss-exclude-tags ("noexport")
            ;;  :html-link-home "/"
            ;;  :html-link-use-abs-url t
            ;;  :html-link-org-files-as-html t)

            ;; ("rss"
            ;;  :base-directory ,input-dir
            ;;  :base-extension "org"
            ;;  :publishing-directory ,output-dir
            ;;  :publishing-function org-rss-publish-to-rss
            ;;  :rss-extension "xml"
            ;;  :html-link-home "https://systemcrafters.net/"
            ;;  :html-link-use-abs-url t
            ;;  :exclude "articles.html"
            ;;  :recursive t))))

(defun org-html-htmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-html-htmlize-output-type' to `css', calls
to the function `org-html-htmlize-region-for-paste' will
produce code that uses these same face definitions."
  (interactive)
  (unless (require 'htmlize nil t)
    (error "htmlize library missing.  Aborting"))
  (and (get-buffer "*html*") (kill-buffer "*html*"))
  (with-temp-buffer
    (let ((fl (nreverse (face-list)))
	  (htmlize-css-name-prefix "org-")
	  (htmlize-output-type 'css)
	  f i)
      (while (setq f (pop fl)
		   i (and f (face-attribute f :inherit)))
	(when (and (symbolp f) (or (not i) (not (listp i))))
	  (insert (org-add-props (copy-sequence "1") nil 'face f))))
      (htmlize-region (point-min) (point-max))))
  (pop-to-buffer-same-window "*html*")
  (goto-char (point-min))
  (when (re-search-forward "<style" nil t)
    (delete-region (point-min) (match-beginning 0)))
  (when (re-search-forward "</style>" nil t)
    (delete-region (1+ (match-end 0)) (point-max)))
  (beginning-of-line 1)
  (when (looking-at " +") (replace-match ""))
  (goto-char (point-min)))


(provide 'publish)
;;; publish.el ends here
