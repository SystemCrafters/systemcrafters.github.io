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
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

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
(require 'cl-lib)

;; Install other dependencies
(use-package esxml
  :pin "melpa-stable"
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package webfeeder
  :ensure t)

(defvar yt-iframe-format
  (concat "<div class=\"video\">"
          "  <iframe src=\"https://www.youtube.com/embed/%s\" allowfullscreen></iframe>"
          "</div>"))

(defun dw/embed-video (video-id)
  (format yt-iframe-format video-id))

(setq user-full-name "David Wilson")
(setq user-mail-address "david@systemcrafters.net")

(defvar dw/site-url (if (string-equal (getenv "CI") "true")
                        "https://systemcrafters.net"
                      "http://localhost:8080")
  "The URL for the site being generated.")

(defun dw/embed-list-form ()
  `(div (@ (class "list-form center"))
        (div (@ (class "list-form-title")) "Subscribe to the System Crafters Newsletter!")
        (form (@ (method "POST")
                 (action "https://www.simplelists.com/subscribe.php"))
              (input (@ (type "hidden") (name "format") (value "text")))
              (input (@ (type "hidden") (name "action") (value "subscribe")))
              (input (@ (type "hidden") (name "list") (value "news@lists.systemcrafters.net")))
              (div (@ (class "list-form-message"))
                   "Stay up to date with the latest System Crafters news and updates!  Read the "
                   (a (@ (href "/newsletter/")) "Newsletter")
                   " page for more information.")
              (div (@ (class "row"))
                   (div (@ (class "column"))
                        (div (@ (class "row center list-form-label")) "Name (optional)")
                        (div (@ (class "row")) (input (@ (type "text") (name "name")))))
                   (div (@ (class "column"))
                        (div (@ (class "row center list-form-label")) "Email Address")
                        (div (@ (class "row")) (input (@ (type "text") (name "email"))))))
              (div nil
                   (input (@ (type "submit") (value "Subscribe!")))))))

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
     (dw/embed-video path))))

(defun dw/site-header ()
  (list `(header (@ (class "site-header"))
                 (div (@ (class "container"))
                      (div (@ (class "site-title"))
                           (img (@ (class "logo")
                                   (src ,(concat dw/site-url "/img/sc_logo.png"))
                                   (alt "System Crafters")))))
                 (div (@ (class "site-masthead"))
                      (div (@ (class "container"))
                           (nav (@ (class "nav"))
                                (a (@ (class "nav-link") (href "/")) "Home") " "
                                (a (@ (class "nav-link") (href "/guides/")) "Guides") " "
                                (a (@ (class "nav-link") (href "/news/")) "News") " "
                                (a (@ (class "nav-link") (href "/community/")) "Community") " "
                                (a (@ (class "nav-link") (href "https://store.systemcrafters.net?utm_source=sc-site-nav")) "Store") " "
                                (a (@ (class "nav-link") (href "/how-to-help/")) "How to Help")))))))

(defun dw/site-footer ()
  (list `(footer (@ (class "site-footer"))
                 (div (@ (class "container"))
                      (div (@ (class "row"))
                           (div (@ (class "column"))
                                (p (a (@ (href ,(concat dw/site-url "/privacy-policy/"))) "Privacy Policy")
                                   " · "
                                   (a (@ (href ,(concat dw/site-url "/credits/"))) "Credits")
                                   " · "
                                   (a (@ (href ,(concat dw/site-url "/rss/"))) "RSS Feeds")
                                   " · "
                                   (a (@ (rel "me") (href "https://fosstodon.org/@daviwil")) "Fediverse"))
                                (p "© 2021-2023 System Crafters LLC"))
                           (div (@ (class "column align-right"))
                                (p (a (@ (href "https://codeberg.org/SystemCrafters/systemcrafters.net"))
                                      (img (@ (src ,(concat dw/site-url "/img/codeberg.png"))
                                              (style "width: 120px")
                                              (alt "Contribute on Codeberg")))))))))))

(defun get-article-output-path (org-file pub-dir)
  (let ((article-dir (concat pub-dir
                             (downcase
                              (file-name-as-directory
                               (file-name-sans-extension
                                (file-name-nondirectory org-file)))))))

    (if (string-match "\\/index.org\\|\\/404.org$" org-file)
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

(cl-defun dw/generate-page (title
                            content
                            info
                            &key
                            (publish-date)
                            (head-extra)
                            (pre-content)
                            (exclude-header)
                            (exclude-footer))
  (concat
   "<!-- Generated from " (dw/get-commit-hash)  " on " (format-time-string "%Y-%m-%d @ %H:%M") " with " org-export-creator-string " -->\n"
   "<!DOCTYPE html>"
   (sxml-to-xml
    `(html (@ (lang "en"))
           (head
            (meta (@ (charset "utf-8")))
            (meta (@ (author "System Crafters - David Wilson")))
            (meta (@ (name "viewport")
                     (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
            (link (@ (rel "icon") (type "image/png") (href "/img/favicon.png")))
            (link (@ (rel "alternative")
                     (type "application/rss+xml")
                     (title "System Crafters News")
                     (href ,(concat dw/site-url "/rss/news.xml"))))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/fonts/iosevka-aile/iosevka-aile.css"))))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/fonts/jetbrains-mono/jetbrains-mono.css"))))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/css/code.css"))))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/css/site.css"))))
            (script (@ (defer "defer")
                       (data-domain "systemcrafters.net")
                       (src "https://plausible.io/js/plausible.js"))
                    ;; Empty string to cause a closing </script> tag
                    "")
            ,(when head-extra head-extra)
            (title ,(concat title " - System Crafters")))
           (body ,@(unless exclude-header
                     (dw/site-header))
                 (div (@ (class "container"))
                      (div (@ (class "site-post"))
                           (h1 (@ (class "site-post-title"))
                               ,title)
                           ,(when publish-date
                              `(p (@ (class "site-post-meta")) ,publish-date))
                           ,(if-let ((video-id (plist-get info :video)))
                                (dw/embed-video video-id))
                           ,(when pre-content pre-content)
                           (div (@ (id "content"))
                                ,content))
                      ,(dw/embed-list-form))
                 ,@(unless exclude-footer
                     (dw/site-footer)))))))

(defun dw/org-html-template (contents info)
  (dw/generate-page (org-export-data (plist-get info :title) info)
                    contents
                    info
                    :publish-date (org-export-data (org-export-get-date info "%B %e, %Y") info)))

(defun dw/org-html-link (link contents info)
  "Removes file extension and changes the path into lowercase file:// links."
  (when (and (string= 'file (org-element-property :type link))
             (string= "org" (file-name-extension (org-element-property :path link))))
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
     ((string-prefix-p "/" (org-element-property :raw-link link))
      (format "<a href=\"%s\">%s</a>"
              (org-element-property :raw-link link)
              contents))
     (t (org-export-with-backend 'html link contents info)))))

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

(defun dw/org-html-src-block (src-block _contents info)
  (let* ((lang (org-element-property :language src-block))
	       (code (org-html-format-code src-block info)))
    (format "<pre>%s</pre>" (string-trim code))))

(defun dw/org-html-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((block-type (org-element-property :type special-block))
         (attributes (org-export-read-attribute :attr_html special-block)))
	  (format "<div class=\"%s center\">\n%s\n</div>"
            block-type
            (or contents
                (if (string= block-type "cta")
                    "If you find this guide helpful, please consider supporting System Crafters via the links on the <a href=\"/how-to-help/#support-my-work\">How to Help</a> page!"
                  "")))))

(org-export-define-derived-backend 'site-html 'html
  :translate-alist
  '((template . dw/org-html-template)
    (link . dw/org-html-link)
    (src-block . dw/org-html-src-block)
    (special-block . dw/org-html-special-block)
    (headline . dw/org-html-headline))
  :options-alist
  '((:video "VIDEO" nil nil)))

(defun org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, using the FILENAME as the output directory."
  (let ((article-path (get-article-output-path filename pub-dir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 ;; The 404 page is a special case, it must be named "404.html"
                 (concat article-path
                         (if (string= (file-name-nondirectory filename) "404.org") "404" "index")
                         extension))))
      (org-publish-org-to 'site-html
                          filename
                          (concat "." (or (plist-get plist :html-extension)
                                          "html"))
                          plist
                          article-path))))

(defun dw/publish-newsletter-page (plist filename pub-dir)
  "Publish a newsletter .txt file to a simple HTML page."
  (let* ((issue-name (file-name-sans-extension
                      (file-name-nondirectory filename)))
         (output-file (expand-file-name
                       (concat issue-name ".html")
                       pub-dir))
         (contents (with-temp-buffer
                     (insert-file-contents filename)
                     (buffer-string))))
    (with-temp-file output-file
      (insert
       (dw/generate-page
        (concat "Issue "
                (nth 2 (split-string issue-name "-")))
        (format "<pre class=\"newsletter-text\">%s</pre>"
                (replace-regexp-in-string
                 "\\(http\\|https\\)://[^ \t\n\r<>\"']*[^ \t\n\r<>\".,;!?']"
                 (lambda (match)
                   (format "<a href=\"%s\">%s</a>" match match))
                 contents))
        '()
        :exclude-header t
        :exclude-footer t)))))

(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./.org-cache/"
      org-export-with-section-numbers nil
      org-export-use-babel nil
      org-export-with-smart-quotes t
      org-export-with-sub-superscripts nil
      org-export-with-tags 'not-in-toc
      org-html-htmlize-output-type 'css
      org-html-prefer-user-labels t
      org-html-link-home dw/site-url
      org-html-link-use-abs-url t
      org-html-link-org-files-as-html t
      org-html-html5-fancy t
      org-html-self-link-headlines t
      org-export-with-toc nil
      make-backup-files nil)

(defun dw/format-live-stream-entry (entry style project)
  "Format posts with author and published data in the index page."
  (cond ((not (directory-name-p entry))
         (format "[[file:%s][%s]] - %s"
                 entry
                 (org-publish-find-title entry project)
                 (format-time-string "%B %d, %Y"
                                     (org-publish-find-date entry project))))
        ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun dw/format-news-entry (entry style project)
  "Format posts with author and published data in the index page."
  (cond ((not (directory-name-p entry))
         (format "[[file:%s][%s]] - %s · %s"
                 entry
                 (org-publish-find-title entry project)
                 (car (org-publish-find-property entry :author project))
                 (format-time-string "%B %d, %Y"
                                     (org-publish-find-date entry project))))
        ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun dw/news-sitemap (title files)
  (format "#+title: %s\n\n%s"
          title
          (mapconcat (lambda (file)
                       (format "- %s\n" file))
                     (cadr files)
                     "\n")))

(defun dw/rss-extract-title (html-file)
  "Extract the title from an HTML file."
  (with-temp-buffer
    (insert-file-contents html-file)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (dom-text (car (dom-by-class dom "site-post-title"))))))

(defun dw/rss-extract-date (html-file)
  "Extract the post date from an HTML file."
  (with-temp-buffer
    (insert-file-contents html-file)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (date-string (dom-text (car (dom-by-class dom "site-post-meta"))))
           (parsed-date (parse-time-string date-string))
           (day (nth 3 parsed-date))
           (month (nth 4 parsed-date))
           (year (nth 5 parsed-date)))
      ;; NOTE: Hardcoding this at 8am for now
      (encode-time 0 0 8 day month year))))

;(defun dw/rss-extract-summary (html-file)
;  )

(setq webfeeder-title-function #'dw/rss-extract-title
      webfeeder-date-function #'dw/rss-extract-date)

(setq org-publish-project-alist
      (list '("systemcrafters:main"
              :base-directory "./content"
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :with-title nil
              :with-timestamps nil)
            '("systemcrafters:faq"
              :base-directory "./content/faq"
              :base-extension "org"
              :publishing-directory "./public/faq"
              :publishing-function org-html-publish-to-html
              :with-title nil
              :with-timestamps nil)
            '("systemcrafters:assets"
              :base-directory "./assets"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|woff2\\|ttf"
              :publishing-directory "./public"
              :recursive t
              :publishing-function org-publish-attachment)
            '("systemcrafters:live-streams"
              :base-directory "./content/live-streams"
              :base-extension "org"
              :publishing-directory "./public/live-streams"
              :publishing-function org-html-publish-to-html
              :auto-sitemap t
              :sitemap-filename "../live-streams.org"
              :sitemap-title "Live Streams"
              :sitemap-format-entry dw/format-live-stream-entry
              :sitemap-style list
              :sitemap-sort-files anti-chronologically
              :with-title nil
              :with-timestamps nil)
            '("systemcrafters:news"
              :base-directory "./content/news"
              :base-extension "org"
              :publishing-directory "./public/news"
              :publishing-function org-html-publish-to-html
              :auto-sitemap t
              :sitemap-filename "../news.org"
              :sitemap-title "System Crafters News"
              :sitemap-format-entry dw/format-news-entry
              :sitemap-style list
              ;; :sitemap-function dw/news-sitemap
              :sitemap-sort-files anti-chronologically
              :with-title nil
              :with-timestamps nil)
            '("systemcrafters:newsletter"
              :base-directory "./content/newsletter"
              :base-extension "txt"
              :publishing-directory "./public/newsletter"
              :publishing-function dw/publish-newsletter-page)
            '("systemcrafters:videos"
              :base-directory "./content/videos"
              :base-extension "org"
              :recursive t
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :with-title nil
              :with-timestamps nil)))

;; TODO: Generate a _redirects file instead once Codeberg Pages releases a new version
(defun dw/generate-redirects (redirects)
  (dolist (redirect redirects)
    (let ((output-path (concat "./public/" (car redirect) "/index.html"))
          (redirect-url (concat dw/site-url "/" (cdr redirect) "/")))
      (make-directory (file-name-directory output-path) t)
      (with-temp-file output-path
        (insert
         (dw/generate-page "Redirecting..."
                           (concat "You are being redirected to "
                                   "<a href=\"" redirect-url "\">" redirect-url "</a>")
                           '()
                           :head-extra
                           (concat "<meta http-equiv=\"refresh\" content=\"0; url='" redirect-url "'\"/>")))))))

(defun dw/publish ()
  "Publish the entire site."
  (interactive)
  (org-publish-all (string-equal (or (getenv "FORCE")
                                     (getenv "CI"))
                                 "true"))

  (webfeeder-build "rss/news.xml"
                   "./public"
                   dw/site-url
                   (let ((default-directory (expand-file-name "./public/")))
                     (remove "news/index.html"
                             (directory-files-recursively "news"
                                                          ".*\\.html$")))
                   :builder 'webfeeder-make-rss
                   :title "System Crafters News"
                   :description "News and Insights from System Crafters!"
                   :author "David Wilson")

  (dw/generate-redirects '(("support-the-channel" . "how-to-help")
                           ("videos" . "guides")))

  ;; Copy the domains file to ensure the custom domain resolves
  (copy-file ".domains" "public/.domains" t)

  ;; Copy the .well-known folder for Matrix
  (unless (file-exists-p "public/.well-known")
    (copy-directory ".well-known" "public/" t)))

(provide 'publish)
;;; publish.el ends here
