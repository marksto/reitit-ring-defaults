(ns reitit.ring.middleware.defaults
  (:refer-clojure :exclude [compile])
  (:require [reitit.coercion :as rc]
            [reitit.ring.coercion :as coercion]
            [reitit.ring.middleware.exception :as exception]
            [reitit.ring.middleware.muuntaja :as format]
            [reitit.ring.middleware.parameters :as parameters]
            [ring.middleware.absolute-redirects :refer [wrap-absolute-redirects]]
            [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.cookies :refer [wrap-cookies]]
            [ring.middleware.default-charset :refer [wrap-default-charset]]
            [ring.middleware.flash :refer [wrap-flash]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.nested-params :refer [wrap-nested-params]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [ring.middleware.proxy-headers :refer [wrap-forwarded-remote-addr]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.ssl :refer [wrap-forwarded-scheme wrap-hsts
                                         wrap-ssl-redirect]]
            [ring.middleware.x-headers :refer [wrap-content-type-options
                                               wrap-frame-options
                                               wrap-xss-protection]]))

(defn- compile
  "Returns a middleware compilation function for `wrap-fn`, only mounting
   the middleware when the `:defaults` route data is configured for this
   middleware.  `opts-fn` takes the defaults configuration map for this route,
   and shall return options to pass as a second argument to `wrap-fn`.
   A return value of `true` means that the middleware takes no options,
   and falsy options will not mount the middleware."
  [wrap-fn opts-fn]
  (fn [data _route-opts]
    (when-let [opts (opts-fn (:defaults data))]
      (if (true? opts)
        wrap-fn
        #(wrap-fn % opts)))))

(def forwarded-remote-addr-middleware
  {:name ::forwarded-remote-addr
   :compile (compile wrap-forwarded-remote-addr (comp boolean :proxy))})

(def forwarded-scheme-middleware
  {:name ::forwarded-scheme
   :compile (compile wrap-forwarded-scheme (comp boolean :proxy))})

(def ssl-redirect-middleware
  {:name ::ssl-redirect
   :compile (compile wrap-ssl-redirect #(get-in % [:security :ssl-redirect] false))})

(def hsts-middleware
  {:name ::hsts
   :compile (compile wrap-hsts #(get-in % [:security :hsts] false))})

(def content-type-options-middleware
  {:name ::content-type-options
   :compile (compile wrap-content-type-options #(get-in % [:security :content-type-options] false))})

(def frame-options-middleware
  {:name ::frame-options
   :compile (compile wrap-frame-options #(get-in % [:security :frame-options] false))})

(def xss-protection-middleware
  {:name ::xss-protection
   :compile (fn [data _]
              (let [opts (get-in data [:defaults :xss-protection])]
                (when (and opts (:enable? opts true))
                  (let [opts (dissoc opts :enable?)]
                    #(wrap-xss-protection % true opts)))))})

(def not-modified-middleware
  {:name ::not-modified
   :compile (compile wrap-not-modified #(get-in % [:responses :not-modified-responses] false))})

(def default-charset-middleware
  {:name ::default-charset
   :compile (compile wrap-default-charset #(get-in % [:responses :default-charset] false))})

(def content-type-middleware
  {:name ::content-type
   :compile (compile wrap-content-type #(get-in % [:responses :content-types] false))})

(def absolute-redirects-middleware
  {:name ::absolute-redirects
   :compile (compile wrap-absolute-redirects #(get-in % [:responses :absolute-redirects] false))})

(def cookies-middleware
  {:name ::cookies
   :compile (compile wrap-cookies #(get-in % [:cookies] false))})

(def params-middleware
  {:name ::params
   :compile (fn [data _]
              (when (get-in data [:defaults :params :urlencoded])
                parameters/parameters-middleware))})

(def multipart-parsing-middleware
  "Parses multipart params into `:multipart-params` and `:params`, like
   the stock `ring.middleware.multipart-params/wrap-multipart-params`.

   Parsing only: coercion into `[:parameters :multipart]` is done by the
   `multipart-coercion-middleware`, mounted after request coercion.

   Splitting into separate parsing and coercion steps also lets coercion
   errors surface inside `coercion/coerce-exceptions-middleware` as 400s
   instead of escaping the chain as 500s."
  {:name ::multipart-parsing
   :compile (fn [data _]
              (when-let [opts (get-in data [:defaults :params :multipart])]
                (if (true? opts)
                  wrap-multipart-params
                  #(wrap-multipart-params % opts))))})

(def nested-params-middleware
  {:name ::nested-params
   :compile (compile wrap-nested-params #(get-in % [:params :nested] false))})

(def keyword-params-middleware
  {:name ::keyword-params
   :compile (compile wrap-keyword-params #(get-in % [:params :keywordize] false))})

(def session-middleware
  {:name ::session
   :compile (compile wrap-session #(:session % false))})

(def flash-middleware
  {:name ::flash
   :compile (compile wrap-flash #(get-in % [:session :flash] false))})

(def anti-forgery-middleware
  {:name ::anti-forgery
   :compile (compile wrap-anti-forgery #(get-in % [:security :anti-forgery] false))})

(def exception-middleware
  {:name ::exception
   :compile (fn [data _]
              (when-let [opts (get-in data [:defaults :exception])]
                (if-let [handlers (:handlers opts)]
                  (exception/create-exception-middleware handlers)
                  exception/exception-middleware)))})

(defn- coerced-request [request coercers]
  (if-let [coerced (if coercers (rc/coerce-request coercers request))]
    (update request :parameters merge coerced)
    request))

(def multipart-coercion-middleware
  "Coerces the multipart params pre-parsed by `multipart-parsing-middleware`
   into `[:parameters :multipart]`.

   Coercion only: the request body is not touched. Must be mounted after the
   `coercion/coerce-request-middleware`, which overwrites `:parameters` and
   would discard anything merged into it earlier.

   It mounts only when multipart params are enabled in the `:defaults` route
   data and the route has a `:multipart` params schema."
  {:name ::multipart-coercion
   :compile (fn [{:keys [parameters coercion] :as data} route-opts]
              (when (and (get-in data [:defaults :params :multipart])
                         (:multipart parameters))
                (let [parameter-coercion {:multipart (rc/->ParameterCoercion
                                                      :multipart-params :string true true)}
                      opts (assoc route-opts ::rc/parameter-coercion parameter-coercion)
                      coercers (rc/request-coercers coercion parameters opts)]
                  {:data {:swagger {:consumes ^:replace #{"multipart/form-data"}}}
                   :wrap (fn [handler]
                           (fn
                             ([request]
                              (-> request
                                  (coerced-request coercers)
                                  (handler)))
                             ([request respond raise]
                              (-> request
                                  (coerced-request coercers)
                                  (handler respond raise)))))})))})

(def ring-defaults-middleware
  "Applies the same middleware as `ring.middleware.defaults/wrap-defaults`,
   but as Reitit data-driven middleware with per-route compilation.

   To configure middleware per route, add `:defaults` route data, maybe using
   one of the curated configurations from `ring.middleware.defaults`."
  [forwarded-remote-addr-middleware
   forwarded-scheme-middleware

   ssl-redirect-middleware
   hsts-middleware

   content-type-options-middleware
   frame-options-middleware
   xss-protection-middleware

   not-modified-middleware
   default-charset-middleware
   content-type-middleware

   ;; We don't add `wrap-file` or `wrap-resource` as this is better done with
   ;; Reitit's `create-resource-handler` and `create-file-handler`.

   absolute-redirects-middleware
   cookies-middleware

   params-middleware
   multipart-parsing-middleware
   nested-params-middleware
   keyword-params-middleware

   session-middleware
   flash-middleware

   anti-forgery-middleware])

(def defaults-middleware
  "All of `ring-defaults-middleware`, plus Reitit's content negotiation
   (using Muuntaja), exception and coercion middleware.

   IMPLEMENTATION NOTE:
   Reitit's multipart middleware parses and coerces in a single wrap, but
   the two halves have contradictory placement requirements: parsing must
   happen before the `nested-params`, `keyword-params` and `anti-forgery`
   middleware, which expect multipart params merged into `:params`, while
   the coercion result only survives after `coerce-request-middleware`,
   which overwrites `:parameters` wholesale (see the note added in 0.10.0
   to `reitit.ring.middleware.multipart`). We split it into two separate
   middlewares — `multipart-parsing-middleware` does parsing via Ring and
   `multipart-coercion-middleware` coerces the same way Reitit does it."
  (conj
   ring-defaults-middleware

   format/format-negotiate-middleware
   format/format-response-middleware

   exception-middleware

   format/format-request-middleware

   coercion/coerce-exceptions-middleware
   coercion/coerce-request-middleware
   coercion/coerce-response-middleware

   multipart-coercion-middleware))
