"""Only the window BOLDcurator opened may use its local server.

Round 9, item 9b (``docs/SECURITY_FINDINGS.md``, P1). The app is a web server
on ``127.0.0.1``, and before this any client that could reach the port could
drive it: another user logged in to the same machine, or a web page using DNS
rebinding to make the browser talk to it under a name of its choosing. This
wraps the ASGI app (the app itself and the first-run setup screen) in two
checks, for every HTTP request and websocket alike:

1. **Host.** On a loopback address, a request whose ``Host`` header is not
   ``127.0.0.1``, ``localhost`` or ``[::1]`` is refused. That is what defeats
   DNS rebinding: the rebinding page's requests carry its own host name.
   A websocket's ``Origin``, when sent, must name an allowed host too.
2. **A one-time launch token, swapped for a cookie.** The server makes a
   random token at start and opens its window at ``/?bc_token=<token>``. The
   first request with it gets an ``HttpOnly``, ``SameSite=Strict`` cookie
   holding a *different* random secret, and a redirect to the same page
   without the token. From then on every request needs that cookie. The
   token works once, so seeing it afterwards -- it is on the browser's command
   line, which other users of the machine can list -- is no use. Another
   user's client, or another site's page, has neither.

Bound to anything but loopback (``boldcurator gui --host 0.0.0.0``, a
deliberate developer choice), there is no fixed list of names the server
answers to, so the host check is off; the token and cookie still apply. That
mode serves the app, unencrypted, to the network: anyone who sees the
launch URL before the browser uses it can take the session.

Plain ASGI with no imports beyond the standard library, so it sits outside
``ui/`` and the GUI-free tests can exercise it directly.
"""

from __future__ import annotations

import hmac
import ipaddress
import secrets
import threading
from http.cookies import SimpleCookie
from urllib.parse import parse_qsl, urlencode, urlsplit

TOKEN_PARAM = "bc_token"  # nosec B105 # the query parameter's name, not a secret
#: Each server's cookie gets its own random suffix: cookies are per host, not
#: per port, so two BOLDcurator servers in one browser (the setup screen then
#: the app, or the app opened twice) would otherwise overwrite each other's.
COOKIE_PREFIX = "bc_session_"
LOOPBACK_HOSTS = frozenset({"127.0.0.1", "localhost", "::1"})

_REFUSED_PAGE = (
    b"<!doctype html><meta charset='utf-8'><title>BOLDcurator</title>"
    b"<p style='font-family:sans-serif;margin:2em'>This BOLDcurator window "
    b"isn't the one it opened, so it can't connect. Close it and open "
    b"BOLDcurator again from its shortcut.</p>"
)


def is_loopback(host: str) -> bool:
    if host == "localhost":
        return True
    try:
        return ipaddress.ip_address(host.strip("[]")).is_loopback
    except ValueError:
        return False


def _hostname(value: str) -> str:
    """``Host``-header or URL-authority text to its bare host name."""
    value = value.strip().lower()
    if value.startswith("["):                       # [::1]:8000
        return value[1:].split("]", 1)[0]
    return value.rsplit(":", 1)[0] if value.count(":") == 1 else value


class LocalGuard:
    """ASGI middleware: the host check and the launch token, as above."""

    def __init__(self, app, *, check_host: bool = True,
                 allowed_hosts=LOOPBACK_HOSTS) -> None:
        self.app = app
        self.check_host = check_host
        self.allowed_hosts = frozenset(allowed_hosts)
        self._token = secrets.token_urlsafe(32)
        self._cookie = secrets.token_urlsafe(32)
        self.cookie_name = COOKIE_PREFIX + secrets.token_hex(6)
        self._token_used = False
        self._lock = threading.Lock()

    def launch_url(self, base: str) -> str:
        """``base`` (``http://127.0.0.1:port``) with the one-time token."""
        return f"{base.rstrip('/')}/?{TOKEN_PARAM}={self._token}"

    # -- checks ----------------------------------------------------------

    def _host_ok(self, headers: dict[str, str]) -> bool:
        if not self.check_host:
            return True
        if _hostname(headers.get("host", "")) not in self.allowed_hosts:
            return False
        origin = headers.get("origin")
        if origin and origin != "null":
            return _hostname(urlsplit(origin).netloc) in self.allowed_hosts
        return True

    def _has_cookie(self, headers: dict[str, str]) -> bool:
        jar = SimpleCookie()
        try:
            jar.load(headers.get("cookie", ""))
        except Exception:  # noqa: BLE001 -- a malformed header is just no cookie
            return False
        sent = jar.get(self.cookie_name)
        return sent is not None and hmac.compare_digest(sent.value, self._cookie)

    def _claim_token(self, offered: str) -> bool:
        with self._lock:
            if self._token_used or not hmac.compare_digest(offered, self._token):
                return False
            self._token_used = True
            return True

    # -- ASGI ------------------------------------------------------------

    async def __call__(self, scope, receive, send):
        if scope["type"] not in ("http", "websocket"):
            return await self.app(scope, receive, send)    # lifespan
        headers = {k.decode("latin-1").lower(): v.decode("latin-1")
                   for k, v in scope.get("headers", [])}
        if not self._host_ok(headers):
            return await self._refuse(scope, send)
        if self._has_cookie(headers):
            return await self.app(scope, receive, send)
        query = parse_qsl(scope.get("query_string", b"").decode("latin-1"),
                          keep_blank_values=True)
        offered = next((v for k, v in query if k == TOKEN_PARAM), "")
        if scope["type"] == "http" and offered and self._claim_token(offered):
            rest = urlencode([(k, v) for k, v in query if k != TOKEN_PARAM])
            location = scope.get("path", "/") + (f"?{rest}" if rest else "")
            cookie = (f"{self.cookie_name}={self._cookie}; Path=/; HttpOnly; "
                      "SameSite=Strict")
            await send({"type": "http.response.start", "status": 303,
                        "headers": [(b"location", location.encode("latin-1")),
                                    (b"set-cookie", cookie.encode("latin-1")),
                                    (b"cache-control", b"no-store"),
                                    (b"content-length", b"0")]})
            await send({"type": "http.response.body", "body": b""})
            return None
        return await self._refuse(scope, send)

    @staticmethod
    async def _refuse(scope, send) -> None:
        if scope["type"] == "websocket":
            # Closing before accepting turns the handshake into a 403.
            await send({"type": "websocket.close", "code": 1008})
            return
        await send({"type": "http.response.start", "status": 403,
                    "headers": [(b"content-type", b"text/html; charset=utf-8"),
                                (b"content-length",
                                 str(len(_REFUSED_PAGE)).encode()),
                                (b"cache-control", b"no-store")]})
        await send({"type": "http.response.body", "body": _REFUSED_PAGE})
