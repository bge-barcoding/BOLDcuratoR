"""Round 9, item 9b: only the window BOLDcurator opened may use its server."""

from __future__ import annotations

import asyncio
from urllib.parse import urlsplit

from boldcurator.localguard import TOKEN_PARAM, LocalGuard, is_loopback


async def _ok_app(scope, receive, send):
    if scope["type"] == "websocket":
        await send({"type": "websocket.accept"})
        return
    await send({"type": "http.response.start", "status": 200, "headers": []})
    await send({"type": "http.response.body", "body": b"ok"})


def _call(guard, *, path="/", query="", host="127.0.0.1:8000", cookie="",
          origin="", kind="http"):
    headers = [(b"host", host.encode())]
    if cookie:
        headers.append((b"cookie", cookie.encode()))
    if origin:
        headers.append((b"origin", origin.encode()))
    scope = {"type": kind, "path": path, "query_string": query.encode(),
             "headers": headers}
    sent: list[dict] = []

    async def receive():
        return {"type": "websocket.connect"} if kind == "websocket" else \
            {"type": "http.request", "body": b"", "more_body": False}

    async def send(message):
        sent.append(message)

    asyncio.run(guard(scope, receive, send))
    return sent


def _status(sent):
    first = sent[0]
    return first.get("status") or first["type"]


def _login(guard):
    """Use the launch URL once; return the cookie header the window sends."""
    query = urlsplit(guard.launch_url("http://127.0.0.1:8000")).query
    sent = _call(guard, query=query)
    assert _status(sent) == 303
    headers = dict(sent[0]["headers"])
    assert headers[b"location"] == b"/"
    set_cookie = headers[b"set-cookie"].decode()
    assert "HttpOnly" in set_cookie and "SameSite=Strict" in set_cookie
    return set_cookie.split(";", 1)[0]


def test_nothing_gets_in_without_the_launch_token():
    guard = LocalGuard(_ok_app)
    assert _status(_call(guard)) == 403
    assert _status(_call(guard, query=f"{TOKEN_PARAM}=guess")) == 403
    assert _call(guard, kind="websocket")[0] == {"type": "websocket.close",
                                                  "code": 1008}


def test_the_token_becomes_a_cookie_once_and_the_cookie_lets_everything_in():
    guard = LocalGuard(_ok_app)
    query = urlsplit(guard.launch_url("http://127.0.0.1:8000")).query
    cookie = _login(guard)
    assert _status(_call(guard, cookie=cookie)) == 200
    assert _status(_call(guard, path="/session/x/download/dl_all",
                         cookie=cookie)) == 200
    assert _call(guard, kind="websocket", cookie=cookie)[0]["type"] == \
        "websocket.accept"
    # The token is spent: seeing it later (it is on a command line) is no use.
    assert _status(_call(guard, query=query)) == 403
    # Another server's cookie is not this one's.
    name, value = cookie.split("=", 1)
    assert _status(_call(guard, cookie=f"{name}=x{value}")) == 403
    assert _status(_call(LocalGuard(_ok_app), cookie=cookie)) == 403


def test_other_query_parameters_survive_the_redirect():
    guard = LocalGuard(_ok_app)
    token = urlsplit(guard.launch_url("http://127.0.0.1:8000")).query
    sent = _call(guard, query=f"bc_resume=abc&{token}")
    assert dict(sent[0]["headers"])[b"location"] == b"/?bc_resume=abc"


def test_a_foreign_host_or_origin_is_refused_even_with_the_cookie():
    """DNS rebinding: the attacking page's requests name its own host."""
    guard = LocalGuard(_ok_app)
    cookie = _login(guard)
    assert _status(_call(guard, cookie=cookie, host="evil.example:8000")) == 403
    for host in ("localhost:8000", "[::1]:8000", "127.0.0.1"):
        assert _status(_call(guard, cookie=cookie, host=host)) == 200
    assert _call(guard, kind="websocket", cookie=cookie,
                 origin="http://evil.example")[0]["type"] == "websocket.close"
    assert _call(guard, kind="websocket", cookie=cookie,
                 origin="http://127.0.0.1:8000")[0]["type"] == "websocket.accept"


def test_off_loopback_the_host_check_is_off_but_the_token_is_not():
    guard = LocalGuard(_ok_app, check_host=False)
    assert _status(_call(guard, host="lab-pc.example:8000")) == 403
    cookie = _login(guard)
    assert _status(_call(guard, host="lab-pc.example:8000", cookie=cookie)) == 200


def test_is_loopback():
    assert all(map(is_loopback, ("127.0.0.1", "localhost", "::1", "127.0.0.2")))
    assert not any(map(is_loopback, ("0.0.0.0", "192.168.1.5", "::")))  # nosec B104
