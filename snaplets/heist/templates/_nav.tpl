<ul>
  <li><a href="/">Home</a></li>
  <ifLoggedOut>
    <li><a href="/login">Login</a></li>
    <li><a href="/register">Register</a></li>
  </ifLoggedOut>
  <ifLoggedIn>
    <li><a href="/account">Account</a></li>
    <li><a href="/characters">Character list</a></li>
    <li><a href="/logout">Logout</a></li>
  </ifLoggedIn>
</ul>
