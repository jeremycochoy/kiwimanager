<apply template="_base">
  <form method="post" action="/login">
  <div id="login_error">
  <error_message />
  </div>
  <table id="login_form">
    <tr>
      <td>Login:</td>
      <td><input type="text" name="login" size="20" /></td>
    </tr>
    <tr>
      <td>Password:</td>
      <td><input type="password" name="password" size="20" /></td>
    </tr>
    <tr>
      <td collspan="2" id="submit_cell"><input type="submit" value="Login" /></td>
    </tr>
  </table>
</form>
</apply>
