<apply template="_base">
  <form method="post" action="/register">
  <div id="register_error">
  <error_message />
  </div>
  <table id="register_form">
    <tr>
      <td>Login:</td>
      <td><input type="text" name="login" size="20" /></td>
    </tr>
    <tr class="desc">
      <td>Must be between <minUserLen/> and <maxUserLen/> characters. </td>
      <td></td>
    </tr>
    <tr>
      <td>Password:</td>
      <td><input type="password" name="password" size="20" /></td>
    </tr>
    <tr class="desc">
      <td>Must have more than <minPasswordLen/> characters. </td>
      <td></td>
    </tr>
    <tr>
      <td>Confirm password:</td>
      <td><input type="password" name="confirm_password" size="20" /></td>
    </tr>
    <tr>
      <td>E-Mail:</td>
      <td><input type="text" name="email" size="20" /></td>
    </tr>
    <tr>
      <td collspan="2" id="submit_cell"><input type="submit" value="Register" /></td>
    </tr>
  </table>
</form>
</apply>
