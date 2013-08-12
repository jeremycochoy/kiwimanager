<apply template="_base">
    <p id="head_char">
      Voici la liste de vos personnages :
    </p>
    <character>
      <table class="character type_${characterType}">
        <tr class="character_name">
          <th colspan="2" id="c_name"><characterName /></th>
        </tr>
        <tr class="level">
          <td class="level cellL">Level: </td>
          <td class="level cellR"><characterLevel /></td>
        </tr>
        <tr class="type">
          <td class="type cellL">Class: </td>
          <td class="type cellR"><characterType /></td>
        </tr>
        <tr class="st_pts">
          <td class="st_pts cellL">State points: </td>
          <td class="st_pts cellR"><characterStatePts /></td>
        </tr>
        <tr class="int">
          <td class="int cellL">Inteligence: </td>
          <td class="int cellR"><characterInt /></td>
        </tr>
        <tr class="str">
          <td class="str cellL">Strength: </td>
          <td class="str cellR"><characterStr /></td>
        </tr>
        <tr class="dex">
          <td class="dex cellL">Dexterity: </td>
          <td class="dex cellR"><characterDex /></td>
        </tr>
        <tr class="agi">
          <td class="agi cellL">Agility: </td>
          <td class="agi cellR"><characterAgi /></td>
        </tr>
        <tr class="vit">
          <td class="vit cellL">Vitality: </td>
          <td class="vit cellR"><characterVit /></td>
        </tr>
        <tr class="exp">
          <td class="exp cellL">Experience: </td>
          <td class="exp cellR"><characterExp /></td>
        </tr>
        <tr class="loc">
          <td class="loc cellL">Coordinates: </td>
          <td class="loc cellR"><span class="cX"><characterPosX /></span> - <span class="cY"><characterPosY /></span></td>
        </tr>
      </table>
    </character>
</apply>
