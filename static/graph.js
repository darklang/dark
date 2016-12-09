stage = new Facade(document.querySelector('#root > canvas'));
textheight = 12;
textwidth = 120
starting_x = 200
starting_y = 200

function ds(name, fields, sx, sy) {
  numFields = Object.keys(fields).length;
  var group = new Facade.Group({
    width: textwidth,
    height: textheight * (numFields + 1),
    x: sx,
    y: sy
  });

  name = new Facade.Text(name, {
    fontSize: textheight,
    fontStyle: "bold",
    strokeStyle: '#333E4B',
    fillStyle: 'black',
    anchor: "bottom/right",
    y: textheight - 2,
    x: textwidth - 2
  })
  rect = new Facade.Rect({
    y: textheight,
    width: textwidth,
    height: textheight * numFields,
    strokeStyle: '#333E4B',
    fillStyle: '#1C73A8'
  });
  group.addToGroup(name)
  group.addToGroup(rect)

  x = 0
  y = textheight // we're one tag in for the name
  for (let f in fields) {
    var field = new Facade.Text(f, {
      fontSize: textheight,
      y: y,
      x: x
    });
    var tag = new Facade.Text(fields[f], {
      y: y,
      x: textwidth - x,
      fontSize: textheight,
      anchor: "right"
    });

    y += textheight;
    group.addToGroup(field);
    group.addToGroup(tag);
  }
  stage.addToStage(group)
}



ds("Post", {"likes": "user", "comments": "comments", "date": "date", "handle": "username"}, starting_x, starting_y)
starting_x += 300
ds("User", {"name": "string", "email": "email", "password": "password", "handle": "username"}, starting_x, starting_y)

Object for the DS, giving the location for the "port" for the inputs
Facade.line
