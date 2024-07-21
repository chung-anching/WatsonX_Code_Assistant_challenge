var express = require('express');
var app = express();
var port = process.env.PORT || 8080;

app.use(express.static(__dirname + '/www'));
app.get('*', handleRender);

function handleRender(req, res) {
  res.sendFile('index.html', {root: __dirname + '/www'});
};

app.listen(port);
console.log('working on 8080');
