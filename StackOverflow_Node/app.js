
/**
 * Module dependencies.
 */

var express = require('express')
  , routes = require('./routes')
  , user = require('./routes/user')
  , http = require('http')
  , path = require('path');

var app = express();

var sumclassification = require('./routes/Sumclassification');

var que = require('./routes/questionprediction');

// all environments
app.set('port', process.env.PORT || 3000);
app.set('views', __dirname + '/views');
app.set('view engine', 'ejs');
app.use(express.favicon());
app.use(express.logger('dev'));
app.use(express.bodyParser());
app.use(express.methodOverride());
app.use(app.router);
app.use(express.static(path.join(__dirname, 'public')));

// development only
if ('development' == app.get('env')) {
  app.use(express.errorHandler());
}

app.get('/', routes.index);
app.get('/users', user.list);
app.get('/sumclassification',sumclassification.sumclassificationfunction);
app.post('/apriorirelation',sumclassification.apriorirelation);
app.get('/queprediction',que.queprediction);
app.get('/showdata',sumclassification.showdata);
app.post('/top10',sumclassification.topten);
app.post('/hiechartapriori',sumclassification.hiechartapriori);
app.post('/hiechart',sumclassification.hiechart);
app.post('/showSVMclassificationTag',sumclassification.showSVMclassificationTag);
app.post('/showSVMclassificationTag1',sumclassification.showSVMclassificationTag1);

http.createServer(app).listen(app.get('port'), function(){
  console.log('Express server listening on port ' + app.get('port'));
});
