var express = require('express'),
    httpProxy = require('http-proxy');

var app = express();

app.use(express.static('./public'));

var apiProxy = httpProxy.createProxyServer({});

apiProxy.on('error', (error) => {
    console.log(error.toString());
});

app.use('/api', (req, res) => {
    console.log('forwarding requst through proxy');
    apiProxy.web(req, res, { target: "http://localhost:8000"});
});

exports.start = (done) => {
    let port = 8080;
    return app.listen(port, () => {
        console.log('listening on port' + port);
        done();
    });
};

