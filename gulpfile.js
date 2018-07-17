let gulp = require('gulp'),
    plumber = require('gulp-plumber'),
    notify = require('gulp-notify'),
    elm = require('gulp-elm'),
    server = require(__dirname + '/src/server/index');

gulp.task('elm-init', elm.init);

gulp.task('html', () => {
    return gulp.src('src/html/*.html')
        .pipe(gulp.dest('public/'));
});

gulp.task('elm', ['elm-init'], () => {
    return gulp.src('src/elm/*.elm')
        .pipe(plumber({
            errorHandler: (err) => {
                notify.onError({
                    title: "Gulp error in " + err.plugin,
                    message:  err.toString()
                })(err);
            }
        }))
        .pipe(elm.bundle('elm-app.js'))
        .pipe(gulp.dest('public/'));
});

gulp.task('build', ['elm', 'html']);

gulp.task('watch', () => {
    return gulp.watch(['src/**/*.@(elm|html)'], ['build']);
});

gulp.task('start-server', (done) => {
    server.start(done);
});

gulp.task('test', ['build', 'start-server', 'watch']);
