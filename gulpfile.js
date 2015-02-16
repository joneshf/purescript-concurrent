'use strict'

var gulp       = require('gulp')
  , concat     = require('gulp-concat')
  , gulpif     = require('gulp-if')
  , purescript = require('gulp-purescript')
  , run        = require('gulp-run')
  ;

var paths = {
    src: 'src/**/*.purs',
    bowerSrc: [
      'bower_components/purescript-*/src/**/*.purs',
      'bower_components/purescript-*/src/**/*.purs.hs'
    ],
    dest: '',
    docsDest: 'README.md',
    jsSrc: 'src/**/*.js'
};

var options = {
    compiler: {main: 'Concurrent.MVar'},
    pscDocs: {}
};

var compile = function(compiler) {
    var psc = compiler(options.compiler);
    psc.on('error', function(e) {
        console.error(e.message);
        psc.end();
    });
    return gulp.src([paths.src].concat(paths.bowerSrc))
        .pipe(psc)
        .pipe(gulp.dest(paths.dest));
};

gulp.task('example', ['browser'], function() {
    return gulp.src([paths.jsSrc, 'psc.js'])
        .pipe(concat('psc.js'))
        .pipe(gulp.dest('.'))
        .pipe(run('node psc.js'));
});

gulp.task('make', function() {
    return compile(purescript.pscMake);
});

gulp.task('dotPsci', function() {
  gulp.src([paths.src].concat(paths.bowerSrc))
    .pipe(purescript.dotPsci());
});

gulp.task('browser', function() {
    return compile(purescript.psc);
});

gulp.task('docs', function() {
    var pscDocs = purescript.pscDocs(options.pscDocs);
    pscDocs.on('error', function(e) {
        console.error(e.message);
        pscDocs.end();
    });
    return gulp.src(paths.src)
      .pipe(pscDocs)
      .pipe(gulp.dest(paths.docsDest));
});

gulp.task('watch-browser', function() {
    gulp.watch(paths.src, ['browser', 'docs']);
});

gulp.task('watch-make', function() {
    gulp.watch(paths.src, ['make', 'dotPsci', 'docs']);
});

gulp.task('default', ['make', 'dotPsci', 'docs']);
