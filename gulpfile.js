'use strict'

var gulp        = require('gulp')
  , bump        = require('gulp-bump')
  , concat      = require('gulp-concat')
  , filter      = require('gulp-filter')
  , git         = require('gulp-git')
  , gulpif      = require('gulp-if')
  , purescript  = require('gulp-purescript')
  , run         = require('gulp-run')
  , runSequence = require('run-sequence')
  , tagVersion  = require('gulp-tag-version')
  ;

var paths = {
    src: 'src/**/*.purs',
    bowerSrc: [
      'bower_components/purescript-*/src/**/*.purs',
      'bower_components/purescript-*/src/**/*.purs.hs'
    ],
    dest: '',
    docsDest: 'README.md',
    jsSrc: 'src/**/*.js',
    manifests: [
      'bower.json',
      'package.json'
    ]
};

var options = {
    compiler: {},
    examples: {main: 'Examples.Concurrent'},
    pscDocs: {}
};

var compile = function(compiler, src, options) {
    var psc = compiler(options);
    psc.on('error', function(e) {
        console.error(e.message);
        psc.end();
    });
    return gulp.src([paths.src].concat(src).concat(paths.bowerSrc))
        .pipe(psc)
        .pipe(gulp.dest(paths.dest));
};

function bumpType(type) {
    return gulp.src(paths.manifests)
        .pipe(bump({type: type}))
        .pipe(gulp.dest('./'));
}

gulp.task('tag', function() {
    return gulp.src(paths.manifests)
        .pipe(git.commit('Update versions.'))
        .pipe(filter('bower.json'))
        .pipe(tagVersion());
});

gulp.task('bump-major', function() {
    return bumpType('major')
});
gulp.task('bump-minor', function() {
    return bumpType('minor')
});
gulp.task('bump-patch', function() {
    return bumpType('patch')
});

gulp.task('bump-tag-major', function() {
    return runSequence('bump-major', 'tag');
});
gulp.task('bump-tag-minor', function() {
    return runSequence('bump-minor', 'tag');
});
gulp.task('bump-tag-patch', function() {
    return runSequence('bump-patch', 'tag');
});

gulp.task('example', function() {
    return compile(purescript.psc, ['examples/**/*.purs'], options.examples)
        .pipe(run('node'));
});

gulp.task('make', function() {
    return compile(purescript.pscMake, [], options.compiler);
});

gulp.task('dotPsci', function() {
  gulp.src([paths.src].concat(paths.bowerSrc))
    .pipe(purescript.dotPsci());
});

gulp.task('browser', function() {
    return compile(purescript.psc, [], options.compiler);
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
