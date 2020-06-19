const merge2 = require('merge2');
const path = require('path');
const gulp = require('gulp');
const { watch } = require('gulp');
const { exec } = require('child_process');
const rimraf = require('rimraf');
const babel = require('gulp-babel');
const sourcemaps = require('gulp-sourcemaps');
const cached = require('gulp-cached');
const remember = require('gulp-remember');
const argv = require('minimist')(process.argv.slice(2));
const ts = require('gulp-typescript');
const alias = require('./alias').gulp;

const cwd = process.cwd();
const libDir = path.join(cwd, 'lib');
const tsDefaultReporter = ts.reporter.defaultReporter();

const tsConfig = require('./getTSCommonConfig')();

function getBabelCommonConfig() {
  const plugins = [
    [
      'babel-plugin-module-resolver',
      {
        alias,
      },
    ],
    '@babel/plugin-proposal-export-default-from',
    '@babel/plugin-proposal-export-namespace-from',
    ['@babel/plugin-proposal-decorators', {
      legacy: true,
    }],
    ['@babel/plugin-proposal-class-properties', {
      loose: true,
    }],
    ['babel-plugin-import',
      {
        libraryName: 'choerodon-ui',
        style: true,
      },
      'choerodon-ui',
    ], ['babel-plugin-import',
      {
        libraryName: 'choerodon-ui/pro',
        style: true,
      },
      'choerodon-ui-pro',
    ],
    'babel-plugin-lodash',
    ['babel-plugin-try-import', {
      tryImport: 'C7NTryImport',
      hasModule: 'C7NHasModule',
    }],
  ];
  return {
    presets: [
      '@babel/preset-react',
      ['@babel/preset-env', {
        modules: false,
      }],
    ],
    plugins,
  };
}

function babelify(js) {
  const babelConfig = getBabelCommonConfig();
  const stream = js
    .pipe(sourcemaps.init())
    .pipe(babel(babelConfig))
    .pipe(sourcemaps.write('.'));
  return stream;
}

function compileTS() {
  const assets = gulp.src(['react/**/*.@(jpg|png|gif|svg|scss|less|css|html|ico)']);
  let error = 0;
  const source = ['react/**/*.tsx', 'react/**/*.ts'];
  const tsResult = gulp.src(source).pipe(cached('typescript')).pipe(
    ts(tsConfig, {
      error(e) {
        tsDefaultReporter.error(e);
        error = 1;
      },
      finish: tsDefaultReporter.finish,
    }),
  );
  function check() {
    if (error && !argv['ignore-error']) {
      process.exit(1);
    }
  }

  tsResult.on('finish', check);
  tsResult.on('end', check);
  return merge2([babelify(tsResult.js), tsResult.dts, assets]);
}
function compileJS() {
  const source = ['react/**/*.jsx', 'react/**/*.js'];
  return babelify(gulp.src(source).pipe(cached('javascript')));
}
function clear(done) {
  rimraf.sync(libDir);
  done();
}
gulp.task('compile-ts', (done) => { 
  compileTS()
    .pipe(remember('typescript'))
    .pipe(gulp.dest(libDir))
    .on('finish', done);
});
gulp.task('compile-js', (done) => {
  compileJS()  
    .pipe(remember('javascript'))
    .pipe(gulp.dest(libDir))
    .on('finish', done);
});
gulp.task(
  'compile',
  gulp.series(
    clear,
    'compile-ts',
    'compile-js',
  ),
);

function yalc(done) {
  exec('yalc publish --push');
  done();
}
gulp.task('watch', () => {
  const source = ['react/**/*', 'react/**/*'];
  rimraf.sync(libDir);
  watch(source, { ignoreInitial: false }, gulp.series(
    'compile-ts',
    'compile-js', 
    yalc,
  ));
});
