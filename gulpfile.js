const merge2 = require('merge2');
const path = require('path');
const gulp = require('gulp');
const watch = require('gulp-watch');
const { exec } = require('child_process');
const rimraf = require('rimraf');
const babel = require('gulp-babel');
const sourcemaps = require('gulp-sourcemaps');
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
  const tsResult = gulp.src(source).pipe(
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
  return babelify(gulp.src(source));
}
gulp.task('compile-ts', (done) => {
  rimraf.sync(libDir);
  compileTS()
    .pipe(gulp.dest(libDir))
    .on('finish', done);
});
gulp.task('compile-js', (done) => {
  compileJS()
    .pipe(gulp.dest(libDir))
    .on('finish', done);
});
gulp.task(
  'compile',
  gulp.parallel(
    'compile-ts',
    'compile-js',
  ),
);


function updateFile() {
  let timer;
  return function () {
    if (timer) {
      clearTimeout(timer);
    }
    timer = setTimeout(() => {
      // eslint-disable-next-line no-console
      console.log('content update');
      exec('yalc publish --push');
    }, 1500);
  };
}
const updateFileTask = updateFile();
gulp.task('watch', async () => {
  const source = [
    'react/**/*.js',
    'react/**/*.jsx',
    'react/**/*.ts',
    'react/**/*.tsx',
  ];
  await Promise.all([
    babelify(gulp.src(source).pipe(watch(source))).on('data', updateFileTask),
    gulp.src(['react/**/*.@(jpg|png|gif|svg|scss|less|html|ico)']).pipe(watch(['react/**/*.@(jpg|png|gif|svg|scss|less|html|ico)'])).pipe(gulp.dest(libDir)).on('data', updateFileTask),
  ]);
});
