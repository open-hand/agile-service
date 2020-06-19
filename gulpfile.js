const path = require('path');
const gulp = require('gulp');
var watch = require('gulp-watch');
const { exec } = require('child_process')
const rimraf = require('rimraf');
const babel = require('gulp-babel');
const through2 = require('through2');
const alias = require('./alias').gulp;
const cwd = process.cwd();
const libDir = path.join(cwd, 'lib');

function compileAssets() {
  return gulp.src(['react/**/*.@(jpg|png|gif|svg|scss|less|html|ico)']).pipe(gulp.dest(libDir));
}
function getBabelCommonConfig() {
  const plugins = [
    [
      'babel-plugin-module-resolver',
      {
        alias,
      }
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
      '@babel/preset-typescript',
      '@babel/preset-react',
      ['@babel/preset-env', {
        modules: false,
      }],
    ],
    plugins,
  };
}

function babelify(js, dir = '') {
  const babelConfig = getBabelCommonConfig();
  const stream = js.pipe(babel(babelConfig));
  return stream
    // eslint-disable-next-line func-names
    .pipe(through2.obj(function (file, encoding, next) {
      const matches = file.path.match(/(routes|dashboard|guide|entry|entrywithoutsider)\.nunjucks\.(js|jsx)/);
      if (matches) {
        const content = file.contents.toString(encoding);
        file.contents = Buffer.from(content
          .replace(`'{{ ${matches[1]} }}'`, `{{ ${matches[1]} }}`)
          // eslint-disable-next-line quotes
          .replace(`'{{ home }}'`, '{{ home }}')
          // eslint-disable-next-line quotes
          .replace(`'{{ master }}'`, '{{ master }}'));
      }
      this.push(file);
      next();
    }))
    .pipe(gulp.dest(path.join(libDir, dir)));
}

function compileFile() {
  const source = [
    'react/**/*.js',
    'react/**/*.jsx',
  ];
  return babelify(gulp.src(source));
}

async function compile() {
  rimraf.sync(libDir);
  await compileAssets();
  await compileFile();
}

gulp.task('compile', () => {
  return compile();
});
function updateFile() {
  let timer;
  return function () {
    if (timer) {
      clearTimeout(timer)
    }
    timer = setTimeout(() => {
      console.log('content update')
      exec('yalc publish --push')
    }, 1500);
  }
}
const updateFileTask = updateFile()
gulp.task('watch', async () => {
  const source = [
    'react/**/*.js',
    'react/**/*.jsx',
    'react/**/*.ts',
    'react/**/*.tsx',
  ];
  await Promise.all([
    babelify(gulp.src(source).pipe(watch(source))).on('data', updateFileTask),
    gulp.src(['react/**/*.@(jpg|png|gif|svg|scss|less|html|ico)']).pipe(watch(['react/**/*.@(jpg|png|gif|svg|scss|less|html|ico)'])).pipe(gulp.dest(libDir)).on('data', updateFileTask)
  ])
})