module.exports = {
  modules: [
    '.',
    '@choerodon/base-pro',
  ],
  webpackConfig(config) {
    config.module.rules.push({
      test: /\.(js|jsx|ts|tsx)$/,
      include: /\\@choerodon\\testmanager\\lib/,
      loader: 'babel-loader',
      options: {
        plugins: [
          [
            'babel-plugin-module-resolver',
            {
              alias: {
                '@choerodon/agile/lib': ('./react'),
                '@choerodon/agile': ('./react'),
              },
            },
          ],
        ],
      },
    });
    return config;
  },
};
