const config = {
  local: true,
  // use for c7n start
  clientId: 'localhost',
  master: './node_modules/@choerodon/master/lib/master.js',
  projectType: 'choerodon',
  buildType: 'single',
  modules: [
    '.',
  ],
  dashboard: {},
  resourcesLevel: ['site', 'origanization', 'project', 'user'],
};
module.exports = config;
