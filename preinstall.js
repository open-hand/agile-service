const chProcess = require('child_process');
const fs = require('fs');
// npm install --package-lock-only --ignore-scripts && npx npm-force-resolutions
const userInstallAgent = process.env.npm_config_user_agent || 'npm/6.14.16';
if (!(userInstallAgent.indexOf('npm') === 0)) {
  process.exit(0);
}
if (fs.existsSync('./package-lock.json')) {
  console.log(`${process.env.npm_package_name} preinstall start...`);
  chProcess.exec('npm install --package-lock-only --ignore-scripts;npx npm-force-resolutions').on('error', (e) => {
    console.error(e);
  });
}
