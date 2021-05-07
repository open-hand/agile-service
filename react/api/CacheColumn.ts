import { axios } from '@choerodon/boot';
import Api from './Api';

class CacheColumnApi extends Api<CacheColumnApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  getDefault(code:string) {
    // return axios({
    //   method: 'get',
    //   url: `${this.prefix}/`,
    //   params: {
    //     code,
    //   },
    // });
    return new Promise((resolve) => {
      setTimeout(() => {
        resolve(['summary']);
      }, 3000);
    });
  }
}

const cacheColumnApi = new CacheColumnApi();
const cacheColumnApiConfig = new CacheColumnApi(true);
export { cacheColumnApi, cacheColumnApiConfig };
