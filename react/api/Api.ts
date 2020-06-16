import { axios } from '@choerodon/boot';
import { AxiosRequestConfig } from 'axios';
import globalCache from './Cache';

interface RequestConfig extends AxiosRequestConfig {
  cache?: boolean
}
function getCacheKey(config: any) {
  const {
    method, url, data, params,
  } = config;
  return JSON.stringify({
    method, url, data, params,
  });
}
class Api {
  isConfig: boolean;

  constructor(isConfig: boolean = false) {
    this.isConfig = isConfig;
  }

  request(AxiosConfig: RequestConfig) {
    if (this.isConfig) {
      return AxiosConfig;
    }
    const { cache } = AxiosConfig;
    if (cache) {
      return new Promise((resolve) => {
        globalCache.apply({
          request: axios.bind(null, AxiosConfig),
          cacheKey: getCacheKey(AxiosConfig),
          callback: resolve,
        });
      });
    } else {
      return axios(AxiosConfig);
    }
  }
}

export default Api;
