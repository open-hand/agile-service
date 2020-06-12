import { axios } from '@choerodon/boot';

class Api {
  isConfig: boolean;

  constructor(isConfig: boolean = false) {
    this.isConfig = isConfig;
  }

  request(AxiosConfig: any) {
    return this.isConfig ? AxiosConfig : axios(AxiosConfig);
  }
}

export default Api;
