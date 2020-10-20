import { axios, stores } from '@choerodon/boot';
import Api from './Api';

interface ICreateData {
}
interface IUpdateData{

}
const { AppState } = stores;

class PageRuleApi extends Api<PageRuleApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
  * 查询页面贵则
  */
  load(params: { page: number, size: number}) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/configuration_rule`,
      params,
    });
  }

  getPageRuleSystemFields() {
    return this.request({
      method: 'get',
      url: `${this.prefix}/configuration_rule/fields`,
    });
  }

  /**
  * 创建页面规则
  * @param data
  */
  create(data: ICreateData) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/configuration_rule`,
      data,
    });
  }

  getRule(ruleId: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/configuration_rule/${ruleId}`,
    });
  }

  /**
   * 更新页面规则
   * @param ruleId
   * @param data
   */
  update(ruleId: string, data: IUpdateData) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/configuration_rule/${ruleId}`,
      data,
    });
  }

  /**
    * 删除页面规则
    * @param ruleId
    */
  delete(ruleId: string) {
    return this.request({
      method: 'delete',
      url: `${this.prefix}/configuration_rule/${ruleId}`,
    });
  }

  startRule(ruleId: string) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/configuration_rule/${ruleId}`,
    });
  }

  stopRule(ruleId: string) {
    return this.request({
      method: 'put',
      url: `${this.prefix}/configuration_rule/${ruleId}`,
    });
  }

  /**
    * 检查名字是否重复
    * @param name
    */
  checkName(name: string) {
    const userId = AppState.userInfo.id;
    return axios({
      method: 'get',
      url: `${this.prefix}/personal_filter/check_name`,
      params: {
        userId,
        name,
      },
    });
  }
}

const pageRuleApi = new PageRuleApi();
const pageRuleApiConfig = new PageRuleApi(true);
export { pageRuleApi, pageRuleApiConfig };
