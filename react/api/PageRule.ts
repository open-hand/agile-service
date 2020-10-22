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
  * 查询页面规则
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
      method: 'post',
      url: `${this.prefix}/configuration_rule/${ruleId}/enabled`,
      data: {},
    });
  }

  stopRule(ruleId: string) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/configuration_rule/${ruleId}/disabled`,
      data: {},
    });
  }

  /**
    * 检查名字是否可以通过校验
    * @param name
    */
  checkName(name: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/configuration_rule/check_unique_name`,
      params: {
        name,
      },
    });
  }
}

const pageRuleApi = new PageRuleApi();
const pageRuleApiConfig = new PageRuleApi(true);
export { pageRuleApi, pageRuleApiConfig };
