import { axios } from '@choerodon/boot';
import Api from './Api';

export interface ISearchQuickFilter {
  contents: Array<string>,
  filterName: string
}
interface IQuickFilter {
  name: string,
  childIncluded: boolean, // 是否包含子任务
  expressQuery: string, // 快速搜索表达式
  description: string, // 描述  中间+++
  projectId: number,
  quickFilterValueVOList?: Array<any>, // 快速搜索值
  relationOperations?: Array<string>, // 多个搜索表达式之间的关系
}
interface UQuickFilter extends IQuickFilter {
  objectVersionNumber: number,
}
interface DragQuickFilter {

}
class QuickFilterApi extends Api<QuickFilterApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
   * 根据快速过滤id加载
   * @param filterId
   */
  load(filterId: number | string) {
    return axios.get(`${this.prefix}/quick_filter/${filterId}`);
  }

  /**
    * 加载快速筛选列表
    * @param searchData 理论上可以不传值，但不传值后端抛出错误
    */
  loadAll(searchData: ISearchQuickFilter = { contents: [], filterName: '' }) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/quick_filter/query_all`,
      params: {
        page: 1,
        size: 0,
      },
      data: searchData,
    }).then((res: any = {}) => res.content || []);
  }

  /**
    * 加载快速筛选列表
    * @param searchData 理论上可以不传值，但不传值后端抛出错误
    */
  loadList(searchData: ISearchQuickFilter = { contents: [], filterName: '' }, page: number = 1, size = 10) {
    return this.request({
      method: 'post',
      url: `${this.prefix}/quick_filter/query_all`,
      params: {
        page,
        size,
      },
      data: searchData,
    });
  }

  /**
    * 加载系统字段列表
    */
  loadField() {
    return axios.get(`${this.prefix}/quick_filter/fields`);
  }

  /**
   * 创建快速筛选
   * @param data
   */
  create(data: IQuickFilter) {
    return axios.post(`${this.prefix}/quick_filter`, data, {
      noPrompt: true,
    });
  }

  /**
   * 更新快速筛选
   * @param data
   */
  update(filterId: number | string, data: UQuickFilter) {
    return axios.put(`${this.prefix}/quick_filter/${filterId}`, data, {
      noPrompt: true,
    });
  }

  /**
   * 删除快速筛选
   * @param filterId
   */
  delete(filterId: number) {
    return axios.delete(`${this.prefix}/quick_filter/${filterId}`);
  }

  /**
   * 拖拽对筛选排序
   * @param data
   */
  drag(data: DragQuickFilter) {
    return axios.put(`${this.prefix}/quick_filter/drag`, data);
  }

  /**
   * 检查快速筛选名称是否重复
   * @param quickFilterName
   */
  checkName(quickFilterName: string) {
    return axios({
      method: 'get',
      url: `${this.prefix}/quick_filter/check_name`,
      params: {
        quickFilterName,
      },
    });
  }
}

const quickFilterApi = new QuickFilterApi();
const quickFilterApiConfig = new QuickFilterApi(true);
export { quickFilterApi, quickFilterApiConfig };
