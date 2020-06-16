import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

interface ISearchQuickFilter {
    contents: Array<string> | [],
    filterName: string
}
interface IQuickFilter {
    name: string,
    childIncluded: boolean, // 是否包含子任务
    expressQuery: string, // 快速搜索表达式
    description:string, // 描述  中间+++ 
    projectId:number,
    quickFilterValueVOList?: Array<any>, // 快速搜索值
    relationOperations?: Array<string>, // 多个搜索表达式之间的关系
}
class QuickFilterApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
    * 加载快速筛选列表
    * @param searchData 理论上可以不传值，但不传值后端抛出错误
    */
  loadAll(searchData: ISearchQuickFilter = { contents: [], filterName: '' }) {
    return axios({
      method: 'post',
      url: `${this.prefix}/quick_filter/query_all`,
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
  create(data:IQuickFilter) {
    return axios.post(`${this.prefix}/quick_filter`, data);
  }

  /**
   * 检查快速筛选名称是否重复
   * @param quickFilterName 
   */
  checkName(quickFilterName:string) {
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
export { quickFilterApi };
