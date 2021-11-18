import { axios, stores } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import Api from './Api';

interface IPersonalFilter {
  filterJson: string, // 搜索条件json字符串
  name: string,
}
export interface UPersonalFilter {
  name?: string,
  objectVersionNumber: number,
  default?: boolean
}
const { AppState } = stores;
class PersonalFilterApi extends Api<PersonalFilterApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
  * 查询用户的全部筛选
  * @param userId 默认查询当前用户
  * @param filterTypeCode 默认为agile_issue
  */
  loadAll(userId: number = AppState.userInfo.id, filterTypeCode: string = 'agile_issue') {
    // const { userInfo: { id: userId } } = AppState;
    return this.request({
      method: 'get',
      url: `${this.prefix}/personal_filter/query_all/${userId}`,
      params: {
        filterTypeCode,
      },
    });
  }

  /**
            * 创建我的筛选
            * @param data
            * @param filterTypeCode
            */
  create(data: IPersonalFilter, filterTypeCode: string = 'agile_issue') {
    return this.request({
      url: `${this.prefix}/personal_filter`,
      method: 'post',
      data: {
        filterTypeCode,
        ...data || {},
      },
    });
  }

  /**
           * 更新我的筛选
           * @param filterId
           * @param updateData
           */
  update(filterId: string, updateData: UPersonalFilter):Promise<any> {
    return this.request({
      url: `${this.prefix}/personal_filter/${filterId}`,
      method: 'put',
      data: updateData,
    });
  }

  /**
    * 删除我的筛选
    * @param filterId
    */
  delete(filterId: string) {
    return this.request({
      url: `${this.prefix}/personal_filter/${filterId}`,
      method: 'delete',

    });
  }

  /**
    * 检查名字是否重复
    * @param name
    * @param filterTypeCode
    */
  checkName(name: string, filterTypeCode: string = 'agile_issue') {
    const userId = AppState.userInfo.id;
    return this.request({
      method: 'get',
      url: `${this.prefix}/personal_filter/check_name`,
      params: {
        userId,
        name,
        filterTypeCode,
      },
    });
  }
}

const personalFilterApi = new PersonalFilterApi();
export { personalFilterApi };
