import { axios } from '@choerodon/boot';
// @ts-ignore
import JSONbig from 'json-bigint';
import { getProjectId } from '@/utils/common';
import Api from './Api';

const JSONbigString = JSONbig({ storeAsString: true });

class EpicApi extends Api<EpicApi> {
  get prefix() {
    return `/agile/v1/projects/${this.projectId}`;
  }

  /**
   * 查询当前项目下所有史诗
   */
  loadEpics() {
    return this.request({
      method: 'get',
      url: `${this.prefix}/issues/epics`,
    });
  }

  /**
   * 史诗名检查
   * @param epicName
   */
  checkName(epicName: string, epicId?: string) {
    return this.request({
      method: 'get',
      url: `${this.prefix}/issues/check_epic_name`,
      params: {
        epicName,
        epicId,
      },
    });
  }

  /**
  * 查询当前项目下的史诗，供下拉列表使用
  * @param projectId
  * @param params
  *  @default 1 `page` 页
  *  @default 50 `size` 页大小
  *  @default false `onlyUnCompleted`  仅未完成的史诗
  * @param epicIds 选中的史诗ids 以便第一页可显示
  * @returns
  */
  loadEpicsForSelect = (projectId?: string, params?: { page?: number, size?: number, onlyUnCompleted?: boolean, param?: string }, epicIds?: string[]) => {
    const defaultParams = { page: 1, size: 50, onlyUnCompleted: false };
    let requestConfig: any = {};
    if (params?.size === 0) {
      requestConfig = {
        transformResponse: (data: string) => {
          try {
            return JSONbigString.parse(data)?.content || [];
          } catch (error) {
            return Array.isArray(data) ? data : [];
          }
        },
      };
    }
    return this.request({
      url: `/agile/v1/projects/${projectId || getProjectId()}/issues/epics/select_data`,
      method: 'post',
      params: {
        ...defaultParams,
        ...params,
      },
      data: epicIds,
      ...requestConfig,
    });
  }

  /**
   * 在项目群中获取史诗列表 供下拉列表使用
  * @param params
  *  @default 1 `page` 页
  *  @default 50 `size` 页大小
  *  @default false `onlyUnCompleted`  仅未完成的史诗
  * @param epicIds 选中的史诗ids 以便第一页可显示  *
  *  @returns
   */
  loadProgramEpics = (params?: { page?: number, size?: number, onlyUnCompleted?: boolean, param?: string }, epicIds?: string[]) => {
    const defaultParams = { page: 1, size: 50, onlyUnCompleted: true };
    let requestConfig: any = {};
    if (params?.size === 0) {
      requestConfig = {
        transformResponse: (data: string) => {
          try {
            return JSONbigString.parse(data)?.content || [];
          } catch (error) {
            return Array.isArray(data) ? data : [];
          }
        },
      };
    }
    return this.request({
      method: 'post',
      url: `${this.prefix}/issues/epics/select_program_data`,
      params: {
        ...defaultParams,
        ...params,
      },
      data: epicIds,
      ...requestConfig,
    });
  }

  /**
   * 将批量的issue加入到史诗中
   * @param epicId
   * @param issueIds
   */
  addIssues(epicId: number, issueIds: Array<number>) {
    return axios.post(`${this.prefix}/issues/to_epic/${epicId}`, issueIds);
  }
}

const epicApi = new EpicApi();
const epicConfigApi = new EpicApi(true);
export { epicApi, epicConfigApi };
